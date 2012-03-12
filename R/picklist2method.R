
picklists2methods <-
function(pickLists, methodPrefix="", MSmode=c("positive","negative"),
                        template="test.method",
                        MSMSManual_ListCollisionEnergy=15,
                        MSMSManual_ListIsolationWidth=8)
  {
    for (i in 1:length(pickLists)){
        methodname <- paste(methodPrefix,"_",i, ".m", sep="")
        picklist2method(pickLists[[i]], methodname, MSmode, template,
                    MSMSManual_ListCollisionEnergy=MSMSManual_ListCollisionEnergy,
                    MSMSManual_ListIsolationWidth=MSMSManual_ListIsolationWidth)

        message(paste("Created ", methodname,
                      "with", nrow(pickLists[[i]]), "MS2 regions"))
  }
}





picklist2method <-
function(pickList, methodPrefix="", MSmode=c("positive","negative"),
                        template="test.method",
                        MSMSManual_ListCollisionEnergy=15,
                        MSMSManual_ListIsolationWidth=8)
{
    require(XML) || stop("Couldn't load package XML")

    for (MSMSManual_ListCollisionEnergy in MSMSManual_ListCollisionEnergy) {
        method <- paste(methodPrefix, "-", MSMSManual_ListCollisionEnergy, "eV.m", sep="")
  ##
  ## Load and parse Template
  ##

  tree <-  xmlTreeParse(paste(template, "microTOFQAcquisition.method", sep="/"))
  root <-  xmlRoot(tree)

  ##
  ## Create a new timetable and add the first segment
  ## which has all the machine settings
  ##

  newTable <- xmlNode("timetable")

    firstSegment <- root[["method"]][["qtofacq"]][["timetable"]][[1]]
  newTable <- addChildren(newTable, firstSegment)

  ## Second Segment has to be the first MRM in the template
  segmentTemplate <- root[["method"]][["qtofacq"]][["timetable"]][[2]]

  newSegment <- segmentTemplate
  dependentNr <- 2

##    if (MSmode=="positive") {
##      if (xmlAttrs(newSegment[[dependentNr]])["polarity"] != "positive")
##          stop(paste("Polarity ", MSmode, "at wrong position in template"))
##    } else {
##      if (xmlAttrs(newSegment[[dependentNr]])["polarity"] != "positive")
##          stop(paste("Polarity ", MSmode, "at wrong position in template"))
##    }



  ##
  ## Some sanity checks
  ##


  if ("3.000000" != value4attribute(newSegment, "Mode_ScanMode")) {
      stop("Second Segment in Template file is not MRM.")
  }

  if (length(newSegment[[dependentNr]][[1]])>1)
    warning("More than one parent mass in template")


  posMSMSManual_ListIsolationMass <- index4attribute(newSegment[[dependentNr]],
                                                     "MSMSManual_ListIsolationMass")
  posMSMSManual_ListIsolationWidth <- index4attribute(newSegment[[dependentNr]],
                                                      "MSMSManual_ListIsolationWidth")
  posMSMSManual_ListCollisionEnergy <- index4attribute(newSegment[[dependentNr]],
                                                       "MSMSManual_ListCollisionEnergy")

  if (is.null(posMSMSManual_ListIsolationMass))
      stop("Template file corrupt: MSMSManual_ListIsolationMass not here.")
  if (is.null(posMSMSManual_ListIsolationWidth))
      stop("Template file corrupt: MSMSManual_ListIsolationWidth not here.")
  if (is.null(posMSMSManual_ListCollisionEnergy))
      stop("Template file corrupt: MSMSManual_ListCollisionEnergy not here.")

  ##
  ## Create the new segments
  ##


  for (i in 1:nrow(pickList)) {
    newSegment <- segmentTemplate
    newSegment <- removeAttributes(newSegment, "endtime")
    newSegment <- addAttributes(newSegment,
                                .attrs=c(endtime = pickList[i,"rtmax"]/60))

    newSegment[[dependentNr]][[posMSMSManual_ListIsolationMass]][[1]] <- xmlNode("entry_double",
                                                   attrs=c(value=pickList[i,"mzmed"]))


     ## MSMSManual_ListIsolationWidth <- rbind(mzmin=c(mz=150, MSMSManual_ListIsolationWidth=1),
     ##                                        mzmax=c(mz=900, MSMSManual_ListIsolationWidth=3))

    if (!missing(MSMSManual_ListIsolationWidth) && class(MSMSManual_ListIsolationWidth)=="matrix" && ncol(MSMSManual_ListIsolationWidth == 2)) {
      ## linear interpolation
      isomin <- MSMSManual_ListIsolationWidth["mzmin","MSMSManual_ListIsolationWidth"]
      isomax <- MSMSManual_ListIsolationWidth["mzmax","MSMSManual_ListIsolationWidth"]
      mzmin <- MSMSManual_ListIsolationWidth["mzmin","mz"]
      mzmax <- MSMSManual_ListIsolationWidth["mzmax","mz"]
      mz <- pickList[i,"mzmed"]

      currentMSMSManual_ListIsolationWidth <- isomin + ( ((mz-mzmin)*isomax-(mz-mzmin)*isomin ) / (mzmax - mzmin)   )
    } else {
      currentMSMSManual_ListIsolationWidth <- MSMSManual_ListIsolationWidth
    }
    currentMSMSManual_ListIsolationWidth


    newSegment[[dependentNr]][[posMSMSManual_ListIsolationWidth]][[1]] <- xmlNode("entry_double",
                                                    attrs=c(value=currentMSMSManual_ListIsolationWidth))

    newSegment[[dependentNr]][[posMSMSManual_ListCollisionEnergy]][[1]] <- xmlNode("entry_double",
                                                    attrs=c(value=MSMSManual_ListCollisionEnergy))

    ## Add this segment to the table
    newTable <- addChildren(newTable, newSegment)
  }

  ## Manuallz Add the Calibration Segment
  newSegment <- root[["method"]][["qtofacq"]][["timetable"]][[3]]
  newSegment <- removeAttributes(newSegment, "endtime")
  newSegment <- addAttributes(newSegment, .attrs=c(endtime = "18"))
  newTable <- addChildren(newTable, newSegment)

  newTable <- addChildren(newTable, root[["method"]][["qtofacq"]][["timetable"]][[3]])
  newTable <- addChildren(newTable, root[["method"]][["qtofacq"]][["timetable"]][[4]])

  root[["method"]][["qtofacq"]][["timetable"]] <- newTable

  ##
  ## Copy the template directory
  ## and inject the new method
  ##

  unlink(method, recursive=TRUE)
  dir.create(method, recursive=TRUE)
  file.copy(list.files(template, full.names=TRUE),
            method, recursive = TRUE)

  saveXML(root, file=paste(method, "microTOFQAcquisition.method", sep="/"))
}
  invisible(pickList[,,drop=FALSE])
}

