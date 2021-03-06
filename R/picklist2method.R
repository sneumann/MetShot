
picklists2methods <-
    function(pickLists, methodPrefix="", ...)
{
    for (i in 1:length(pickLists)) {
        methodname <- paste(methodPrefix,i, sep="_")
        picklist2method(pickLists[[i]], methodname, ...)
        message(paste("Created ", methodname,
                      "with", nrow(pickLists[[i]]), "MS2 regions"))
    }
}





picklist2method <-
    function(pickList, methodPrefix="", MSmode=c("positive","negative"),
             template="test.m/microTOFQAcquisition.method",
             templateSegmentNr=2,
             MSMSManual_ListCollisionEnergy=15,
             MSMSManual_ListIsolationWidth=8,
             instrumentprefix = "qtofacq")
{
    if (is.null(pickList)) {
        warning("Skipping empty picklist")
        return (NULL)
    }

    ## 
    ## MSMSManual_ListCollisionEnergy is now a matrix,
    ## Where the columns are individual method files,
    ## and the rows are repeated scans within a segment 
    ## which can have different collision energies
    ## and isolation widths in the row(s)
    ##

    MSMSManual_ListCollisionEnergyMatrix <- NULL
    
    if (is.null(dim(MSMSManual_ListCollisionEnergy))) {
        MSMSManual_ListCollisionEnergyMatrix <- t(MSMSManual_ListCollisionEnergy)
    } else {
        MSMSManual_ListCollisionEnergyMatrix <- MSMSManual_ListCollisionEnergy
    }
        
    for (j in 1:ncol(MSMSManual_ListCollisionEnergyMatrix)) {

        MSMSManual_LstCollisionEnergy <- MSMSManual_ListCollisionEnergyMatrix[,j]        
        method <- paste(methodPrefix, "-",
                        paste(MSMSManual_LstCollisionEnergy, collapse=","),
                        "eV.m", sep="")
        
        ##
        ## Modify the collision energy in case of negative mode
        ##
        if (MSmode=="positive" & MSMSManual_ListCollisionEnergy <0 ) {
            MSMSManual_ListCollisionEnergy <- -1 * MSMSManual_ListCollisionEnergy
            message(paste("Polarity is positive, changed MSMSManual_ListCollisionEnergy to",
                          MSMSManual_ListCollisionEnergy))
        } else if (MSmode=="negative" & MSMSManual_ListCollisionEnergy >0 ) {
            MSMSManual_ListCollisionEnergy <- -1 * MSMSManual_ListCollisionEnergy
            message(paste("Polarity is negative, changed MSMSManual_ListCollisionEnergy to",
                          MSMSManual_ListCollisionEnergy))
        }

        ##
        ## Load and parse Template
        ##

        tree <-  xmlTreeParse(template)
        root <-  xmlRoot(tree)

        ##
        ## Create a new timetable and add the first segment
        ## which has all the machine settings
        ##

        newTable <- xmlNode("timetable")

        
        ## TODO: add a check if instrumentprefix exists.       

        for (i in seq(1, templateSegmentNr-1) ) {
            firstSegment <- root[["method"]][[instrumentprefix]][["timetable"]][[i]]
            firstSegmentEndtime <- as.numeric(xmlAttrs(firstSegment)["endtime"])
            newTable <- addChildren(newTable, firstSegment)            
        }

        ## templateSegmentNr (e.g. Second) Segment has to be the first MRM in the template
        segmentTemplate <- root[["method"]][[instrumentprefix]][["timetable"]][[templateSegmentNr]]
        segmentTemplateEndtime <- as.numeric(xmlAttrs(segmentTemplate)["endtime"])

        newSegment <- segmentTemplate
        
        if (! "dependent" %in% names(newSegment)) 
          stop(paste("Segment", templateSegmentNr, "incomplete, possibly not MRM ?"))
        
        if (MSmode=="positive") {
            if (xmlAttrs(newSegment[["dependent"]])["polarity"] != "positive")
                stop(paste("Polarity ", MSmode, "at wrong position in template"))
        } else if (MSmode=="negative") {
            if (xmlAttrs(newSegment[["dependent"]])["polarity"] != "negative")
                stop(paste("Polarity ", MSmode, "at wrong position in template"))
        } else {
            stop(paste("Unknown Polarity ", MSmode))
        }



        ##
        ## Some sanity checks
        ##

        if (templateSegmentNr<2) {
          warn("The template method should have at least one segmnet before the MRM Segment nr. ", templateSegmentNr,
               " which should set most of the instrument parameters.")
        }
        
        if (length(root[["method"]][[instrumentprefix]][["timetable"]]) <= templateSegmentNr) {
          stop("The template method requires at least one segment after the MRM Segment nr. ", templateSegmentNr)
        }

        if ( as.integer(value4attribute(newSegment, "Mode_ScanMode")) != 3) {
            cat('value4attribute(newSegment, "Mode_ScanMode") = ', value4attribute(newSegment, "Mode_ScanMode"), "\n")
            stop("Segment nr. ", templateSegmentNr, " in Template file is not MRM.")
        }

        if (length(newSegment[["dependent"]][[1]])>1)
            warning("More than one parent mass in template")


        posMSMSManual_ListIsolationMass <- index4attribute(newSegment[["dependent"]],
                                                           "MSMSManual_ListIsolationMass")
        posMSMSManual_ListIsolationWidth <- index4attribute(newSegment[["dependent"]],
                                                            "MSMSManual_ListIsolationWidth")
        posMSMSManual_ListCollisionEnergy <- index4attribute(newSegment[["dependent"]],
                                                             "MSMSManual_ListCollisionEnergy")

        if (is.null(posMSMSManual_ListIsolationMass))
            stop("Template file missing expected content: MSMSManual_ListIsolationMass not here.")
        if (is.null(posMSMSManual_ListIsolationWidth))
            stop("Template file missing expected content: MSMSManual_ListIsolationWidth not here.")
        if (is.null(posMSMSManual_ListCollisionEnergy))
            stop("Template file missing expected content: MSMSManual_ListCollisionEnergy not here.")

        ##
        ## Create the new segments
        ##


        for (i in 1:nrow(pickList)) {
            if (pickList[i,"rtmax"]/60 < firstSegmentEndtime) {
                warning("The end of pick item ", i,
                        "at", pickList[i,"rtmax"]/60 ,
                        "is before the end of the template segment at", firstSegmentEndtime);
                next;
            }

            newSegment <- segmentTemplate
            newSegment <- removeAttributes(newSegment, "endtime")

            if (i < nrow(pickList)) {
                newSegment <- addAttributes(newSegment,
                                            .attrs=c(endtime = pickList[i,"rtmax"]/60))
            } else {
                newSegment <- addAttributes(newSegment,
                                            .attrs=c(endtime = segmentTemplateEndtime))
            }

            for (k in 1:length(MSMSManual_LstCollisionEnergy)) {
            newSegment[["dependent"]][[posMSMSManual_ListIsolationMass]][[k]] <- xmlNode("entry_double",
                                                                                         attrs=c(value=pickList[i,"mzmed"]))
            }

            ## MSMSManual_ListIsolationWidth <- rbind(mzmin=c(mz=150, MSMSManual_ListIsolationWidth=1),
            ##                                        mzmax=c(mz=900, MSMSManual_ListIsolationWidth=3))

            if (!missing(MSMSManual_ListIsolationWidth) && class(MSMSManual_ListIsolationWidth)=="matrix" && ncol(MSMSManual_ListIsolationWidth == 2)) {
                ## linear interpolation
                isomin <- MSMSManual_ListIsolationWidth["mzmin","MSMSManual_ListIsolationWidth"]
                isomax <- MSMSManual_ListIsolationWidth["mzmax","MSMSManual_ListIsolationWidth"]
                mzmin <- MSMSManual_ListIsolationWidth["mzmin","mz"]
                mzmax <- MSMSManual_ListIsolationWidth["mzmax","mz"]
                mz <- pickList[i,"mzmed"]

                ## Rounded (ceiling) with one decimal
                currentMSMSManual_ListIsolationWidth <- ceiling(10*(isomin + ( ((mz-mzmin)*isomax-(mz-mzmin)*isomin ) / (mzmax - mzmin)   )))/10
            } else {
                currentMSMSManual_ListIsolationWidth <- MSMSManual_ListIsolationWidth
            }

            for (k in 1:length(MSMSManual_LstCollisionEnergy)) {
            newSegment[["dependent"]][[posMSMSManual_ListIsolationWidth]][[k]] <- xmlNode("entry_double",
                                                                                          attrs=c(value=currentMSMSManual_ListIsolationWidth))

            newSegment[["dependent"]][[posMSMSManual_ListCollisionEnergy]][[k]] <- xmlNode("entry_double",
                                                                                           attrs=c(value=MSMSManual_ListCollisionEnergy[k]))
            }

            ## Add this segment to the table
            newTable <- addChildren(newTable, newSegment)
        }


        for (i in seq(templateSegmentNr+1, length(root[["method"]][[instrumentprefix]][["timetable"]])) ) {
            lastSegment <- root[["method"]][[instrumentprefix]][["timetable"]][[i]]
            newTable <- addChildren(newTable, lastSegment)            
        }

        root[["method"]][[instrumentprefix]][["timetable"]] <- newTable

        ##
        ## Copy the template directory
        ## and inject the new method
        ##

        unlink(method, recursive=TRUE)
        dir.create(method, recursive=TRUE)
        file.copy(list.files(dirname(template), full.names=TRUE),
                  method, recursive = TRUE)

        saveXML(root, file=paste(method, basename(template), sep="/"))
    }
    invisible(pickList[,,drop=FALSE])
}

