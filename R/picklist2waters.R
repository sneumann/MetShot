picklists2waters <- function(pickLists, methodPrefix, ...)  
{
    for (i in 1:length(pickLists)) {
        methodname <- paste(methodPrefix,i, sep="_")
        
        picklist2waters(pickList=pickLists[[i]], methodPrefix=methodname, ...)
      }
}


picklist2waters <- 
  function(pickList, methodPrefix="", MSmode=c("positive","negative"),
           template=NULL,
           MSMSManual_ListCollisionEnergy=15,
           TOFConeVoltage=NULL,
           lockMassMZ=NULL
           # ,calibrationFileName=NULL
           )
{

  polarity <- ifelse (MSmode=="negative", "Negative", "Positive")
  
  for (MSMSManual_ListCollisionEnergy in MSMSManual_ListCollisionEnergy) {
    method <- paste(methodPrefix, "-", MSMSManual_ListCollisionEnergy, "eV.EXP", sep="")
    ##
    ## Load and parse Template
    ##

    d <- readLines(template)

    headerStart <- grep("GENERAL INFORMATION", d)[1]
    headerEnd   <- grep("FUNCTION 1", d)[1]-1
    headerBlock <- d[headerStart:headerEnd]

    ExperimentDuration <- sub("ExperimentDuration,([0-9.]*)", "\\1", headerBlock[grep("ExperimentDuration,([0-9.]*)", headerBlock)])
    ExperimentDuration <- as.numeric(ExperimentDuration)
    
    headerBlock[grep("PositivePolarity,", headerBlock, fixed=TRUE)] <- paste("PositivePolarity", ifelse (MSmode=="positive", "1", "0"), sep=",")

    ## if (!missing(calibrationFileName)) {
    ##   headerBlock[grep("ExperimentCalibrationFilename,", headerBlock, fixed=TRUE)] <- paste("ExperimentCalibrationFilename", calibrationFileName, sep=",")
    ## }

    if (!missing(lockMassMZ)) {
      headerBlock[grep("ReferenceSetMass,", headerBlock, fixed=TRUE)] <- paste("ReferenceSetMass", lockMassMZ, sep=",")
      usedLockMassMZ <- lockMassMZ
    } else {
      usedLockMassMZ <- sub("ReferenceSetMass,([0-9.]*)", "\\1", headerBlock[grep("ReferenceSetMass,([0-9.]*)", headerBlock)])
      usedLockMassMZ <- as.numeric(usedLockMassMZ)
    }
       
    footerStart <- grep("^MaldiLaserType", d)[1]
    footerEnd   <- length(d)
    footerBlock <- d[footerStart:footerEnd]

    if (!missing(lockMassMZ)) {
      footerBlock[grep("CentroidLockMass,", footerBlock, fixed=TRUE)] <- paste("CentroidLockMass", lockMassMZ, sep=",")
    }

    
    expFile <- list()

    templateStart <- grep("FUNCTION 1", d)[1]
    templateEnd   <- grep("FUNCTION 2", d)[1]-1
    templateBlock <- d[templateStart:templateEnd]

    templateBlock[grep("FunctionPolarity,", templateBlock, fixed=TRUE)] <- paste(polarity, MSmode, sep="FunctionPolarity,")

    
    ##
    ## Some sanity checks
    ##

    ## TBD.
    

    ## Make sure segments don't overlap
    if (nrow(pickList) > 1) {
    for (i in 2:nrow(pickList)) {
      if (pickList[i,"rtmin"] == pickList[i-1,"rtmax"]) {
        pickList[i-1,"rtmax"] <- pickList[i-1,"rtmax"]-0.5
        pickList[i,"rtmin"] <- pickList[i,"rtmin"]+0.5
      }        
    }
  }
    ##
    ## Create the new segments
    ##
    
    functionNr <- 1
    for (i in 1:nrow(pickList)) {

      newFunction <- templateBlock
      newFunction[1] <- paste("FUNCTION", functionNr)

      ## Set Polarity for each function
      newFunction[grep("FunctionPolarity,", newFunction, fixed=TRUE)] <- paste("FunctionPolarity", ifelse (MSmode=="positive", "Positive", "Negative"), sep=",")
      
      ## Set start/end time
      rtmin <- pickList[i,"rtmin"]/60
      rtmax <- pickList[i,"rtmax"]/60

      if (rtmax < 0) {
        warning("Skipping MS/MS function with negative end time ", rtmax)
        next();
      }
      if (rtmin < 0) {
        warning("Truncating MS/MS function with negative start time ", rtmin, " to zero")
        rtmin <- 0
      }
      if (rtmin > ExperimentDuration) {
        warning("Skipping MS/MS function beyond ExperimentDuration ", ExperimentDuration,
                " having start time ", rtmin, " to zero")
        next()
      }
      if (rtmax > ExperimentDuration) {
        warning("Truncating MS/MS function end time ", rtmax,
                " beyond ExperimentDuration ", ExperimentDuration)
        rtmax <- ExperimentDuration
      }

            
      newFunction[grep("FunctionStartTime(min)", newFunction, fixed=TRUE)] <- paste("FunctionStartTime(min)",
                                                                rtmin, sep=",")
      newFunction[grep("FunctionEndTime(min)", newFunction, fixed=TRUE)] <- paste("FunctionEndTime(min)",
                                                              rtmax, sep=",")
      
      ## Isolation mass and scan window
      newFunction[grep("TOFSetMass", newFunction, fixed=TRUE)] <- paste("TOFSetMass",
                                                    pickList[i,"mzmed"], sep=",")

      newFunction[grep("FunctionStartMass", newFunction, fixed=TRUE)] <- paste("FunctionStartMass",
                                                                40, sep=",")
      newFunction[grep("FunctionEndMass", newFunction, fixed=TRUE)] <- paste("FunctionEndMass",
                                                              max(usedLockMassMZ, round(pickList[i,"mzmed"], -2))+100, sep=",")
      
      ## Set collision energy
      newFunction[grep("TOFCollisionEnergy", newFunction, fixed=TRUE)] <- paste("TOFCollisionEnergy",
                                                            MSMSManual_ListCollisionEnergy, sep=",")

      if (!missing(TOFConeVoltage)) {
        newFunction[grep("TOFConeVoltage,", newFunction, fixed=TRUE)] <- paste("TOFConeVoltage",
                                                           TOFConeVoltage, sep=",")
      }

      CEProfileIdx <- grep("^CEProfile[0-9][0-9]", newFunction)
      CEProfiles <- t(sapply(strsplit(newFunction[CEProfileIdx], ","), function(x) x))
      CEProfiles[,2] <- MSMSManual_ListCollisionEnergy
      CEProfiles <- apply(CEProfiles, 1, function(x) paste(x, collapse=","))
      newFunction[CEProfileIdx] <- CEProfiles

      ## Add this segment to the table
      expFile[[functionNr]] <- newFunction
      functionNr <- functionNr +1 
    }
    
    ##
    ## Now write the (actual) number of Functions into the Header
    ##
    numberFuncsIdx <- grep("^NumberOfFunctions,", headerBlock) 
    headerBlock[numberFuncsIdx] <- paste("NumberOfFunctions", functionNr-1,  sep=",")

    typeFuncsIdx <- grep("^FunctionTypes,", headerBlock)
    headerBlock[typeFuncsIdx] <- paste("FunctionTypes",
                                         paste(rep("Tof MSMS", functionNr-1), collapse=","), sep=",")

    if (functionNr <= 1) {
      ## Don't write empty EXP files
      next()
    }
    
    dir.create(dirname(method), recursive = TRUE, showWarnings = FALSE)
    write.table(c(headerBlock, unlist(expFile), footerBlock), method,
                sep = ",", quote = FALSE, row.names = FALSE, col.names=FALSE)

    message(paste("Created ", method,
                  "with", functionNr-1, "MS2 regions"))

  }
  invisible(pickList[,,drop=FALSE])
}

## picklist <- rbind(c(mzmed=222, rtmin=331, rtmax=333), c(mzmed=333, rtmin=441, rtmax=442),
##                      c(mzmed=666, rtmin=111, rtmax=222), c(mzmed=777, rtmin=222, rtmax=333))
## picklist2waters (picklist, MSmode="negative", method="tryptophane", 
##                  template="/vol/R/BioC/devel-29/MetShot/inst/waters-template/tryptophan5_30eV_30eV.EXP",
##                  MSMSManual_ListCollisionEnergy=15,
##                  MSMSManual_ListIsolationWidth=8)
