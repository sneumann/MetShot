picklist2waters <- 
  function(pickList, methodPrefix="", MSmode=c("positive","negative"),
           template="test.exp",
           MSMSManual_ListCollisionEnergy=15,
           MSMSManual_ListIsolationWidth=8,
           TOFConeVoltage=NULL,
           lockMassMZ=NULL,
           calibrationFileName=NULL)
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
    
    headerBlock[grep("PositivePolarity,", headerBlock, fixed=TRUE)] <- paste("PositivePolarity", ifelse (MSmode=="positive", "1", "0"), sep=",")

    if (!missing(calibrationFileName)) {
      headerBlock[grep("ExperimentCalibrationFilename,", headerBlock, fixed=TRUE)] <- paste("ExperimentCalibrationFilename", calibrationFileName, sep=",")
    }

    if (!missing(lockMassMZ)) {
      headerBlock[grep("ReferenceSetMass,", headerBlock, fixed=TRUE)] <- paste("ReferenceSetMass", lockMassMZ, sep=",")
      usedLockMassMZ <- lockMassMZ
    } else {
      usedLockMassMZ <- grep("ReferenceSetMass,([0-9.]*)", headerBlock, fixed=TRUE)
    }

   
    numberFuncsIdx <- grep("^NumberOfFunctions,", headerBlock) 
    headerBlock[numberFuncsIdx] <- paste("NumberOfFunctions", nrow(pickList), sep=",")

    typeFuncsIdx <- grep("^FunctionTypes,", headerBlock)
    headerBlock[typeFuncsIdx] <- paste("FunctionTypes",
                                         paste(rep("Tof MSMS", nrow(pickList)), collapse=","), sep=",")
    
    footerStart <- grep("^MaldiLaserType", d)[1]
    footerEnd   <- length(d)
    footerBlock <- d[footerStart:footerEnd]

    if (!missing(lockMassMZ)) {
      footerBlock[grep("CentroidLockMass,", footerBlock, fixed=TRUE)] <- paste("CentroidLockMass", lockMassMZ, sep=",")
    }

    
    expFile <- list()
    expFile[[1]] <- headerBlock

    templateStart <- grep("FUNCTION 1", d)[1]
    templateEnd   <- grep("FUNCTION 2", d)[1]-1
    templateBlock <- d[templateStart:templateEnd]

    templateBlock[grep("FunctionPolarity,", templateBlock, fixed=TRUE)] <- paste(polarity, MSmode, sep="FunctionPolarity,")

    
    ##
    ## Some sanity checks
    ##

    ## TBD.
    

    ## Make sure segments don't overlap
    for (i in 2:nrow(pickList)) {
      if (pickList[i,"rtmin"] == pickList[i-1,"rtmax"]) {
        pickList[i-1,"rtmax"] <- pickList[i-1,"rtmax"]-0.5
        pickList[i,"rtmin"] <- pickList[i,"rtmin"]+0.5
      }        
    }
    
    ##
    ## Create the new segments
    ##
    

    for (i in 1:nrow(pickList)) {
      cat ("pick", i, "\n")
      newFunction <- templateBlock
      newFunction[1] <- paste("FUNCTION", i)

      ## Set Polarity for each function
      newFunction[grep("FunctionPolarity,", newFunction, fixed=TRUE)] <- paste("FunctionPolarity", ifelse (MSmode=="positive", "Positive", "Negative"), sep=",")
      
      ## Set start/end time
      newFunction[grep("FunctionStartTime(min)", newFunction, fixed=TRUE)] <- paste("FunctionStartTime(min)",
                                                                pickList[i,"rtmin"]/60, sep=",")
      newFunction[grep("FunctionEndTime(min)", newFunction, fixed=TRUE)] <- paste("FunctionEndTime(min)",
                                                              pickList[i,"rtmax"]/60, sep=",")
      
      ## Isolation mass and scan window
      newFunction[grep("TOFSetMass", newFunction, fixed=TRUE)] <- paste("TOFSetMass",
                                                    pickList[i,"mzmed"], sep=",")

      newFunction[grep("FunctionStartMass", newFunction, fixed=TRUE)] <- paste("FunctionStartMass",
                                                                40, sep=",")
      newFunction[grep("FunctionEndMass", newFunction, fixed=TRUE)] <- paste("FunctionEndMass",
                                                              max(usedLockMassMz, round(pickList[i,"mzmed"], -2))+100, sep=",")
      
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
      expFile[[i+1]] <- newFunction
      
    }


    ##
    ## Copy the template directory
    ## and inject the new method
    ##

    expFile[[i+2]] <- footerBlock
    write.table(unlist(expFile), method,
                sep = ",", quote = FALSE, row.names = FALSE, col.names=FALSE)

  }
  invisible(pickList[,,drop=FALSE])
}

## picklist <- rbind(c(mzmed=222, rtmin=331, rtmax=333), c(mzmed=333, rtmin=441, rtmax=442),
##                      c(mzmed=666, rtmin=111, rtmax=222), c(mzmed=777, rtmin=222, rtmax=333))
## picklist2waters (picklist, MSmode="negative", method="tryptophane", 
##                  template="/vol/R/BioC/devel-29/MetShot/inst/waters-template/tryptophan5_30eV_30eV.EXP",
##                  MSMSManual_ListCollisionEnergy=15,
##                  MSMSManual_ListIsolationWidth=8)
