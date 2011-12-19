picklist2waters <- 
  function(pickList, methodPrefix="", MSmode=c("positive","negative"),
           template="test.exp",
           MSMSManual_ListCollisionEnergy=15,
           MSMSManual_ListIsolationWidth=8)
{
  for (MSMSManual_ListCollisionEnergy in MSMSManual_ListCollisionEnergy) {
    method <- paste(methodPrefix, "-", MSMSManual_ListCollisionEnergy, "eV.EXP", sep="")
    ##
    ## Load and parse Template
    ##

    d <- readLines(template)

    headerStart <- grep("GENERAL INFORMATION", d)[1]
    headerEnd   <- grep("FUNCTION 1", d)[1]-1
    headerBlock <- d[headerStart:headerEnd]

    expFile <- list()
    expFile[[1]] <- headerBlock

    templateStart <- grep("FUNCTION 1", d)[1]
    templateEnd   <- grep("FUNCTION 2", d)[1]-1
    templateBlock <- d[templateStart:templateEnd]

    
    ##
    ## Some sanity checks
    ##

    ## TBD.
    


    ##
    ## Create the new segments
    ##
    

    for (i in 1:nrow(pickList)) {
      cat ("pick", i, "\n")
      newFunction <- templateBlock
      newFunction[1] <- paste("FUNCTION", i)

      ## Set start/end time
      newFunction[grep("FunctionStartTime(min)", newFunction, fixed=TRUE)] <- paste("FunctionStartTime(min)",
                                                                pickList[i,"rtmin"]/60, sep=",")
      newFunction[grep("FunctionEndTime(min)", newFunction, fixed=TRUE)] <- paste("FunctionEndTime(min)",
                                                              pickList[i,"rtmax"]/60, sep=",")

      
      ## Set collision energy

      newFunction[grep("TOFSetMass", newFunction, fixed=TRUE)] <- paste("TOFSetMass",
                                                    pickList[i,"mzmed"], sep=",")
      newFunction[grep("TOFCollisionEnergy", newFunction, fixed=TRUE)] <- paste("TOFCollisionEnergy",
                                                            MSMSManual_ListCollisionEnergy, sep=",")
      
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

    write.table(unlist(expFile), method,
                sep = ",", quote = FALSE, row.names = FALSE, col.names=FALSE)

  }
  invisible(pickList[,,drop=FALSE])
}

## picklist <- rbind(c(mzmed=666, rtmin=111, rtmax=222), c(mzmed=777, rtmin=222, rtmax=333))
## picklist2waters (picklist, MSmode="negative", method="tryptophane", 
##                  template="/vol/R/BioC/devel-29/MetShot/inst/waters-template/tryptophan5_30eV_30eV.EXP",
##                  MSMSManual_ListCollisionEnergy=15,
##                  MSMSManual_ListIsolationWidth=8)
