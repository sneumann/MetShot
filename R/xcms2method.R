xcms2method <-
function(peaklist, methodPrefix="", MSmode=c("positive","negative"),
                        template="test.method",
                        gradientStart=NULL, gradientEnd=NULL,
                        widthFactor=1, minWidth=1,
                        MSMSManual_ListCollisionEnergy=15,
                        MSMSManual_ListIsolationWidth=8)
{


    pickLists <- peaklist2picklist(peaklist,
                                  gradientStart=gradientStart, gradientEnd=gradientEnd,
                                  widthFactor=widthFactor,
                                  minWidth=minWidth)

    if (is.null(pickLists)) {
        warning("No suitable peaks found, returning empty picklist")
        return(NULL)
    }
    
    for (i in 1:length(pickLists)){
        methodname<-paste(methodPrefix,"_",i, ".m", sep="")
        picklist2method(pickLists[[i]], methodname, MSmode, template,
                    MSMSManual_ListCollisionEnergy=MSMSManual_ListCollisionEnergy,
                    MSMSManual_ListIsolationWidth=MSMSManual_ListIsolationWidth)    

        message(paste("Created ", methodname,
                      "with", nrow(pickLists[[i]]), "MS2 regions"))

      }
    invisible(pickLists)
}

