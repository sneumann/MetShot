xcms2method <-
function(peaklist, method="", MSmode=c("positive","negative"),
                        template="test.method",
                        gradientStart=NULL, gradientEnd=NULL,
                        widthFactor=1, minWidth=1,
                        MSMSManual_ListCollisionEnergy=15,
                        MSMSManual_ListIsolationWidth=8)
{


    pickList <- peaklist2picklist(peaklist,
                                  gradientStart=gradientStart, gradientEnd=gradientEnd,
                                  widthFactor=widthFactor,
                                  minWidth=minWidth)

    if (is.null(pickList)) {
        warning("No suitable peaks found, returning empty picklist")
        return(NULL)
    }

    picklist2method(pickList, method, MSmode, template,
                    MSMSManual_ListCollisionEnergy=MSMSManual_ListCollisionEnergy,
                    MSMSManual_ListIsolationWidth=MSMSManual_ListIsolationWidth)    
}

