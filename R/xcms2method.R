xcms2method <- function(peaklist, methodPrefix="", MSmode=c("positive","negative"),
                        template="test.m/microTOFQAcquisition.method",
                        templateSegmentNr=2,
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

    picklists2methods(pickLists, methodname, MSmode=MSmode,
                      template=template, templateSegmentNr=templateSegmentNr,
                      MSMSManual_ListCollisionEnergy=MSMSManual_ListCollisionEnergy,
                      MSMSManual_ListIsolationWidth=MSMSManual_ListIsolationWidth)    
    
    invisible(pickLists)
}

