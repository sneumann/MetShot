getParentCentroid <-
function(xr, ...) {

    xrr <- makeMiniXR(xr)
    peaks <- findPeaks(xrr, method="centWave", ...)

    invisible(peaks)
}

