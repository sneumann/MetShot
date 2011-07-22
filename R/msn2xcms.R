msn2xcms <-
function(xmsn) {
    x <- xmsn
                                        # BROKEN
    x@tic <- x@msnAcquisitionNum
                                        # Fake time in secs
    x@scantime <- x@msnRt
    x@acquisitionNum <- x@msnAcquisitionNum
    x@scanindex <- x@msnScanindex

    x@env$mz <- x@env$msnMz
    x@env$intensity <- x@env$msnIntensity
    invisible(x)
}

