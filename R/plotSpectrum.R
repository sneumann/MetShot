plotSpectrum <-
function(xf, parentPeakID, maxlabel=5, log=TRUE) {

    pseudospec <- which(xf@peaks[,"MSnParentPeakID"]==parentPeakID)

    mz <- xf@peaks[pseudospec,"mz"]

    if (log) {
        intensity <- log10(xf@peaks[pseudospec,"intensity"])
    } else {
        intensity <- xf@peaks[pseudospec,"intensity"]
    }

    plot(mz, intensity, type="h")

    text(mz[1:min(maxlabel,length(mz))],
         intensity[1:min(maxlabel,length(mz))],
         labels=format(mz[1:min(maxlabel,length(mz))], digits=4))

}

