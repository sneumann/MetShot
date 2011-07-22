xcmsRaw2xcmsFragments <-
function(xr, MS1peaks=NULL, ...) {

    if (is.null(MS1peaks)) {
        ## Get MS1 peaks
        MS1peaks <- getParentCentroid(xr)
    }

    xf <- new("xcmsFragments")
    xf@peaks <- cbind(1:nrow(MS1peaks),
                      rep(0,nrow(MS1peaks)), # parent peak
                      rep(1,nrow(MS1peaks)), # ms level
                      MS1peaks[,c("rt","mz","into")],
                      rep(1,nrow(MS1peaks)), # Dummy: sample
                      rep(1,nrow(MS1peaks))  # Dummy: group id
                      )
    colnames(xf@peaks) <- c("peakID", "MSnParentPeakID","msLevel","rt", "mz",
                            "intensity", "Sample","GroupPeakMSn")

    ## Get MS2 peaks
    MS2peaks <- findPeaks(msn2xcms(xr),
                          method="centWave", ...)

    newpeaks <- do.call("rbind", lapply (as.list(1:nrow(xf@peaks)), function(x, xf, MS2peaks) {
        rt <- xf@peaks[x,"rt"]
        pseudospec <- which(abs(MS2peaks[,"rt"] - xf@peaks[x, "rt"])<3)
        newpeakids <- seq(nrow(xf@peaks)+1, nrow(xf@peaks)+length(pseudospec))
        if (length(pseudospec)==0 || is.null("newpeaks")) {
            return(NULL)
        }
        newpeaks <- cbind(peakID=newpeakids,
                          MSnParentPeakID=rep(x, length(pseudospec)),
                          msLevel=rep(2, length(pseudospec)),
                          MS2peaks[pseudospec, c("rt","mz","maxo"),drop=FALSE],
                          rep(1, length(pseudospec)),
                          rep(1, length(pseudospec)))
        colnames(newpeaks) <- c("peakID", "MSnParentPeakID","msLevel","rt", "mz",
                                "intensity", "Sample","GroupPeakMSn")
        newpeaks
    }, xf, MS2peaks))

    xf@peaks <- rbind(xf@peaks, newpeaks)
    xf@peaks[,"peakID"] <- seq(1:nrow(xf@peaks))

    invisible(xf)
}

