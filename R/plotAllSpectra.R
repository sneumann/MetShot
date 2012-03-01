plotAllSpectra <-
function(xa, specperpage=9, prefix="Spec",
                           pspecs=1:length(xa@pspectra),
                           titles="", ...) {

    par(oma=c(0,0,4,0), mfrow=c(3,3), new=TRUE)

    currentPlot <- 1
    for (pspec in pspecs) {
        rt <- median(getpspectra(xa, pspec)[,"rt"])
        sp <- getpspectra(xa, grp=pspec)[,c("mz", "maxo")]

        plotPsSpectrum(xa, pspec=pspec,
                  title=paste("Spec:", pspec, "RT:", as.integer(rt)),
                  ...)

        if (currentPlot %% specperpage == 0 | pspec == length(xa@pspectra)) {
            par(mfrow=c(1,1), oma=c(0,0,1,0))
#            mtext(samplename, 3, outer = TRUE, cex = par("cex.main"))
            par(oma=c(0,0,4,0), mfrow=c(3,3))
        }
        currentPlot <- currentPlot + 1
    }
}

