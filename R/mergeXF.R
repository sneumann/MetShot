mergeXF <-
function(xf1, xf2, xf3) {

    p2 <- cbind(peakID=seq(max(xf1@peaks[,"peakID"])+1,
                length=length(which(xf2@peaks[,"msLevel"]==2))),
                xf2@peaks[which(xf2@peaks[,"msLevel"]==2),2:ncol(xf2@peaks)])
    p3 <- cbind(peakID=seq(max(p2[,"peakID"])+1,
                length=length(which(xf3@peaks[,"msLevel"]==2))),
                xf3@peaks[which(xf3@peaks[,"msLevel"]==2),2:ncol(xf3@peaks)])

    xf1@peaks <- rbind(xf1@peaks, p2, p3)

    xf1
}

