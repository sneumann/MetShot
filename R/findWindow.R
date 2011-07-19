findWindow <-
function(peaklist, rtmin, rtmax) {

  ##
  ## Prefilter peaks between window of interest
  ##
  inWindow <-    which(  peaklist[,"rtmin"] >= rtmin
                       & peaklist[,"rtmax"] >  rtmin
                       & peaklist[,"rtmax"] <= rtmax
                       & peaklist[,"rtmin"] <  rtmax)

  if (length(inWindow) == 0) {
      ## recursion end
      return (NULL);
  }

  ##
  ## Select Peak to Pick and replace min/max
  ##

  center <- max(1,floor(length(inWindow)/2))
  peak <-    peaklist[inWindow[center], ];

##   cat(paste("Using peak", rownames(peaklist)[inWindow[center]],
##              "from", peak["rtmin"],
##              "to", peak["rtmax"],
##             "\n"))

  wl <- findWindow(peaklist,
                   min(rtmin, peak["rtmin"]),
                   max(rtmin, peak["rtmin"]))

  wr <- findWindow(peaklist,
                   min(rtmax, peak["rtmax"]),
                   max(rtmax, peak["rtmax"]))

  newpeak <- rbind(peak)
  rownames(newpeak) <- rownames(peaklist)[inWindow[center]]

  return (as.matrix(rbind(newpeak, wl, wr), ncol=8))
}

