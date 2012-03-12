peaklist2picklist <-
function(anyPeaklist,
                              gradientStart=NULL, gradientEnd=NULL,
                              widthFactor=1, minWidth=1, fillGaps=TRUE) {

  ##
  ## handle peaks(xs), groups(xs), diffreport()
  ##

  

  if (typeof(anyPeaklist)=="list") {
      peaklist <- as.matrix(anyPeaklist[,c("mzmin","mzmed", "mzmax","rtmin","rtmed", "rtmax")])
  } else {
    peaklist <- anyPeaklist
    rownames(peaklist) <- seq(1,nrow(peaklist))
  }

  ##
  ## For a "manual" list without rtmin/rtmax
  ## fake those
  ##

  if (! "rtmin" %in% colnames(peaklist) ) {
      peaklist <- cbind(peaklist, rtmin=peaklist[,"rtmed"])
  }
  if (! "rtmax" %in% colnames(peaklist) ) {
      peaklist <- cbind(peaklist, rtmax=peaklist[,"rtmed"])
  }

  ##
  ## Recalculate Begin/End of peaks
  ## and forcably expand small peaks
  ##

  ## widen peaks relatively
  widths <- peaklist[,"rtmax"]-peaklist[,"rtmin"]
  newWidths <- pmax(widths * widthFactor, rep(minWidth, length(widths)))

  newMin <- peaklist[, "rtmin", drop=FALSE] - ( newWidths - widths ) / 2
  newMax <- peaklist[, "rtmax", drop=FALSE] + ( newWidths - widths ) / 2


  ##
  ## If no rt range is supplied,
  ## calculate _after_ enlarging
  ##

  if (is.null(gradientStart)) {
    gradientStart <- min(newMin)
  }
  if (is.null(gradientEnd)) {
    gradientEnd <- max(newMax)
  }

  peaklist[, c("rtmin", "rtmax")] <- cbind( rtmin=pmax(rep(gradientStart, length(widths)), newMin),
                                            rtmax=pmin(rep(gradientEnd,   length(widths)), newMax))

  ##
  ## Recursively traverse peaklist
  ##

  o <- order(peaklist[, "rtmed"])

  priorities <- c(length(peaklist):1) #peaklist is sorted according to fold change; you can change it here 

  pickLists <- getSchedule(peaklist,priorities) 
  #pickList <- findWindow(peaklist[o,,drop=FALSE],
  #                       rtmin=gradientStart, rtmax=gradientEnd)

  if (is.null(pickLists)) {
      return(NULL)
  }

  pickListsFilled <- list()
  for (j in 1:length(pickLists)){
      if (is.null(pickLists[[j]])) {
        break
      }
      o <- order(pickLists[[j]][, "rtmed"]) # already ordered
      pickListOrdered <- pickLists[[j]][o,,drop=FALSE]
      pickListFilled <- pickLists[[j]]

          if (nrow(pickLists[[j]]) > 1 & fillGaps){
            ##more than one entry
            for (i in 1:(nrow(pickListOrdered)-1)) {
              meanTime <- mean(c(pickListOrdered[i,"rtmax"],
                                 pickListOrdered[i+1,"rtmin"]))
              pickListFilled[i, "rtmax"] <- meanTime
              pickListFilled[i+1,"rtmin"] <- meanTime
            }
          }
      pickListsFilled[[j]] <- pickListFilled
    }

  invisible(pickListsFilled)
}

