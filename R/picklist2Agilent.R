
picklist2Agilent <- function(pickList, filename="TargetedMSMSTable.csv", 
                             delta_ret_time=10,
                             iso_width=c("Narrow (~1.3 m/z)", "Medium (~4 m/z)"),
                             MSMSManual_ListCollisionEnergy = 20) {
  cnames <- c("On","Prec. m/z","Z","Ret. time (min)","Delta ret. time (min)","Iso. width","Collision energy","Acquisition time (ms/spec)")

  iso_width <- match.arg(iso_width)

  x <- matrix("", nrow=nrow(pickList) + 2, ncol=length(cnames))
  x[1,1] <- "TargetedMSMSTable"
  x[2, ] <- cnames

  r <- 3:(nrow(pickList) + 2)

  x[r,1] <- "TRUE"
  x[r,2] <- round(pickList[,"mzmed"],4)       ## Prec. m/z
  x[r,3] <- 1                                 ## Z , CAVE: This is a hack, since pickList doesn't have charge info
  x[r,4] <- round(pickList[,"rtmed"] / 60, 3)  ## Ret. time (min)
  x[r,5] <- round(delta_ret_time / 60, 3)      ## "Delta ret. time (min)"
  x[r,6] <- iso_width
  x[r,7] <- MSMSManual_ListCollisionEnergy
  
  idx <- order(as.numeric(x[r,4])) ## sort by RT
  x[r,] <- x[r[idx],]
  
  write.table(x, file = filename, append = FALSE, quote = FALSE,
              sep = ",", eol = "\n", na = "NA", dec = ".",
              row.names = FALSE,
              col.names = FALSE)
}

