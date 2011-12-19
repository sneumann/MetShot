setClass("MetShot",
         representation(peaklist="data.frame",
                        peaknames="character",
                        picklists="list",
                        pickNames="character",
                        schedParams="list"
                        ),        
         contains=c("Versioned"),
         prototype=prototype(
           peaklist=data.frame(),
           peaknames="",
           picklists=list(),
           picknames="",
           validity=function(object) {
             TRUE
           }))


setGeneric("schedParams", function(object) standardGeneric("schedParams"))

setGeneric("scheduleRuns", function(object) standardGeneric("scheduleRuns"))

setGeneric("writeBruker", function(object, ...) standardGeneric("writeBruker"))
setGeneric("writeTraML", function(object, ...) standardGeneric("writeTraML"))

setMethod("show",
          signature="MetShot",
          function(object) {
            cat("An MetShot object for ",
                nrow(object@peaklist),
                ", interesting peaks")
            if (length(object@picklists>0)) {
            cat("Which are scheduled across ", 
                length(object@picklists),
                " MSMS runs")
            }
            object
          })


          
setMethod("writeBruker",
          signature="MetShot",
          function(object, index=seq(1:length(object@picklists)),
                   templateFile="",
                   collisionEnergies=c(10),
                   plot=TRUE, csv=TRUE) {
            for (i in index) {
              picklist2method(object@picklists[[i]],
                              methodPrefix=object@picknames,
                              template=templateFile,
                              MSMSManual_ListCollisionEnergy=collisionEnergies,
                              MSmode="positive")
              if (plot) {
                plotMS2windows(object@peaklist,
                               object@peaklist[object@picklists[[i]], ], 
                               col=i, peaks=TRUE,
                               labels=object@peaknames)
              }
              if (csv) {
                
              }
            }
          })


