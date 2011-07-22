groupCorrhack <-
function (xa,xraw=NULL,cor_eic_th=0.75) {

    maxscans <- length(xraw@scantime)
    scantimes<-list();
    scantimes[[1]] <- xraw@scantime
    pdata <- peaks(xa@xcmsSet)

    EIC <- array(integer(0),c(nrow(pdata),maxscans,1))
    EIC[,,1] <- CAMERA:::getEICs(xraw,pdata,maxscans)
    CL <- vector("list",nrow(pdata))
    CIL <- list()
    ncl<-length(CL);npeaks=0;
    npspectra <- length(xa@pspectra);
    cat('Calculating peak correlations... \n% finished: '); lp <- -1;
    for(i in 1:npspectra){
        pi <- xa@pspectra[[i]];
                                        #percent output
                                        #   cat(i);
        npeaks<-npeaks+length(pi);
        perc <- round((npeaks) / ncl * 100)
        if ((perc %% 10 == 0) && (perc != lp)) { cat(perc,' '); lp <- perc }
        if (.Platform$OS.type == "windows") flush.console()
                                        #end percent output
        if(length(pi)>1){
            for(x in 2:length(pi)){
                xi <- pi[x];
                for (y in 1:(x-1)){
                    yi <- pi[y];
                    if ( ! (yi %in% CL[[xi]] || yi == xi)){
                        cors <-0;
                        ##debug
                        f <- xa@sample; #sample 1
                        ##end debug
                        eicx <-  EIC[xi,,1]
                        eicy <-  EIC[yi,,1]
                        px <- peaks(xa@xcmsSet)[xi,]
                        py <- peaks(xa@xcmsSet)[yi,]
                        crt <- range(px["rtmin"],px["rtmax"],py["rtmin"],py["rtmax"])
                        rti <- which(scantimes[[1]] >= crt[1] & scantimes[[1]] <= crt[2])
                        if (length(rti)>1){
                            dx <- eicx[rti]; dy <- eicy[rti]
                            dx[dx==0] <- NA; dy[dy==0] <- NA;
                            if (length(which(!is.na(dx) & !is.na(dy))) >= 4){
                                ct <- NULL
                                options(show.error.messages = FALSE)
                                try(ct <- cor.test(dx,dy,method='pearson',use='complete'))
                                options(show.error.messages = TRUE)
                                if (!is.null(ct) && !is.na(ct)){
                                    if (ct$p.value <= 0.05) cors <- ct$estimate else cors <- 0;
                                }else cors <- 0;
                            }else cors <- 0;
                        }else cors <- 0;
                        if(cors>cor_eic_th){
                            CL[[xi]] <- c(CL[[xi]],yi)
                            CL[[yi]] <- c(CL[[yi]],xi) ## keep the list symmetric
                            CIL[[length(CIL)+1]] <- list(p=c(xi,yi),cor=cors)
                        }
                    }
                }
            }
        }
    }
    cat("\n");

    if (length(CIL) >0){
        CI <- data.frame(t(sapply(CIL,function(x) x$p)),sapply(CIL,function(x) x$cor) )
        colnames(CI) <- c('xi','yi','cors')

        ##Subgraph Zerlegung nach Kantenkonnektivität##
        xa@pspectra <- CAMERA:::calc_pc(peaks,CL,CI)
    } else {
        warning("Keine Grp möglich in ", xraw@filepath)
    }
    return(invisible(xa));
}

