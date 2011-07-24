
#build one optimal scheduling for inputlist
scheduling.table <- function(weights, start, finish){
    if (length(weights)!=length(start) | length(start)!=length(finish))#invalid number
        return(NULL)

    
    n<-length(weights)
    values<-c(rep(0,n))#keep best route: points
    remember<-c(rep(-1,n))#which way
    values[n]<-weights[n]

	if (n<=1)
		return(1)

    for (i in (n-1):1){#dynamic programming: keep best route
        minstart<-finish[i]
        minpeak<- -1
        for (j in (i+1):n){#find min start point after current finish point
            if (start[j] >= finish[i]){
                
                    minstart<-start[j]
                    minpeak<-j
                    break;
            }
        }
        x1<-weights[i]#either take ith peak 
        if (minpeak>-1) x1<-x1+values[minpeak]#plus following peaks
        x2<-values[i+1]#or take the peak with rtmin directly after ith peak
		
        if (x1 > x2)#remember which way to take
            remember[i]=minpeak else remember[i]=0
        values[i]=max(x1,x2)
        
    }

    #extract optimal schedule for 1 run
    outlist<-c()
    i<-1
	print(values[1])#score
    repeat{
        if (remember[i]==-1){#last entry
            outlist<-c(outlist,i)
            break
        }
        if (remember[i]>0) {#if >0 keep this peak and follow link;
            outlist<-c(outlist,i);i<-remember[i];} else i<-i+1;# else: take other way


    }
    
    return(outlist)#return position in peaklist
    
}

#make schedules for nrun runs
getSchedule<-function(peaklist,weights,nrun=10000){
    #get weights, start- end end-time out of peaklist
	peaklist<-as.data.frame(peaklist)
    start<-peaklist$rtmin
    finish<-peaklist$rtmax

    os<-order(start)#sort according to starting time 
    weights<-weights[os]
    start<-start[os]
    finish<-finish[os]
	plist<-peaklist[order(peaklist$rtmin),]

    #now start scheduling
    rcount<-1#number of runs
    runs<-list()

	#do scheduling
    repeat{
        if (rcount>nrun) break;#max number of runs reached

        outlist<-scheduling.table(weights,start,finish)#make one optimal weighted schedule
		runs[[rcount]]<-peaklist[(rownames(plist)[outlist]),]
		
        reduce<-c(1:length(weights))[-outlist]#remove accepted peaks from schedule-peaklist
        weights<-weights[reduce]
        start<-start[reduce]
        finish<-finish[reduce]
		plist<-plist[reduce,]
        if (length(weights)==0) break;#no peaks left
        rcount<-rcount+1

    }
    #return(runs)#return list with Ids for all runs
	invisible(runs)
}



#runs<-getSchedule(peaklist,c(length(peaklist):1))

