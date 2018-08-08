###-------ECONOMIC MANAGEMENT APPROVAL--###
###-----------GENERAL SETUP-------------###
###-------------Mats Ahrenshop----------###

setwd("C:\\Users\\User\\Downloads")
pe <- read.csv2("Presidential Economic Management.csv")
pe$date <- ISOdate(pe$Year, pe$Month, pe$Day)
pe$date <- as.Date(pe$date)


## The Problem: Aggregating six time series

plot(x = pe$Appr[pe$House == "Gallup"], axes = F, 
     ylab = "%", ylim = c(15, 85),
     xlab = "Day", type = "l", lwd = 2, col = "black",
     main = "Approval of Presidential economic management")
axis(1, at = c(1:894),
     labels = pe$date, cex.axis = .7)
axis(2)
lines(pe$Appr[pe$House == "ABC"], col = "red", lwd = 2)
lines(pe$Appr[pe$House == "ABCWP"], col = "blue", lwd = 2)
lines(pe$Appr[pe$House == "CBS"], col = "green", lwd = 2)
lines(pe$Appr[pe$House == "CBSNYT"], col = "orange", lwd = 2)
lines(pe$Appr[pe$House == "LATIMES"], col = "grey", lwd = 2)
legend(x = "topright", legend = c("Gallup", "ABC", "ABCWP", "CBS", "CBSNYT", "LATIMES"),
       col = c("black", "red", "blue", "green", "orange", "grey"),
       lty = 1, cex = .6)
box()



## The Solution: Dyad ratios algorithm using \texttt{extract} function

## obtain via http://stimson.web.unc.edu/software/ 

##--------------------------##
### FUNCTIONS FROM STIMSON ###

display<-function(out,filename=NULL) {
  if (is.null(filename)) filename=""
  d<-out$dimensions
  p<-out$period
  m<-out$latent1
  if (d==2) m2<-out$latent2     
  T<-out$T
  mo=100*(p-as.integer(p))
  for (t in 1:T) {
    yr<-format(as.integer(p[t]),nsmall=0)
    month<-format(mo[t],digits=2)
    lat1<-format(m[t],nsmall=3)
    if (d==1) {
      cat(c(yr,month,lat1),fill=TRUE,file=filename,append=TRUE)
    } else {
      lat2<-format(m2[t],nsmall=3)
      cat(c(yr,month,lat1,lat2),fill=TRUE,file=filename,append=TRUE)
    }
  }
}
##########################################################################################
plot.Zextract<-function(outobject) {
  dim<- outobject$dimensions
  T<- outobject$T
  vect1<-outobject$latent1
  t<-seq(1:T)
  if (dim>1) {
    vect2<-outobject$latent2
    miny<-min(vect1)
    if (miny>min(vect2)) miny<-min(vect2)
    maxy<-max(vect1)
    if (maxy<max(vect2)) maxy<-max(vect2)
    dummy<-rep(miny,T-1) #dummy is a fake variable used to reset axes to handle min/max of both series
    dummy[T]<-maxy
    leg.text<-c("","Dimension 1","Dimension 2")
    plot(t,dummy,type="l",lty=0,main="Final Estimation Results: Two Dimensions",xlab="Time Point",ylab="Latent Variables")
    lines(t,vect1,col=1)
    lines(t,vect2,col=2)
    legend(1,maxy,leg.text,col=c(0,1,2),lty=c(0,1,1))
  } else {
    plot(t,vect1,type="l",main="Final Estimation Results",xlab="Time Point",ylab="Latent Variable")
    if (dim == 2) lines(t,vect2,col=2)
  }
}
##########################################################################################
summary.Zextract<- function(outobject) {
  T=outobject$T
  nvar=outobject$nvar
  dim<- outobject$dimensions
  vn<- c(outobject$varname,"Variable Name")
  vn<- format(vn,justify="right")
  nc<- format(outobject$N,justify="right")
  ld<- format(outobject$loadings1,digits=3,justify="right")
  mean<- format(outobject$means,digits=6,justify="right")
  sd<- format(outobject$std.deviations,digits=6,justify="right")
  cat("Variable Loadings and Descriptive Information: Dimension 1\n")
  cat(paste(vn[nvar+1],"Cases","Loading","   Mean ","Std Dev","\n"))
  for (v in 1:nvar) {
    cat(paste(vn[v],"  ",nc[v]," ",ld[v],mean[v],sd[v],"\n"))
  }
  if (dim == 2) {
    ld<- format(outobject$loadings2,digits=3,justify="right")
    cat("\nVariable Loadings and Descriptive Information: Dimension 2\n")
    cat(paste(vn[nvar+1],"Cases","Loading","   Mean ","Std Dev","\n"))
    for (v in 1:nvar) {
      cat(paste(vn[v],"  ",nc[v]," ",ld[v],mean[v],sd[v],"\n"))
    }
  }
}
##########################################################################################
findper<-function(unit,curdate,mind,miny,minper,aggratio) { #returns intFindPer
  datcurdate<-curdate
  class(datcurdate)<-"Date"
  mo <- findmonth(datcurdate)
  qu <- 1 + as.integer((mo - 1)/3)
  dy <- findday(datcurdate)
  yr <- findyear(datcurdate)
  arinv<- 1/aggratio
  if (unit == "D") intFindPer <- curdate - mind +1 #curdate - mindate + 1
  if (unit == "A" || unit == "O") intFindPer <- as.integer((yr - miny) / aggratio) + 1
  if (unit == "Q") part <- qu
  if (unit == "M") part <- mo
  if (unit == "Q" || unit == "M") intFindPer <- (yr - miny - 1) * arinv + part + (arinv - (minper - 1))
  return(intFindPer)
} #findper
##########################################################################################
findday<-function(DateVar) {
  z<-as.POSIXlt(DateVar)
  v<-unlist(z)
  findday<-as.integer(v[4])
} #end findday
##########################################################################################
findmonth<-function(DateVar) {
  z<-as.POSIXlt(DateVar)
  v<-unlist(z)
  findmonth<-as.integer(v[5])+1
} #end findmonth
##########################################################################################
findyear<-function(DateVar) {
  z<-as.POSIXlt(DateVar)
  v<-unlist(z)
  findyear<-as.integer(v[6])+1900
} #end findyear
##########################################################################################
aggregate<- function(varname,date,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) {   #
  #READ A NEW RECORD, CALCULATE PERIOD, AND SET UP AGGREGATION INTO MAT.ISSUE[NPERIODS,NVAR] 
  vl<- character(nvar)
  mind<- as.integer(mindate)/86400
  maxd<- as.integer(maxdate)/86400
  vfac<- factor(varname) #make a factor vector
  vlev<- levels(vfac)    #find unique categories
  Mat.Issue<- array(dim=c(nperiods,nvar))
  
  nrec<-length(varname) #added for R compatibility
  lp<- 0
  per<- 0
  x<- 0
  c<- 0
  nkeep<- 0
  lv<- "0"
  for (record in 1:nrec) { # MASTER LOOP THROUGH INPUT DATA, 1 TO NREC
    if (ncases[record] == 0 || is.na(ncases[record])) ncases[record] <- 1000
    mo <- findmonth(date[record])
    qu <- 1 + as.integer((mo - 1)/3)
    dy <- findday(date[record])
    yr <- findyear(date[record])
    curdate<- as.integer(date[record])
    if (curdate >= mind &&  curdate <= maxd) {  #is date within range?
      nkeep <- nkeep + 1
      if (nkeep==1) { #startup routine for first good case
        firstcase<- TRUE
        lp <- findper(unit,curdate,mind,miny,minper,aggratio)
        lv <- varname[record]
        x <- index[record] * ncases[record] #start new sums for case 1
        c <- ncases[record]
        for (i in 1:nvar) {
          if (lv==vlev[i]) v=i #determine v by matching to position of labels vector
        } #end for
      } else {
        firstcase<- FALSE
      } #end if
      if (firstcase == FALSE) { #skip over the rest for first good case
        per<- findper(unit,curdate,mind,miny,minper,aggratio) #here we translate date into agg category
        if ((varname[record] !=  lv) || (per !=lp)) { #found a new period or variable name
          if (lp > 0 &&  lp <= nperiods) {
            Mat.Issue[lp, v] <- x / c #recompute for either period or var change
            x<- 0
            c<- 0
          }
          if (varname[record] !=  lv) { #new var only
            for (i in 1:nvar) {
              if (varname[record]==vlev[i]) v=i #determine v by matching to position of labels vector
            } #end for
            vl[v]<- varname[record] #this will only catch names that have good cases
            lv<-vl[v]  #reassign new varname to lastvar
          } # new var
          lp <- findper(unit,curdate,mind,miny,minper,aggratio)
          x <- index[record] * ncases[record] #start new sums for current case
          c <- ncases[record]
        } else {
          x<- x + index[record] * ncases[record] #a continuing case, increment sums
          c<- c + ncases[record]
        }
      } # end of first case special loop
    } #end of date test loop
  } #newrec: next record
  vl<- vlev #overwrite previous assignment which had good names only
  agglist<- list(lab=vl,iss=Mat.Issue)
  return(agglist) #list includes labels and issue matrix
} #end aggregate function
##########################################################################################

esmooth<- function(mood, fb, alpha){ 
  ##########################################################################################
  smooth<- function(alpha) { #for time series "series" and alpha "alpha[1]" compute sum of squared forecast error
    ferror<- numeric(1)
    T<- length(series)
    xvect<- numeric(T)        
    xvect[1] <-  series[1]
    for (t in 2:T) { 
      xvect[t] <-  alpha[1] * series[t] + (1 - alpha[1]) * xvect[t - 1]
    }
    sumsq <-  0
    for (t in 3:T) { 
      ferror <-  series[t] - xvect[t - 1]
      sumsq <-  sumsq + ferror ^ 2
    } 
    return(sumsq) #this is the value of the function for a particular parameter alpha[1]
  } # END OF FUNCTION SMOOTH   
  ##########################################################################################
  
  series<- mood[fb,] #create series to be smoothed
  sm.out<- optim(c(.75),smooth,method="L-BFGS-B",lower=0.5,upper=1)  #call smoother
  alpha<- sm.out$par                          #assign result to alpha
  #NOW SMOOTH USING ALPHA
  T<- length(series)
  for (t in 2:T) { 
    mood[fb,t] <-  alpha * series[t] + (1 - alpha) * mood[fb,t - 1]
  }
  return(alpha)
} #END OF FUNCTION ESMOOTH
##########################################################################################
residmi<- function(issue,v,mood) { #function regresses issue(v) on mood and then residualizes it
  o<- lm(issue[,v] ~ mood[3,]) #regress issue on mood to get a,b
  issue[,v]<- 100 + issue[,v] - (o$coef[1]+o$coef[2]*mood[3,]) #100 + Y - (a+bx)
  return(issue[,v])
} 
##########################################################################################
iscorr<- function(issue,mood) { #compute issue-scale correlations
  Nv<- length(issue[1,])
  Np<- length(issue[,1])
  Rvector<- numeric(Nv)
  for (v in 1:Nv) {
    N<- Np - sum(is.na(issue[,v]))
    if (N > 1) Rvector[v]<- cor(issue[,v],mood[3,],use="complete.obs",method="pearson")
  }
  return(Rvector)
} #end function iscorr
##########################################################################################
dominate<- function(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha) {
  nitems<- numeric(nperiods)
  if (fb==2) alpha1<-alpha
  if (fb==1) {
    unexp<-numeric(1)
    everlap<- integer(1)
    alpha<- 1
    alpha1<- 1
  } 
  
  if (fb == 1) {
    startper <- 1
    mood[fb, startper] <- 100
    firstj <- 2
    lastj <- nperiods
    stepj <- 1
    jprev <- 1
  } else {
    startper <- nperiods
    mood[fb, startper] <- mood[1, nperiods] #reuse forward metric
    firstj <- nperiods - 1
    lastj <- 1
    stepj <- -1
    jprev <- nperiods
  } #    end if
  for (j in seq(firstj,lastj,by=stepj)) {  
    mood[fb, j] <- 0
    everlap <- 0 ## of years which have contributed sums to mood
    if (fb == 1) {
      firstj2 <- 1
      lastj2 <- j - 1
    } else  {
      firstj2 <- j + 1
      lastj2 <- nperiods
    } # end if
    
    for (j2 in firstj2:lastj2) { 
      sum <- 0     #has already been estimated
      consum <- 0  #sum of communalities across issues
      overlap <- 0
      for (v in 1:nvar) { 
        xj <- issue[j, v]                      #xj is base year value
        sngx2 <- issue[j2, v]                  #sngx2 is comparison year value
        if (!is.na(xj) && !is.na(sngx2)) {  
          overlap <- overlap + 1               #numb of issues contributing to sum
          ratio <- xj / sngx2
          if (csign[v] < 0)  ratio <- 1 / ratio
          sum <- sum + valid[v] * ratio * mood[fb, j2] 
          consum <- consum + valid[v]
        } #              end if
      } #next v
      if (overlap > 0) {
        everlap <- everlap + 1
        mood[fb, j] <- mood[fb, j] + sum / consum
      } # end if
    } #next j2
    nitems[j] <- everlap
    if (everlap > 0) mood[fb, j] <- mood[fb, j] / everlap else mood[fb, j] <- mood[fb, jprev] #if undefined, set to lag(mood)
    jprev <- j #last value of j, whether lead or lag
  } #next j
  if (smoothing == TRUE) {
    alpha<- esmooth(mood, fb, alpha)     #NOW SMOOTH USING ALPHA
    mood.sm<- mood[fb,] #set up alternate vector mood.sm
    for (t in 2:nperiods) { 
      mood.sm[t]<- alpha*mood[fb,t]+(1-alpha)*mood.sm[t-1]
    } #end for
    mood[fb,]<- mood.sm #now assign back smoothed version
  } else {
    alpha1 <- 1
    alpha <- 1
  } 
  if (smoothing == TRUE && fb == 1) alpha1 <- alpha
  dominate.out<- list(alpha1=alpha1,alpha=alpha,latent=mood[fb,]) #output object
  return(dominate.out)  
  #  return(mood[fb,])
} #end dominate algorithm  
##########################################################################################





#begindt<-NA #ISOdate(2004,6,1)
#enddt<-NA #ISOdate(2004,10,31)

##########################################################################################
## MAIN EXTRACT CODE BEGINS HERE #########################################################
extract<- function(varname,date,index,ncases=NULL,unit="A",mult=1,begindt=NA,enddt=NA,npass=1,smoothing=TRUE,endmonth=12) {
  formula<-match.call(extract)
  nrecords<- length(varname)
  if (is.null(ncases)) ncases<- rep(0,nrecords)
  moddate<- date #create temporary date vector, leaving original unmodified
  if ((unit=="A" || unit=="O") && endmonth<12) {
    for (i in 1:nrecords) { #first loop through raw data file
      month<- findmonth(moddate[i])
      year<- findyear(moddate[i])
      if (month>endmonth) moddate[i]<- ISOdate(year+1,1,1) #modified date become 1/1 of next year
    } #end loop through data
  } # end if
  
  if (is.na(begindt)) minper<-findmonth(min(moddate)) else minper<-findmonth(begindt)
  if (is.na(begindt)) miny<-findyear(min(moddate)) else miny<-findyear(begindt)
  if (is.na(begindt)) minday<-findday(min(moddate)) else minday<-findday(begindt)
  if (is.na(enddt)) maxper<-findmonth(max(moddate)) else maxper<-findmonth(enddt)
  if (is.na(enddt)) maxy<-findyear(max(moddate)) else maxy<-findyear(enddt)
  if (is.na(enddt)) maxday<-findday(max(moddate)) else maxday<-findday(enddt)
  if (unit=="Q") {
    minper<- as.integer((minper-1)/3)+1
    maxper<- as.integer((maxper-1)/3)+1
  }
  mindate<- ISOdate(miny,minper,minday,0,0,0,tz="GMT")
  maxdate<- ISOdate(maxy, maxper, maxday,0,0,0,tz="GMT") #86400=24*60*60
  
  #SETCONS:
  latent<- numeric(1)
  aggratio<- 0
  fb<- 1 #initialize
  auto<- "start"  #meaningless value
  alpha<- 1
  alpha1<- 1
  pass<- 1
  holdtola<- 0.001
  tola<- holdtola
  iter<- 0
  lastconv<- 99999
  wtmean<- 0 #for it=1
  wtstd<- 1
  fract<- 1
  
  if (unit=="A") {
    nperiods<- maxy-miny+1
    aggratio<- 1
    months<- 12
  }
  if (unit=="O") {
    years<- mult
    months<- years*12
    aggratio<- 2
    odd<- (maxy-miny+1) %% mult  #mod
    nperiods=as.integer((maxy-miny)/mult) + odd
  }
  if (unit=="M") {
    fract<- 100
    nperiods<- (maxy-miny)*12
    nperiods<- nperiods-12 + (12-minper+1) + maxper
    aggratio<- 1/12
    months<- 1
  }
  if (unit=="Q") {
    aggratio<- 1/4
    months<- 3
    nperiods<- as.integer((maxy-miny)/aggratio)
    nperiods<- nperiods-4 + (4-minper+1) + maxper
    fract<- 10
  }
  if (unit=="D") {
    months=1
    nperiods<- (as.integer(maxdate)-as.integer(mindate))/86400 + 1 #86400=24*60*60
  }
  
  arinv<- 1/aggratio
  aggratio<- months/12
  nrecords<- length(index)
  
  #HERE WE SET UP FUNDAMENTAL DIMENSIONS AND DECLARE VECTORS
  if (fb != 2)  mood<- array(dim=c(3,nperiods))
  vfac<- factor(varname) #make a factor vector
  vlev<- levels(vfac)    #find unique categories
  nvar<- length(vlev)    #how many are there?, includes unusable series
  valid<- numeric(nvar)
  csign<<- numeric(nvar)
  vl<- character(nvar)
  r<- numeric(nvar)
  oldr<- rep(1,nvar) # r=1 for all v initially
  
  issue<- array(dim=c(nperiods,nvar))
  count<- numeric(nperiods)
  vl<- numeric(nvar)
  period<- numeric(nperiods)
  converge<- 0
  evalue<- 0
  
  # create numeric variable period, eg, yyyy.0m 
  if (unit=="D") {
    period<-seq(1:nperiods) 
  } else {
    if (months >= 12) {
      for (l in 1:nperiods) { 
        p <- (l - 1) * aggratio
        period[l] <- miny + p
      } #next l
    } else {
      y <- 0
      i <- 0
      my <- miny
      if (minper == 1)  my <- my - 1
      for (l in 1:nperiods) { 
        i<- 1 + ((l-1) %% arinv)
        mq <- minper + i - 1
        mq<- 1 + ((mq-1) %% arinv) 
        if (mq == 1)  y <- y + 1 #first month or quarter, increment year
        period[l] <- my + y + mq / fract
      } # end for
    } #end else
  } # end if
  
  
  agglist<- aggregate(varname,moddate,index,ncases,mindate,maxdate,nperiods,nvar,aggratio,unit,miny,minper) # call aggregate to produce issue matrix
  vl<- agglist$lab #extract two elements of the list from aggregate call
  issue<- agglist$iss
  rm(agglist) #don't need this anymore
  
  #NOW REDUCE ISSUE MATRIX TO ELIMINATE UNUSABLE SERIES (WN<2)
  ndrop<- 0
  nissue<- numeric(nperiods)
  std<- numeric(nperiods)
  for (v in 1:nvar) {
    std[v]<- 0 #default
    nissue[v]<- sum(!is.na(issue[,v])) #criterion is 2 cases for npass=1 or 3 for npass=2
    if (nissue[v]>npass) std[v]<- sqrt(var(issue[,v],na.rm=TRUE)) #this is just a test for variance >0
    if (std[v]<.001) {  #case dropped if std uncomputable (NA) or actually zero (constant)
      ndrop<- ndrop+1
      print(paste("Series",vl[v],"discarded.  After aggregation cases =",nissue[v]))
    }
  }
  nvarold<- nvar
  nvar<- nvar-ndrop
  pointer<- 1
  found<- FALSE
  
  for (v in 1:nvar) { #now reduced nvar
    while (found==FALSE && pointer<=nvarold) { #find first valid column and push down
      if (std[pointer]>.001) { #good case, transfer
        issue[,v]<- issue[,pointer]
        vl[v]<- vl[pointer]
        pointer<- pointer+1
        found<- TRUE
      } else {
        pointer<- pointer+1 #bad case, increment pointer
      } #end if
    } #end while
    found<- FALSE
  } #for
  length(vl)<- nvar #reduce  
  length(issue)<- nperiods*nvar  #chop off unused columns
  attr(issue,"dim")<- c(nperiods,nvar)
  N<- numeric(nvar)
  
  #export<<-list(nperiods,nvar,issue)
  
  
  for (pass in 1:npass) { #newpass: RESTART FOR SECOND DIMENSION CASE
    if (pass == 2) { #reset iteration control parameters
      iter <- 0
      tola = holdtola
      lastconv <- 99999
      converge<- lastconv
      conv<- converge
    } else {
      av<- numeric(nvar)
      std<- numeric(nvar)
      #      ngood<- 0
      for (v in 1:nvar) { #compute av and std by issue nvar now reduced to good cases
        wn<- as.integer(nperiods-sum(is.na(issue[,v])))
        av[v] <- mean(issue[,v],na.rm=TRUE)
        std[v]<- sqrt(var(issue[,v],na.rm=TRUE) * ((wn - 1)/wn)) #convert to population standard deviation
        issue[,v]<- 100 + 10 * (issue[,v] - av[v])/std[v]  #standardize
        #        ngood<- ngood+1
      }#end for
    }
    #READY FOR ESTIMATION, SET UP AND PRINT OPTIONS INFO     
    out<- as.character(10) #initial length only
    out[1]<- print(paste("Estimation report:"))
    if (pass == 1) {
      if (months >= 12) {
        out[2]<- print(paste("Period:", miny, " to", maxy,"     ", nperiods, " time points"))
      } else {
        out[2]<- print(paste("Period:", miny,  minper, " to", maxy, maxper, nperiods, " time points"))
      }
      out[3]<- print(paste("Number of series: ", nvar+ndrop))
      out[4]<- print(paste("Number of usable series: ", nvar))
      out[5]<- print(paste("Exponential smoothing: ",smoothing))
    }
    out[6]<- print(paste("Iteration history: Dimension ",pass))
    print(" ")
    out[7]<- print("Iter Convergence Criterion Reliability Alphaf Alphab")
    outcount<- 7
    
    for (p in 1:nperiods) {
      count[p]<- sum(!is.na(issue[p,]))
    }
    valid<- rep(1,times=nvar)
    csign<<- rep(1,times=nvar)
    auto <- "y"              #iterative estimation on by default
    quit <- 0                #false implies go ahead and estimate
    
    while (iter == 0 || converge > tola) {   #MASTER CONTROL LOOP WHICH ITERATES UNTIL SOLUTION REACHED
      
      for (fb in 1:2) { #    MASTER fb LOOP       fb=1 is forward, 2 backward 
        dominate.out<- dominate(fb,issue,nperiods,nvar,mood,valid,smoothing,alpha)  #master estimation routine
        alpha1<- dominate.out$alpha1
        alpha<- dominate.out$alpha
        mood[fb,]<- dominate.out$latent
      } #next fb
      
      fb <- 3 #average mood from here on
      for (p in 1:nperiods) { #    AVERAGE
        mood[fb, p] <- (mood[1, p] + mood[2, p]) / 2
      } #next p
      moodmean<-mean(mood[3,])
      sdmood<-sd(mood[3,])
      for (p in 1:nperiods) {  #PLACEMENT OF THIS LOOP MAY NOT BE RIGHT
        mood[fb,p] <- ((mood[fb,p] - moodmean) * wtstd / sdmood) + wtmean
      } #end for
      
      #plot commands
      t<- seq(1:nperiods) #time counter used for plot below
      lo<- 50 #force scale of iterative plot to large range
      hi<- 150
      if (min(mood[3,]) < lo) lo=min(mood[3,]) #whichever larger, use
      if (max(mood[3,]) > hi) hi=max(mood[3,])
      dummy<- rep(lo,nperiods) #dummy is fake variable used to set plot y axis to 50,150
      dummy[nperiods]<- hi
      if (iter==0) {
        plot(t,dummy,type="l",lty=0,xlab="Time Period",ylab="Estimate by iteration",main="Estimated Latent Dimension") #create box, no visible lines
      } else {
        lines(t,mood[3,],col=iter)
      }  
      
      iter <- iter + 1 
      if (auto == "y") r<- iscorr(issue,mood) else auto <- "y"   #recompute correlations
      
      wtmean<- 0
      wtstd<- 0
      vsum<- 0
      goodvar<- 0
      converge<- 0 #start off default
      evalue<- 0
      totalvar<- 0
      
      for (v in 1:nvar) {
        wn<- nperiods-sum(is.na(issue[,v]))
        if (!is.na(sign(r[v]))) csign[v]<<- sign(r[v])
        wn<- nperiods-sum(is.na(issue[,v]))
        if (wn>1) { #sum over variables actually used
          vratio <- wn / nperiods
          evalue <- evalue + vratio * r[v]^2
          totalvar <- totalvar + vratio
        } #end if
        
        #convergence tests
        if (wn > 3) {
          conv <- abs(r[v] - oldr[v])      #conv is convergence test for item=v
          conv <- conv * (wn / nperiods)                #weight criterion by number of available periods
          if (conv > converge)  converge <- conv        #converge is the global max of conv
        } #end if
        if (!is.na(r[v])) oldr[v] <- r[v]
        if (!is.na(r[v])) valid[v] <- r[v]^2
        if (!is.na(av[v])) wtmean <- wtmean + av[v] * valid[v]
        if (!is.na(std[v])) wtstd <- wtstd + std[v] * valid[v]
        if (!is.na(r[v])) vsum <- vsum + valid[v]
      } #end v loop
      
      if (vsum > 0)  wtmean <- wtmean / vsum
      if (vsum > 0)  wtstd <- wtstd / vsum
      if (pass == 1) {
        mean1 <- wtmean
        std1 <- wtstd
        e1=evalue
      } else {
        wtmean <- mean1
        wtstd <- std1 #*unexp
      } #end if
      fbcorr <- cor(mood[1,],mood[2,]) #fnfrontback 
      
      if (quit != 1) {
        outcount<- outcount+1
        cv<- format(round(converge,4),nsmall=4) 
        itfmt<-format(round(iter),justify="right",length=4)
        out[outcount]<- print(paste(itfmt,"       ",cv,"   ",round(tola,4),"    ",round(fbcorr,3),round(alpha1,4),round(alpha,4)))
      }
      if (converge > lastconv)  tola <- tola * 2
      lastconv <- converge
      
      auto = "y"  #skip corr on iter=1, set auto on
      
      if (iter >= 50) break #get out of while loop
    } #END MASTER WHILE ITERATION CONTROL LOOP
    
    if (auto == "y" && converge<tola) { #IF WE REACH THIS CODE WE HAVE A FINAL SOLUTION TO BE REPORTED
      if (pass == 1) out1<- out #hold output for 2 dimensional solution
      auto <- "Q"
      quit <- 1                #flag solution reached, last time through
      r<- iscorr(issue,mood)   #final iteration correlations
      if (pass == 1) r1<- r #hold correlations for 2 dimensional solution
      
      if (pass > 1) {
        unexp <- totalvar 
        totalvar <- unexp * totalvar
        evalue <- evalue * unexp
      } #    end if
      
      if (pass == 1) {
        expprop <- evalue / totalvar
        tot1 <- totalvar
      } else {
        erel <- evalue / totalvar          #% exp relative
        totalvar <- (1 - expprop) * tot1   #true var=original var discounted by %exp
        evalue <- erel * totalvar          #rescale to retain %exp relationship
        expprop <- evalue / tot1           #now reduce eral to expprop
      } #    end if
      
      for (v in 1:nvar) {
        N[v]<- sum(!is.na(issue[,v]))
      }
      var.out<- list(varname=vl,loadings=r,means=av,std.deviations=std)
      
      print(" ")  
      outcount<- outcount+1
      out[outcount]<- print(paste("Eigen Estimate ", round(evalue,2), " of possible ",round(tot1,2)))  
      outcount<- outcount+1
      out[outcount]<- print(paste("  Percent Variance Explained: ",round(100 * expprop,2)))
      
      if (pass !=  2 && npass>1) {
        for (v in 1:nvar) { 
          valid[v] <- 0               #reset all, regmoodissue will set good=1
          if (csign[v] != 0)  issue[,v]<- residmi(issue,v,mood)   #regmoodissue()
        } #v loop
      }  # if
      #begin prn output routine # mood[fb,] is now our estimate,    WHAT ABOUT A SECOND DIMENSION
      latent<- mood[fb,] #vector holds values for output
      if (pass == 1) latent1<- latent #hold first dimension
      print(" ")
      out[outcount+1]<- print(paste("Final Weighted Average Metric:  Mean: ",round(wtmean,2)," St. Dev: ",round(wtstd,2)))
      #for Zelig output
      if (npass==1) {
        extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1)
      } else {
        for (i in 6:outcount) {
          out[i-5]=out[i]
        }
        length(out)<- outcount-5
        extract.out<- list(formula=formula,T=nperiods,nvar=nvar,unit=unit,dimensions=npass,period=period,varname=vl,N=N,means=av,std.deviations=std,setup1=out1,loadings1=r1,latent1=latent1,setup2=out,loadings2=r,latent2=latent)
      }
    } #end if auto="y" 
  } #end of for pass=1,2 loop 
  
  par(col=1) #reset on termination
  class(extract.out)<- "Zextract"
  return(extract.out)
} #end of extract



## Algorithm Implementation

output <- extract(varname = pe$House, date = pe$date,
                  index = pe$Appr, ncases = pe$N, unit = "M") # monthly
output2 <- extract(varname = pe$House, date = pe$date,
                   index = pe$Appr, ncases = pe$N, unit = "A") # annually

length(unique(output$period))

## Plot latent variable monthly
plot(x = output$latent1, axes = F, # monthly representation
     ylab = "%",
     xlab = "Month", type = "l",
     main = "Approval of Presidential economic management")
axis(1, at = c(1:447),
     labels = output$period, cex.axis = .7)
axis(2)
abline(v = 1, col = "red") # Reagan
text(25, 70, "Reagan", col = "red")
abline(v = which(output$period == 1989.01), col = "red") # Bush
text(105, 70, "Bush", col = "red")
abline(v = which(output$period == 1993.01), col = "blue") # Clinton
text(153, 70, "Clinton", col = "blue")
abline(v = which(output$period == 2001.01), col = "red") # Bush
text(247, 70, "Bush", col = "red")
abline(v = which(output$period == 2009.01), col = "blue") # Obama
text(355, 70, "Obama", col = "blue")
abline(v = which(output$period == 2017.01), col = "red") # Trump
text(439, 70, "Trump", col = "red")
box()

## Plot latent variable annually
length(unique(output2$period)) # 38
plot(x = output2$latent1, axes = F, # annually representation
     ylab = "%",
     xlab = "Year", type = "l",
     main = "Approval of Presidential economic management")
axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
               21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38),
     labels = output2$period, cex.axis = .7)
axis(2)
abline(v = 1, col = "red") # Reagan
text(3, 70, "Reagan", col = "red")
abline(v = 9, col = "red") # Bush
text(11, 70, "Bush", col = "red")
abline(v = 13, col = "blue") # Clinton
text(15, 70, "Clinton", col = "blue")
abline(v = 21, col = "red") # Bush
text(23, 70, "Bush", col = "red")
abline(v = 29, col = "blue") # Obama
text(31, 70, "Obama", col = "blue")
abline(v = 37, col = "red") # Trump
text(37.5, 70, "Trump", col = "red")
box()

## Produce numerical output of estimation procedure
summary(output)


## Reproduce Figure 3 in Kellstedt/de Boef (2004)
kellstedt <- subset(pe, subset = pe$Year <= 2001)
kellstedt_lat <- extract(varname = kellstedt$House, date = kellstedt$date,
                         index = kellstedt$Appr, ncases = kellstedt$N, unit = "M")
length(unique(kellstedt_lat$period))
plot(x = kellstedt_lat$latent1, axes = F, # monthly representation
     ylab = "%",
     xlab = "Month", type = "l",
     main = "Approval of Presidential economic management (Kellstedt 2004)")
axis(1, at = c(1:249),
     labels = kellstedt_lat$period, cex.axis = .7)
axis(2)
abline(v = 1, col = "red") # Reagan
text(25, 70, "Reagan", col = "red")
abline(v = which(kellstedt_lat$period == 1989.01), col = "red") # Bush
text(105, 70, "Bush", col = "red")
abline(v = which(kellstedt_lat$period == 1993.01), col = "blue") # Clinton
text(153, 70, "Clinton", col = "blue")
abline(v = which(kellstedt_lat$period == 2001.01), col = "red") # Bush
text(247, 70, "Bush", col = "red")
box()




## Correlations of time series with Stimson metric

# compute averages for each year for each survey house

# for each survey house
mu_abc_y <- tapply(pe$Appr[pe$House == "ABC"], pe$Year[pe$House == "ABC"], mean)
mu_abcwp_y <- tapply(pe$Appr[pe$House == "ABCWP"], pe$Year[pe$House == "ABCWP"], mean)
mu_cbs_y <- tapply(pe$Appr[pe$House == "CBS"], pe$Year[pe$House == "CBS"], mean)
mu_cbsnyt_y <- tapply(pe$Appr[pe$House == "CBSNYT"], pe$Year[pe$House == "CBSNYT"], mean)
mu_gal_y <- tapply(pe$Appr[pe$House == "Gallup"], pe$Year[pe$House == "Gallup"], mean)
mu_lat_y <- tapply(pe$Appr[pe$House == "LATIMES"], pe$Year[pe$House == "LATIMES"], mean)
latent_y <- output2$latent1
names(latent_y) <- output2$period
mu_total_y <- tapply(pe$Appr, pe$Year, mean) # for reference only and total correlation

# now fill with NA's at time-incongruent positions
mu_abc_y <- c(mu_abc_y[1], mu_abc_y[2], NA, NA, NA,
              NA, NA, NA, NA, NA, mu_abc_y[3], mu_abc_y[4],
              mu_abc_y[5], mu_abc_y[6], mu_abc_y[7], mu_abc_y[8],
              NA, NA, NA, NA, mu_abc_y[9], mu_abc_y[10], mu_abc_y[11],
              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
names(mu_abc_y) <- output2$period
#mu_abc_y

mu_abcwp_y <- c(mu_abcwp_y[1:18], NA, NA, mu_abcwp_y[19:36])
names(mu_abcwp_y) <- output2$period
#mu_abcwp_y

mu_cbs_y <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, mu_cbs_y[1:28])
names(mu_cbs_y) <- output2$period
#mu_cbs_y

mu_cbsnyt_y <- c(mu_cbsnyt_y[1:36], NA, NA)
names(mu_cbsnyt_y) <- output2$period
#mu_cbsnyt_y

mu_gal_y <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
              mu_gal_y[1:5], NA, mu_gal_y[6], NA, mu_gal_y[7:24])
names(mu_gal_y) <- output2$period
#mu_gal_y

mu_lat_y <- c(mu_lat_y[1], NA, mu_lat_y[2:4], NA, mu_lat_y[5],
              NA, NA, NA, mu_lat_y[6:11], NA, mu_lat_y[12:13], NA,
              mu_lat_y[14:21], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
names(mu_lat_y) <- output2$period
#mu_lat_y


# now combine into single matrix

matr <- data.frame(latent = output2$latent1,
                   abc = mu_abc_y, abcwp = mu_abcwp_y,
                   cbs = mu_cbs_y, cbsnyt = mu_cbsnyt_y,
                   gallup = mu_gal_y, latimes = mu_lat_y,
                   total = mu_total_y)

cor.matrix <- cor(matr, use = "pairwise.complete.obs")
cor.matrix

#stargazer(cor.matrix, type = "latex",
          #title = "Correlation Matrix Time Series and Metric")


