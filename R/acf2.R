acf2 <-
function(series,max.lag=NULL){
  num=length(series)
  if (num > 49 & is.null(max.lag)) max.lag=ceiling(10+sqrt(num))
  if (num < 50 & is.null(max.lag))  max.lag=floor(5*log10(num))
  if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
  ACF=stats::acf(series, max.lag, plot=FALSE)$acf[-1]
  PACF=stats::pacf(series, max.lag, plot=FALSE)$acf
  LAG=1:max.lag/frequency(series)
  minA=min(ACF)
  minP=min(PACF)
  U=2/sqrt(num)
  L=-U
  minu=min(minA,minP,L)-.01
  old.par <- par(no.readonly = TRUE)
  par(mfrow=c(2,1), mar = c(3,3,2,0.8),
    oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
  plot(LAG, ACF, type="h",ylim=c(minu,1), 
    main=paste("Series: ",deparse(substitute(series))))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  plot(LAG, PACF, type="h",ylim=c(minu,1))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  on.exit(par(old.par))  
  ACF<-round(ACF,2); PACF<-round(PACF,2)    
  return(cbind(ACF, PACF)) 
  }

