acf1 <-
function(series, max.lag=NULL, plot=TRUE, main=NULL, ylim=NULL, pacf=FALSE, 
          ylab=NULL, xlab=NULL, na.action = na.pass, ...){
  frequency = stats::frequency
  acf       = stats::acf
  xfreq     = frequency(series)
  num       = length(series)

  if (num < 3) stop("More than 2 observations are needed")
  if (num > 59 & is.null(max.lag))  max.lag = max(ceiling(10+sqrt(num)), 4*xfreq) 
  if (num < 60 & is.null(max.lag))  max.lag = min(floor(6*log10(num+5)), num-2)
  if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
  if (is.null(main)) main = paste("Series: ",deparse(substitute(series)))

  if (pacf) {
   ACF = stats::pacf(series, max.lag, plot=FALSE, na.action = na.action, ...)$acf
   ACF = as.numeric(ACF)
   } else {
   ACF = acf(series, max.lag, plot=FALSE, na.action = na.action, ...)$acf[-1]
  }

  LAG = (1:max.lag)/xfreq

 if(plot){ 
  abline = graphics::abline
  U = (-1/num) + (2/sqrt(num))
  L = (-1/num) - (2/sqrt(num))
  if (is.null(ylim)) { 
   minA = min(ACF)
   maxA = max(ACF)
   minu = min(minA, L)-.01
   maxu = min(maxA+.2, 1)
   ylim = c(minu,maxu) 
 }
  Xlab = ifelse(xfreq>1, paste('LAG \u00F7', xfreq), 'LAG')
  Ylab = ifelse(pacf, 'PACF', 'ACF')  
  if (!is.null(ylab)) Ylab=ylab
  if (!is.null(xlab)) Xlab=xlab
  tsplot(LAG, ACF, ylim=ylim, main=main, xlab=Xlab, ylab=Ylab, type='h', ...)
  abline(h=c(0,L,U), lty=c(1,2,2), col=c(8,4,4))
  return(round(ACF, 2)) 
 } else {
  return(ACF)
  }
}


