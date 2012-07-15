\name{astsadata}
\alias{astsadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{All the Data in One Swell Foop
}
\description{
Conveniently loads all the astsa data files.
}
\usage{
astsadata()
}
%- maybe also 'usage' for other objects documented here.
\arguments{\item{}{There are none.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%%\value{\item{ACF}{The sample ACF}
%%\item{PACF}{The sample PACF}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
\details{Loads all the astsa data files to save you time (and consequently money).  If you save the workspace, the data files will be there the next time you start R.  Note that the size of the all the data files combined is fairly small, less than 1/2 MB compressed.  This is available in versions 1.1 and higher.
}
\references{\url{http://www.stat.pitt.edu/stoffer/tsa3/}
%% ~put references to the literature/web site here ~
}
\author{
D.S. Stoffer 
}
%\note{
%This is bacisally a front end for \code{acf()} and \code{pacf()} provided in \code{tseries}.
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%acf2(rnorm(100))
%acf2(rnorm(100), 25)
%}
%% ##--	or do  help(data=index)  for the standard data sets.
%% 
%% ## The function is currently defined as
%% function(series,max.lag=NULL){
%%   num=length(series)
%%   if (num > 49 & is.null(max.lag)) max.lag=ceiling(10+sqrt(num))
%%   if (num < 50 & is.null(max.lag))  max.lag=floor(5*log10(num))
%%   if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
%%   ACF=acf(series, max.lag, plot=FALSE)$acf[-1]
%%   PACF=pacf(series, max.lag, plot=FALSE)$acf
%%   LAG=1:max.lag/frequency(series)
%%   minA=min(ACF)
%%   minP=min(PACF)
%%   U=2/sqrt(num)
%%   L=-U
%%   minu=min(minA,minP,L)-.01
%%   old.par <- par(no.readonly = TRUE)
%%   par(mfrow=c(2,1), mar = c(3,3,2,0.8),
%%     oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
%%   plot(LAG, ACF, type="h",ylim=c(minu,1), 
%%     main=paste("Series: ",deparse(substitute(series))))
%%     abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
%%   plot(LAG, PACF, type="h",ylim=c(minu,1))
%%     abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
%%   on.exit(par(old.par))  
%%   ACF<-round(ACF,2); PACF<-round(PACF,2)    
%%   return(cbind(ACF, PACF)) 
%%   }
%}
%% % Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}
