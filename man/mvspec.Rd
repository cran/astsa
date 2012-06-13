\name{mvspec}
\alias{mvspec}
%- Also NEED an '\alias' specify EACH other topic documented here.
\title{Multivariate Spectral Estimation
%%  ~~function to do ... ~~
}
\description{This is \code{spec.pgram} with a few changes in the defaults and written so you can easily extract the estimate of the multivariate spectral matrix as \code{fxx}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mvspec(x, spans = NULL, kernel = NULL, taper = 0, pad = 0, 
         fast = TRUE, demean = TRUE, detrend = FALSE, 
         plot = FALSE, na.action = na.fail, ...)
}
%- maybe also 'usage' specify other objects documented here.
\arguments{
  \item{x }{p-variate time series (i.e., the p columns of x are time series)
%%     ~~Describe \code{x} here~~
}
  \item{spans }{specify smoothing; same as \code{spec.pgram}
%%     ~~Describe \code{spans} here~~
}
  \item{kernel }{specify kernel; same as \code{spec.pgram}
%%     ~~Describe \code{kernel} here~~
}
  \item{taper }{specify taper; same as \code{spec.pgram}
%%     ~~Describe \code{taper} here~~
}
  \item{pad }{specify padding; same as \code{spec.pgram}
%%     ~~Describe \code{pad} here~~
}
  \item{fast }{specify use of FFT; same as \code{spec.pgram}
%%     ~~Describe \code{fast} here~~
}
  \item{demean }{if TRUE, series is demeaned first; same as \code{spec.pgram}
%%     ~~Describe \code{demean} here~~
}
  \item{detrend }{if TRUE, series is detrended first; same as \code{spec.pgram}
%%     ~~Describe \code{detrend} here~~
}
  \item{plot }{plot the estimate; same as \code{spec.pgram}
%%     ~~Describe \code{plot} here~~
}
  \item{na.action }{same as \code{spec.pgram}
%%     ~~Describe \code{na.action} here~~
}
  \item{\dots }{additional arguments; same as \code{spec.pgram}
%%     ~~Describe \code{\dots} here~~
}
}
\details{This is \code{spec.pgram} with a few changes in the defaults and written so you can easily extract the estimate of the multivariate spectral matrix as \code{fxx}. See Example 7.12 on page 461 for a demonstration.
%%  ~~ If necessary, more details than the description above ~~
}
\value{An object of class "spec", which is a list containing at least the following components:

\item{fxx }{spectral matrix estimates; an array of dimensions \code{dim = c(p,p,nfreq)}}
\item{freq }{vector of frequencies at which the spectral density is estimated.}
\item{spec }{vector (for univariate series) or matrix (for multivariate series) of estimates of the spectral density at frequencies corresponding to freq.}
\item{coh }{NULL for univariate series. For multivariate time series, a matrix containing the squared coherency between different series. Column i + (j - 1) * (j - 2)/2 of coh contains the squared coherency between columns i and j of x, where i < j.}
\item{phase }{NULL for univariate series. For multivariate time series a matrix containing the cross-spectrum phase between different series. The format is the same as coh.}
\item{series }{The name of the time series.}
\item{snames }{For multivariate input, the names of the component series.}
\item{method }{The method used to calculate the spectrum.}

The result is returned invisibly if plot is true.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{\url{http://www.stat.pitt.edu/stoffer/tsa3/}
%% ~put references to the literature/web site here ~
}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%%%  \examples{
%%%  ##---- Should be DIRECTLY executable !! ----
%%%  ##-- ==>  Define data, use random,
%%%  ##--	or do  help(data=index)  specify the standard data sets.
%%%  
%%%  ## The function is currently defined as
%%%  function(x, spans = NULL, kernel = NULL, taper = 0, pad = 0, 
%%%      fast = TRUE, demean = TRUE, detrend = FALSE, plot = FALSE, 
%%%      na.action = na.fail,...) 
%%%  {
%%%      series <- deparse(substitute(x))
%%%      x <- na.action(as.ts(x))
%%%      xfreq <- frequency(x)
%%%      x <- as.matrix(x)
%%%      N <- N0 <- nrow(x)
%%%      nser <- ncol(x)
%%%      if (!is.null(spans)) 
%%%          kernel <- {
%%%              if (is.tskernel(spans)) 
%%%                  spans
%%%              else kernel("modified.daniell", spans\%/\%2)
%%%          }
%%%      if (!is.null(kernel) && !is.tskernel(kernel)) 
%%%          stop("must specify 'spans' or a valid kernel")
%%%      if (detrend) {
%%%          t <- 1:N - (N + 1)/2
%%%          sumt2 <- N * (N^2 - 1)/12
%%%          specify (i in 1:ncol(x)) x[, i] <- x[, i] - mean(x[, i]) - 
%%%              sum(x[, i] * t) * t/sumt2
%%%      }
%%%      else if (demean) {
%%%          x <- sweep(x, 2, colMeans(x))
%%%      }
%%%      x <- spec.taper(x, taper)
%%%      u2 <- (1 - (5/8) * taper * 2)
%%%      u4 <- (1 - (93/128) * taper * 2)
%%%      if (pad > 0) {
%%%          x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
%%%          N <- nrow(x)
%%%      }
%%%      NewN <- if (fast) 
%%%          nextn(N)
%%%      else N
%%%      x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
%%%      N <- nrow(x)
%%%      Nspec <- floor(N/2)
%%%      freq <- seq(from = xfreq/N, by = xfreq/N, length = Nspec)
%%%      xfft <- mvfft(x)
%%%      pgram <- array(NA, dim = c(N, ncol(x), ncol(x)))
%%%      specify (i in 1:ncol(x)) {
%%%          specify (j in 1:ncol(x)) {
%%%              pgram[, i, j] <- xfft[, i] * Conj(xfft[, j])/(N0 * 
%%%                  xfreq)
%%%              pgram[1, i, j] <- 0.5 * (pgram[2, i, j] + pgram[N, 
%%%                  i, j])
%%%          }
%%%      }
%%%      if (!is.null(kernel)) {
%%%          specify (i in 1:ncol(x)) specify (j in 1:ncol(x)) pgram[, i, 
%%%              j] <- kernapply(pgram[, i, j], kernel, circular = TRUE)
%%%          df <- df.kernel(kernel)
%%%          bandwidth <- bandwidth.kernel(kernel)
%%%      }
%%%      else {
%%%          df <- 2
%%%          bandwidth <- sqrt(1/12)
%%%      }
%%%      df <- df/(u4/u2^2)
%%%      df <- df * (N0/N)
%%%      bandwidth <- bandwidth * xfreq/N
%%%      pgram <- pgram[2:(Nspec + 1), , , drop = FALSE]
%%%      spec <- matrix(NA, nrow = Nspec, ncol = nser)
%%%      specify (i in 1:nser) spec[, i] <- Re(pgram[1:Nspec, i, i])
%%%      if (nser == 1) {
%%%          coh <- phase <- NULL
%%%      }
%%%      else {
%%%          coh <- phase <- matrix(NA, nrow = Nspec, ncol = nser * 
%%%              (nser - 1)/2)
%%%          specify (i in 1:(nser - 1)) {
%%%              specify (j in (i + 1):nser) {
%%%                  coh[, i + (j - 1) * (j - 2)/2] <- Mod(pgram[, 
%%%                    i, j])^2/(spec[, i] * spec[, j])
%%%                  phase[, i + (j - 1) * (j - 2)/2] <- Arg(pgram[, 
%%%                    i, j])
%%%              }
%%%          }
%%%      }
%%%      specify (i in 1:nser) spec[, i] <- spec[, i]/u2
%%%      spec <- drop(spec)
%%%  #========================
%%%      fxx=array(NA, dim=c(nser,nser,Nspec))
%%%      specify (k in 1:Nspec){
%%%  		fxx[,,k]=pgram[k,,]
%%%  	    }
%%%  #========================    
%%%      spg.out <- list(freq = freq, spec = spec, coh = coh, phase = phase, 
%%%          kernel = kernel, df = df, bandwidth = bandwidth, n.used = N, 
%%%          fxx=fxx,
%%%          orig.n = N0, series = series, snames = colnames(x), method = ifelse(!is.null(kernel), 
%%%              "Smoothed Periodogram", "Raw Periodogram"), taper = taper, 
%%%          pad = pad, detrend = detrend, demean = demean)
%%%      class(spg.out) <- "spec"
%%%      if (plot) {
%%%          plot(spg.out, ...)
%%%          return(invisible(spg.out))
%%%      }
%%%      else return(spg.out)
%%%    }
%%%  }
%%%  % Add one or more standard keywords, see file 'KEYWORDS' in the
%%%  % R documentation directory.
\keyword{ ts }
%%%  %\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
%%%  