\name{ARMAtoAR}
\alias{ARMAtoAR}
\title{Convert ARMA Process to Infinite AR Process}
\description{
  Gives the \eqn{\pi}-weights in the invertible representation of an ARMA model.
} 
\usage{
ARMAtoAR(ar = 0, ma = 0, lag.max=20)
}
\arguments{
  \item{ar}{vector of AR coefficients}
  \item{ma}{vector of MA coefficients}
  \item{lag.max}{number of pi-weights desired}
}
\value{
  A vector of coefficients.
}
\references{You can find demonstrations of  astsa capabilities at  
\href{https://github.com/nickpoison/astsa/blob/master/fun_with_astsa/fun_with_astsa.md}{FUN WITH ASTSA}.

The most recent version of the package can be found  at \url{https://github.com/nickpoison/astsa/}.

In addition, the News and ChangeLog files are at  \url{https://github.com/nickpoison/astsa/blob/master/NEWS.md}.

The webpages for the texts and some help on using R for time series analysis can be found at
 \url{https://nickpoison.github.io/}.   
}
\examples{
ARMAtoAR(ar=.9, ma=.5, 10)
}
\keyword{ts}
