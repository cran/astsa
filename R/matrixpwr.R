matrixpwr <-
function(A, power){
   if (!is.numeric(power) || length(power) > 1 || 
        !is.finite(power))  stop("power must be a single number") 
   if (!is.matrix(A))  stop("object not a matrix") 
   if (abs( nrow(A) - ncol(A) ) > 0 ) stop("matrix must be square") 
   if (power == 0) return(diag(1, nrow(A)))
   if (power == 1) return(A)
   if (power < 0 ) {
      singtest <- "matrix" %in% class(try(solve(A),silent=TRUE))
      if (!singtest) stop("matrix singular")
   }
   if (isSymmetric(unname(A))) {
    with(eigen(A), vectors %*% (values^power * t(vectors)))
   } else {
    with(eigen(A), vectors %*% (values^power * solve(vectors))) 
   }
}

"%^%" <- function(A, power) matrixpwr(A, power)  
