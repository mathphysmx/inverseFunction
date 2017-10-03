#' @title Evaluates a function passed as a list with function name and named parameters.
#'
#' @description Evaluates a function passed as a list with function name and named parameters.
#'
#' @param x Numeric vector where \code{fun} will be evaluated. If \code{fun} is not a list, \code{x} is not used.
#' This parameter was placed first because is a requirement of the \link{stats}{uniroot} function.
#' @param fun A list with the function name and named parameters. See examples.
#' @param p Positive integer with the position in \code{fun} of the name of the function. Default to 1.
#'
#' @details Evaluates a function in a list with the function name and named parameters. x go to the first input parameter of the function \code{f}.
#' @return If \code{fun} is a list, the evaluation of \code{fun} at points \code{x}. If \code{f} is not a list, the result is the input \code{f}.
#' @export
#'
#' @examples
#' # Example 1: no evaluation because f is already given.
#' evalFunc(x = 0:2, fun = 1)
#' evalFunc(x = 0:2, fun = 1:3)
#'
#' # Example 2: function evaluation
#' fe <- list(fun = 'dnorm', mean = 3, sd = 2)
#' evalFunc(x = 0:2, fun = fe) # # dnorm(0:2, mean = 3, sd =2)
#'
#' # Example 3: Function name not being the first element of the list fe.
#' fe <- list(mean = 1.5, sd = 2, f = 'dnorm')
#' evalFunc(x = 0:2, fun = fe, p = 3) # dnorm(0:2, mean = 1.5, sd =2)
#'
#' # Example 4:
#' library(empiricalDistribution)
#' library(bernstein)
#' ex <- cbind(1:5, c(2, 4, 3, 6, 7)); print(ex)
#' set.seed(1); ex <- ex[sample(1:5), ]
#' eDiffEC <- forwardDifference(empiricalCDF2Dcounts(ex))
#' eu <- 0.3; ev <- 0.1
#' bernstein2DderivativeX(v = ev, u = eu, diffEC = eDiffEC)
#' efun <- list(fun = 'bernstein2DderivativeX', u = eu, diffEC = eDiffEC)
#' evalFunc(x = ev, fun = efun)
#'
evalFunc <- function(x, fun, p = 1){

  fval <- fun
  if(is.list(fun)){
    fpar <- fun; fpar[p] <- NULL
    fval <- do.call(what = fun[[p]],
                  args = c(list(x), fpar))
  }

  return(fval)
}
