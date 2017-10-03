#' @title Numerical computation of a univariate function.
#'
#' @description Only computes a sigle inverse. For multiple inverseses, use a (for, while) loop.
#'
#' @param dvar scalar value. \emph{d}ependent \emph{var}iable, usually writen the cartesian plane as 'y'.
#' @param fun A list of parameters (name included as \code{fun}) describing a function.
#' @param interval a vector containing the end-points of the interval to be searched for the root.
#' @param silent logical: should the report of error messages be suppressed?
#' @param ... further arguments passed to \code{stats}{uniroot}.
#'
#' @export
#'
#' @importFrom "stats" "uniroot"
#' @examples
#' myFun <- function(x, b=3) return(2*x+b)
 #'fe <- list(fun='myFun', b = 4)
 #'# dvar = y = 10 = 2*3 + 4
 #'inverseFunction(dvar = 10, fun = fe, interval = c(-10, 10), silent = TRUE)
 #'
#' # Example 2:
#' fe <- list(fun = 'pnorm', mean = 3, sd = 2)
#' # pnorm(4.7, mean = 3, sd = 2) = 0.8023375
#' inverseFunction(dvar = 0.8023375, fun = fe, interval = c(4, 5), silent = TRUE)
#'
#' # Example 3:
#' exy <- cbind(1:5, c(2, 4, 3, 6, 7)); print(exy)
#' set.seed(1); exy <- exy[sample(1:5), ]
#' library(bernstein)
#' library(empiricalDistribution)
#' empCopulaCountsmatrix <- forwardDifference(empiricalCDF2Dcounts(exy))
#' eu <- 0.3; ev <- 0.8
#' et <- bernstein2DderivativeX(v = ev, u = eu, diffEC = empCopulaCountsmatrix)
#' efun <- list(fun = 'bernstein2DderivativeX', u = eu,
#'             diffEC = empCopulaCountsmatrix)
#' inverseFunction(dvar = et, fun = efun)
#'
#  TODO:Make a vectorized version of this function <13-09-17, yourname> #
#  TODO: Make an embarrassingly parallel verion of this function by splitting the \code{dvar} vector <13-09-17, yourname> #
#'
inverseFunction <- function (dvar, fun, interval = c(0,1),
                                      silent = TRUE, ...) {

	tmpFunc <- function(x, fun, dvar){ # dvar = [d]ependent [var]iable = y
		return(evalFunc(x, fun) - dvar)
	}

	inv <- NULL
	while(!is.numeric(inv)){
		inv <- try(uniroot(f = tmpFunc, fun, interval = interval,
		                   dvar = dvar, ...)$root, silent = silent)
	}
	return(inv)
}

