#' Empirical distribution functions
#' 
#' Empirical cdf with cumsum. Empirical pdf with kernel density function
#' 
#' @param x a vectors of values
#' @param x_f lower bound of x
#' @param x_e upper bound of x
#' @param eps tolarance
#' @param ... arguments for densratio::
#'
#' @return function of empirical pdf/cdf
#' 
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' cdf_x <- emp_cdf(x)
#' print(cdf_x(0))
emp_cdf  <-  function(x, x_f, x_e, eps) {
  x <- sort(x)
  if (missing(eps)) {
    eps <- diff(sort(unique(x))) / 10
  }
  
  if (missing(x_f)) {
    x_f <- min(x) - eps
  } else {
    x_f <- min(x_f, min(x) - eps)
  }
  
  if (missing(x_e)) {
    x_e <- max(x) + eps
  } else {
    x_e <- max(x_e, max(x) + eps)
  }
  
  xu <- c(x_f, unique(x), x_e)
  yu <- sapply(xu, function(xi) mean(fBasics::Heaviside(xi - x)))
  
  fn <- approxfun(xu, yu, method="linear", yleft=0, yright=1, rule=2)
  fn
}  
