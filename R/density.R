#' Empirical distribution functions
#' 
#' Empirical cdf with cumsum. Empirical pdf with kernel density function
#' 
#' @param x a vectors of values
#'
#' @return function of empirical pdf/cdf
#' 
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' cdf_x <- emp_cdf(x)
#' print(cdf_x(0))
#' 
#' pdf_x <- emp_pdf(x)
#' print(pdf_x(0))
emp_cdf  <-  function(x) {
  x   <-   sort(x)
  x.u <-   unique(x)
  n  <-  length(x) 
  x.rle  <-  rle(x)$lengths
  y  <-  (cumsum(x.rle)-0.5) / n
  fn  <-  approxfun(x.u, y, method="linear", yleft=0, yright=1, rule=2)
  fn 
}  


#' @export
#' @rdname emp_cdf
emp_pdf <- function(x, ...) {
  x <- sort(x)
  den <- density(x, ...)
  
  fn <- approxfun(den$x, den$y, method="linear", rule=2)
    
  fn  
}


