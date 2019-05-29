emp_cdf  <-  function(x) {
  x   <-   sort(x)
  x.u <-   unique(x)
  n  <-  length(x) 
  x.rle  <-  rle(x)$lengths
  y  <-  (cumsum(x.rle)-0.5) / n
  fn  <-  approxfun(x.u, y, method="linear", yleft=0, yright=1, rule=2)
  fn 
}  


emp_pdf <- function(x) {
  x <- sort(x)
  den <- density(x)
  
  fn <- approxfun(den$x, den$y, method="linear", rule=2)
    
  fn  
}


