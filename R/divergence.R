#' 
#'
#' @param x values sampled from the distribution of interest
#' @param y values sampled from the comparator distribution
#' @param type using empirical p.d.f. or c.d.f.
#'
#' @return value of KL divergence
#' @export
#'
#' @examples
#' x <- rnorm(100, 1)
#' y <- rnorm(100, 3)
#' KL_divergence(x, y, "pdf")
#' KL_divergence(x, y, "cdf")
#' KL_divergence(y, x, "pdf")
#' KL_divergence(y, x, "cdf")
KL_divergence <- function(x, y, type=c("pdf", "cdf")) {
  type <- match.arg(type)
  
  if (type == "cdf") {
    mx <- max(y); mn <- min(y)
    x[x>mx] <- mx; x[x < mn] <- mn
    
    dx <- diff(sort(unique(x)))
    dy <- diff(sort(unique(y)))
    ex <- min(dx); ey <- min(dy)
    e <- min(ex, ey)/50
    n <- length(x)    
    P <- emp_cdf(x); Q <- emp_cdf(y)
    kl <- mean(log(P(x)-P(x-e))- log(Q(x)-Q(x-e)))
  } else {
    P <- emp_pdf(x); Q <- emp_pdf(y)
    kl <- mean((log(P(x)) - log(Q(x))))
  }
  kl
}