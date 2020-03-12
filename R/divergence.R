#' Kullback-Leibler Divergence Estimation
#'
#' @param x values sampled from the distribution of interest
#' @param y values sampled from the comparator distribution
#' @param type using empirical p.d.f. or c.d.f.
#' @param ... options for density(...) if empirical p.d.f. applied
#'
#' @references 
#' Pérez-Cruz F. Kullback-Leibler divergence estimation of continuous distributions. In2008 IEEE international symposium on information theory 2008 Jul 6 (pp. 1666-1670). IEEE.
#' 
#'
#' @return value of KL divergence
#' @export
#'
#' @examples
#' library(divKL)
#' 
#' x <- rnorm(100, 1)
#' y <- rnorm(100, 3)
#' KL_divergence(x, y, "pdf")
#' KL_divergence(x, y, "cdf")
#' KL_divergence(y, x, "pdf")
#' KL_divergence(y, x, "cdf")
KL_divergence <- function(x, y, type=c("pdf", "cdf"), ...) {
  type <- match.arg(type)
  
  if (type == "cdf") {
    dx <- diff(sort(unique(x)))
    dy <- diff(sort(unique(y)))
    ex <- min(dx); ey <- min(dy)
    eps <- min(ex, ey)/10
    
    mx <- max(c(x, y)) + eps
    mn <- min(c(x, y)) - eps
    
    n <- length(x)    
    P <- emp_cdf(x, mn, mx, eps); Q <- emp_cdf(y, mn, mx, eps)
    kl <- mean(log(P(x + eps/2) - P(x - eps/2)) - log(Q(x + eps/2) - Q(x - eps/2))) - 1
  } else {
    dr <- densratio::densratio(x, y, ...)
    kl <- mean(log(dr$compute_density_ratio(x)))
  }
  kl
}
