hist_loss <- function(loss, plot.normal=TRUE, plot.kernel=TRUE, kernel="gaussian", plot.risk=NULL, ...) {
  density_loss <- density(loss, kernel=kernel)
  hist(loss, freq=FALSE, ylim=c(0,ceiling(max(density_loss$y))), ...)
  if (plot.kernel) { lines(density_loss, col="red", lwd=2) }
  pos <- seq(min(loss), max(loss), length.out=100)
  if(plot.normal) { lines(pos, dnorm(pos, mean=mean(as.vector(loss)), sd=sd(as.vector(loss))), col="blue", lwd=2) }

  if ("mean" %in% plot.risk) { lines(c(mean(loss), mean(loss)), c(0, ceiling(max(density_loss$y))), col="green", lwd=2) }

}