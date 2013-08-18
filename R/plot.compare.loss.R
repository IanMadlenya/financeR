function plot.compare.loss(loss1, loss2, main1="", main2="") {
  # get y axis limits for barplots of returns
  ret_min <- min(c(loss1, loss2)) 
  ret_max <- max(c(loss1, loss2)) 
  
  # get y axis limits for histograms of loss functions
  h1 <- hist(loss1, plot=FALSE)
  h2 <- hist(loss2, plot=FALSE)
  hist_max <- max(h1$counts, h2$counts)
  
  # plot 2x2
  op <- par(mfrow=c(2, 2))
  barplot(loss1, ylim=c(ret_min, ret_max), main=main1)
  barplot(loss2, ylim=c(ret_min, ret_max), main=main2)
  hist(loss1, xlim=c(ret_min, ret_max), ylim=c(0, hist_max), main=main1, xlab="")
  hist(loss2, xlim=c(ret_min, ret_max), ylim=c(0, hist_max), main=ticker2, xlab="")
  par(op)
}