##############################################
###                                        ###
### PLOT NORMAL DISTRIBUTION DENSITY CURVE ###
###       MEAN = mean AND SD = sd          ###
###                                        ###
##############################################

###
# function to plot normal distribution density curve
###
plot_norm_dist <- function(norm_dist,
                           title = "Empirical Rule", 
                           subtitle = "(Normal distribution)",
                           title.cex = 2, title.font = 4,
                           subtitle.cex = 1.25, subtitle.font = 1,
                           n.sd = 4,
                           file.name = "normal_distribution.png",
                           annotations = TRUE,
                           save.plot = FALSE) {
   #' This function plots the normal distribution density curve with 
   #' empirical rule annotations
   
   # kernel density estimation
   kde <- density(norm_dist)
   
   # find mean and sd of the dist
   m <- round(mean(norm_dist), 2)
   sd <- round(sd(norm_dist), 2)
   
   # if just printing the plot on screen
   if (save.plot) png(filename = file.name, width = 1200, height = 627, res = 125)
   
   # plot normal distribution curve bare bones
   plot(kde, lwd = 3, main = "", xlab = "", ylab = "", bty = "n", axes = 0)
   mtext(text = title, side = 3, line = 2, cex = title.cex, font = title.font)
   mtext(text = subtitle, side = 3, line = 0.5, cex = subtitle.cex, font = subtitle.font)
   mtext(text = bquote(mu==.(m)*","~sigma==.(sd)), side = 3, line = -0.75, cex = 1, font = 1)
   
   # draw x-axes (actual mean and sd values and mean +- number of sd notation)
   ticks <- seq(m - n.sd*sd, m + n.sd*sd, sd)
   x_ticks_labs <- lapply(-n.sd:n.sd, 
                          function(x) {
                             if (x == 0) quote(mu) 
                             else if (x < 0) bquote(mu-.(abs(x))*sigma) 
                             else bquote(mu+.(x)*sigma)
                          })
   x_ticks_labs <- as.expression(x_ticks_labs)
   axis(side = 1, at = ticks, labels = ticks, line = -.5)
   axis(side = 1, at = ticks, labels = x_ticks_labs, tick = F, line = 0.5)
   
   # if need annotations
   if (annotations) {
      # local function to find value of y for specific sd on x
      find_y_kde <- function(nsd) kde$y[kde$x == min(kde$x[kde$x >= m+sd*nsd])]
      
      # # add vertical line segments for mean and sd
      segments(x0 = ticks, y0 = 0, x1 = ticks, 
               y1 = c(0, 
                      find_y_kde(3)*10, find_y_kde(2)*2.5, find_y_kde(1)*1.3,
                      max(kde$y),
                      find_y_kde(1)*1.3, find_y_kde(2)*2.5, find_y_kde(3)*10, 
                      0), 
               lty = 2, col = "gray")
      
      # add annotation arrows
      arrows(x0 = sort(ticks[ticks < m]),
             x1 = sort(ticks[ticks > m], decreasing = T),
             y0 = c(0, find_y_kde(3)*10, find_y_kde(2)*2.5, find_y_kde(1)*1.3),
             y1 = c(0, find_y_kde(3)*10, find_y_kde(2)*2.5, find_y_kde(1)*1.3),
             col = c(NA, "red", "blue", "green"),
             code = 3, lty = 1)

      # # add text annotation boxes
      rect(xleft = m-0.45*sd,
           ybottom = c(find_y_kde(1)*1.3, find_y_kde(2)*2.5, find_y_kde(3)*10)  - 0.015/sd,
           xright = m+0.45*sd,
           ytop = c(find_y_kde(1)*1.3, find_y_kde(2)*2.5, find_y_kde(3)*10) + 0.015/sd,
           border = c("green", "blue", "red"), col = "white")
      text(x = m,
           y = c(find_y_kde(1)*1.3, find_y_kde(2)*2.5, find_y_kde(3)*10),
           labels = c("68%", "95%", "99.7%"),
           cex = 1.25, font = 2, col = "black")
   }
   
   # close dev.off if saving plot to file
   if (save.plot) dev.off()
}

###
# simulate and plot normal distribution
###

# set mean and sd
m = 0
sd = 1

# number of random samples
n = 1e6

# simulate sampling from normal distribution
set.seed(1)
norm_dist <- rnorm(n = n, mean = m, sd = sd)

# plot 
plot_norm_dist(norm_dist = norm_dist, annotations = T)

# # for saving plot (.png extension)
# plot_norm_dist(norm_dist = norm_dist, annotations = T,
#                save.plot = TRUE, file.name = "plots/normal_distribution.png")
