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
                           save.plot = TRUE,
                           annotations = TRUE) {
   #' This function plots the normal distribution density curve with 
   #' empirical rule annotations
   
   # kernel density estimation
   kde <- density(norm_dist)
   
   # find mean and sd of the dist
   m <- round(mean(norm_dist))
   sd <- round(sd(norm_dist))
   
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
      
      # local function to add box and text annotations
      add_text_box <- function(x, y, text, col, text.width) {
         legend(x, y, legend = text, box.col = col, text.font = 2, cex = 1.25, 
                xjust = 0.5, yjust = 0.5, x.intersp = -0.75, y.intersp = 0, 
                text.width = text.width)
      }
      
      # add text annotation boxes
      add_text_box(m, find_y_kde(1)*1.3, text = "68%", col = "green", text.width = 0.75)
      add_text_box(m, find_y_kde(2)*2.5, text = "95%", col = "blue", text.width = 1)
      add_text_box(m, find_y_kde(3)*10, text = "99.7%", col = "red", text.width = 1.25)
   }
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

# if you want to save plot as image or pdf
png(filename = "normal_distribution.png", width = 1200, height = 627)
plot_norm_dist(norm_dist = norm_dist)
dev.off()

# set mean and sd
m = 0
sd = 1

# number of random samples
n = 1e6

# simulate sampling from normal distribution
set.seed(1)
norm_dist <- rnorm(n = n, mean = m, sd = sd)
# kernel density estimation
kde <- density(norm_dist)

# plot simulated normal distribution
plot(kde, lwd = 3, main = "", xlab = "", ylab = "", bty = "n", axes = 0)
mtext(text = "Empirical Rule", side = 3, line = 2, cex = 2, font = 4)
mtext(text = "(Normal distribution)", side = 3, line = 0.5, cex = 1.25, font = 1)
mtext(text = bquote(mu==.(m)*","~sigma==.(sd)), side = 3, line = -0.75, cex = 1, font = 1)

ticks <- seq(m - 4*sd, m + 4*sd, sd)
x_ticks_labs <- as.expression(lapply(-4:4, function(x) 
   if (x==0) quote(mu) else if (x<0) bquote(mu-.(abs(x))*sigma) else bquote(mu+.(x)*sigma)))
axis(side = 1, at = ticks, labels = ticks, line = -.5)
axis(side = 1, at = ticks, labels = x_ticks_labs, tick = F, line = 0.5)

segments(x0 = m, y0 = 0, x1 = m, y1 = max(kde$y), lty = 2, col = "gray")
segments(x0 = c(m-1*sd,m+1*sd), x1 = c(m-1*sd,m+1*sd), y0 = 0, y1 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*1])]*1.3, lty = 2, col = "gray")
segments(x0 = c(m-2*sd,m+2*sd), x1 = c(m-2*sd,m+2*sd), y0 = 0, y1 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*2])]*2.5, lty = 2, col = "gray")
segments(x0 = c(m-3*sd,m+3*sd), x1 = c(m-3*sd,m+3*sd), y0 = 0, y1 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*3])]*10, lty = 2, col = "gray")
arrows(x0 = m-1*sd, x1 = m+1*sd, y0 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*1])]*1.3, y1 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*1])]*1.3, lty = 1, col = "green", code = 3)
arrows(x0 = c(m,m), x1 = c(m-2*sd,m+2*sd), y0 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*2])]*2.5, y1 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*2])]*2.5, lty = 1, col = "blue")
arrows(x0 = c(m,m), x1 = c(m-3*sd,m+3*sd), y0 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*3])]*10, y1 = kde$y[kde$x == min(kde$x[kde$x >= m+sd*3])]*10, lty = 1, col = "red")

legend(x = m, y = kde$y[kde$x == min(kde$x[kde$x >= m+sd*1])]*1.3, legend = "68%", box.col = "green", text.font = 2, cex = 1.25, xjust = 0.5, yjust = 0.5, x.intersp = -0.75, y.intersp = 0, text.width = 0.75)
legend(x = m, y = kde$y[kde$x == min(kde$x[kde$x >= m+sd*2])]*2.5, legend = "95%", box.col = "blue", text.font = 2, cex = 1.25, xjust = 0.5, yjust = 0.5, x.intersp = -0.75, y.intersp = 0, text.width = 1)
legend(x = m, y = kde$y[kde$x == min(kde$x[kde$x >= m+sd*3])]*10, legend = "99.7%", box.col = "red", text.font = 2, cex = 1.25, xjust = 0.5, yjust = 0.5, x.intersp = -0.75, y.intersp = 0, text.width = 1.25)


# chebyshev's inequality
set.seed(1)
d_bimod <- c(rnorm(0.4*n, -2), rnorm(0.6*n, 2))
d_bimod_den <- density(d_bimod)
plot(d_bimod_den)

