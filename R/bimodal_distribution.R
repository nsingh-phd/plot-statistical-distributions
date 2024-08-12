###############################################
###                                         ###
### PLOT BIMODAL DISTRIBUTION DENSITY CURVE ###
###                                         ###
###############################################

plot_bimod_dist <- function(peak1 = NA_real_, sd1 = NA_real_,
                            peak2 = NA_real_, sd2 = NA_real_,
                            prop = 0.6, n = 1e6,
                            title = "Bimodal Distribution", 
                            subtitle = NULL,
                            title_cex = 2, title_font = 4,
                            subtitle_cex = 1.25, subtitle_font = 1,
                            n.sd = 4,
                            file.name = NA_character_,
                            annotations = TRUE,
                            save.plot = FALSE) {
   #' This function plots the normal distribution density curve with 
   #' empirical rule annotations
   
   # Input Validation
   if (!all(sapply(c(peak1, peak2, sd1, sd2), is.numeric)) || sd1 <= 0 || sd2 <= 0) {
      stop("Invalid input: 'm' and 'sd' must be numeric, and 'sd' must be positive.")
   }
   
   # Set random seed for reproducibility
   set.seed(123)
   
   # Generate data from the two normal distributions
   data1 <- rnorm(n * prop, peak1, sd1)
   data2 <- rnorm(n * (1 - prop), peak2, sd2)
   
   # Combine the data and find density
   bimodal_data <- c(data1, data2)
   kde <- density(bimodal_data)
   m <- mean(bimodal_data)
   sd <- sd(bimodal_data)
   
   # if just printing the plot on screen
   if (save.plot) png(filename = file.name, width = 1200, height = 627, res = 125)

   # plot normal distribution curve bare bones
   plot(kde, lwd = 3, main = "", xlab = "", ylab = "", bty = "n", axes = 0)
   mtext(text = title, side = 3, line = 2, cex = title_cex, font = title_font)
   mtext(text = subtitle, side = 3, line = 0.5, cex = subtitle_cex, font = subtitle_font)
   mtext(text = bquote(peak[1] == .(peak1) * "," ~ peak[2] == .(peak2) ~ 
                          "(" * mu == .(round(m, 2)) * "," ~ sigma == .(round(sd, 2)) * ")"), 
         side = 3, line = -0.75, cex = 1, font = 1)

   # draw x-axes (actual mean and sd values and mean +- number of sd notation)
   ticks <- round(seq(m - n.sd*sd, m + n.sd*sd, sd), 2)
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
      # find kde for 1-3 sd
      # (multiplied by small factor to allow vertical lines go over kde)
      y_kde_1 <- NA_real_
      y_kde_2 <- find_y_kde(2) * 2.5
      y_kde_3 <- find_y_kde(3) * 50

      # # add vertical line segments for mean and sd
      segments(x0 = ticks, y0 = 0, x1 = ticks,
               y1 = c(0,
                      y_kde_3, y_kde_2, y_kde_1,
                      0,
                      y_kde_1, y_kde_2, y_kde_3,
                      0),
               lty = 2, col = "gray")

      # add annotation arrows
      arrows(x0 = sort(ticks[ticks < m]),
             x1 = sort(ticks[ticks > m], decreasing = T),
             y0 = c(0, y_kde_3, y_kde_2, y_kde_1),
             y1 = c(0, y_kde_3, y_kde_2, y_kde_1),
             col = c(NA, "red", "blue", NA),
             code = 3, lty = 1)

      # # add text annotation boxes
      rect(xleft = m-0.45*sd,
           ybottom = c(y_kde_1, y_kde_2, y_kde_3)  - 0.015/sd,
           xright = m+0.45*sd,
           ytop = c(y_kde_1, y_kde_2, y_kde_3) + 0.015/sd,
           border = c(NA, "blue", "red"), col = "white")
      text(x = m,
           y = c(y_kde_1, y_kde_2, y_kde_3),
           labels = c(NA_character_, "75%", "88.9%"),
           cex = 1.25, font = 2, col = "black")
   }

   # close dev.off if saving plot to file
   if (save.plot) dev.off()
}

# plot distribution
plot_bimod_dist(peak1 = 0, sd1 = 2.5,
                peak2 = 10, sd2 = 3,
                prop = 0.6,
                title = "Bimodal Distribution",
                subtitle = "(Chebyshev's inequality)",
                annotations = T)

# for saving plot (.png extension)
plot_bimod_dist(peak1 = 0, sd1 = 2.5,
                peak2 = 10, sd2 = 3,
                prop = 0.6,
                title = "Bimodal Distribution",
                subtitle = "(Chebyshev's inequality)",
                annotations = T,
                file.name = "plots/bimodal_Chebyshev-ineqality.png",
                save.plot = T)
