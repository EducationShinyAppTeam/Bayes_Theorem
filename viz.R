pick <- rnorm(1000)
test <- rnorm(1000)


plot(x = NULL, y = NULL, 
     xlim = c(0, 40),
     ylim = c(-5, 25),
     xaxt="n",
     yaxt="n",
     ann=FALSE,
     bty = "n")

k = 1

for (i in c(1:40)) {
  for (j in c(1:25)) {
    if (pick[k] > qnorm(1 - (input$infect / 1000))) { #input$infect
      if (test[(i*j)] > qnorm(1 - input$r2)) { #input$r2
        points(i, j, pch = 21, col = "blue", bg = "green", cex = 2)
      }
      else {
        points(i, j, pch = 19, col = "red", cex = 2)
      }
    }
    else {
      if (test[(i*j)] > qnorm(input$r1)) { #input$r1
        points(i, j, pch = 19, col = "blue", cex = 2)
      }
      else {
        points(i, j, pch = 19, col = "black", cex = 0.75)
      }
    }
  }
}

legend(0, -2, legend = c("True Negative", "True Positive", "False Negative", "False Positive"), 
       horiz = TRUE, bty = "n", pch = 21, col = c("black", "blue", "red", "blue"),
       pt.cex = c(0.75, 2, 2, 2), pt.bg = c("black", "green", "red", "blue"),
       cex = 2)

plot(cumsum(runif(n = 100)))

# draw legend with lines and point but without labels and box. x.intersp controls horizontal distance between lines
L = legend(x = 'bottom', legend = rep(NA,5), col=1:2, lty=c(1,1,2,2), ncol=2, bty='n', x.intersp=0.5, pch=c(1,2,1,2), inset=0.02)

# use position data of previous legend to draw legend with invisble lines and points but with labels and box. x.intersp controls distance between lines and labels
legend(x = L$rect$left, y = L$rect$top, legend = c('Group A', 'Group B'), col=rep(NA,2), lty=c(1,1), ncol=1, x.intersp = 3, bg = NA)


