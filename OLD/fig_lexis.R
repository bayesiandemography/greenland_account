
makeFrame <- function() {
    par(xaxs = "i",
        yaxs = "i",
        mar = c(4.1, 8.1, 2.1, 3.1),
        xpd = NA,
        lwd = 1,
        cex = 0.6)
    plot(0:2,
         type = "n",
         xlab = "Time",
         ylab = "",
         xlim = c(0, 1),
         ylim = c(0, 2),
         las = 2,
         xaxt = "n",
         yaxt = "n",
         frame.plot = FALSE)
    axis(1,
         at = 0:1,
         labels = c(expression(italic(t)), expression(italic(t)+1)),
         line = 0)
    axis(2,
         at = 0:2,
         labels = c("0", "1", "2"),
         las = 2,
         line = 0)
    axis(4,
         at = 0:2,
         labels = c("0", "1", "2"),
         las = 2,
         line = 0)
    mtext(side = 2,
          text = "Age",
          las = 2,
          cex = 0.7,
          line = 3)
    abline(h = 1, col = "black", lty = "solid", xpd = FALSE)
    for (a in (-1):1)
        abline(a = a, b = 1, col = "black", lty = "dashed", xpd = FALSE)
    box()
}

pdf(file = "fig_lexis.pdf",
    w = 2.1,
    h = 2.3)

makeFrame()

text(x = 0.05,
     y = 0.6,
     labels = "Upper",
     pos = 4,
     cex = 0.9)
text(x = 0.55,
     y = 0.4,
     labels = "Lower",
     pos = 4,
     cex = 0.9)
text(x = 0.05,
     y = 1.6,
     labels = "Upper",
     pos = 4,
     cex = 0.9)
text(x = 0.55,
     y = 1.4,
     labels = "Lower",
     pos = 4,
     cex = 0.9)

points(x = c(0.5, 0.6),
       y = c(0.27, 0.85),
       cex = 0.9,
       pch = 19)
text(x = c(0.5, 0.6),
     y = c(0.27, 0.85),
     labels = c("A", "B"),
     cex = 0.9,
     pos = c(4, 2))

dev.off()

         



