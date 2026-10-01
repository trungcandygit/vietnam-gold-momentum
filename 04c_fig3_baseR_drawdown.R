# 04c_fig3_baseR_drawdown.R -- Figure 3 redrawn as a DRAWDOWN chart (base R graphics),
# matching the user-supplied reference Nature_Figure_3_Drawdown.pdf: tick marks on all
# 4 sides of the box, filled semi-transparent area from 0% down to each series' running
# drawdown, legend below in a 2x2 grid. Drawdown_t = W_t / cummax(W_t) - 1, computed from
# the same cumulative-wealth series already used for the old Fig3 (fig3_wealth.csv).
setwd("/Users/nguyenvantrung/Downloads/Python for Algorithmic Trading/NCKH/BAI_GOLD/analysis")
library(data.table)

D <- "figdata"; FIG <- "figs"; SUB <- "figures_submission"

d3 <- fread(file.path(D, "fig3_wealth.csv")); d3[, Date := as.Date(Date)]
segs <- unique(d3$Series)
d3[, DD := W / cummax(W) - 1, by = Series]

pal <- palette("R4")[c(4, 2, 3, 7)]  # blue, red, green, yellow -- same base-R default set as Fig2

draw <- function() {
  par(mar = c(4.2, 4.2, 1.5, 1.5) + 0.1, oma = c(3.6, 0, 0, 0), family = "sans")
  plot(d3$Date, d3$DD * 100, type = "n", xlab = "", ylab = "Drawdown (%)",
       ylim = c(-100, 2), xaxt = "n", yaxt = "n", bty = "o")
  title(xlab = "Time (year)", line = 2.3)
  yt <- seq(0, -100, by = -20)
  axis(2, at = yt, labels = paste0(yt, "%"))
  axis(4, at = yt, labels = FALSE, tck = 0.01)
  xt <- as.Date(paste0(seq(2016, 2026, 2), "-01-01"))
  axis(1, at = xt, labels = format(xt, "%Y"))
  axis(3, at = xt, labels = FALSE, tck = 0.01)
  for (i in seq_along(segs)) {
    g <- d3[Series == segs[i]]
    polygon(c(g$Date, rev(g$Date)), c(g$DD * 100, rep(0, nrow(g))),
            col = adjustcolor(pal[i], alpha.f = 0.25), border = NA)
  }
  for (i in seq_along(segs)) {
    g <- d3[Series == segs[i]]
    lines(g$Date, g$DD * 100, col = pal[i], lwd = 1.2)
  }
  legend(x = grconvertX(0.5, "ndc", "user"), y = grconvertY(0.03, "ndc", "user"),
         xjust = 0.5, yjust = 0, legend = segs, col = pal, lwd = 1.4, lty = 1,
         ncol = 2, bty = "n", xpd = NA, seg.len = 2)
}

png(file.path(FIG, "Fig3_wealth.png"), width = 7.4, height = 5.6, units = "in", res = 300)
draw(); dev.off()
pdf(file.path(SUB, "Fig3_wealth.pdf"), width = 7.4, height = 5.6); draw(); dev.off()
tryCatch({ svg(file.path(SUB, "Fig3_wealth.svg"), width = 7.4, height = 5.6); draw(); dev.off() },
         error = function(e) message("svg device unavailable, skipped: ", conditionMessage(e)))
png(file.path(SUB, "Fig3_wealth_1200dpi.png"), width = 7.4, height = 5.6, units = "in", res = 1200)
draw(); dev.off()

cat("done\n")
