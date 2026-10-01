# 04b_fig2_baseR.R -- Figure 2 redrawn with BASE R graphics (plot/lines/legend), zero
# extra packages, zero theme styling: white panel, full box border, no gridlines, R's
# own default discrete palette (palette("R4"), the base-R default since R 4.0.0).
# Matches the user-supplied reference: box plot + bottom 2-row legend, no grey panel.
setwd("/Users/nguyenvantrung/Downloads/Python for Algorithmic Trading/NCKH/BAI_GOLD/analysis")
library(data.table)

D <- "figdata"; FIG <- "figs"; SUB <- "figures_submission"

d2 <- fread(file.path(D, "fig2_spreads.csv")); d2[, Date := as.Date(Date)]
segs <- unique(d2$Seg)
policy_date <- as.Date(fread(file.path(D, "fig2_policy_date.csv"))$policy_date)

pal <- palette("R4")[c(5, 2, 3, 7)]  # base-R default palette, 4 of its 8 colours

draw <- function() {
  par(mar = c(4.2, 4.2, 1, 1) + 0.1, oma = c(3.2, 0, 0, 0), family = "sans")
  plot(d2$Date, d2$S, type = "n", xlab = "", ylab = "Bid-ask spread (%)",
       yaxt = "n", bty = "o")
  title(xlab = "Time (year)", line = 2.3)
  yt <- pretty(d2$S)
  axis(2, at = yt, labels = paste0(yt, "%"))
  abline(v = policy_date, lty = 2)
  for (i in seq_along(segs)) {
    g <- d2[Seg == segs[i]]
    lines(g$Date, g$S, col = pal[i], lwd = 1.4)
  }
  legend(x = grconvertX(0.5, "ndc", "user"), y = grconvertY(0.03, "ndc", "user"),
         xjust = 0.5, yjust = 0, legend = segs, col = pal, lwd = 1.4, lty = 1,
         ncol = 2, bty = "n", xpd = NA, seg.len = 2)
}

png(file.path(FIG, "Fig2_spreads.png"), width = 7.4, height = 5.6, units = "in", res = 300)
draw(); dev.off()
pdf(file.path(SUB, "Fig2_spreads.pdf"), width = 7.4, height = 5.6); draw(); dev.off()
tryCatch({ svg(file.path(SUB, "Fig2_spreads.svg"), width = 7.4, height = 5.6); draw(); dev.off() },
         error = function(e) message("svg device unavailable, skipped: ", conditionMessage(e)))
png(file.path(SUB, "Fig2_spreads_1200dpi.png"), width = 7.4, height = 5.6, units = "in", res = 1200)
draw(); dev.off()

cat("done\n")
