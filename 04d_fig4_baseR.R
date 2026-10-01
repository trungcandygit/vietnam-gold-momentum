# 04d_fig4_baseR.R -- Figure 4 redrawn with base R graphics, matching the user-supplied
# Nature-style two-panel reference: panel (a) risk-return ratio by holding period with a
# shaded CI ribbon per cost scenario and a dashed buy-and-hold reference line; panel (b)
# annual return decomposition (gross / spread cost / net) as point+line series with square/
# triangle/circle markers. Tick marks on all 4 sides, bold panel labels "a"/"b", legend
# inside each panel (bottom-right), white background, no gridlines.
setwd("/Users/nguyenvantrung/Downloads/Python for Algorithmic Trading/NCKH/BAI_GOLD/analysis")
library(data.table)

D <- "figdata"; FIG <- "figs"; SUB <- "figures_submission"

d4a <- fread(file.path(D, "fig4a_rr_ci.csv"))
d4b <- fread(file.path(D, "fig4b_decomposition.csv"))

colA <- c("Zero cost" = "#2297E6", "Net of realised spread" = "#DF536B")
colB <- c("Gross return" = "#1B9E77", "Spread cost" = "#DF536B", "Net return" = "black")
pchB <- c("Gross return" = 15, "Spread cost" = 17, "Net return" = 16)  # square, triangle, circle

panel_a <- function() {
  par(mar = c(4.2, 4.2, 2.2, 1) + 0.1, family = "sans")
  rr <- range(c(d4a$RR_lo, d4a$RR_hi, d4a$RR_BH))
  plot(d4a$n, d4a$RR, type = "n", xlab = "Holding period n (days)", ylab = "Risk-return ratio",
       xlim = c(1, 5), ylim = rr, xaxt = "n", bty = "o")
  axis(1, at = 1:5); axis(3, at = 1:5, labels = FALSE, tck = 0.015)
  axis(4, labels = FALSE, tck = 0.015)
  abline(h = d4a$RR_BH[1], lty = 2)
  for (g in names(colA)) {
    s <- d4a[Cost == g][order(n)]
    polygon(c(s$n, rev(s$n)), c(s$RR_lo, rev(s$RR_hi)), col = adjustcolor(colA[g], alpha.f = 0.2), border = NA)
    lines(s$n, s$RR, col = colA[g], lwd = 1.8)
  }
  legend("bottomright", legend = c(names(colA), "Buy-and-hold"), col = c(colA, "black"),
         lwd = c(1.8, 1.8, 1), lty = c(1, 1, 2), bty = "n", seg.len = 2, cex = 0.85)
  mtext("a", side = 3, line = 0.7, at = par("usr")[1] - 0.12 * diff(par("usr")[1:2]), font = 2, cex = 1.3, xpd = NA)
}

panel_b <- function() {
  par(mar = c(4.2, 4.2, 2.2, 1) + 0.1, family = "sans")
  dC <- data.table(n = d4b$n, `Gross return` = d4b$Gross_ann * 100,
                    `Spread cost` = -d4b$Cost_ann * 100, `Net return` = d4b$Net_ann * 100)
  yr <- range(dC[, -"n"])
  plot(d4b$n, d4b$Net_ann, type = "n", xlab = "Holding period n (days)", ylab = "Annual return (%)",
       xlim = c(1, 5), ylim = yr, xaxt = "n", yaxt = "n", bty = "o")
  axis(1, at = 1:5); axis(3, at = 1:5, labels = FALSE, tck = 0.015)
  yt <- pretty(yr); axis(2, at = yt, labels = paste0(yt, "%")); axis(4, at = yt, labels = FALSE, tck = 0.015)
  abline(h = 0)
  for (g in names(colB)) {
    lines(dC$n, dC[[g]], col = colB[g], lwd = 1.6)
    points(dC$n, dC[[g]], col = colB[g], pch = pchB[g], bg = colB[g], cex = 1.1)
  }
  legend("bottomright", legend = names(colB), col = colB, pt.bg = colB, lwd = 1.6, pch = pchB,
         bty = "n", seg.len = 2, cex = 0.85)
  mtext("b", side = 3, line = 0.7, at = par("usr")[1] - 0.18 * diff(par("usr")[1:2]), font = 2, cex = 1.3)
}

draw <- function() { par(mfrow = c(1, 2), oma = c(0, 1.2, 0, 0)); panel_a(); panel_b() }

png(file.path(FIG, "Fig4_rr_decomposition.png"), width = 10, height = 4.6, units = "in", res = 300)
draw(); dev.off()
pdf(file.path(SUB, "Fig4_rr_decomposition.pdf"), width = 10, height = 4.6); draw(); dev.off()
tryCatch({ svg(file.path(SUB, "Fig4_rr_decomposition.svg"), width = 10, height = 4.6); draw(); dev.off() },
         error = function(e) message("svg device unavailable, skipped: ", conditionMessage(e)))
png(file.path(SUB, "Fig4_rr_decomposition_1200dpi.png"), width = 10, height = 4.6, units = "in", res = 1200)
draw(); dev.off()

cat("done\n")
