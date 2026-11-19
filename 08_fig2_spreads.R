# 08_fig2_spreads.R -- Figure 2: 60-session rolling mean of the bid-ask spread (% of mid price) by segment,
# each series up to its last archived quote; dashed line = June 2024. Writes figs/Fig2_spreads.png and .pdf.
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("01_engine.R")
seg <- function(set) { s <- sapply(set, function(k) { x <- 200 * CHS[, k]; x[seq_len(TT) > IDX_LAST[[k]]] <- NA; x }); rowMeans(s, na.rm = FALSE) }
roll <- function(x, k = 60) { o <- rep(NA_real_, length(x)); for (i in k:length(x)) if (!anyNA(x[(i - k + 1):i])) o[i] <- mean(x[(i - k + 1):i]); o }
segs <- c("PNJ 10K-18K jewellery", "PNJ ring and 24K jewellery", "PNJ bullion (Ho Chi Minh)", "SJC bullion (Ho Chi Minh, Hanoi)")
S <- list(roll(seg(c("Jewellery 10K", "Jewellery 14K", "Jewellery 18K"))), roll(seg(c("Ring PNJ 24K", "Jewellery 24K"))),
          roll(seg("PNJ Ho Chi Minh")), roll(seg(c("SJC Ho Chi Minh", "SJC Hanoi"))))
col <- c("#0000FF", "#FF0000", "black", "#7F7F7F"); lty <- c(1, 2, 1, 3); lwd <- c(2.6, 2.2, 1.1, 2.2)
keep <- DATES >= as.Date("2015-01-05") & DATES <= as.Date("2025-07-22")
MAI <- c(0.36, 0.62, 0.08, 0.10); PIN <- c(5.58, 2.55); W <- MAI[2] + PIN[1] + MAI[4]; H <- MAI[1] + PIN[2] + MAI[3]
draw <- function() {
  par(family = "sans", mai = MAI, tcl = 0.3, mgp = c(2.0, 0.3, 0), las = 1, lend = 1, ljoin = 1)
  xr <- range(DATES[keep][!is.na(S[[1]][keep]) | !is.na(S[[4]][keep])])
  plot(NA, xlim = xr, ylim = c(0, 10), xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE)
  yrs <- seq(as.Date("2016-01-01"), as.Date("2025-01-01"), by = "1 year")
  axis(1, at = yrs, labels = format(yrs, "%Y"), lwd = 0, lwd.ticks = 0.7); axis(3, at = yrs, labels = FALSE, lwd = 0, lwd.ticks = 0.7)
  axis(2, at = seq(0, 10, 2), lwd = 0, lwd.ticks = 0.7); axis(4, at = seq(0, 10, 2), labels = FALSE, lwd = 0, lwd.ticks = 0.7)
  mtext("Bid-ask spread (in %)", side = 2, line = 1.7, las = 0)
  abline(v = POLICY_DATE, lty = 2, lwd = 0.8, col = "grey40")
  text(POLICY_DATE, 9.3, "June 2024", pos = 2, offset = 0.2, cex = 0.85)
  for (i in 4:1) lines(DATES[keep], S[[i]][keep], col = col[i], lty = lty[i], lwd = lwd[i])
  box(lwd = 0.7)
  lab <- function(x, y, s, i, adj = 0) text(as.Date(x), y, s, col = col[i], cex = 0.82, adj = c(adj, 0.5))
  lab("2017-09-01", 8.35, segs[1], 1); lab("2015-05-01", 2.45, segs[2], 2)
  lab("2020-12-01", 3.1, segs[3], 3, adj = 1); lab("2024-02-01", 3.62, segs[4], 4, adj = 1)
}
dir.create("figs", showWarnings = FALSE)
png("figs/Fig2_spreads.png", width = W, height = H, units = "in", res = 300, pointsize = 11); draw(); dev.off()
pdf("figs/Fig2_spreads.pdf", width = W, height = H, pointsize = 11); draw(); dev.off()
