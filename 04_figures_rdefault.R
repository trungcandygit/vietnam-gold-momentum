# 04_figures_rdefault.R -- rebuild the 4 manuscript figures using PLAIN ggplot2 defaults:
# no theme_set(), no custom palette (default hue scale), no custom fonts. This is exactly
# what ggplot() + geom_*() produces with zero styling calls, i.e. theme_gray() + scale_*_hue().
# The only non-default choice is legend.position = "top", to match the reference image the
# user supplied (ggplot2's own default is legend on the right). Reads the same clean CSVs in
# analysis/figdata/ used by the earlier R and Python attempts; does not read or reference any
# prior figure file.
setwd("/Users/nguyenvantrung/Downloads/Python for Algorithmic Trading/NCKH/BAI_GOLD/analysis")
suppressMessages({library(ggplot2); library(patchwork); library(scales); library(data.table)})

D <- "figdata"; FIG <- "figs"; SUB <- "figures_submission"
dir.create(FIG, showWarnings = FALSE); dir.create(SUB, showWarnings = FALSE)

save_fig <- function(p, name, w, h) {
  ggsave(file.path(SUB, paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path(SUB, paste0(name, ".svg")), p, width = w, height = h)
  ggsave(file.path(SUB, paste0(name, "_1200dpi.png")), p, width = w, height = h, dpi = 1200)
  ggsave(file.path(FIG, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
}

# ---------------------------------------------------------------- Figure 1: research design
bx <- fread(file.path(D, "fig1_design.csv"))
bx[, body2 := gsub("\\\\n", "\n", body)]
pF <- ggplot(bx) +
  geom_rect(aes(xmin = x - 0.95, xmax = x + 0.95, ymin = 0.05, ymax = 1.0), fill = "white", colour = "black") +
  geom_text(aes(x = x, y = 0.86, label = head), fontface = "bold", size = 3.1) +
  geom_text(aes(x = x, y = 0.4, label = body2), size = 2.6, lineheight = 0.95) +
  geom_segment(data = bx[id < 5], aes(x = x + 0.95, xend = x + 1.15, y = 0.5, yend = 0.5),
               arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  scale_x_continuous(limits = c(0, 10.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  theme_void()
save_fig(pF, "Fig1_design", 7.8, 1.6)

# ---------------------------------------------------------------- Figure 2: spreads over time
d2 <- fread(file.path(D, "fig2_spreads.csv")); d2[, Date := as.Date(Date)]
d2[, Seg := factor(Seg, levels = unique(Seg))]
policy_date <- as.Date(fread(file.path(D, "fig2_policy_date.csv"))$policy_date)
pS <- ggplot(d2, aes(Date, S, colour = Seg)) +
  geom_line() +
  geom_vline(xintercept = policy_date, linetype = "dashed") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Bid-ask spread (% of mid price, 60-day mean)", colour = NULL) +
  theme(legend.position = "top")
save_fig(pS, "Fig2_spreads", 6.8, 3.6)

# ---------------------------------------------------------------- Figure 3: cumulative wealth (log)
d3 <- fread(file.path(D, "fig3_wealth.csv")); d3[, Date := as.Date(Date)]
d3[, Series := factor(Series, levels = unique(Series))]
pW <- ggplot(d3, aes(Date, W, colour = Series)) +
  geom_line() +
  scale_y_log10() +
  labs(x = NULL, y = "Cumulative wealth (log scale, start = 1)", colour = NULL) +
  theme(legend.position = "top")
save_fig(pW, "Fig3_wealth", 6.8, 3.6)

# ---------------------------------------------------------------- Figure 4: RR with CI (A) + decomposition (B)
d4a <- fread(file.path(D, "fig4a_rr_ci.csv"))
d4a[, Cost := factor(Cost, levels = unique(Cost))]
bh <- unique(d4a$RR_BH)
pA <- ggplot(d4a, aes(factor(n), RR, colour = Cost)) +
  geom_hline(yintercept = bh, linetype = "dashed") +
  geom_pointrange(aes(ymin = RR_lo, ymax = RR_hi), position = position_dodge(width = 0.4)) +
  labs(x = "Look-back window n (days)", y = "Risk-return ratio (RR)", colour = NULL,
       title = "(a) Risk-return ratio by look-back window") +
  theme(legend.position = "top")

d4b <- fread(file.path(D, "fig4b_decomposition.csv"))
dC <- rbind(data.table(n = d4b$n, Component = "Gross return", v = d4b$Gross_ann * 100),
            data.table(n = d4b$n, Component = "Realised cost", v = -d4b$Cost_ann * 100),
            data.table(n = d4b$n, Component = "Net return", v = d4b$Net_ann * 100))
dC[, Component := factor(Component, levels = c("Gross return", "Realised cost", "Net return"))]
pB <- ggplot(dC, aes(factor(n), v, fill = Component)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0) +
  labs(x = "Look-back window n (days)", y = "Annualised return (%)", fill = NULL,
       title = "(b) Annual return decomposition") +
  theme(legend.position = "top")

save_fig(pA | pB, "Fig4_rr_decomposition", 7.25, 3.6)

cat("done\n")
