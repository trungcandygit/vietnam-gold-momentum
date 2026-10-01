# 05_numbers.R -- per-instrument inference (HAC), XAU/VND checks, and the JSON of every number quoted in the revised text.
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("01_engine.R"); W <- 1:5
M <- readRDS(file.path(OUT, "main_results.rds")); tabs <- M$tabs; RB <- readRDS(file.path(OUT, "robust_results.rds"))
# per-instrument HAC test of the mean daily net return (H0: mean = 0) and share significantly negative
pi_tab <- rbindlist(lapply(NAMES, function(k) rbindlist(lapply(W, function(n) { R <- strat_k(k, n)$R; nw <- nw_t(R)
  data.table(Instrument = k, n = n, Mean_daily_pct = 100 * nw["mean"], NW_t = nw["t"], p_two_sided = nw["p2"], Ra = ann(R)["Ra"], RR = ann(R)["RR"]) }))))
fwrite(pi_tab, file.path(OUT, "per_instrument_hac.csv"))
# XAU/VND: is momentum different from buy-and-hold on the international benchmark? (matched window = EW window)
ewb <- ew_bh_k(NAMES); xm <- XAU; xm[is.na(ewb)] <- NA
xau_rows <- rbindlist(lapply(W, function(n) { R <- strat(XAU, rep(0, TT), n)$R; R[is.na(ewb)] <- NA; nw <- nw_t(R)
  data.table(n = n, RR_mom = rr(R), RR_BH = rr(xm), Mean_daily_pct = 100 * nw["mean"], NW_t = nw["t"], p = nw["p2"]) }))
fwrite(xau_rows, file.path(OUT, "xau_momentum.csv"))
# key numbers
t7 <- tabs$T7; t8 <- tabs$T8; t9 <- tabs$T9
R <- list(
  n_rows = TT, n_returns = TT - 1, first = format(min(DATES)), last = format(max(DATES)),
  spread_mean_pooled_real = mean(2 * CHS[cbind(unlist(lapply(NAMES, function(k) 2:IDX_LAST[[k]])), rep(match(NAMES, colnames(CHS)), sapply(NAMES, function(k) IDX_LAST[[k]] - 1)))] * 100),
  spread_mean_simple13 = mean(tabs$T1$Avg_spread_pct), spread_min_avg = min(tabs$T1$Avg_spread_pct), spread_max_avg = max(tabs$T1$Avg_spread_pct), spread_max_obs = max(tabs$T1$Max_spread_pct),
  bh_ew_rr = RES_bh <- tabs$T3[Instrument == "EW portfolio (13, available real data)", RR], bh_ew_ra = tabs$T3[Instrument == "EW portfolio (13, available real data)", Ra],
  bh_ew_sa = tabs$T3[Instrument == "EW portfolio (13, available real data)", sa], bh_ew_rr_submitted = tabs$T3[Instrument == "EW portfolio as submitted (incl. imputed tail)", RR],
  bh_xau_rr_matched = tabs$T3[Instrument == "XAU/VND (matched to EW window)", RR], bh_xau_rr_full = tabs$T3[Instrument == "XAU/VND (full sample)", RR],
  lo_net_rr = tabs$T4[Instrument == "EW portfolio", unlist(.SD), .SDcols = paste0("n", W)], lo_net_ra = tabs$T5[Instrument == "EW portfolio", unlist(.SD), .SDcols = paste0("n", W)],
  ls_net_rr = tabs$T6[Instrument == "EW portfolio", unlist(.SD), .SDcols = paste0("n", W)],
  share_lo_net_negative_cells = mean(tabs$T4[Instrument %in% NAMES, unlist(.SD), .SDcols = paste0("n", W)] < 0),
  share_hac_neg_sig1pct = mean(pi_tab$NW_t < 0 & pi_tab$p_two_sided < 0.01), n_cells = nrow(pi_tab), best_ra_cell = pi_tab[which.max(Ra), .(Instrument, n, Ra)],
  gross_ann = t7[Mode == "LO", Gross_ann], cost_ann = t7[Mode == "LO", Cost_ann], net_ann = t7[Mode == "LO", Net_ann], trades_yr = t7[Mode == "LO", Trades_per_year], breakeven = t7[Mode == "LO", Breakeven_half_spread_pct],
  avg_half_spread = t7[Mode == "LO", Avg_half_spread_pct][1], exposure = t7[Mode == "LO", Exposure_pct],
  ls_gross = t7[Mode == "LS", Gross_ann], ls_cost = t7[Mode == "LS", Cost_ann], ls_breakeven = t7[Mode == "LS", Breakeven_half_spread_pct],
  t8 = t8, t9 = t9, xau = xau_rows, windows = RB$R_windows, policy_spread = RB$R_policy_spread, policy = RB$R_policy, cost = RB$R_cost, diag = RB$R_diag, sub = RB$R_sub, main_rob = RB$R_main, weights = RB$R_weights)
write_json(R, file.path(OUT, "results.json"), auto_unbox = TRUE, pretty = TRUE, digits = 6)
cat("share of 65 instrument-window cells with mean net return significantly < 0 at 1%:", round(R$share_hac_neg_sig1pct, 3), "; cells", R$n_cells, "\n")
cat("best Ra cell:"); print(R$best_ra_cell); print(round(xau_rows, 3))
cat("pooled mean full spread (real windows):", round(R$spread_mean_pooled_real, 3), " simple mean of 13 averages:", round(R$spread_mean_simple13, 3), " min/max avg:", round(R$spread_min_avg, 2), round(R$spread_max_avg, 2), " max obs:", round(R$spread_max_obs, 2), "\n")
cat("LO gross/cost/net ann:\n"); print(round(rbind(R$gross_ann, R$cost_ann, R$net_ann, R$trades_yr, R$breakeven), 3))
