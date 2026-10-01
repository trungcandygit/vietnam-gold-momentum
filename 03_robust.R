# 03_robust.R -- robustness suite requested by Reviewer 1 (comment on additional tests) and Reviewer 2 (Robustness Checks 1-4; Data and Sample 1-5), plus integrity items.
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("01_engine.R"); W <- 1:5
D <- function(x) as.Date(x)
inw <- function(d0, d1) DATES >= D(d0) & DATES <= D(d1)
rr_win <- function(x, w) rr(x[w])
ew_set <- function(set, n, cost = "dyn", real = TRUE, timing = "std") ew_strategy_k(set, n, "LO", timing, cost, real)
row_for <- function(label, set, d0 = "2015-01-05", d1 = "2025-12-31", real = TRUE, notes = "") {
  w <- inw(d0, d1); bh <- ew_bh_k(set, real)
  net <- sapply(W, function(n) rr_win(ew_set(set, n, "dyn", real), w)); zero <- sapply(W, function(n) rr_win(ew_set(set, n, "zero", real), w))
  Rn1 <- ew_set(set, 1, "dyn", real)[w]; G1 <- ew_strategy_k(set, 1, "LO", part = "gross", real = real)[w]; C1 <- ew_strategy_k(set, 1, "LO", part = "cost", real = real)[w]
  data.table(Test = label, Instruments = length(set), From = d0, To = d1, N = sum(!is.na(Rn1)),
    as.data.table(setNames(as.list(net), paste0("net_n", W))), as.data.table(setNames(as.list(zero), paste0("zero_n", W))),
    BH_RR = rr_win(bh, w), gross_ann_n1 = 252 * mean(G1, na.rm = TRUE), cost_ann_n1 = 252 * mean(C1, na.rm = TRUE), Notes = notes) }
w13c <- c("2015-01-05", "2024-07-29")
BUL_D <- intersect(DISTINCT, BULLION); JEW <- JEWEL
tabs <- list()
tabs$R_main <- rbindlist(list(
  row_for("R0 Main: 13 instruments, real windows, standard timing", NAMES),
  row_for("R1 Including imputed tail (as submitted sample)", NAMES, real = FALSE, notes = "LOCF tail kept"),
  row_for("R2 Common real window, all 13 instruments", NAMES, w13c[1], w13c[2]),
  row_for("R3 Bullion only (8 series)", BULLION), row_for("R4 Jewellery only (5 series)", JEWEL),
  row_for("R5 Distinct series only (9)", DISTINCT, notes = "drops PNJ DN, PNJ HN, SJC DN, PNJ MT"),
  row_for("R6 Excluding PNJ Mekong Delta (12)", setdiff(NAMES, "PNJ Mekong Delta")),
  row_for("R7 Excluding both Mekong Delta series (11)", setdiff(NAMES, c("PNJ Mekong Delta", "SJC Mekong Delta"))),
  row_for("R8 SJC bullion only (4)", grep("^SJC", NAMES, value = TRUE)), row_for("R9 PNJ bullion only, Ho Chi Minh (1 distinct)", "PNJ Ho Chi Minh"),
  row_for("R10 PNJ bullion only, all 4 regions", grep("^PNJ", BULLION, value = TRUE)),
  row_for("R11 Excluding jewellery and Mekong Delta (distinct bullion, 3)", setdiff(BUL_D, "SJC Mekong Delta")) ))
# --- sub-periods (strategies computed on the full real history, evaluated inside windows: no warm-up loss)
subp <- list(c("Pre-COVID 2015-2019", "2015-01-05", "2019-12-31"), c("COVID 2020-2022", "2020-01-01", "2022-12-31"), c("Post-COVID 2023-2025 (real data)", "2023-01-01", "2025-12-31"))
tabs$R_sub <- rbindlist(lapply(subp, function(s) row_for(s[1], NAMES, s[2], s[3])))
# --- policy episode (June 2024): distinct bullion series only, jewellery has no post-June-2024 real data except 2 months
pol_set <- BUL_D
pre <- row_for("Pre-June 2024 (bullion, distinct 4)", pol_set, "2015-01-05", "2024-05-31"); post <- row_for("Post-June 2024 (bullion, distinct 4; real data to 2025-07-22)", pol_set, "2024-06-01", "2025-07-22")
tabs$R_policy <- rbind(pre, post)
sp_pre <- 200 * as.vector(CHS[DATES < POLICY_DATE & DATES <= D("2025-07-22"), pol_set]); sp_post <- 200 * as.vector(CHS[DATES >= POLICY_DATE & DATES <= D("2025-07-22"), pol_set])
# spread comparison by instrument (pre vs post), Welch t and stationary-bootstrap-free descriptive
sp_tab <- rbindlist(lapply(pol_set, function(k) { a <- 200 * CHS[DATES < POLICY_DATE, k]; b <- 200 * CHS[DATES >= POLICY_DATE & DATES <= D("2025-07-22"), k]
  data.table(Instrument = k, pre_mean = mean(a), post_mean = mean(b), diff = mean(b) - mean(a), pre_median = median(a), post_median = median(b), welch_p = t.test(b, a)$p.value, n_pre = length(a), n_post = length(b)) }))
tabs$R_policy_spread <- sp_tab
# --- alternative weights: inverse trailing 250-day volatility (uses information up to t-1 only)
inv_vol_w <- function(set) { vol <- sapply(set, function(k) { r <- real_ret(k); v <- rep(NA_real_, TT); for (t in 252:TT) { x <- r[(t - 251):(t - 1)]; if (sum(!is.na(x)) > 200) v[t] <- sd(x, na.rm = TRUE) }; v })
  w <- 1 / vol; w[is.na(w)] <- 0; w <- w / rowSums(w); w[is.nan(w)] <- 1 / length(set); w }
wv <- inv_vol_w(NAMES)
wt_ret <- function(mat, w) { mat2 <- mat; mat2[is.na(mat2)] <- 0; avail <- !is.na(mat); ww <- w * avail; ww <- ww / rowSums(ww); ww[is.nan(ww)] <- 0; rowSums(mat2 * ww) }
mat_of <- function(n, cost) sapply(NAMES, function(k) strat_k(k, n, "LO", cost = cost)$R)
w <- inw("2016-01-05", "2025-12-31")
iv_net <- sapply(W, function(n) rr_win(wt_ret(mat_of(n, "dyn"), wv), w)); iv_zero <- sapply(W, function(n) rr_win(wt_ret(mat_of(n, "zero"), wv), w))
bhmat <- sapply(NAMES, real_ret); iv_bh <- rr_win(wt_ret(bhmat, wv), w)
ew_net <- sapply(W, function(n) rr_win(ew_set(NAMES, n), w)); ew_zero <- sapply(W, function(n) rr_win(ew_set(NAMES, n, "zero"), w)); ew_bh <- rr_win(ew_bh_k(NAMES), w)
tabs$R_weights <- rbind(data.table(Weights = "Equal weight (2016-2025)", as.data.table(t(c(ew_net, ew_zero, BH = ew_bh)))), data.table(Weights = "Inverse 250-day volatility (2016-2025)", as.data.table(t(c(iv_net, iv_zero, BH = iv_bh)))), use.names = FALSE)
setnames(tabs$R_weights, c("Weights", paste0("net_n", W), paste0("zero_n", W), "BH_RR"))
# --- cost scenarios
cs <- list(list("Zero cost", "zero"), list("Half of realised spread", "half"), list("Realised spread (main)", "dyn"), list("Twice realised spread", "double"), list("Fixed 0.25% per trade", 0.0025))
tabs$R_cost <- rbindlist(lapply(cs, function(z) data.table(Scenario = z[[1]], as.data.table(t(sapply(W, function(n) rr(ew_set(NAMES, n, z[[2]]))))))))
setnames(tabs$R_cost, c("Scenario", paste0("n", W)))
# --- longer look-back windows
WL <- c(1, 5, 10, 20, 60, 120)
tabs$R_windows <- rbindlist(lapply(c("LO", "LS"), function(md) rbindlist(lapply(c("dyn", "zero"), function(cs_) data.table(Mode = md, Cost = cs_, as.data.table(t(sapply(WL, function(n) rr(ew_strategy_k(NAMES, n, md, cost = cs_))))))))))
setnames(tabs$R_windows, c("Mode", "Cost", paste0("n", WL)))
# --- timing convention per instrument (paper vs standard), EW row
tabs$R_timing <- rbindlist(list(
  data.table(Timing = "Standard (main)", Cost = "dyn", as.data.table(t(sapply(W, function(n) rr(ew_set(NAMES, n)))))),
  data.table(Timing = "Standard (main)", Cost = "zero", as.data.table(t(sapply(W, function(n) rr(ew_set(NAMES, n, "zero")))))),
  data.table(Timing = "Signal lagged twice (submitted)", Cost = "dyn", as.data.table(t(sapply(W, function(n) rr(ew_set(NAMES, n, "dyn", TRUE, "paper")))))),
  data.table(Timing = "Signal lagged twice (submitted)", Cost = "zero", as.data.table(t(sapply(W, function(n) rr(ew_set(NAMES, n, "zero", TRUE, "paper")))))))); setnames(tabs$R_timing, c("Timing", "Cost", paste0("n", W)))
# --- data-quality diagnostics: first-order autocorrelation, share of exactly-zero returns (stale/rounded quotes)
tabs$R_diag <- rbindlist(lapply(NAMES, function(k) { r <- RET[2:IDX_LAST[[k]], k]; nz <- r[r != 0]
  data.table(Instrument = k, N = length(r), zero_return_share_pct = 100 * mean(r == 0), acf1_all = acf(r, lag.max = 1, plot = FALSE)$acf[2], acf1_nonzero = acf(nz, lag.max = 1, plot = FALSE)$acf[2],
             ljung_box_p_10 = Box.test(r, 10, "Ljung-Box")$p.value) }))
# --- how much does the imputed tail matter for the submitted buy-and-hold headline?
tabs$R_tail <- data.table(Item = c("EW B&H RR, real windows", "EW B&H RR, incl. imputed tail (submitted)", "EW B&H Ra real", "EW B&H Ra incl tail"),
  Value = c(rr(ew_bh_k(NAMES)), rr(ew_bh_k(NAMES, FALSE)), ann(ew_bh_k(NAMES))["Ra"], ann(ew_bh_k(NAMES, FALSE))["Ra"]))
for (nm in names(tabs)) fwrite(tabs[[nm]], file.path(OUT, paste0("rob_", nm, ".csv")))
saveRDS(tabs, file.path(OUT, "robust_results.rds"))
pr <- function(x) print(x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 2) else v)], width = 200)
cat("\n== R_main ==\n"); pr(tabs$R_main[, c(1:5, 6:10, 11:15, 16), with = FALSE]); cat("\n== R_sub ==\n"); pr(tabs$R_sub[, c(1, 5:16), with = FALSE]); cat("\n== R_policy ==\n"); pr(tabs$R_policy[, c(1, 5:19), with = FALSE])
cat("\n== spreads ==\n"); pr(tabs$R_policy_spread); cat("\n== weights ==\n"); pr(tabs$R_weights); cat("\n== cost ==\n"); pr(tabs$R_cost); cat("\n== windows ==\n"); pr(tabs$R_windows)
cat("\n== timing ==\n"); pr(tabs$R_timing); cat("\n== diag ==\n"); pr(tabs$R_diag); cat("\n== tail ==\n"); pr(tabs$R_tail)
