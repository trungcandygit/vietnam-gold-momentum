# 02_main.R -- revision MAIN results (specification S1 = standard timing, real-data windows, EW of per-instrument strategy returns)
# Answers: R1 (transparent rules/benchmark), R2 (gross/cost/net decomposition, statistical inference, risk-free rate), Integrity IL-SERIOUS-14/15.
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("01_engine.R"); suppressMessages(library(sandwich))
W <- 1:5; RES <- list(); tabs <- list()
set.seed(20260928)

# ------------------------------------------------------------------ Table 1 / 2 : sample & returns (real windows)
t1 <- rbindlist(lapply(NAMES, function(k) { L <- IDX_LAST[[k]]; mid <- MID[seq_len(L), k]; sp <- 2 * CHS[seq_len(L), k] * 100
  data.table(Instrument = k, Type = INST[[k]][3], N_total = TT - 1, N_real = L - 1, Real_end = format(DATES[L]), Avg_mid_price_M_VND_per_luong = mean(mid),
             Avg_spread_pct = mean(sp), Median_spread_pct = median(sp), Max_spread_pct = max(sp)) }))
tabs$T1 <- t1
t2 <- rbindlist(lapply(c(NAMES, "XAU/VND (International)"), function(k) { if (k == NAMES[1] || k %in% NAMES) { L <- IDX_LAST[[k]]; r <- RET[2:L, k] * 100 } else r <- XAU[-1] * 100
  data.table(Instrument = k, N = length(r), Mean = mean(r), Std = sd(r), Min = min(r), P25 = quantile(r, .25), Median = median(r), P75 = quantile(r, .75), Max = max(r)) }))
tabs$T2 <- t2

# ------------------------------------------------------------------ Table 3 : buy-and-hold (mid) and net of one round-trip spread
bh_row <- function(k, r, rn = NULL) { a <- ann(r); data.table(Instrument = k, N = sum(!is.na(r)), Ra = a["Ra"], sa = a["sa"], RR = a["RR"],
  RR_net_roundtrip = if (is.null(rn)) NA_real_ else rr(rn), Skew = moments::skewness(r[!is.na(r)])) }
t3 <- rbindlist(lapply(NAMES, function(k) { L <- IDX_LAST[[k]]; bh_row(k, real_ret(k), bh_net(k, DATES[2], DATES[L])) }))
ewb <- ew_bh_k(NAMES); ewb_full <- ew_bh_k(NAMES, real = FALSE)
xau_matched <- XAU; xau_matched[is.na(ewb)] <- NA
t3 <- rbind(t3, bh_row("EW portfolio (13, available real data)", ewb, NULL), bh_row("XAU/VND (matched to EW window)", xau_matched, NULL), bh_row("XAU/VND (full sample)", XAU, NULL),
            bh_row("EW portfolio as submitted (incl. imputed tail)", ewb_full, NULL))
tabs$T3 <- t3
RES$bh <- list(ew_rr = rr(ewb), ew_Ra = ann(ewb)["Ra"], ew_sa = ann(ewb)["sa"], ew_rr_full = rr(ewb_full), xau_rr_matched = rr(xau_matched),
               ew_rr_rf = sapply(c(0, .03, .05, .07), function(f) rr(ewb, f)), xau_rr_rf = sapply(c(0, .03, .05, .07), function(f) rr(xau_matched, f)))

# ------------------------------------------------------------------ Tables 4-6 : LO RR, LO annual net return, LS RR   (main spec)
grid <- function(part, mode, fun) { rows <- lapply(c(NAMES, "EW portfolio"), function(k) {
  v <- sapply(W, function(n) { if (k == "EW portfolio") fun(ew_strategy_k(NAMES, n, mode)) else fun(strat_k(k, n, mode)$R) }); as.list(v) })
  d <- as.data.table(do.call(rbind, lapply(rows, unlist))); setnames(d, paste0("n", W)); d[, Instrument := c(NAMES, "EW portfolio")]
  xv <- sapply(W, function(n) { Rx <- strat(XAU, rep(0, TT), n, mode)$R; Rx[is.na(ew_bh_k(NAMES))] <- NA; fun(Rx) }); xl <- as.list(xv); names(xl) <- paste0("n", W); d <- rbind(d, as.data.table(c(xl, list(Instrument = "XAU/VND (zero cost, EW window)"))), use.names = TRUE)
  setcolorder(d, c("Instrument", paste0("n", W))); d }
tabs$T4 <- grid("R", "LO", rr)
tabs$T5 <- grid("R", "LO", function(R) ann(R)["Ra"])
tabs$T6 <- grid("R", "LS", rr)

# ------------------------------------------------------------------ Table 7 : gross / cost / net decomposition (EW of strategies)
decomp <- function(mode) rbindlist(lapply(W, function(n) {
  G <- ew_strategy_k(NAMES, n, mode, part = "gross"); C <- ew_strategy_k(NAMES, n, mode, part = "cost"); R <- ew_strategy_k(NAMES, n, mode); TU <- ew_strategy_k(NAMES, n, mode, part = "turn")
  expo <- ew_of(sapply(NAMES, function(k) { s <- strat_k(k, n, mode); ifelse(is.na(s$R), NA, abs(s$I)) }))
  data.table(Mode = mode, n = n, Gross_ann = TD * mean(G, na.rm = TRUE), Cost_ann = TD * mean(C, na.rm = TRUE), Net_ann = TD * mean(R, na.rm = TRUE), Trades_per_year = TD * mean(TU, na.rm = TRUE),
             Exposure_pct = 100 * mean(expo, na.rm = TRUE), Avg_half_spread_pct = 100 * mean(CHS[, NAMES], na.rm = TRUE),
             Breakeven_half_spread_pct = 100 * mean(G, na.rm = TRUE) / mean(TU, na.rm = TRUE),
             BH_ann_mean = TD * mean(ewb, na.rm = TRUE)) }))
tabs$T7 <- rbind(decomp("LO"), decomp("LS"))

# ------------------------------------------------------------------ Table 8 : inference on the EW long-only strategy (HAC t, stationary-bootstrap CI, RR difference vs B&H)
B <- 2000; Lbar <- 10
IDXC <- list()
# note: p-value of RR difference is computed below with the centred bootstrap (see function pdiff)
pdiff <- function(x, bm, idx) { d0 <- rr(x) - rr(bm); dif <- apply(idx, 2, function(ix) rr(x[ix]) - rr(bm[ix])); z <- dif - mean(dif); c(d0 = d0, p2 = mean(abs(z) >= abs(d0)), lo = unname(quantile(dif, .025)), hi = unname(quantile(dif, .975))) }
t8 <- list()
for (cost in list("dyn", "zero")) for (n in W) {
  R <- ew_strategy_k(NAMES, n, "LO", cost = cost); ok <- !is.na(R) & !is.na(ewb); x <- R[ok]; bm <- ewb[ok]; T <- length(x)
  key <- as.character(T); if (is.null(IDXC[[key]])) IDXC[[key]] <- stat_boot_idx(T, B, Lbar); idx <- IDXC[[key]]
  rrb <- apply(idx, 2, function(ix) rr(x[ix])); nw <- nw_t(x); pd <- pdiff(x, bm, idx)
  t8[[length(t8) + 1]] <- data.table(Cost = cost, n = n, T = T, RR = rr(x), RR_lo = unname(quantile(rrb, .025)), RR_hi = unname(quantile(rrb, .975)), Mean_daily_pct = 100 * nw["mean"], NW_t = nw["t"],
    p_two_sided = nw["p2"], RR_BH = rr(bm), RR_diff = pd["d0"], diff_lo = pd["lo"], diff_hi = pd["hi"], p_diff = pd["p2"]) }
tabs$T8 <- rbindlist(t8)

# ------------------------------------------------------------------ Table 9 : White Reality Check + Hansen SPA over the 65 (instrument x window) long-only rules
spa <- function(D, idx, label) { T <- nrow(D); K <- ncol(D); dbar <- colMeans(D)
  boot_means <- sapply(seq_len(ncol(idx)), function(b) colMeans(D[idx[, b], , drop = FALSE])); if (K == 1) boot_means <- matrix(boot_means, 1)
  omega <- apply(sqrt(T) * boot_means, 1, sd); omega[omega < 1e-12] <- 1e-12
  Tspa <- max(max(sqrt(T) * dbar / omega), 0)
  thr <- sqrt(omega^2 / T * 2 * log(log(T))); muc <- ifelse(dbar <= -thr, dbar, 0); mul <- pmin(dbar, 0)
  zs <- function(mu) apply(sqrt(T) * (boot_means - dbar + mu) / omega, 2, function(z) max(max(z), 0))
  p_c <- mean(zs(muc) >= Tspa); p_l <- mean(zs(mul) >= Tspa); p_u <- mean(zs(dbar * 0) >= Tspa)
  V <- max(sqrt(T) * dbar); Vb <- apply(sqrt(T) * (boot_means - dbar), 2, max); p_rc <- mean(Vb >= V)
  data.table(Test = label, K = K, T = T, best_mean_daily_pct = 100 * max(dbar), best_rule = colnames(D)[which.max(dbar)], RC_stat = V, RC_p = p_rc, SPA_stat = Tspa, SPA_p_lower = p_l, SPA_p_consistent = p_c, SPA_p_upper = p_u) }
w13 <- DATES >= as.Date("2015-01-05") & DATES <= as.Date("2024-07-29")      # window with real data for all 13 instruments
mk <- function(cost, bench_zero) { cols <- list(); nm <- c()
  for (k in NAMES) for (n in W) { R <- strat_k(k, n, "LO", cost = cost)$R; rb <- real_ret(k); cols[[length(cols) + 1]] <- if (bench_zero) R else R - rb; nm <- c(nm, paste0(k, "|n=", n)) }
  D <- do.call(cbind, cols)[w13, ]; colnames(D) <- nm; D }
T13 <- sum(w13); idx13 <- stat_boot_idx(T13, B, Lbar, seed = 7)
t9 <- rbind(spa(mk("dyn", TRUE), idx13, "Net of realised spread: rule return vs. zero (cash)"),
            spa(mk("dyn", FALSE), idx13, "Net of realised spread: rule return vs. buy-and-hold (same instrument)"),
            spa(mk("zero", TRUE), idx13, "Zero cost: rule return vs. zero (cash)"),
            spa(mk("zero", FALSE), idx13, "Zero cost: rule return vs. buy-and-hold (same instrument)"))
tabs$T9 <- t9

# ------------------------------------------------------------------ Table 10 : risk-free sensitivity (EW LO n=1..5, EW B&H, XAU)
rf_list <- c(0, .03, .05, .07)
t10 <- rbindlist(lapply(rf_list, function(f) data.table(rf = f, BH_EW = rr(ewb, f), XAU = rr(xau_matched, f),
  as.data.table(setNames(as.list(sapply(W, function(n) rr(ew_strategy_k(NAMES, n, "LO"), f))), paste0("LO_n", W))),
  as.data.table(setNames(as.list(sapply(W, function(n) rr(ew_strategy_k(NAMES, n, "LO", cost = "zero"), f))), paste0("LO0_n", W))))))
tabs$T10 <- t10

# ------------------------------------------------------------------ Table 11 : submitted specification (replication) vs revision main
t11 <- rbindlist(list(
  data.table(Spec = "Submitted: one rule on average return, signal lagged twice, full sample incl. imputed tail", Cost = "dyn", as.data.table(t(sapply(W, function(n) rr(ew_signal_on_avg(NAMES, n))))) ),
  data.table(Spec = "Submitted spec, zero cost", Cost = "zero", as.data.table(t(sapply(W, function(n) rr(ew_signal_on_avg(NAMES, n, cost = "zero")))))),
  data.table(Spec = "One rule on average return, standard timing, full sample", Cost = "dyn", as.data.table(t(sapply(W, function(n) rr(ew_signal_on_avg(NAMES, n, timing = "std")))))),
  data.table(Spec = "One rule on average return, standard timing, zero cost", Cost = "zero", as.data.table(t(sapply(W, function(n) rr(ew_signal_on_avg(NAMES, n, timing = "std", cost = "zero")))))),
  data.table(Spec = "MAIN: EW of 13 strategy returns, standard timing, real windows", Cost = "dyn", as.data.table(t(sapply(W, function(n) rr(ew_strategy_k(NAMES, n)))))),
  data.table(Spec = "MAIN, zero cost", Cost = "zero", as.data.table(t(sapply(W, function(n) rr(ew_strategy_k(NAMES, n, cost = "zero")))))))); setnames(t11, c("Spec", "Cost", paste0("n", W)))
tabs$T11 <- t11

for (nm in names(tabs)) fwrite(tabs[[nm]], file.path(OUT, paste0("main_", nm, ".csv")))
saveRDS(list(tabs = tabs, RES = RES), file.path(OUT, "main_results.rds"))
cat("\n=== T4 (RR, LO, main) ===\n"); print(tabs$T4[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\n=== T7 decomposition ===\n"); print(tabs$T7[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\n=== T8 inference ===\n"); print(tabs$T8[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\n=== T9 RC / SPA ===\n"); print(tabs$T9[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\n=== T10 rf ===\n"); print(tabs$T10[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\n=== T11 ===\n"); print(tabs$T11[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\n=== T3 ===\n"); print(tabs$T3[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
