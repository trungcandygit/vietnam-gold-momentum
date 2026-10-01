# 01_engine.R -- shared data + strategy engine for the JEBS.1563 revision.  Source this file; it defines functions and loads data.
# Conventions (identical to the submitted manuscript unless stated):
#   r_t = ln(P_mid,t / P_mid,t-1) (fraction)   c_t = (ask-bid)/(2*mid) (half-spread, fraction)
#   Long-only rule; long-short rule = +1 / -1 / 0.  252 trading days.  RR = (Ra - rf)/sigma_a, rf = 0 unless stated.
# TIMING
#   "std"   (revision MAIN specification): a_t = mean(r_{t-1..t-n}) is known at the close of t-1; position I_t earns r_t;
#           trading cost |I_t - I_{t-1}| * c_{t-1} (half-spread at the quote used to trade, close of t-1).
#   "paper" (submitted manuscript, kept for replication): same a_t, but the return earned on day t is I_{t-1} * r_t and the cost is
#           |I_t - I_{t-1}| * c_t, i.e. the signal is lagged twice.
suppressMessages({library(data.table); library(readxl); library(jsonlite)})
if (!exists("ROOT")) { ROOT <- normalizePath(file.path(getwd(), "..")); if (!dir.exists(file.path(ROOT, "vietnam-gold-momentum"))) ROOT <- getwd() }
REPO <- file.path(ROOT, "vietnam-gold-momentum"); OUT <- file.path(ROOT, "analysis", "out"); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
TD <- 252

INST <- list(
 "Ring PNJ 24K"=c("Nhan_PNJ_24K_Buy","Nhan_PNJ_24K_Sell","jewelry"), "Jewellery 10K"=c("NuTrang_10K_Buy","NuTrang_10K_Sell","jewelry"),
 "Jewellery 14K"=c("NuTrang_14K_Buy","NuTrang_14K_Sell","jewelry"), "Jewellery 18K"=c("NuTrang_18K_Buy","NuTrang_18K_Sell","jewelry"),
 "Jewellery 24K"=c("NuTrang_24K_Buy","NuTrang_24K_Sell","jewelry"), "PNJ Da Nang"=c("PNJ_DN_Buy","PNJ_DN_Sell","bullion"),
 "PNJ Hanoi"=c("PNJ_HN_Buy","PNJ_HN_Sell","bullion"), "PNJ Mekong Delta"=c("PNJ_MT_Buy","PNJ_MT_Sell","bullion"),
 "PNJ Ho Chi Minh"=c("PNJ_TPHCM_Buy","PNJ_TPHCM_Sell","bullion"), "SJC Da Nang"=c("SJC_DN_Buy","SJC_DN_Sell","bullion"),
 "SJC Hanoi"=c("SJC_HN_Buy","SJC_HN_Sell","bullion"), "SJC Mekong Delta"=c("SJC_MT_Buy","SJC_MT_Sell","bullion"),
 "SJC Ho Chi Minh"=c("SJC_TPHCM_Buy","SJC_TPHCM_Sell","bullion"))
NAMES <- names(INST)
BULLION <- NAMES[sapply(INST, `[`, 3) == "bullion"]; JEWEL <- setdiff(NAMES, BULLION)
# de-duplicated set: drop PNJ Da Nang / PNJ Hanoi (identical to PNJ Ho Chi Minh on 99.9% of days), SJC Da Nang (== SJC Ho Chi Minh), PNJ Mekong Delta (corrupted)
DISTINCT <- setdiff(NAMES, c("PNJ Da Nang", "PNJ Hanoi", "SJC Da Nang", "PNJ Mekong Delta"))
LAST_REAL <- c(setNames(rep(as.Date("2024-07-29"), length(JEWEL)), JEWEL), setNames(rep(as.Date("2025-07-22"), length(BULLION)), BULLION))  # from 00_data_audit.R
POLICY_DATE <- as.Date("2024-06-01")   # SBV direct-sale programme (June 2024) as described in the manuscript

m <- as.data.table(read_excel(file.path(REPO, "Master_Gold_Dataset_Cleaned_Quant.xlsx"))); m[, Date := as.Date(Date)]; setorder(m, Date)
DATES <- m$Date; TT <- nrow(m)
MID <- sapply(INST, function(v) (m[[v[1]]] + m[[v[2]]]) / 2)
CHS <- sapply(INST, function(v) (m[[v[2]]] - m[[v[1]]]) / (m[[v[2]]] + m[[v[1]]]))           # half-spread / mid
RET <- apply(MID, 2, function(p) c(NA_real_, diff(log(p))))
XAU <- c(NA_real_, diff(log(m$XAU_VND_QuyDoi)))

lagv <- function(x, k = 1) c(rep(NA_real_, k), head(x, -k))
rollmean_n <- function(x, n) { if (n == 1) return(x); out <- rep(NA_real_, length(x)); cs <- cumsum(ifelse(is.na(x), 0, x)); bad <- cumsum(is.na(x))
  for (i in n:length(x)) if ((bad[i] - if (i > n) bad[i - n] else 0) == 0) out[i] <- (cs[i] - if (i > n) cs[i - n] else 0) / n; out }
signal <- function(r, n, mode = "LO") {          # position held over day t (info through t-1)
  a <- rollmean_n(lagv(r), n)
  a <- round(a, 14)                              # avoid floating-point sign flips on exact zeros
  if (mode == "LO") ifelse(!is.na(a) & a > 0, 1, 0) else ifelse(!is.na(a) & a > 0, 1, ifelse(!is.na(a) & a < 0, -1, 0))
}
# returns list(R = net daily return, gross, cost, turn, I) aligned with dates (NA on day 1)
strat <- function(r, c, n, mode = "LO", timing = "std", cost = "dyn") {
  I <- signal(r, n, mode); dI <- c(NA_real_, abs(diff(I)))
  cc <- if (identical(cost, "dyn")) c else if (identical(cost, "half")) 0.5 * c else if (identical(cost, "double")) 2 * c else if (identical(cost, "zero")) rep(0, length(c)) else rep(as.numeric(cost), length(c))
  if (timing == "std") { gross <- I * r; cst <- dI * lagv(cc) } else { gross <- lagv(I) * r; cst <- dI * cc }
  gross[1] <- NA; cst[1] <- NA
  list(R = gross - cst, gross = gross, cost = cst, turn = dI, I = I)
}
ann <- function(R, rf = 0) { R <- R[!is.na(R)]; if (length(R) < 30) return(c(Ra = NA, sa = NA, RR = NA)); Ra <- (1 + mean(R))^TD - 1; sa <- sd(R) * sqrt(TD)
  c(Ra = Ra, sa = sa, RR = if (sa > 0) (Ra - rf) / sa else NA_real_) }
rr <- function(R, rf = 0) unname(ann(R, rf)["RR"])
# window helper: restrict a vector to dates in [d0, d1]
inwin <- function(d0 = as.Date("1900-01-01"), d1 = as.Date("2100-01-01")) DATES >= d0 & DATES <= d1
# EW portfolio of per-instrument strategy returns over a set of instruments
ew_strategy <- function(set, n, mode = "LO", timing = "std", cost = "dyn") {
  M <- sapply(set, function(k) strat(RET[, k], CHS[, k], n, mode, timing, cost)$R); rowMeans(M) }
# original submission: ONE rule applied to the equally weighted average return / average spread ("paper" EW)
ew_signal_on_avg <- function(set, n, mode = "LO", timing = "paper", cost = "dyn") {
  r <- apply(RET[, set, drop = FALSE], 1, function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)); c <- rowMeans(CHS[, set, drop = FALSE])
  strat(r, c, n, mode, timing, cost)$R }
bh_ew <- function(set) apply(RET[, set, drop = FALSE], 1, function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE))
# buy-and-hold with one entry and one exit half-spread
bh_net <- function(k, d0 = DATES[2], d1 = tail(DATES, 1)) { r <- RET[, k]; ii <- which(DATES >= d0 & DATES <= d1); ii <- ii[!is.na(r[ii])]
  x <- r; x[ii[1]] <- x[ii[1]] - CHS[ii[1] - 1, k]; x[tail(ii, 1)] <- x[tail(ii, 1)] - CHS[tail(ii, 1), k]; x }
# stationary bootstrap indices (Politis & Romano 1994), circular, mean block length L
stat_boot_idx <- function(T, B, L, seed = 20260928) { set.seed(seed); p <- 1 / L; idx <- matrix(0L, T, B)
  for (b in seq_len(B)) { s <- sample.int(T, 1); v <- integer(T); v[1] <- s
    new <- runif(T - 1) < p; st <- sample.int(T, T - 1, replace = TRUE)
    for (t in 2:T) v[t] <- if (new[t - 1]) st[t - 1] else if (v[t - 1] == T) 1L else v[t - 1] + 1L
    idx[, b] <- v }
  idx }
nw_lag <- function(T) floor(4 * (T / 100)^(2 / 9))
nw_t <- function(x) { x <- x[!is.na(x)]; T <- length(x); L <- nw_lag(T); xc <- x - mean(x); g0 <- sum(xc^2) / T; s <- g0
  for (l in seq_len(L)) s <- s + 2 * (1 - l / (L + 1)) * sum(xc[-(1:l)] * xc[1:(T - l)]) / T
  se <- sqrt(s / T); c(mean = mean(x), se = se, t = mean(x) / se, p2 = 2 * pnorm(-abs(mean(x) / se)), p_neg = pnorm(mean(x) / se)) }

# ---- real-data truncation: every instrument is evaluated only up to its own last genuine quote (see 00_data_audit.R) ----
IDX_LAST <- setNames(match(LAST_REAL[NAMES], DATES), NAMES)
pad <- function(x, L) c(x[seq_len(L)], rep(NA_real_, TT - L))
strat_k <- function(k, n, mode = "LO", timing = "std", cost = "dyn", real = TRUE) {
  L <- if (real) IDX_LAST[[k]] else TT
  s <- strat(RET[seq_len(L), k], CHS[seq_len(L), k], n, mode, timing, cost)
  lapply(s, pad, L = L) }
real_ret <- function(k, real = TRUE) { L <- if (real) IDX_LAST[[k]] else TT; pad(RET[, k], L) }
ew_of <- function(mat) apply(mat, 1, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
ew_strategy_k <- function(set, n, mode = "LO", timing = "std", cost = "dyn", real = TRUE, part = "R")
  ew_of(sapply(set, function(k) strat_k(k, n, mode, timing, cost, real)[[part]]))
ew_bh_k <- function(set, real = TRUE) ew_of(sapply(set, real_ret, real = real))
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
cat("engine loaded:", TT, "rows;", length(NAMES), "instruments;", format(min(DATES)), "to", format(max(DATES)), "\n")
