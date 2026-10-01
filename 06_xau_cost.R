# 06_xau_cost.R -- R2 (Empirical Results): the XAU/VND benchmark is not frictionless. Momentum on XAU/VND under assumed one-way costs.
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("01_engine.R"); W <- 1:5
ewb <- ew_bh_k(NAMES); ok <- !is.na(ewb)
cs <- c(0, 0.0005, 0.0010, 0.0025)
tab <- rbindlist(lapply(cs, function(c0) { d <- as.data.table(t(sapply(W, function(n) { R <- strat(XAU, rep(c0, TT), n, "LO", "std")$R; R[!ok] <- NA; rr(R) }))); d[, one_way_cost_pct := 100 * c0]; setnames(d, c(paste0("n", W), "one_way_cost_pct")); setcolorder(d, "one_way_cost_pct"); d }))
tab[, BH_RR := rr(replace(XAU, !ok, NA))]
tr <- sapply(W, function(n) { s <- strat(XAU, rep(0, TT), n); mean(s$turn[ok], na.rm = TRUE) * 252 }); tab2 <- data.table(n = W, trades_per_year = tr)
fwrite(tab, file.path(OUT, "xau_cost.csv")); fwrite(tab2, file.path(OUT, "xau_turnover.csv")); print(round(tab, 3)); print(round(tab2, 1))
