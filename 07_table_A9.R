# 07_table_A9.R -- Table A9: sensitivity of risk-return ratios to the risk-free rate and the return definition.
# Cash earns zero in every specification; the risk-free rate enters only the RR numerator.
setwd(dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))))
source("01_engine.R")
sharpe <- function(R) { R <- R[!is.na(R)]; mean(R) * TD / (sd(R) * sqrt(TD)) }
simple_ew <- function(n, cost) ew_of(sapply(NAMES, function(k) { s <- strat_k(k, n, cost = cost); r <- real_ret(k); s$I * (exp(r) - 1) - s$cost }))
ewb <- ew_bh_k(NAMES); ewb_s <- ew_of(sapply(NAMES, function(k) exp(real_ret(k)) - 1))
row <- function(lab, f_net, f_zero, bh) data.frame(Specification = lab,
  n1 = f_net(1), n2 = f_net(2), n3 = f_net(3), n4 = f_net(4), n5 = f_net(5), z1 = f_zero(1), z5 = f_zero(5), BH = bh)
tab <- rbind(
  do.call(rbind, lapply(c(0, .03, .05, .07), function(f) row(sprintf("Risk-free rate %d%%", round(100 * f)),
    function(n) rr(ew_strategy_k(NAMES, n), f), function(n) rr(ew_strategy_k(NAMES, n, cost = "zero"), f), rr(ewb, f)))),
  row("Simple instead of log returns", function(n) rr(simple_ew(n, "dyn")), function(n) rr(simple_ew(n, "zero")), rr(ewb_s)),
  row("Arithmetic Sharpe ratio", function(n) sharpe(ew_strategy_k(NAMES, n)), function(n) sharpe(ew_strategy_k(NAMES, n, cost = "zero")), sharpe(ewb)))
write.csv(tab, file.path(OUT, "tableA9.csv"), row.names = FALSE)
print(format(tab, digits = 3))
