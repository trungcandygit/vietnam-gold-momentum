# 00_data_audit.R  -- Reviewer 2 (Data and Sample): missingness/imputation audit by asset.
# Inputs : vietnam-gold-momentum/Master_Gold_Dataset_Cleaned_Quant.xlsx (cleaned master, 2,838 weekday rows)
#          vietnam-gold-momentum/lich_su_gia_vang_PNJ_MAY_TINH.csv      (raw giavang.org scrape of the PNJ price board)
# Outputs: analysis/out/data_audit.csv , analysis/out/data_audit_summary.json
suppressMessages({library(data.table); library(readxl); library(jsonlite)})
root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
if (!dir.exists(file.path(root, "vietnam-gold-momentum"))) root <- getwd()
repo <- file.path(root, "vietnam-gold-momentum"); out <- file.path(root, "analysis", "out"); dir.create(out, showWarnings = FALSE, recursive = TRUE)

m <- as.data.table(read_excel(file.path(repo, "Master_Gold_Dataset_Cleaned_Quant.xlsx"))); m[, Date := as.Date(Date)]
raw <- fread(file.path(repo, "lich_su_gia_vang_PNJ_MAY_TINH.csv"), encoding = "UTF-8")
setnames(raw, c("region", "type", "buy", "sell", "stamp", "page_date"))
raw <- raw[!grepl("^http", region)]
raw[, page_date := as.Date(page_date)]
raw[, stamp_date := as.Date(substr(stamp, 10, 19), format = "%d/%m/%Y")]
raw[, `:=`(buy = as.numeric(buy), sell = as.numeric(sell))]

# instrument -> (region, type) in raw and (buy, sell) column in master
map <- rbindlist(list(
  data.table(inst = "Ring PNJ 24K",     region = "Giá vàng nữ trang", type = "Nhẫn PNJ (24K)", b = "Nhan_PNJ_24K_Buy", s = "Nhan_PNJ_24K_Sell", group = "jewelry"),
  data.table(inst = "Jewellery 10K",    region = "Giá vàng nữ trang", type = "Nữ trang 10K",    b = "NuTrang_10K_Buy",   s = "NuTrang_10K_Sell",   group = "jewelry"),
  data.table(inst = "Jewellery 14K",    region = "Giá vàng nữ trang", type = "Nữ trang 14K",    b = "NuTrang_14K_Buy",   s = "NuTrang_14K_Sell",   group = "jewelry"),
  data.table(inst = "Jewellery 18K",    region = "Giá vàng nữ trang", type = "Nữ trang 18K",    b = "NuTrang_18K_Buy",   s = "NuTrang_18K_Sell",   group = "jewelry"),
  data.table(inst = "Jewellery 24K",    region = "Giá vàng nữ trang", type = "Nữ trang 24K",    b = "NuTrang_24K_Buy",   s = "NuTrang_24K_Sell",   group = "jewelry"),
  data.table(inst = "PNJ Da Nang",      region = "Đà Nẵng",  type = "PNJ", b = "PNJ_DN_Buy",    s = "PNJ_DN_Sell",    group = "bullion"),
  data.table(inst = "PNJ Hanoi",        region = "Hà Nội",   type = "PNJ", b = "PNJ_HN_Buy",    s = "PNJ_HN_Sell",    group = "bullion"),
  data.table(inst = "PNJ Mekong Delta", region = "Miền Tây", type = "PNJ", b = "PNJ_MT_Buy",    s = "PNJ_MT_Sell",    group = "bullion"),
  data.table(inst = "PNJ Ho Chi Minh",  region = "TPHCM",    type = "PNJ", b = "PNJ_TPHCM_Buy", s = "PNJ_TPHCM_Sell", group = "bullion"),
  data.table(inst = "SJC Da Nang",      region = "Đà Nẵng",  type = "SJC", b = "SJC_DN_Buy",    s = "SJC_DN_Sell",    group = "bullion"),
  data.table(inst = "SJC Hanoi",        region = "Hà Nội",   type = "SJC", b = "SJC_HN_Buy",    s = "SJC_HN_Sell",    group = "bullion"),
  data.table(inst = "SJC Mekong Delta", region = "Miền Tây", type = "SJC", b = "SJC_MT_Buy",    s = "SJC_MT_Sell",    group = "bullion"),
  data.table(inst = "SJC Ho Chi Minh",  region = "TPHCM",    type = "SJC", b = "SJC_TPHCM_Buy", s = "SJC_TPHCM_Sell", group = "bullion")))

res <- list()
for (i in seq_len(nrow(map))) {
  mp <- map[i]; rr <- raw[region == mp$region & type == mp$type][order(page_date)]
  rr <- rr[!duplicated(page_date, fromLast = TRUE)]
  mm <- m[, .(Date, buy = get(mp$b), sell = get(mp$s))]
  j <- merge(mm, rr[, .(Date = page_date, rbuy = buy, rsell = sell, stamp_date)], by = "Date", all.x = TRUE)
  have_raw <- !is.na(j$rbuy)
  agree <- have_raw & abs(j$buy - j$rbuy) < 1e-9 & abs(j$sell - j$rsell) < 1e-9
  # last date on which the master quote changed / last date the raw board carried a fresh timestamp
  chg <- which(c(TRUE, diff(j$buy) != 0 | diff(j$sell) != 0)); last_change <- j$Date[max(chg)]
  last_stamp <- max(rr$stamp_date, na.rm = TRUE)
  trailing <- sum(j$Date > last_change)
  # sessions whose board timestamp predates the session date by >= 1 calendar day (source did not refresh that day)
  stale_src <- sum(have_raw & !is.na(j$stamp_date) & (j$Date - j$stamp_date) >= 1)
  res[[i]] <- data.table(instrument = mp$inst, group = mp$group, rows = nrow(j),
    no_raw_page = sum(!have_raw), raw_agrees_with_master = sum(agree), raw_disagrees = sum(have_raw & !agree),
    last_quote_change = as.character(last_change), last_raw_board_stamp = as.character(last_stamp),
    trailing_locf_sessions = trailing, trailing_locf_pct = round(100 * trailing / nrow(j), 2),
    stale_board_sessions = stale_src, stale_board_pct = round(100 * stale_src / nrow(j), 2),
    real_window_end = as.character(last_change), real_obs_share_pct = round(100 * (1 - trailing / nrow(j)), 2))
}
a <- rbindlist(res); fwrite(a, file.path(out, "data_audit.csv"))
print(a[, .(instrument, rows, no_raw_page, raw_disagrees, last_quote_change, last_raw_board_stamp, trailing_locf_sessions, trailing_locf_pct, stale_board_pct)])

# benchmark series
xa <- m[, .(Date, XAU_Close, USDVND_Close, XAU_VND_QuyDoi)]
bm <- list(xau_trailing_same = sum(rev(cumsum(rev(c(diff(xa$XAU_Close) == 0, FALSE)))) > 0 & FALSE),
           xau_last_change = as.character(xa$Date[max(which(c(TRUE, diff(xa$XAU_Close) != 0)))]),
           usdvnd_last_change = as.character(xa$Date[max(which(c(TRUE, diff(xa$USDVND_Close) != 0)))]),
           usdvnd_zero_change_share = round(mean(diff(xa$USDVND_Close) == 0), 3))
summ <- list(master_rows = nrow(m), first = as.character(min(m$Date)), last = as.character(max(m$Date)),
             raw_dates = uniqueN(raw$page_date), raw_first = as.character(min(raw$page_date)), raw_last = as.character(max(raw$page_date)),
             raw_regions = unique(raw$region), raw_types = unique(raw$type), benchmark = bm,
             sjc_series_source = "PNJ price board at giavang.org/trong-nuoc/pnj/lich-su (rows with type=SJC)")
write_json(summ, file.path(out, "data_audit_summary.json"), auto_unbox = TRUE, pretty = TRUE)
cat("\nraw covers", uniqueN(raw$page_date), "calendar dates;", "master", nrow(m), "weekday rows\n")
print(bm)
