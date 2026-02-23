DATA_FILE = "Master_Gold_Dataset_Cleaned_Quant.xlsx"
TRADING_DAYS = 252
WINDOWS = [1, 2, 3, 4, 5]

INSTRUMENTS = {
    "Ring PNJ 24K":      ("Nhan_PNJ_24K_Buy",  "Nhan_PNJ_24K_Sell",  "Ring",     "PNJ", "24K", "National"),
    "Jewellery 10K":     ("NuTrang_10K_Buy",    "NuTrang_10K_Sell",   "Jewellery","PNJ", "10K", "National"),
    "Jewellery 14K":     ("NuTrang_14K_Buy",    "NuTrang_14K_Sell",   "Jewellery","PNJ", "14K", "National"),
    "Jewellery 18K":     ("NuTrang_18K_Buy",    "NuTrang_18K_Sell",   "Jewellery","PNJ", "18K", "National"),
    "Jewellery 24K":     ("NuTrang_24K_Buy",    "NuTrang_24K_Sell",   "Jewellery","PNJ", "24K", "National"),
    "PNJ Da Nang":       ("PNJ_DN_Buy",         "PNJ_DN_Sell",        "Bullion",  "PNJ", "24K", "Da Nang"),
    "PNJ Hanoi":         ("PNJ_HN_Buy",         "PNJ_HN_Sell",        "Bullion",  "PNJ", "24K", "Hanoi"),
    "PNJ Mekong Delta":  ("PNJ_MT_Buy",         "PNJ_MT_Sell",        "Bullion",  "PNJ", "24K", "Mekong Delta"),
    "PNJ Ho Chi Minh":   ("PNJ_TPHCM_Buy",      "PNJ_TPHCM_Sell",     "Bullion",  "PNJ", "24K", "Ho Chi Minh"),
    "SJC Da Nang":       ("SJC_DN_Buy",         "SJC_DN_Sell",        "Bullion",  "SJC", "24K", "Da Nang"),
    "SJC Hanoi":         ("SJC_HN_Buy",         "SJC_HN_Sell",        "Bullion",  "SJC", "24K", "Hanoi"),
    "SJC Mekong Delta":  ("SJC_MT_Buy",         "SJC_MT_Sell",        "Bullion",  "SJC", "24K", "Mekong Delta"),
    "SJC Ho Chi Minh":   ("SJC_TPHCM_Buy",      "SJC_TPHCM_Sell",     "Bullion",  "SJC", "24K", "Ho Chi Minh"),
}

BENCHMARK = "XAU_VND_QuyDoi"
BENCHMARK_LABEL = "XAU/VND (International)"
