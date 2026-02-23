import os
import sys

def generate_readme(file_path="README.md"):
    # open with r"""
    readme_content = r"""# Momentum-Based Expert Trading System for the Vietnamese Physical Gold Market

> Replication and extension of Nguyen et al. (2021)  
> *Borsa Istanbul Review*, Vol. 21(1), pp. 23–35  
> DOI: [10.1016/j.bir.2020.05.005](https://doi.org/10.1016/j.bir.2020.05.005)

---

## Overview

This repository provides a full quantitative replication kit examining whether short-term momentum strategies generate positive risk-adjusted returns in the Vietnamese physical gold market (January 2015 – December 2025, N = 2,837 sessions).

The central methodological contribution is the **endogenization of transaction costs** via realized bid-ask half-spreads, replacing the fixed-fee assumption (0.25%) used in the original equity-market study.

**Key finding:** All 65 instrument × lookback-window combinations (13 instruments × 5 windows) yield negative risk-return ratios under both long-only and long-short strategies. Buy-and-hold dominates (EW portfolio RR = 1.23), consistent with the sticky-pricing microstructure of Vietnam's physical gold market.

---

## Repository Structure

```text
vietnam-gold-momentum/
│
├── config.py           # Constants: instruments, TD=252, windows
├── core.py             # Engine: prices, signals, metrics, HTML/CSS
├── tables.py           # Tables 1–6 → output_tables.html
├── figures.py          # Fig. 3–4 → PNG equity curves
├── run_all.py          # Master runner (one command)
│
├── Master_Gold_Dataset_Cleaned_Quant.xlsx  # Input dataset
│
├── output_tables.html  # Auto-generated (open in browser)
├── fig3_longonly.png   # Auto-generated
├── fig4_longshort.png  # Auto-generated
│
├── requirements.txt
└── README.md