# Vietnam physical gold momentum: data and code

Dataset and R code for "Do short-term momentum profits survive realised bid–ask
spreads? Evidence from the Vietnamese physical gold market" (JEBS.1563).

## Data

- `lich_su_gia_vang_PNJ_MAY_TINH.csv` — raw archive of PNJ's daily price board (buy/sell quotes).
- `Master_Gold_Dataset_Cleaned_Quant.csv` / `.xlsx` — cleaned dataset used by the analysis scripts.
- `readme.pdf` — data dictionary.

## Analysis scripts

Run in order; each stage writes its outputs to `out/` for the next stage to read.

| Script | Purpose |
|---|---|
| `00_data_audit.R` | Sample audit: coverage, missing sessions, last archived quote and LOCF tail |
| `01_engine.R` | Core trading-rule and cost engine (sourced by the other scripts) |
| `02_main.R` | Main results: returns, risk-return ratios, significance tests |
| `03_robust.R` | Robustness checks (samples, compositions, sub-periods, cost scenarios, extended windows) |
| `05_numbers.R` | Collects headline numbers used in the manuscript |
| `06_xau_cost.R` | International benchmark (XAU/VND) cost sensitivity |
| `07_table_A9.R` | Table A9: risk-free rate and return-definition sensitivity |
| `08_fig2_spreads.R` | Figure 2: bid-ask spreads by segment (60-session rolling mean) |

Figure 1 (research design) is a diagram drawn in the manuscript and has no script.
Figure outputs are in `figs/`.

## Requirements

R packages: see `requirements.txt`.
