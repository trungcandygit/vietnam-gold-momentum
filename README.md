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
| `00_data_audit.R` | Sample audit: coverage, missing sessions, LOCF imputation |
| `01_engine.R` | Core trading-rule and cost engine |
| `02_main.R` | Main results: returns, risk-return ratios, significance tests |
| `03_robust.R` | Robustness checks (sub-periods, cost scenarios, extended windows) |
| `05_numbers.R` | Collects headline numbers used in the manuscript |
| `06_xau_cost.R` | International benchmark (XAU/VND) cost sensitivity |
| `04_figures_rdefault.R` | Figure 1 (research design) |
| `04b_fig2_baseR.R` | Figure 2 (bid-ask spreads by segment) |
| `04c_fig3_baseR_drawdown.R` | Figure 3 (portfolio drawdown) |
| `04d_fig4_baseR.R` | Figure 4 (risk-return ratios and return decomposition) |

Figure outputs are in `figs/`.

## Requirements

R packages: see `requirements.txt`.
