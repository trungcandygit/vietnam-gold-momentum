import pandas as pd
import numpy as np
from scipy.stats import skew
from config import DATA_FILE, TRADING_DAYS, WINDOWS, INSTRUMENTS, BENCHMARK, BENCHMARK_LABEL


def load_data() -> pd.DataFrame:
    df = pd.read_excel(DATA_FILE)
    df["Date"] = pd.to_datetime(df["Date"])
    return df.sort_values("Date").reset_index(drop=True)


def mid_price(df: pd.DataFrame, bid: str, ask: str) -> pd.Series:
    return (df[bid] + df[ask]) / 2.0


def log_return(prices: pd.Series) -> pd.Series:
    return np.log(prices / prices.shift(1))


def half_spread(df: pd.DataFrame, bid: str, ask: str) -> pd.Series:
    mid = mid_price(df, bid, ask)
    return (df[ask] - df[bid]) / (2.0 * mid)


def annualize(r: pd.Series) -> tuple:
    Ra = (1.0 + r.mean()) ** TRADING_DAYS - 1.0
    sa = r.std() * np.sqrt(TRADING_DAYS)
    RR = Ra / sa if sa > 0 else float("nan")
    return Ra, sa, RR


def momentum_signal(r: pd.Series, n: int, mode: str) -> pd.Series:
    a = r.shift(1).rolling(n).mean()
    if mode == "long_only":
        return pd.Series(np.where(a > 0, 1.0, 0.0), index=r.index)
    return pd.Series(
        np.where(a > 0, 1.0, np.where(a < 0, -1.0, 0.0)),
        index=r.index
    )


def net_return(r: pd.Series, c: pd.Series, n: int, mode: str) -> pd.Series:
    I = momentum_signal(r, n, mode)
    return (I.shift(1) * r - np.abs(I.diff()) * c).dropna()


def strategy_metrics(r: pd.Series, c: pd.Series, n: int, mode: str) -> tuple:
    R = net_return(r, c, n, mode)
    if len(R) < 30:
        return float("nan"), float("nan")
    Ra, sa, RR = annualize(R)
    return round(Ra, 3), round(RR, 2)


def equity_curve(r: pd.Series, c: pd.Series, n: int, mode: str, dates: pd.Series) -> pd.Series:
    I = momentum_signal(r, n, mode)
    R = (I.shift(1) * r - np.abs(I.diff()) * c).fillna(0.0)
    eq = (1.0 + R).cumprod()
    eq.index = dates
    return eq.dropna()


def build_instrument_series(df: pd.DataFrame) -> dict:
    series = {}
    for name, (bid, ask, *_) in INSTRUMENTS.items():
        m = mid_price(df, bid, ask)
        series[name] = (log_return(m), half_spread(df, bid, ask))

    r_list = [v[0] for v in series.values()]
    c_list = [v[1] for v in series.values()]
    r_ew = pd.concat(r_list, axis=1).mean(axis=1)
    c_ew = pd.concat(c_list, axis=1).mean(axis=1)
    series["EW Portfolio"] = (r_ew, c_ew)

    r_bm = log_return(df[BENCHMARK])
    c_bm = pd.Series(0.0, index=df.index)
    series[BENCHMARK_LABEL] = (r_bm, c_bm)

    return series


CSS = """
* { box-sizing:border-box; margin:0; padding:0; }
body {
    font-family: "Times New Roman", Times, serif;
    font-size: 13px; color: #111; background: #fff;
    padding: 44px 56px; max-width: 1060px; margin: auto;
}
section  { margin-bottom: 54px; }
h1       { font-size: 13px; font-weight: bold; margin-bottom: 4px; }
.sub     { font-style: italic; font-size: 12.5px; margin-bottom: 14px;
           color: #222; line-height: 1.55; }
table    { border-collapse: collapse; width: 100%; font-size: 12.5px; }
thead tr { border-top: 2px solid #000; border-bottom: 1px solid #000; }
tbody tr:last-child { border-bottom: 2px solid #000; }
th       { font-weight: bold; text-align: right; padding: 6px 11px; }
th:first-child { text-align: left; }
td       { padding: 5px 11px; }
td.L     { text-align: left; min-width: 160px; }
td.R     { text-align: right; font-variant-numeric: tabular-nums; }
td.neg   { color: #c00; }
td.hi    { font-weight: bold; }
tbody tr:nth-child(even) td { background: #f9f9f9; }
tr.avg td { border-top: 1px solid #888; font-weight: bold;
            background: #eef2ff !important; }
tr.ben td { border-top: 1px dashed #bbb; background: #f5f5f5 !important; }
.note    { margin-top: 9px; font-size: 11.5px; color: #444; line-height: 1.65; }
"""


def html_page(sections: list) -> str:
    return (
        f'<!DOCTYPE html><html lang="en">\n'
        f'<head><meta charset="UTF-8">'
        f'<title>Vietnamese Gold – Trading Strategy Results</title>'
        f'<style>{CSS}</style></head>\n'
        f'<body>\n{"".join(sections)}\n</body></html>'
    )


def render_table(df_tbl: pd.DataFrame, name_col: str,
                 val_cols: list, fmt: str, threshold: float = None) -> str:
    header = (
        f'<th style="text-align:left"></th>'
        + "".join(f"<th>{c}</th>" for c in val_cols)
    )
    rows_html = ""
    for _, row in df_tbl.iterrows():
        nm = str(row[name_col])
        tr_cls = (
            ' class="avg"' if "EW PORTFOLIO" in nm.upper()
            else (' class="ben"' if "XAU" in nm.upper() else "")
        )
        cells = f'<td class="L">{nm}</td>'
        for col in val_cols:
            v = row[col]
            if pd.isna(v):
                s, cc = "—", ""
            else:
                try:
                    fv = float(v)
                    s  = f"{fv:{fmt}}"
                    cc = "neg" if fv < 0 else ("hi" if threshold and fv >= threshold else "")
                except (ValueError, TypeError):
                    s  = str(v)
                    cc = ""
            cells += f'<td class="R {cc}">{s}</td>'
        rows_html += f"<tr{tr_cls}>{cells}</tr>\n"
    return f"<thead><tr>{header}</tr></thead><tbody>{rows_html}</tbody>"

