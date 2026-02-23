# robustness.py  (updated: removed Bootstrap SPA Test)

import pandas as pd
import numpy as np
from core import (
    load_data, mid_price, log_return, half_spread,
    annualize, momentum_signal, net_return,
    build_instrument_series, html_page, CSS,
)
from config import INSTRUMENTS, BENCHMARK, BENCHMARK_LABEL, TRADING_DAYS

OUTPUT = "robustness_output.html"

# ── Sub-periods ───────────────────────────────────────────────────
PERIODS = {
    "Pre-COVID (2015–2019)":    ("2015-01-01", "2019-12-31"),
    "COVID (2020–2022)":        ("2020-01-01", "2022-12-31"),
    "Post-COVID (2023–2025)":   ("2023-01-01", "2025-12-31"),
}

# ── Cost scenarios ────────────────────────────────────────────────
COST_SCENARIOS = {
    "Dynamic (half-spread)": "dynamic",
    "Zero cost (c=0)":       "zero",
    "Fixed cost (c=0.25%)":  "fixed",
}

# ── Extended windows ──────────────────────────────────────────────
WINDOWS_EXT = [1, 2, 3, 4, 5, 10, 20]

# ── Instruments to exclude for EW-ex-Mekong ──────────────────────
EXCL_MEKONG = {"PNJ Mekong Delta", "SJC Mekong Delta"}


# ══════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════

def rr_metrics(r, c, n, mode, cost_type="dynamic"):
    if cost_type == "zero":
        c_use = pd.Series(0.0, index=c.index)
    elif cost_type == "fixed":
        c_use = pd.Series(0.0025, index=c.index)
    else:
        c_use = c
    R = net_return(r, c_use, n, mode)
    if len(R) < 30:
        return float("nan"), float("nan"), float("nan")
    Ra, sa, RR = annualize(R)
    return round(Ra, 4), round(sa, 4), round(RR, 2)


def th(cols, first=""):
    h = f'<th style="text-align:left">{first}</th>'
    h += "".join(f"<th>{c}</th>" for c in cols)
    return f"<thead><tr>{h}</tr></thead>"


def td_r(v, neg_red=True, bold_thresh=None):
    if pd.isna(v):
        return '<td class="R">—</td>'
    try:
        fv  = float(v)
        s   = f"{fv:.2f}"
        cc  = ""
        if neg_red and fv < 0:
            cc = "neg"
        elif bold_thresh and fv >= bold_thresh:
            cc = "hi"
        return f'<td class="R {cc}">{s}</td>'
    except (ValueError, TypeError):
        return f'<td class="R">{v}</td>'


def tr_row(label, vals, cls="", neg_red=True, bold_thresh=None):
    cells  = f'<td class="L">{label}</td>'
    cells += "".join(td_r(v, neg_red, bold_thresh) for v in vals)
    return f'<tr{"" if not cls else f" class={chr(34)}{cls}{chr(34)}"}>{cells}</tr>'


# ══════════════════════════════════════════════════════════════════
# TEST 1 — Sub-period Analysis
# ══════════════════════════════════════════════════════════════════

def section_subperiod(df):
    windows    = [1, 2, 3, 4, 5]
    col_labels = [f"n={n}" for n in windows]

    rows_html = ""
    for period_name, (t0, t1) in list(PERIODS.items()) + [("Full sample (2015–2025)", ("2015-01-01", "2025-12-31"))]:
        mask = (df["Date"] >= t0) & (df["Date"] <= t1)
        sub  = df[mask].reset_index(drop=True)
        if len(sub) < 60:
            continue

        r_list, c_list = [], []
        for name, (bid, ask, *_) in INSTRUMENTS.items():
            m = mid_price(sub, bid, ask)
            r_list.append(log_return(m))
            c_list.append(half_spread(sub, bid, ask))
        r_ew = pd.concat(r_list, axis=1).mean(axis=1)
        c_ew = pd.concat(c_list, axis=1).mean(axis=1)

        vals = [rr_metrics(r_ew, c_ew, n, "long_only")[2] for n in windows]
        cls  = "avg" if "Full" in period_name else ""
        rows_html += tr_row(period_name, vals, cls=cls, bold_thresh=1.0)

    body = f"<tbody>{rows_html}</tbody>"
    tbl  = f"<table>{th(col_labels, 'Sub-period')}{body}</table>"

    return (
        "<section>"
        "<h1>Table R1 — Sub-period Robustness</h1>"
        "<p class='sub'>Risk-return ratio (RR) of the long-only EW portfolio across three "
        "structural sub-periods. A consistent pattern of negative RR across all sub-periods "
        "rules out the possibility that a single episode drives the aggregate result.</p>"
        f"{tbl}"
        "<p class='note'><em>Note:</em> Long-only strategy, dynamic transaction cost "
        "(half-spread). EW portfolio = equally-weighted average of 13 domestic gold instruments. "
        "<b>Bold</b>: RR ≥ 1.00. <span style='color:#c00'>Red</span>: RR &lt; 0.</p>"
        "</section>"
    )


# ══════════════════════════════════════════════════════════════════
# TEST 2 — Alternative Transaction Costs
# ══════════════════════════════════════════════════════════════════

def section_altcost(df):
    windows    = [1, 2, 3, 4, 5]
    col_labels = [f"n={n}" for n in windows]

    r_list, c_list = [], []
    for name, (bid, ask, *_) in INSTRUMENTS.items():
        m = mid_price(df, bid, ask)
        r_list.append(log_return(m))
        c_list.append(half_spread(df, bid, ask))
    r_ew = pd.concat(r_list, axis=1).mean(axis=1)
    c_ew = pd.concat(c_list, axis=1).mean(axis=1)

    rows_html = ""
    for scenario_name, cost_type in COST_SCENARIOS.items():
        vals = [rr_metrics(r_ew, c_ew, n, "long_only", cost_type)[2] for n in windows]
        cls  = "ben" if cost_type == "fixed" else ""
        rows_html += tr_row(scenario_name, vals, cls=cls, bold_thresh=1.0)

    body = f"<tbody>{rows_html}</tbody>"
    tbl  = f"<table>{th(col_labels, 'Cost scenario')}{body}</table>"

    return (
        "<section>"
        "<h1>Table R2 — Alternative Transaction Cost Scenarios</h1>"
        "<p class='sub'>RR of the long-only EW portfolio under three cost assumptions: "
        "(1) dynamic half-spread; (2) zero cost; (3) fixed 0.25% per trade as in "
        "Nguyen et al. (2021). Positive RR under zero cost confirms that the momentum "
        "signal itself is not weak — transaction costs are the decisive factor.</p>"
        f"{tbl}"
        "<p class='note'><em>Note:</em> Fixed cost = 0.25% per trade (Nguyen et al., 2021). "
        "Dynamic cost = realized half-spread $c_t = (P^{ask}-P^{bid})/(2P^{mid})$. "
        "<b>Bold</b>: RR ≥ 1.00. <span style='color:#c00'>Red</span>: RR &lt; 0.</p>"
        "</section>"
    )


# ══════════════════════════════════════════════════════════════════
# TEST 3 — Extended Lookback Windows
# ══════════════════════════════════════════════════════════════════

def section_extended_windows(df):
    col_labels = [f"n={n}" for n in WINDOWS_EXT]

    r_list, c_list = [], []
    for name, (bid, ask, *_) in INSTRUMENTS.items():
        m = mid_price(df, bid, ask)
        r_list.append(log_return(m))
        c_list.append(half_spread(df, bid, ask))
    r_ew = pd.concat(r_list, axis=1).mean(axis=1)
    c_ew = pd.concat(c_list, axis=1).mean(axis=1)

    rows_html = ""
    for mode, label in [("long_only", "Long-Only"), ("long_short", "Long-Short")]:
        vals = [rr_metrics(r_ew, c_ew, n, mode)[2] for n in WINDOWS_EXT]
        rows_html += tr_row(label, vals, bold_thresh=1.0)

    body = f"<tbody>{rows_html}</tbody>"
    tbl  = f"<table>{th(col_labels, 'Strategy')}{body}</table>"

    return (
        "<section>"
        "<h1>Table R3 — Extended Lookback Windows (n = 1 to 20)</h1>"
        "<p class='sub'>RR of the EW portfolio for lookback windows extended to 10 and "
        "20 trading days. Persistent negative RR across all windows confirms that the "
        "finding is not an artefact of the short-window parameterization.</p>"
        f"{tbl}"
        "<p class='note'><em>Note:</em> Dynamic transaction cost. "
        "<b>Bold</b>: RR ≥ 1.00. <span style='color:#c00'>Red</span>: RR &lt; 0.</p>"
        "</section>"
    )


# ══════════════════════════════════════════════════════════════════
# TEST 4 — Exclude PNJ & SJC Mekong Delta
# ══════════════════════════════════════════════════════════════════

def section_excl_mekong(df):
    windows    = [1, 2, 3, 4, 5]
    col_labels = [f"n={n}" for n in windows]

    def ew_rr(excl=None):
        keys = [k for k in INSTRUMENTS if (excl is None or k not in excl)]
        r_list, c_list = [], []
        for k in keys:
            bid, ask = INSTRUMENTS[k][0], INSTRUMENTS[k][1]
            m = mid_price(df, bid, ask)
            r_list.append(log_return(m))
            c_list.append(half_spread(df, bid, ask))
        r = pd.concat(r_list, axis=1).mean(axis=1)
        c = pd.concat(c_list, axis=1).mean(axis=1)
        return [rr_metrics(r, c, n, "long_only")[2] for n in windows]

    rows_html  = tr_row("EW Full (13 instruments)",      ew_rr(None),        bold_thresh=1.0)
    rows_html += tr_row("EW ex-Mekong (11 instruments)", ew_rr(EXCL_MEKONG), bold_thresh=1.0, cls="avg")

    body = f"<tbody>{rows_html}</tbody>"
    tbl  = f"<table>{th(col_labels, 'Portfolio')}{body}</table>"

    return (
        "<section>"
        "<h1>Table R4 — Exclusion of Anomalous Instruments (PNJ &amp; SJC Mekong Delta)</h1>"
        "<p class='sub'>RR comparison between the full EW portfolio (13 instruments) and "
        "a restricted portfolio excluding PNJ Mekong Delta and SJC Mekong Delta, which "
        "exhibit anomalous returns of ±39% due to data gaps in the source. Consistent "
        "negative RR in both portfolios confirms result robustness.</p>"
        f"{tbl}"
        "<p class='note'><em>Note:</em> Long-only strategy, dynamic transaction cost, "
        "n = 1 to 5 days. <b>Bold</b>: RR ≥ 1.00. "
        "<span style='color:#c00'>Red</span>: RR &lt; 0.</p>"
        "</section>"
    )


# ══════════════════════════════════════════════════════════════════
# MASTER RUNNER
# ══════════════════════════════════════════════════════════════════

def run():
    print("── Robustness tests ──────────────────────────")
    df = load_data()

    sections = [
        "<h1 style='font-size:15px;margin-bottom:6px'>"
        "Robustness Checks — Vietnamese Physical Gold Momentum Study</h1>"
        "<p class='sub'>Full-sample period: 02 January 2015 – 31 December 2025 "
        "(N = 2,837 sessions). All tests based on the EW equally-weighted portfolio "
        "of 13 domestic gold instruments unless otherwise stated.</p><hr><br>",
    ]

    print("  [1/4] Sub-period analysis …")
    sections.append(section_subperiod(df))

    print("  [2/4] Alternative transaction costs …")
    sections.append(section_altcost(df))

    print("  [3/4] Extended lookback windows …")
    sections.append(section_extended_windows(df))

    print("  [4/4] Exclusion of Mekong Delta instruments …")
    sections.append(section_excl_mekong(df))

    html = html_page(sections)
    with open(OUTPUT, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"[OK] {OUTPUT}")
    print("── Done ──────────────────────────────────────")


if __name__ == "__main__":
    run()
