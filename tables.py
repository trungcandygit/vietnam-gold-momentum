import pandas as pd
from scipy.stats import skew
from config import INSTRUMENTS, BENCHMARK, BENCHMARK_LABEL, WINDOWS
from core import (
    load_data, mid_price, log_return, half_spread,
    annualize, strategy_metrics, build_instrument_series,
    html_page, render_table,
)


def make_table1_table2(df: pd.DataFrame) -> tuple:
    rows1, rows2 = [], []

    for name, (bid, ask, asset_type, brand, karat, region) in INSTRUMENTS.items():
        m  = mid_price(df, bid, ask)
        sp = half_spread(df, bid, ask) * 2.0 * 100.0
        r  = log_return(m).dropna() * 100.0
        rows1.append({
            "Instrument":   name,
            "Type":         asset_type,
            "Brand":        brand,
            "Karat":        karat,
            "Region":       region,
            "N":            len(r),
            "Start":        df["Date"].min().strftime("%d/%m/%Y"),
            "End":          df["Date"].max().strftime("%d/%m/%Y"),
            "Avg. Price (M VND)": round(m.mean(), 3),
            "Avg. Spread (%)":    round(sp.dropna().mean(), 4),
        })
        rows2.append({
            "Instrument": name,
            "Mean":   round(r.mean(), 6),
            "Std":    round(r.std(),  6),
            "Min":    round(r.min(),  6),
            "P25":    round(r.quantile(0.25), 6),
            "Median": round(r.quantile(0.50), 6),
            "P75":    round(r.quantile(0.75), 6),
            "Max":    round(r.max(),  6),
        })

    r_bm = log_return(df[BENCHMARK]).dropna() * 100.0
    rows2.append({
        "Instrument": BENCHMARK_LABEL,
        "Mean":   round(r_bm.mean(), 6),
        "Std":    round(r_bm.std(),  6),
        "Min":    round(r_bm.min(),  6),
        "P25":    round(r_bm.quantile(0.25), 6),
        "Median": round(r_bm.quantile(0.50), 6),
        "P75":    round(r_bm.quantile(0.75), 6),
        "Max":    round(r_bm.max(),  6),
    })

    df1 = pd.DataFrame(rows1)
    df2 = pd.DataFrame(rows2)

    v1 = ["Type","Brand","Karat","Region","N","Start","End","Avg. Price (M VND)","Avg. Spread (%)"]
    v2 = ["Mean","Std","Min","P25","Median","P75","Max"]

    s1 = (
        "<section><h1>Table 1</h1>"
        "<p class='sub'>Sample description: Vietnamese gold instruments and market microstructure characteristics.</p>"
        f"<table>{render_table(df1, 'Instrument', v1, '.4g')}</table>"
        "<p class='note'><em>Source:</em> Author's compilation from giavang.org (2015–2025). "
        "Avg. Price = P<sup>mid</sup> = (P<sup>bid</sup>+P<sup>ask</sup>)/2 (million VND per chi). "
        "Avg. Spread = (P<sup>ask</sup>&#8722;P<sup>bid</sup>)/P<sup>mid</sup>&#215;100. "
        "(*) PNJ Mekong Delta: abnormal spread due to data interruptions.</p></section>"
    )

    s2 = (
        "<section><h1>Table 2</h1>"
        "<p class='sub'>Descriptive statistics of daily log returns r<sub>t</sub> = ln(P<sup>mid</sup><sub>t</sub>/P<sup>mid</sup><sub>t&#8722;1</sub>)&#215;100 (%).</p>"
        f"<table>{render_table(df2, 'Instrument', v2, '.6f')}</table>"
        "<p class='note'><em>Source:</em> Author's calculations. N = 2,837 trading sessions (Jan 2015–Dec 2025). "
        "(*) PNJ Mekong Delta: Min = &#8722;39.20%, Max = +39.01% due to data source interruptions.</p></section>"
    )
    return s1, s2


def make_table3(df: pd.DataFrame) -> str:
    series = build_instrument_series(df)
    rows = []
    for name, (r, _) in series.items():
        r_clean = r.dropna()
        Ra, sa, RR = annualize(r_clean)
        sk = float(skew(r_clean.values))
        rows.append({
            "Instrument": name,
            "Ann. Return (R_a)": round(Ra, 6),
            "Ann. Std (σ_a)":    round(sa, 6),
            "RR":                round(RR, 6),
            "Skewness":          round(sk, 6),
        })
    df3 = pd.DataFrame(rows)
    v3  = ["Ann. Return (R_a)", "Ann. Std (σ_a)", "RR", "Skewness"]
    return (
        "<section><h1>Table 3</h1>"
        "<p class='sub'>Annualized buy-and-hold performance of Vietnamese gold instruments and the benchmark.</p>"
        f"<table>{render_table(df3, 'Instrument', v3, '.6f', threshold=1.0)}</table>"
        "<p class='note'><em>Source:</em> Author's calculations (Jan 2015–Dec 2025). N = 2,837 sessions. "
        "R<sub>a</sub>=(1+r&#772;)<sup>252</sup>&#8722;1; "
        "&#963;<sub>a</sub>=&#963;<sub>d</sub>&#183;&#8730;252; "
        "RR=R<sub>a</sub>/&#963;<sub>a</sub> (zero risk-free rate). "
        "<strong>Bold</strong>: RR&#8805;1.00. <em>EW Portfolio</em>: equally-weighted across 13 domestic instruments.</p></section>"
    )


def make_table4(df: pd.DataFrame) -> str:
    series = build_instrument_series(df)
    rows = {
        name: {f"{n}d": strategy_metrics(r, c, n, "long_only")[1] for n in WINDOWS}
        for name, (r, c) in series.items()
    }
    df4 = pd.DataFrame(rows).T.reset_index().rename(columns={"index": "Instrument"})
    df4.columns = ["Instrument"] + [f"{n} day" for n in WINDOWS]
    vc = [f"{n} day" for n in WINDOWS]
    return (
        "<section><h1>Table 4</h1>"
        "<p class='sub'>Annualized risk-return ratios for different lookback windows (long-only strategy). "
        "Column headers denote lookback window length in trading days.</p>"
        f"<table>{render_table(df4, 'Instrument', vc, '.2f', threshold=1.0)}</table>"
        "<p class='note'><em>Source:</em> Author's calculations (Jan 2015–Dec 2025). N = 2,837 sessions. "
        "Momentum signal: a<sub>t,n</sub>=(1/n)&#8721;r<sub>t&#8722;i</sub> (i=1..n, no look-ahead bias). "
        "Position: I<sub>t</sub>=1 if a&gt;0, else 0. "
        "Net return: R<sub>t</sub>=I<sub>t&#8722;1</sub>&#183;r<sub>t</sub>&#8722;|I<sub>t</sub>&#8722;I<sub>t&#8722;1</sub>|&#183;c<sub>t</sub>, "
        "where c<sub>t</sub>=(P<sup>ask</sup>&#8722;P<sup>bid</sup>)/(2P<sup>mid</sup>). "
        "<strong>Bold</strong>: RR&#8805;1. Red: RR&lt;0.</p></section>"
    )


def make_table5_table6(df: pd.DataFrame) -> tuple:
    series = build_instrument_series(df)
    t5 = {
        name: {f"{n}d": strategy_metrics(r, c, n, "long_only")[0]  for n in WINDOWS}
        for name, (r, c) in series.items()
    }
    t6 = {
        name: {f"{n}d": strategy_metrics(r, c, n, "long_short")[1] for n in WINDOWS}
        for name, (r, c) in series.items()
    }

    def to_df(d):
        out = pd.DataFrame(d).T.reset_index().rename(columns={"index": "Instrument"})
        out.columns = ["Instrument"] + [f"{n} day" for n in WINDOWS]
        return out

    df5, df6 = to_df(t5), to_df(t6)
    vc = [f"{n} day" for n in WINDOWS]

    s5 = (
        "<section><h1>Table 5</h1>"
        "<p class='sub'>Annualized returns for different lookback windows (long-only strategy). "
        "Column headers denote lookback window length in trading days.</p>"
        f"<table>{render_table(df5, 'Instrument', vc, '.3f')}</table>"
        "<p class='note'><em>Source:</em> Author's calculations (Jan 2015–Dec 2025). "
        "R<sub>a</sub>=(1+R&#772;<sup>strat</sup>)<sup>252</sup>&#8722;1. Red: R<sub>a</sub>&lt;0.</p></section>"
    )

    s6 = (
        "<section><h1>Table 6</h1>"
        "<p class='sub'>Annualized risk-return ratios for different lookback windows (long-short strategy). "
        "Column headers denote lookback window length in trading days.</p>"
        f"<table>{render_table(df6, 'Instrument', vc, '.2f', threshold=1.0)}</table>"
        "<p class='note'><em>Source:</em> Author's calculations (Jan 2015–Dec 2025). "
        "I<sub>t</sub>=1 if a&gt;0; &#8722;1 if a&lt;0; 0 if a=0. "
        "Short-selling physical gold is legally prohibited in Vietnam — results are theoretical benchmarks only. "
        "<strong>Bold</strong>: RR&#8805;1. Red: RR&lt;0.</p></section>"
    )
    return s5, s6


def run():
    df = load_data()
    s1, s2 = make_table1_table2(df)
    s3     = make_table3(df)
    s4     = make_table4(df)
    s5, s6 = make_table5_table6(df)
    with open("output_tables.html", "w", encoding="utf-8") as f:
        f.write(html_page([s1, s2, s3, s4, s5, s6]))
    print("[OK] output_tables.html")


if __name__ == "__main__":
    run()
