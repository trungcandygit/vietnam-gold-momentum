import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from config import INSTRUMENTS, WINDOWS
from core import load_data, mid_price, log_return, half_spread, equity_curve


def build_ew_series(df: pd.DataFrame) -> tuple:
    r_list, c_list = [], []
    for bid, ask, *_ in INSTRUMENTS.values():
        m = mid_price(df, bid, ask)
        r_list.append(log_return(m))
        c_list.append(half_spread(df, bid, ask))
    return (
        pd.concat(r_list, axis=1).mean(axis=1),
        pd.concat(c_list, axis=1).mean(axis=1),
    )


def plot_equity_figure(mode: str, outfile: str, caption: str):
    df = load_data()
    dates = df["Date"]
    r_ew, c_ew = build_ew_series(df)

    matplotlib.rcParams.update({
        "font.family":      "DejaVu Sans",
        "font.size":         9,
        "axes.linewidth":    0.8,
        "xtick.major.width": 0.6,
        "ytick.major.width": 0.6,
    })

    fig, axes = plt.subplots(
        nrows=5, ncols=1,
        figsize=(7.5, 10.5),
        constrained_layout=True,
    )

    for ax, n in zip(axes, WINDOWS):
        eq = equity_curve(r_ew, c_ew, n, mode, dates)
        ax.plot(eq.index, eq.values, color="#1f77b4", linewidth=0.75)
        ax.set_title(f"lookback = {n}", fontsize=9, pad=4)
        ax.set_xlabel("Date", fontsize=8)
        ax.xaxis.set_major_locator(mdates.YearLocator())
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
        ax.xaxis.set_minor_locator(mdates.MonthLocator(bymonth=[7]))
        ax.yaxis.grid(True, linestyle="--", linewidth=0.4, alpha=0.55)
        ax.set_axisbelow(True)
        ymin, ymax = eq.min(), eq.max()
        margin = (ymax - ymin) * 0.05
        ax.set_ylim(ymin - margin, ymax + margin)
        ax.set_xlim(eq.index[0], eq.index[-1])
        ax.tick_params(axis="both", labelsize=8)
        for spine in ax.spines.values():
            spine.set_linewidth(0.7)

    fig.text(0.5, -0.008, caption, ha="center", va="top",
             fontsize=9, style="italic", wrap=True)
    fig.savefig(outfile, dpi=200, bbox_inches="tight",
                facecolor="white", edgecolor="none")
    plt.close(fig)
    print(f"[OK] {outfile}")


def run():
    plot_equity_figure(
        mode    = "long_only",
        outfile = "fig3_longonly.png",
        caption = (
            "Fig. 3. Equity curve for long-only portfolios (equally-weighted across 13 Vietnamese "
            "gold instruments) with different lookback windows varying from 1 to 5 days."
        ),
    )
    plot_equity_figure(
        mode    = "long_short",
        outfile = "fig4_longshort.png",
        caption = (
            "Fig. 4. Equity curve for long-short portfolios (equally-weighted across 13 Vietnamese "
            "gold instruments) with different lookback windows varying from 1 to 5 days."
        ),
    )


if __name__ == "__main__":
    run()
