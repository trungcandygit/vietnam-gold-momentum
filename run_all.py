import tables
import figures

if __name__ == "__main__":
    print("── Generating tables ──────────────────────────")
    tables.run()
    print("\n── Generating figures ─────────────────────────")
    figures.run()
    print("\n[DONE]")
    print("  output_tables.html")
    print("  fig3_longonly.png")
    print("  fig4_longshort.png")
