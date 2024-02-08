import polars as pl


def get_census_data(
    url: str = "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv"
):
    df = pl.read_csv(url)
    return df.filter(
        pl.col("STNAME").eq("Montana")
    ).select(
        pl.col("CTYNAME", "POPESTIMATE2022")
    ).rename(
        {
            "CTYNAME": "County",
            "POPESTIMATE2022": "Pupulation"
        }
    ).filter(
        pl.col("County").str.contains("County")
    )

