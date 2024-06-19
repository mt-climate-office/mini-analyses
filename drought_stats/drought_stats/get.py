import geopandas as gpd
import polars as pl
import httpx

def get_census_data(
    url: str = "https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/counties/totals/co-est2022-alldata.csv"
) -> pl.DataFrame:
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


def get_usdm_spatial(
    url: str = "https://droughtmonitor.unl.edu/data/json/usdm_current.json"
):
    data = httpx.get(url)
    gdf = gpd.read_file(data.text, format="GeoJSON")
    return gdf

def get_usdm_county_table(
    url: str = "https://droughtmonitor.unl.edu/DmData/GISData.aspx?mode=table&aoi=county&date="
):
    df = pl.read_csv(url)
    return df.filter(
        pl.col("State").eq("MT")
    ).select(
        pl.col("County", "D0", "D1", "D2", "D3", "D4")
    )


# planetary computer option: 

# https://github.com/microsoft/PlanetaryComputerExamples/blob/main/tutorials/census-data.ipynb
# https://planetarycomputer.microsoft.com/dataset/us-census