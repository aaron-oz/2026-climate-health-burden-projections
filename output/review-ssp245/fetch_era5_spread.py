#!/usr/bin/env python
"""Fetch ERA5 ensemble spread (EDA spread) for 2m temperature, 3-hourly, 2022.

Downloads the published analysis-uncertainty field IHME's era2melt.R used as
the daily-temperature noise sd: dataset reanalysis-era5-single-levels,
product_type ensemble_spread, variable 2m_temperature. Native EDA resolution
is 0.5 deg and the spread is 3-hourly (00:00..21:00). One netCDF per month,
resumable (existing files are skipped), up to 3 requests in flight.

Run with the cds venv: /var/home/aoz/data/venvs/cds/bin/python
Credentials: ~/.cdsapirc
Output: /var/home/aoz/data/wb-temp-attr-projections/era5_sd/raw/spread_2022_MM.nc
"""

import concurrent.futures as cf
import pathlib
import sys

import cdsapi

YEAR = "2022"
OUT_DIR = pathlib.Path(
    "/var/home/aoz/data/wb-temp-attr-projections/era5_sd/raw"
)
TIMES = [f"{h:02d}:00" for h in range(0, 24, 3)]
DAYS = [f"{d:02d}" for d in range(1, 32)]
MAX_IN_FLIGHT = 3


def fetch_month(month: str) -> str:
    out = OUT_DIR / f"spread_{YEAR}_{month}.nc"
    if out.exists() and out.stat().st_size > 0:
        return f"{out.name}: exists, skipped"
    client = cdsapi.Client(quiet=True)
    tmp = out.with_suffix(".nc.part")
    client.retrieve(
        "reanalysis-era5-single-levels",
        {
            "product_type": ["ensemble_spread"],
            "variable": ["2m_temperature"],
            "year": [YEAR],
            "month": [month],
            "day": DAYS,
            "time": TIMES,
            "grid": [0.5, 0.5],
            "data_format": "netcdf",
            "download_format": "unarchived",
        },
        str(tmp),
    )
    tmp.rename(out)
    return f"{out.name}: downloaded {out.stat().st_size / 1e6:.1f} MB"


def main() -> int:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    months = [f"{m:02d}" for m in range(1, 13)]
    failures = 0
    with cf.ThreadPoolExecutor(max_workers=MAX_IN_FLIGHT) as pool:
        futures = {pool.submit(fetch_month, m): m for m in months}
        for fut in cf.as_completed(futures):
            month = futures[fut]
            try:
                print(fut.result(), flush=True)
            except Exception as exc:  # noqa: BLE001
                failures += 1
                print(f"month {month}: FAILED: {exc}", flush=True)
    print(f"done, {failures} failures", flush=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
