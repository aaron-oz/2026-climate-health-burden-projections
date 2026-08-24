#!/usr/bin/env python
"""Build the per-pixel ERA5 daily-temperature uncertainty field on the CCKP grid.

Input: monthly netCDFs from fetch_era5_spread.py (3-hourly EDA ensemble spread
of 2m temperature, 0.5 deg, year 2022, lon 0..359.5, lat 90..-90).

The daily-MEAN temperature's uncertainty is not directly published; from the
3-hourly spread s_i (i = 1..8 per day) we bound it with two aggregations:
  sd_corr  = mean_i(s_i)            errors fully correlated within the day
  sd_indep = sqrt(mean_i(s_i^2)/8)  errors independent within the day
The truth lies between; IHME's era2melt.R aggregation is unknown, so both are
carried forward (handoff step A). Each is averaged over the days of the month
to a per-pixel MONTHLY climatology, then bilinearly regridded 0.5 -> 0.25 deg
onto the CCKP grid (lon -180..179.75, lat -90..90, both ascending).

Output:
  era5_sd/era5_t2m_spread_daily_clim_2022_cckp025.nc
    dims (month=12, lat=721, lon=1440); vars sd_corr, sd_indep [degC]
Run with: /var/home/aoz/data/venvs/cds/bin/python build_era5_sd.py
"""

import pathlib
import sys

import numpy as np
import xarray as xr

RAW_DIR = pathlib.Path("/var/home/aoz/data/wb-temp-attr-projections/era5_sd/raw")
OUT = pathlib.Path(
    "/var/home/aoz/data/wb-temp-attr-projections/era5_sd/"
    "era5_t2m_spread_daily_clim_2022_cckp025.nc"
)
CCKP_LON = np.arange(-180.0, 180.0, 0.25)
CCKP_LAT = np.arange(-90.0, 90.25, 0.25)


def month_climatology(path: pathlib.Path) -> xr.Dataset:
    ds = xr.open_dataset(path)
    tname = "valid_time" if "valid_time" in ds.dims else "time"
    var = [v for v in ds.data_vars if ds[v].ndim == 3][0]
    s = ds[var]
    day = ds[tname].dt.floor("D")
    sd_corr = s.groupby(day).mean(tname).mean("floor")
    sd_indep = np.sqrt((s**2).groupby(day).mean(tname) / 8.0).mean("floor")
    out = xr.Dataset({"sd_corr": sd_corr, "sd_indep": sd_indep})
    ds.close()
    return out


def regrid_to_cckp(ds: xr.Dataset) -> xr.Dataset:
    # ERA5 lon runs 0..359.5; pad with a wrapped copy of lon=0 at lon=360 so
    # bilinear interpolation is continuous across the dateline, then query the
    # CCKP longitudes mapped into [0, 360).
    ds = ds.sortby("latitude")
    pad = ds.isel(longitude=0).assign_coords(longitude=360.0)
    ds = xr.concat([ds, pad], dim="longitude")
    q = ds.interp(
        latitude=CCKP_LAT, longitude=np.mod(CCKP_LON, 360.0), method="linear"
    )
    q = q.assign_coords(longitude=CCKP_LON)
    return q.rename({"latitude": "lat", "longitude": "lon"})


def main() -> int:
    files = sorted(RAW_DIR.glob("spread_2022_*.nc"))
    if len(files) != 12:
        print(f"expected 12 monthly files, found {len(files)}", file=sys.stderr)
        return 1
    months = []
    for f in files:
        clim = regrid_to_cckp(month_climatology(f))
        clim = clim.expand_dims(month=[int(f.stem.split("_")[-1])])
        months.append(clim.astype("float32"))
        print(f"{f.name}: done", flush=True)
    out = xr.concat(months, dim="month")
    out.sd_corr.attrs.update(units="K", long_name="EDA spread of daily-mean t2m, correlated-error bound")
    out.sd_indep.attrs.update(units="K", long_name="EDA spread of daily-mean t2m, independent-error bound")
    OUT.parent.mkdir(parents=True, exist_ok=True)
    out.to_netcdf(OUT, encoding={v: {"zlib": True, "complevel": 4} for v in out.data_vars})
    print(f"wrote {OUT}")
    for v in ["sd_corr", "sd_indep"]:
        a = out[v].values
        print(f"{v}: annual-mean global field min {np.nanmin(a):.3f} "
              f"median {np.nanmedian(a):.3f} max {np.nanmax(a):.3f} K")
    return 0


if __name__ == "__main__":
    sys.exit(main())
