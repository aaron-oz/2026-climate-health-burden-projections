# Fetch ERA5 hourly 2m temperature for the UAE, 2022, from the public
# ARCO-ERA5 archive on GCS, saved in the same shape as the existing
# ~/data/era5_raw/{loc}_{year}_t2m.nc files (hourly, lat x lon box).
import gcsfs, xarray as xr, sys

fs = gcsfs.GCSFileSystem(token="anon")
stores = fs.ls("gcp-public-data-arco-era5/ar")
print("available stores:")
for s in stores:
    print(" ", s)
# the analysis-ready surface store (has 2m_temperature, 0.25 deg, hourly)
store = "gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3"
if store not in stores:
    store = "gcp-public-data-arco-era5/ar/1959-2022-full_37-1h-0p25deg-chunk-1.zarr-v2"
print("using:", store)

ds = xr.open_zarr("gs://" + store, chunks=None, storage_options={"token": "anon"})
print("t2m present:", "2m_temperature" in ds or "t2m" in ds)
var = "2m_temperature" if "2m_temperature" in ds else "t2m"
lat_asc = bool(ds.latitude[0] < ds.latitude[-1])
lat_sl = slice(22.0, 26.5) if lat_asc else slice(26.5, 22.0)
lon = ds.longitude
lon_min, lon_max = (51.0, 57.5)
if float(lon.max()) > 180:  # 0-360 grid
    sub = ds[var].sel(time=slice("2022-01-01", "2022-12-31"),
                      latitude=lat_sl, longitude=slice(lon_min, lon_max))
else:
    sub = ds[var].sel(time=slice("2022-01-01", "2022-12-31"),
                      latitude=lat_sl, longitude=slice(lon_min, lon_max))
print("subset shape:", dict(sub.sizes))
sub = sub.rename("t2m").rename({"time": "valid_time"})
out = "/var/home/aoz/data/era5_raw/156_2022_t2m.nc"
sub.to_netcdf(out)
print("wrote", out)
