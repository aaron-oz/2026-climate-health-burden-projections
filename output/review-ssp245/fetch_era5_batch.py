# Fetch ERA5 hourly 2m temperature (2022) for the tropical low-cluster
# countries from the public ARCO-ERA5 archive, same file format as the
# existing ~/data/era5_raw/{loc}_{year}_t2m.nc extracts.
import os, xarray as xr

STORE = "gs://gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3"
OUT = "/var/home/aoz/data/era5_raw"
# loc_id: (lon_min, lon_max, lat_min, lat_max), generous bounding boxes
BOXES = {
    114: (-75.0, -71.0, 17.5, 20.5),   # Haiti
    11:  (94.0, 141.5, -11.5, 6.5),    # Indonesia
    13:  (99.0, 119.5, 0.5, 7.5),      # Malaysia
    190: (29.5, 35.5, -1.5, 4.5),      # Uganda
    189: (29.0, 41.0, -12.0, -0.5),    # Tanzania
    129: (-89.5, -83.0, 12.5, 16.5),   # Honduras
    131: (-87.8, -82.5, 10.5, 15.2),   # Nicaragua
}

ds = xr.open_zarr(STORE, chunks=None, storage_options={"token": "anon"})
lat_asc = bool(ds.latitude[0] < ds.latitude[-1])
lon0360 = float(ds.longitude.max()) > 180

for loc, (x0, x1, y0, y1) in BOXES.items():
    out = f"{OUT}/{loc}_2022_t2m.nc"
    if os.path.exists(out):
        print(loc, "exists, skipping"); continue
    lx0, lx1 = (x0 % 360, x1 % 360) if lon0360 else (x0, x1)
    lat_sl = slice(y0, y1) if lat_asc else slice(y1, y0)
    sub = ds["2m_temperature"].sel(time=slice("2022-01-01", "2022-12-31"),
                                   latitude=lat_sl, longitude=slice(lx0, lx1))
    print(loc, "shape", dict(sub.sizes), flush=True)
    sub.rename("t2m").rename({"time": "valid_time"}).to_netcdf(out)
    print(loc, "written", flush=True)
print("all done")
