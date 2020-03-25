# Convert ROMS GCM netcdf files into rasters

The ROMS files are provided by Mike Jacox. Use beyond the WRAP workshop requires permission by Mike Jacox (michael.jacox@noaa.gov)

You will need to download the Netcdf files locally. Netcdfs are contained in a folder called `2d_fields`.
```
files_long <- list.files('2d_fields/monthly', full.names = TRUE)
files_short <- list.files('2d_fields/monthly', full.names = FALSE)
```

Use the function `Create_ROMS_Rasters.R` to convert the nc files to `.grd` format used in the package.

## ROMS data 

Note: 1980-2010 are not observed values. 1980-present should not be compared to observations since the interannual variability will not match up (by design). 

The ROMS data have

* 8 covariates: sst, bottom temp, ild, chl-a surface, chl-a 50m, bottom oxygen, depth of 2.0 oxytherm, bottom depth layer (reference point)
* 3 global climate models: Hadley, IPSL, GFDL

Dimensions:

* 1452 years: 1980 - 2100 (121 years * 12 months)
* 1452 months: 1 - 12 (121 years * 12 months)
* 181 lats: 30 - 48 degrees north @ 0.1 degree resolution
* 186 lons: 115.5 - 134 degrees west @ 0.1 degree resolution

