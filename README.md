# WRAP Package

## To install
```
library(devtools)
install_github("eeholmes/WRAP", dependencies=TRUE)
```

## Overview and help files

To see all the functions with links to their help files, use

```
library(WRAP)
help(package="WRAP")
```

## Vignettes

There are detailed vignettes that show how to fit models and make a variety of plots. To open links to the vignettes in a browser use:
```
browseVignettes(package="WRAP")
```

To find the Rmd files for these vignettes, look in the `vignettes` folder.

## Functions

The simulation functions are

* `SimulateWorld()` This is the function that generates temperature dependent species distribution and abundance
* `SimulateWorld_ROMS()`  This function uses temperature data from ROMS to generate species distribution and abundance.
* `SimulateWorld_ROMS_TrophicInteraction()` This function uses temperature and chl-a to build suitability for two species, and generate distribution and abundance for one species. 

To run the simulations using ROMS data, you need the ROMS data. It is assumed that you have these data in a folder called "Rasters_2d_monthly" in your working directory. That can be changed. See `?SimulateWorld_ROMS`.  
The simulation functions return a `OM` object. There is a

* `predict` method for `OM` objects. It returns a data frame with hindcast and forecast predicitons or you can pass in newdata.
* `print` method for `OM` objects. Type `sim` and basic info about the simulation is shown. Full information is in `sim$meta`.

The functions to fit a SDM from a grid of covariates and abundances is

* `gam_sdm`
* `brt_sdm`
* `mlp_sdm`

## ROMS data

The rasters in the package were created from netcdf files using the script `Create_ROMS_Rasters.R` in the `doc\ROMSdata` folder. Most users do not need to use this code. 

