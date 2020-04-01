# WRAP_Location_CaseStudy Package

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

There are detailed vignettes that show how to fit models and make a variety of plot.

To see the list type
```
vignette(package="WRAP")
```

This will open a vignette in the help window.

```
vignette("Run_OM", package="WRAP")
vignette("Fitting_SDMs", package="WRAP")
```

To find the Rmd files for these vignettes, look in the `vignettes` folder.

"Fitting SDMs" is a more involved example with many plots comparing GAM, BRT, MLP models. 

## Functions

The simulation functions are

* `SimulateWorld()` Use `?SimulateWorld` to read documentation. This is the function that generates temperature dependent species distribution and abundance
* `SimulateWorld_ROMS()`  This function uses temperature data from ROMS to generate species distribution and abundance.
* `SimulateWorld_ROMS_TrophicInteraction()` This function uses temperature and chl-a to build suitability for two species, and generate distribution and abundance for one species. 

The simulation functions return a `OM` object. There is a

* `predict` method for `OM` objects. Use `predict(sim)` where `sim` is returned by on of the simulation functions.
* `print` method for `OM` objects. Type `sim` and basic info about the simulation is shown. Full infor is in `sim$meta`.

The fitting functions are

* `gam_sdm` Use `?gam_sdm` to read documentation
* `brt_sdm`
* `mlp_sdm`


To run the simulations using ROMS data, you need the ROMS data. It is assumed that you have these data in a folder called "Rasters_2d_monthly" in your working directory. That can be changed. See `?SimulateWorld_ROMS`.  The rasters in the package were created from netcdf files using the script `Create_ROMS_Rasters.R` in the `doc\ROMSdata` folder. Most users do not need to use this code. 

## Package Notes

### Workflow

Best to have a separate folder (and RStudio project) for using the package (running simulations, making plots etc) and then a different one for the package. Let's call these
* Paper: the code that using the package, to do analyses. This would have the Rmd of the paper main text, tables, figures, appendices, etc. Those Rmds, call `library(WRAP)` to get access to the functions as needed.
* Package: the code for generating OMs, functions to make plots, etc.

Have both projects open in RStudio (2 windows).

* When you change the package code, use Build tab to install and restart. Push changes to GitHub if ready. Change the version number in DESCRIPTION when you have changes. That way everyone can keep track that changes have happened to the package.
* To reload the package in the paper project (window), do 
  * Session > Restart R to reload package or
  * `detach(package:WRAP, unload=TRUE); library(WRAP)`
  
This workflow is a bit of a hassle when you are actively working on the paper and package at the same time, but it makes it easier for others to use the OM package and keeps the package code clean.

### How packages work (simplified):

* Code (functions) are in R folder
* DESCRIPTION file says what libraries are used. When people install, the packages (dependencies) can be automatically installed too.
* NAMESPACE is what files are exported to users.
* Documentation is at the top in Roxygen format. The documentation is built when you build the package (see instruction below for that). Users can then use ?SimulateWorld say, to see what the function does.

That's pretty much it.  Package format standardizes documentation but also ensures that your package has no errors that would prevent others from using it. That checking happens (automatically) in the building step.

### To build the package

* When you create a project from the folder with the package files, RStudio will show a Build tab. Click 'Install and Restart' to rebuild. You only do this if you make changes to the code. Collaborators who are not modifying code just install from github.
* Before building however, Click Tools > Project Options > Build Tools and click the 'Generate Documentation with Roxygen'. Click 'Configure' and check 'Build and Reload' check box.

### Vignettes

If a new vignette is added to vignettes folder run this. `build_vignettes` is putting the `doc` folder at the base level instead of keeping in `inst` because normally you make the vignette files when building but I don't want to do that since they take a long time.

```
devtools::build_vignettes()
library(ff)
file.move("doc",file.path("inst", "doc"))
file.remove(file.path("Meta", "vignette.rds"))
file.remove("Meta")
```

Then rebuild.  After that this will show the vignettes:

```
vignette(package="WRAP")
```

And this will open a vignette in the help window.

```
vignette("Run_OM", package="WRAP")
```

