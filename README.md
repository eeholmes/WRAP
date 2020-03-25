# WRAP_Location_CaseStudy Package

To install
```
library(devtools)
install_github("eeholmes/WRAP", dependencies=TRUE)
```

To see how it works
```
library(WRAP)
?SimulateWorld
SimulateWorld()
```

How packages work (simplified):

* Code (functions) are in R folder
* DESCRIPTION file says what libraries are used. When people install, the packages (dependencies) can be automatically installed too.
* NAMESPACE is what files are exported to users.
* Documentation is at the top in Roxygen format. The documentation is built when you build the package (see instruction below for that). Users can then use ?SimulateWorld say, to see what the function does.

That's pretty much it.  Package format standardizes documentation but also ensures that your package has no errors that would prevent others from using it. That checking happens (automatically) in the building step.


To build

* When you create a project from the folder, RStudio will show a Build tab. Click 'Install and Restart' to rebuild. You only do this if you make changes to the code. Collaborators who are not modifying code just install from github.
* Before building however, Click Tools > Project Options > Build Tools and click the 'Generate Documentation with Roxygen'. Click 'Configure' and check 'Build and Reload' check box.


**All this is work and code by Stephanie and collaborators.**

Simple simulation code to prepare for the WRAP Workshop March 23-25, 2020

OPERATING MODELS:

1. SimulateWorld_Function.R: this is the function that generates temperature dependent species distribution and abundance
2. SimulateWorld_ROMS_Function.R: this function uses temperature data from ROMS to generate species distribution and abundance
3. SimulateWorld_ROMS_TrophicInteraction_Function.R: this function uses temperature and chl-a to build suitability for two species, and generate distribution and abundance for one species. 

ESTIMATION MODELS:

1. ModelComparison.R: this code uses the function above to generate data, then builds an example GAM and BRT model, and makes predictions into the future 2020-2080. Couples with functions 1 and 2 above. 
2. ModelComparison_TrophicInteractions.R: this code uses the ROMS_TophicInteraction function above to generate data, then builds an example GAM and BRT model, and makes predictions into the future 2020-2080. Couples with functions 3 above. 

ROMS:
1. Create_ROMS_Rasters.R: code to turn downscaled ROMS netcdf files into rasters. Most users do not need to use this code. 
