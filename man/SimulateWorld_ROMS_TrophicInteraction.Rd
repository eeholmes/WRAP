% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimulatedWorld_ROMS_TrophicInteraction.R
\name{SimulateWorld_ROMS_TrophicInteraction}
\alias{SimulateWorld_ROMS_TrophicInteraction}
\title{Simulated World ROMS Trophic Interactions}
\usage{
SimulateWorld_ROMS_TrophicInteraction(
  PA_shape = c("logistic", "logistic_prev", "linear"),
  abund_enviro = c("lnorm_low", "lnorm_high", "poisson"),
  dir = file.path(here::here(), "Rasters_2d_monthly"),
  roms.years = 1980:2100,
  n.samples = 400,
  maxN = 50,
  verbose = FALSE
)
}
\arguments{
\item{PA_shape}{specifies how enviro suitability determines species B presence-absence. takes values of "logistic" (SB original), "logistic_prev" (JS, reduces knife-edge), "linear" (JS, reduces knife edge, encourages more absences)}

\item{abund_enviro}{specifies abundance for species B (if present). Can be "lnorm_low" (SB original), "lnorm_high" (EW), or "poisson" (JS, increases abundance range)}

\item{dir}{(optional) The path to the directory where the folder 'gfdl' is.}

\item{roms.years}{The years for the ROMS data. The data files use the year and month. Default is 1980:2100.}

\item{n.samples}{The number of samples to take from the ROMS layers each year. Default is 400.}

\item{maxN}{max mean abundance at highest suitability}

\item{verbose}{FALSE means print minimal progress, TRUE means print verbose progress output}
}
\value{
Returns the grid for species B. This is an object of class \code{\link[=OM_class]{OM}}, which is a list with "grid" and "meta". "meta" has all the information about the simulation including all the parameters passed into the function.
}
\description{
This code replicates SimulateWorld_ROMS_function, but adds in a trophic interaction, where:
\itemize{
 \item Species A: distribution and abundance driven by SST and Chl-a. Suitability function is logistic.
 \item Species B: distribution and abundance drivn by SST and Species A
 \item EM for Species B: only have chl-a and temp as covariates. 
}
Note only using GFDL for now
}
\details{
The ROMS data are assumed to be in the working directory in a folder called
 'Rasters_2d_monthly' with subfolder 'gfdl'. If the ROMS data folder is different,
  you can pass that in via the 'dir' argument. However the ROMS data folder must have subfolders 'gfdl/sst_monthly' and 'gfdl/chl_surface' so that 
   it will find the SST and CHL ROMS data.
}
\examples{
\dontrun{
test <- SimulateWorld_ROMS_TrophicInteraction(PA_shape="logistic", abund_enviro="lnorm_low")
}

}
