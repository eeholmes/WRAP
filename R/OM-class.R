#' OM class
#' 
#' This is the format for all output for the OM functions. The output is a list with
#' \strong{meta} A list with elements that describe the model used.
#' \itemize{
#' \item version: Version of WRAP package used.
#' \item func: Name of function that produced the grid.
#' \item call: Exact call used to run model.
#' \item sim.seed: The random seed used for the simulation to replicate the results .Random.seed <- sim.seed before rerunning the OM function. 
#' \item entries for all the arguments to the function (function dependent)
#' \item covariates: The covariates included in the model.
#' \item grid.dimensions: c(nrow, ncol, n cells).
#' \item grid.resolution: c(x, y)
#' \item grid.extent: c(xmin, xmax, ymin, ymax)
#' \item grid.crs: grid coordinate system
#' \item grid.unit Eg. "degree" Add units for clarity. NA if grid is simulated for example a 20x20 grid.
#' \item time: c(start date, end date). For example  c(2001, 2101)
#' \item time.unit = "year" for yearly. 
#' }
#' \strong{grid} A data frame with a row for each grid square. The data frame has the following columns. Other columns can be added but at minimum the following are provided:
#' \itemize{
#' \item lat
#' \item lon
#' \item year
#' \item pres Presence/absence as 0/1
#' \item suitability
#' \item columns for the covariates (one column for each)
#' \item covariates: The covariates included in the model. The number of covariates will vary by model.
#' \item abundance
#' }
#' 
#' @docType class
#' @name OM_class
NULL