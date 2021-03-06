#' Simulated World ROMS Trophic Interactions
#' 
#' This code replicates SimulateWorld_ROMS_function, but adds in a trophic interaction, where:
##' \itemize{
##'  \item Species A: distribution and abundance driven by SST and Chl-a. Suitability function is logistic.
##'  \item Species B: distribution and abundance drivn by SST and Species A
##'  \item EM for Species B: only have chl-a and temp as covariates. 
#' }
#' Note only using GFDL for now
#' 
#' The ROMS data are assumed to be in the working directory in a folder called
#'  'Rasters_2d_monthly' with subfolder 'gfdl'. If the ROMS data folder is different,
#'   you can pass that in via the 'dir' argument. However the ROMS data folder must have subfolders 'gfdl/sst_monthly' and 'gfdl/chl_surface' so that 
#'    it will find the SST and CHL ROMS data.
#'    
#' @param PA_shape specifies how enviro suitability determines species B presence-absence. takes values of "logistic" (SB original), "logistic_prev" (JS, reduces knife-edge), "linear" (JS, reduces knife edge, encourages more absences)
#' @param abund_enviro specifies abundance for species B (if present). Can be "lnorm_low" (SB original), "lnorm_high" (EW), or "poisson" (JS, increases abundance range)
#' @param dir (optional) The path to the directory where the folder 'gfdl' is.
#' @param roms.years The years for the ROMS data. The data files use the year and month. Default is 1980:2100.
#' @param n.samples The number of samples to take from the ROMS layers each year. Default is 400.
#' @param maxN max mean abundance at highest suitability
#' @param verbose FALSE means print minimal progress, TRUE means print verbose progress output
#' 
#' @return Returns the grid for species B. This is an object of class \code{\link[=OM_class]{OM}}, which is a list with "grid" and "meta". "meta" has all the information about the simulation including all the parameters passed into the function.
#' 
#' @examples
#' \dontrun{
#' test <- SimulateWorld_ROMS_TrophicInteraction(PA_shape="logistic", abund_enviro="lnorm_low")
#' }
#' 
#' @export
#' 
SimulateWorld_ROMS_TrophicInteraction <- function(
  PA_shape=c("logistic", "logistic_prev", "linear"), 
  abund_enviro=c("lnorm_low", "lnorm_high", "poisson"),
  dir=file.path(here::here(),"Rasters_2d_monthly"),
  roms.years=1980:2100,
  n.samples=400,
  maxN=50,
  verbose=FALSE){
  PA_shape <- match.arg(PA_shape)
  abund_enviro <- match.arg(abund_enviro)
  if(!dir.exists(dir))
    stop(paste("This function requires that dir points to the ROMSdata.\nCurrently dir is pointing to", dir, "\n"))
  # save all the inputs for meta data
  fun.args <- as.list(environment())
  
  # Within dir, the sst data are here 
  # Use file.path not paste0 to create platform specific file paths
  sst_dr <- file.path(dir, "gfdl", "sst_monthly")
  chl_dr <- file.path(dir, "gfdl", "chl_surface")
  
  # Record the seed used for simulation
  sim.seed <- .Random.seed
  # number of ROMS years
  n.year <- length(roms.years)
  
  # #----Create output file----
  #----Create output file----
  output <- as.data.frame(matrix(NA, nrow=n.samples*n.year, ncol=7))
  colnames(output) <- c("lon","lat","year","pres","suitability", "sst", "chla")

  #----Load in rasters----
  # sst_dr and chl_dr were created at top
  
  files_sst <- list.files(sst_dr, full.names = TRUE, pattern=".grd") #should be 1452 files
  files_chl <- list.files(chl_dr, full.names = TRUE, pattern=".grd") #should be 1452 files
  months <- rep(1:12, n.year) 
  years <- rep(roms.years, each=12)
  august_indexes <- which(months==8) #picking last month in summer
  
  #loop through each year
  for (y in august_indexes){
    if(verbose) print(paste0("Creating environmental simulation for Year ",years[y]))
    
    sst <- raster::raster(files_sst[y])
    chla <- raster::raster(files_chl[y])
    chla <- log(chla)

    #----SPECIES A: assign response curve----
    #Species A: likes high chla and medium temps
    
    #Stack rasters
    spA_stack <- raster::stack(sst, chla)
    names(spA_stack) <- c('sst', 'chla')
    #Assign preferences
    spA_parameters <- virtualspecies::formatFunctions(
      sst = c(fun="dnorm", mean=12, sd=3),
      chla = c(fun="dnorm",mean=1.6,sd=9))
    spA_suitability <- virtualspecies::generateSpFromFun(
      spA_stack,
      parameters=spA_parameters, 
      rescale = FALSE,
      rescale.each.response = FALSE) #Important: make sure rescaling is false. Doesn't work well in the 'for' loop. 
    # plot(spA_suitability$suitab.raster) #plot habitat suitability
    # virtualspecies::plotResponse(spA_suitability) #plot response curves
    
    #manually rescale
    ref_max_sst <- stats::dnorm(spA_parameters$sst$args[1], mean=spA_parameters$sst$args[1], sd=spA_parameters$sst$args[2]) #JS/BM: potential maximum suitability based on optimum temperature
    ref_max_chl <- stats::dnorm(spA_parameters$chla$args[1], mean=spA_parameters$chla$args[1], sd=spA_parameters$chla$args[2]) 
    ref_max <- ref_max_sst * ref_max_chl #simple multiplication of layers. 
    spA_suitability$suitab.raster <- (1/ref_max)*spA_suitability$suitab.raster #JS/BM: rescaling suitability, so the max suitbaility is only when optimum is encountered
    # plot(spA_suitability$suitab.raster) #plot habitat suitability
    # virtualspecies::plotResponse(spA_suitability) #plot response curves
    
    
    #----SPECIES B: assign response curve----
    #Species B: likes to eat Species A, and warmer temperatues
    
    #Stack rasters
    spB_stack <- raster::stack(sst, spA_suitability$suitab.raster)
    names(spB_stack) <- c('sst', 'spA')
    
    #Assign preferences
    spB_parameters <- virtualspecies::formatFunctions(
      sst = c(fun="dnorm",mean=15,sd=4),
      spA = c(fun="dnorm",mean=0.8,sd=1))
    spB_suitability <- virtualspecies::generateSpFromFun(
      spB_stack,
      parameters=spB_parameters, 
      rescale = FALSE,
      rescale.each.response = FALSE)
    # plot(spB_suitability$suitab.raster) #plot habitat suitability
    # virtualspecies::plotResponse(spB_suitability) #plot response curves
    
    #manually rescale
    ref_max_sst <- stats::dnorm(spB_parameters$sst$args[1], mean=spB_parameters$sst$args[1], sd=spB_parameters$sst$args[2]) #JS/BM: potential maximum suitability based on optimum temperature
    ref_max_spA <- stats::dnorm(spB_parameters$spA$args[1], mean=spB_parameters$spA$args[1], sd=spB_parameters$spA$args[2])
    # ref_max <- ref_max_sst * 0.5
    spB_suitability$suitab.raster <- (1/ref_max)*spB_suitability$suitab.raster #JS/BM: rescaling suitability, so the max suitbaility is only when optimum temp is encountered
    # plot(spB_suitability$suitab.raster) #plot habitat suitability
    # virtualspecies::plotResponse(spB_suitability) #plot response curves
    
    #----Convert suitability to Presence-Absence----
    if (PA_shape == "logistic") {
      #SB: specifies alpha and beta of logistic - creates almost knife-edge absence -> presence
      suitability_PA <- virtualspecies::convertToPA(
        spB_suitability, 
        PA.method = "probability", 
        beta = 0.5, alpha = -0.05, 
        species.prevalence = NULL, 
        plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    if (PA_shape == "logistic_prev") {
      #JS: relaxes logistic a little bit, by specifing reduced prevalence and fitting beta (test diff prevalence values, but 0.5 seems realistic)
      suitability_PA <- virtualspecies::convertToPA(
        spB_suitability, 
        PA.method = "probability", 
        beta = "random", alpha = -0.3, 
        species.prevalence = 0.5, plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    if (PA_shape == "linear") {
      #JS: relaxes knife-edge absence -> presence further; also specifies prevalence and fits 'b'
      suitability_PA <- virtualspecies::convertToPA(
        spB_suitability, 
        PA.method = "probability",
        prob.method = "linear", 
        a = NULL, b = NULL, 
        species.prevalence = 0.8, 
        plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    # plot(suitability_PA$pa.raster)
    
    #-----Sample Presences and Absences-----
    presence.points <- virtualspecies::sampleOccurrences(
      suitability_PA,
      n = n.samples,
      type = "presence-absence",
      detection.probability = 1,
      error.probability=0, 
      plot = FALSE)
    df <- cbind(as.data.frame(round(presence.points$sample.points$x,1)),
                as.data.frame(round(presence.points$sample.points$y,1)))
    colnames(df) <- c("x","y")
    
    #----Extract data for each year----
    if(verbose) print("Extracting suitability")
    ei <- n.samples*which(august_indexes==y) #end location in output grid to index to
    se <- ei - (n.samples-1) #start location in output grid to index to
    output$lat[se:ei] <- df$y
    output$lon[se:ei] <- df$x
    output$year[se:ei] <- rep(years[y], n.samples)
    output$pres[se:ei] <-  presence.points$sample.points$Real
    output$suitability[se:ei] <- raster::extract(spB_suitability$suitab.raster, y= df)  #extract points from suitability file
    output$sst[se:ei] <-  raster::extract(sst, y= df)  #extract points from suitability file
    output$chla[se:ei] <-  raster::extract(chla, y= df)
    
  }
  
  #----Create abundance as a function of the environment----
  if (abund_enviro == "lnorm_low") {
    # SB: values in Ecography paper. I think initially they were based on flounder in EBS but not sure if I edited them
    output$abundance <- ifelse(output$pres==1, stats::rlnorm(nrow(output),2,0.1)*output$suitability,0)
  }
  if (abund_enviro == "lnorm_high") {
    # EW: I'm cranking up the rlnorm parameters to make it more comparable to wc trawl survey estimates -- these new ones based on arrowtooth
    # SB: rlnorm parameters (6,1) were too large for estimation model. GAMs had explained deviance <10%. Other suggestions?
    output$abundance <- ifelse(output$pres==1, stats::rlnorm(nrow(output),6,1)*output$suitability,0)
  }
  if (abund_enviro == "poisson") {
    # JS: sample from a Poisson distbn, with suitability proportional to mean (slower, bc it re-samples distbn for each observation)
    #maxN is mean abundance at highest suitability
    output$abundance <- ifelse(output$pres==1, stats::rpois(nrow(output),lambda=output$suitability*maxN),0)
  } 
  
  # meta data is auto generated. You shouldn't need to edit
  meta=list(
    version=utils::packageVersion("WRAP"),
    func=deparse(as.list(match.call())[[1]]),
    call=deparse( sys.call() ),
    sim.seed=sim.seed,
    grid.dimensions=c(attr(sst, "nrows"), attr(sst, "ncols"), attr(sst, "nrows")*attr(sst, "ncols")),
    grid.resolution=c((attr(sst, "extent")@xmax-attr(sst, "extent")@xmin)/attr(sst, "ncols"), (attr(sst, "extent")@ymax-attr(sst, "extent")@ymin)/attr(sst, "nrows")),
    grid.extent=c(attr(sst, "extent")@xmin, attr(sst, "extent")@xmax, attr(sst, "extent")@ymin, attr(sst, "extent")@ymax),
    grid.crs=attr(sst, "crs"),
    grid.unit="degree",
    time=c(min(output$year), max(output$year)),
    time.unit="year"
  )
  meta <- c(meta, fun.args) #add the function arguments
  meta$dir <- c(sst_dr, chl_dr) #update dir to full sst dir  
  
  obj <- list(meta=meta, grid=output)
  
  class(obj) <- "OM"
  
  return(obj)
}
