#' Simulated World ROMS
#' 
#' Function uses covariate data from ROMS to generate species distribution and abundance.
#' 
#' The ROMS data are assumed to be in the working directory in a folder called 'Rasters_2d_monthly/gfdl/'. SST data are in a subfolder called 'sst_monthly'. If the ROMS data folder is different, you can pass that in via the 'dir' argument. However the ROMS data folder must have subfolders 'gfdl/sst_monthly' so that 'paste0(dir,"/gfdl/sst_monthly")' will find the SST ROMS data.
#' Download ROMS data from here: https://www.dropbox.com/sh/aaezimxwq3glwdy/AABHmZbmfjVJM7R4jcHCi4c9a?dl=0
#' Caution: this function uses the downscaled GCMs to simulate species distrubtion. It has slight differences to the other SimulateWorld_function that 'randomly' generates environmental data. Remember: 1980-2010 are not observed data (by design)
#' 
#' @param PA_shape specifies how enviro suitability determines species presence-absence. takes values of "logistic" (SB original), "logistic_prev" (JS, reduces knife-edge), "linear" (JS, reduces knife edge, encourages more absences)
#' @param abund_enviro specifies abundance if present, can be "lnorm_low" (SB original), "lnorm_high" (EW), or "poisson" (JS, increases abundance range)
#' @param covariates A vector with "sst" and/or "chla". One or both can be listed.
#' @param dir (optional) The path to the directory where the folder 'gfdl' is.
#' @param roms.years The years for the ROMS data. The data files use the year and month. Default is 1980:2100.
#' @param n.samples The number of samples to take from the ROMS layers each year. Default is 400.
#' 
#' @return Returns an object of class \code{\link[=OMclass]{OM}}, which is a list with "grid" and "meta". "meta" has all the information about the simulation including all the parameters passed into the function.
#' 
#' @examples
#' \dontrun{
#' test <- SimulateWorld_ROMS(PA_shape="logistic", abund_enviro="lnorm_low")
#' head(test)
#' tail(test)
#' plot(aggregate(suitability~year, data=test, FUN=mean),type='b')
#' plot(aggregate(pres~year, data=test, FUN=sum),type='b')
#' plot(aggregate(abundance~year, data=test, FUN=sum),type='b')
#' plot(aggregate(sst~year, data=test, FUN=mean),type='b')
#' }
#' 
#' @export
SimulateWorld_ROMS <- function(
  PA_shape=c("logistic", "logistic_prev", "linear"), 
  abund_enviro=c("lnorm_low", "lnorm_high", "poisson"), 
  covariates=c("sst"),
  dir=file.path(here::here(),"Rasters_2d_monthly"),
  roms.years=1980:2100,
  n.samples=400){
  PA_shape <- match.arg(PA_shape)
  abund_enviro <- match.arg(abund_enviro)
  covariates <- match.arg(covariates, several.ok=TRUE)
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
  

  #----Create output file----
  #Needs to be modified as variables are added. Starting with sst
  #Assuming 400 'samples' are taken each year, from 1980-2100
  output <- as.data.frame(matrix(NA, nrow=n.samples*n.year, ncol=6))
  colnames(output) <- c("lon","lat","year","pres","suitability","sst")
  
  #----Load in rasters----
  # sst_dr was created at top

  if("sst" %in% covariates) files_sst <- list.files(sst_dr, full.names = TRUE, pattern=".grd") #should be 1452 files
  if("chla" %in% covariates) files_chl <- list.files(chl_dr, full.names = TRUE, pattern=".grd")
  months <- rep(1:12, n.year) 
  years <- rep(roms.years, each=12)
  august_indexes <- which(months==8) #picking last month in summer

  #loop through each year
  for (y in august_indexes){
    print(paste0("Creating environmental simulation for Year ",years[y]))
    if("sst" %in% covariates) sst <- raster::raster(files_sst[y])
    if("chla" %in% covariates){
      chla <- raster::raster(files_chl[y])
      chla <- log(chla)
    }
    #plot(sst)
 
    #----Use Virtual Species to assign response curve----
    if(identical(covariates, "sst")){
      envir_stack <- raster::stack(sst) #must be in raster stack format
      names(envir_stack) <- c('sst')
    }
    if(identical(covariates, "chla")){
      envir_stack <- raster::stack(chla) #must be in raster stack format
      names(envir_stack) <- c('chla')
    }
    if(length(covariates)==2 && "sst" %in% covariates && "chla" %in% covariates){
      envir_stack <- raster::stack(sst, chla)
      names(envir_stack) <- c('sst', 'chla')
    }
    
    #----assign response curve with mean at 4 degrees----
    parameters <- virtualspecies::formatFunctions(sst = c(fun="dnorm",mean=15,sd=4))
    
    #----convert temperature raster to species suitability----
    envirosuitability <- virtualspecies::generateSpFromFun(
      envir_stack,
      parameters=parameters, 
      rescale = FALSE,
      rescale.each.response = FALSE)
    #rescale
    ref_max <- stats::dnorm(parameters$sst$args[1], mean=parameters$sst$args[1], sd=parameters$sst$args[2]) #JS/BM: potential maximum suitability based on optimum temperature
    envirosuitability$suitab.raster <- (1/ref_max)*envirosuitability$suitab.raster #JS/BM: rescaling suitability, so the max suitbaility is only when optimum temp is encountered
    
    #Plot suitability and response curve
    # plot(envirosuitability$suitab.raster) #plot habitat suitability
    # virtualspecies::plotResponse(envirosuitability) #plot response curves
    
    #----Convert suitability to Presence-Absence----
    if (PA_shape == "logistic") {
      #SB: specifies alpha and beta of logistic - creates almost knife-edge absence -> presence
      suitability_PA <- virtualspecies::convertToPA(
        envirosuitability, 
        PA.method = "probability", 
        beta = 0.5, 
        alpha = -0.05, 
        species.prevalence = NULL, 
        plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    if (PA_shape == "logistic_prev") {
      #JS: relaxes logistic a little bit, by specifing reduced prevalence and fitting beta (test diff prevalence values, but 0.5 seems realistic)
      suitability_PA <- virtualspecies::convertToPA(
        envirosuitability, 
        PA.method = "probability", 
        beta = "random",
        alpha = -0.3, 
        species.prevalence = 0.5, 
        plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    if (PA_shape == "linear") {
      #JS: relaxes knife-edge absence -> presence further; also specifies prevalence and fits 'b'
      suitability_PA <- virtualspecies::convertToPA(
        envirosuitability, 
        PA.method = "probability",
        prob.method = "linear", 
        a = NULL,
        b = NULL, 
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
    df <- cbind(as.data.frame(round(presence.points$sample.points$x,1)),as.data.frame(round(presence.points$sample.points$y,1)))
    colnames(df) <- c("x","y")
    
    #----Extract data for each year----
    print("Extracting suitability")
      ei <- n.samples*which(august_indexes==y) #end location in output grid to index to
      se <- ei - (n.samples-1) #start location in output grid to index to
      output$lat[se:ei] <- df$y
      output$lon[se:ei] <- df$x
      output$year[se:ei] <- rep(years[y], n.samples)
      output$pres[se:ei] <-  presence.points$sample.points$Real
      output$suitability[se:ei] <- raster::extract(envirosuitability$suitab.raster, y= df)  #extract points from suitability file
      output$sst[se:ei] <-  raster::extract(sst, y= df)  #extract points from suitability file
  }
  
  #----Create abundance as a function of the environment----
  if (abund_enviro == "lnorm_low") {
    # SB: values in Ecography paper. I think initially they were based on flounder in EBS but not sure if I edited them
    output$abundance <- ifelse(output$pres==1, stats::rlnorm(n.samples*n.year,2,0.1)*output$suitability,0)
  }
  if (abund_enviro == "lnorm_high") {
    # EW: I'm cranking up the rlnorm parameters to make it more comparable to wc trawl survey estimates -- these new ones based on arrowtooth
    # SB: rlnorm parameters (6,1) were too large for estimation model. GAMs had explained deviance <10%. Other suggestions?
    output$abundance <- ifelse(output$pres==1, stats::rlnorm(n.samples*n.year,6,1)*output$suitability,0)
  }
  if (abund_enviro == "poisson") {
    # JS: sample from a Poisson distbn, with suitability proportional to mean (slower, bc it re-samples distbn for each observation)
    maxN <- 50  #max mean abundance at highest suitability
    output$abundance <- ifelse(output$pres==1, stats::rpois(n.samples*n.year, lambda=output$suitability*maxN),0)
  } 
  
  # meta data is auto generated. You shouldn't need to edit
  meta=list(
    version= utils::packageVersion("WRAP"),
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
  meta$dir <- c(sst_dr, chl_dr) 
  
  obj <- list(meta=meta, grid=output)
  
  class(obj) <- "OM"
  
  return(obj)
}
