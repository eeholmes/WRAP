#' Simulated World basic
#' 
#' Function to simulate species distribution and abundance with a linearly increasing temperature. The argument \code{temp_diff} specifies the range of temperature for the beginning and ending years (year 1 and year 100).
#' 
#' @param temp_diff specifies min and max temps at year 1 and year 100 (e.g. temp_diff=c(1,3,5,7) means year 1 varies from 1-3C and year 100 from 5-7C)
#' @param temp_spatial specifies whether we have "simple" linear temp distbn (SB) or added "matern" variation (EW)
#' @param PA_shape specifies how enviro suitability determines species presence-absence. takes values of "logistic" (SB original), "logistic_prev" (JS, reduces knife-edge), "linear" (JS, reduces knife edge, encourages more absences, currently throws errors)
#' @param abund_enviro specifies abundance if present, can be "lnorm_low" (SB original), "lnorm_high" (EW), or "poisson" (JS, increases abundance range)
#' @param covariates Currently only "temp" is allowed in this function
#' @param grid.size Default is a 20x20 grid
#' @param n.year Number of years to simulate. Default is 100.
#' @param start.year For showing results choose the start year.
#' @param response.curve The response curve passed to `virtualspecies::formatFunctions`. All covariates in `covariates` argument must have a response curve specified.
#' @param verbose FALSE means print minimal progress, TRUE means print verbose progress output
#' 
#' @return Returns an object of class \code{\link[=OMclass]{OM}}, which is a list with "grid" and "meta". "meta" has all the information about the simulation including all the parameters passed into the function.
#' 
#' @examples
#' # use defaults
#' data <- SimulateWorld()$grid
#' # plot time-series of total catch in observed years
#' plot(aggregate(abundance~year,data[data$year<=2010,], FUN="sum"),type="l",ylab="Abundance")
#' # plot time-series of total catch in forecast years
#' plot(aggregate(abundance~year,data[data$year>2010,], FUN="sum"),type="l", ylab="Abundance")
#' 
#' @export
SimulateWorld <- function(
  temp_diff=c(1,3,5,7), 
  temp_spatial=c("simple", "matern"), 
  PA_shape=c("logistic", "logistic_prev", "linear"), 
  abund_enviro=c("lnorm_low", "lnorm_high", "poisson"),
  covariates=c("temp"),
  grid.size=20, n.year=100, start.year=2000,
  response.curve=list(temp=c(fun="dnorm",mean=4,sd=1)),
  verbose=FALSE){
  
  temp_spatial <- match.arg(temp_spatial)
  PA_shape <- match.arg(PA_shape)
  abund_enviro <- match.arg(abund_enviro)
  covariates <- match.arg(covariates, several.ok=TRUE)
  
  if(!all(covariates %in% names(response.curve)))
    stop("All the covariates must have a response curve specified.")
  # save all the inputs for meta data
  fun.args <- as.list(environment())
  # Record the seed used for simulation
  sim.seed <- .Random.seed
  

    #----Generate grid of locations through time----
  x_tot <- seq(1, grid.size, 1)
  y_tot <- seq(1, grid.size, 1)
  expand <- expand.grid(lon = x_tot, lat = y_tot)
  grid <- as.data.frame(cbind(lon = rep(expand$lon, n.year), lat = rep(expand$lat, n.year),year = rep(1:n.year,each=grid.size^2)))
  years <- seq(1,n.year,1) #this includes both observed (n.fit) and forecast years (n.fore)
  
  temp_max_slope <- (temp_diff[4] - temp_diff[2])/n.year  # linear slope over 100 years
  temp_min_slope <- (temp_diff[3] - temp_diff[1])/n.year
  temp_max_int <-  temp_diff[2] - temp_max_slope
  temp_min_int <- temp_diff[1] - temp_min_slope
  
  #----Loop through each year----
  if(!verbose) cat("Creating environmental simulation for Year ")
  for (y in years){
    if(verbose) print(paste0("Creating environmental simulation for Year ",y))
    if(!verbose) cat(y, " ")
    
    #----Generate Temperature Covariate----
    temp_plain <- raster::raster(ncol=grid.size,nrow=grid.size)
    ex <- raster::extent(0.5,0.5+grid.size,0.5,0.5+grid.size)
    raster::extent(temp_plain) <- ex
    xy <- raster::coordinates(temp_plain)
    min <- temp_min_slope*y + temp_min_int 
    max <- temp_max_slope*y + temp_max_int
    
    #----Decide on spatial distribution of temperature----
    if (temp_spatial=="simple"){
      temp_plain[] <- seq(min,max,length.out=grid.size^2) # assign values to RasterLayer
      temp <- raster::calc (temp_plain, fun = function(x) jitter(x,amount=1))
    }
    
    if (temp_spatial=="matern"){
      # EW: simulate from matern spatial field. We could extend this by
      # (1) letting matern parameters vary over time, (2) letting latitude 
      # gradient vary over time, and (3) making the field simulation be non-independent
      # across years. Right now this generates an independent field by year
      sim_field = glmmfields::sim_glmmfields(
        n_knots = 40,
        n_draws=1, covariance="matern",
        g = data.frame(lon=xy[,1],lat=xy[,2]),
        n_data_points=nrow(xy),
        B = c(0,-0.05), #making second parameter negative to invert data, and slightly higher (from 0.1) to have better latitudinal siganl
        X = cbind(1,xy[,2]))
      sim_field$dat$new_y = (sim_field$dat$y + abs(min(sim_field$dat$y)))
      # make these compatible with previous range
      temp_plain[] = min + (max-min)*sim_field$dat$new_y / max(sim_field$dat$new_y)
      temp <- temp_plain
    }

    out <- utils::capture.output(type="message",{
    #----Use Virtual Species to assign response curve----
    envir_stack <- raster::stack(temp) #must be in raster stack format
    names(envir_stack) <- c('temp')
    
    #----assign response curve ----
    parameters <- virtualspecies::formatFunctions(temp = response.curve$temp)
    
    #----convert temperature raster to species suitability----
    envirosuitability <- virtualspecies::generateSpFromFun(envir_stack,parameters=parameters, rescale = FALSE,rescale.each.response = FALSE)
    
    #rescale
    ref_max <- stats::dnorm(parameters$temp$args[1], mean=parameters$temp$args[1], sd=parameters$temp$args[2]) #JS/BM: potential maximum suitability based on optimum temperature
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
        beta = 0.5, alpha = -0.05, 
        species.prevalence = NULL, plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    if (PA_shape == "logistic_prev") {
      #JS: relaxes logistic a little bit, by specifing reduced prevalence and fitting beta (test diff prevalence values, but 0.5 seems realistic)
      suitability_PA <- virtualspecies::convertToPA(
        envirosuitability, 
        PA.method = "probability", 
        beta = "random", alpha = -0.3,
        species.prevalence = 0.5, plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    if (PA_shape == "linear") {
      #JS: relaxes knife-edge absence -> presence further; also specifies prevalence and fits 'b'
      suitability_PA <- virtualspecies::convertToPA(
        envirosuitability, PA.method = "probability",
        prob.method = "linear",
        a = NULL, b = NULL,
        species.prevalence = 0.8,
        plot = FALSE)
      # plotSuitabilityToProba(suitability_PA) #Let's you plot the shape of conversion function
    }
    }) #end capture output
    if(verbose){out <- stringr::str_trim(out); cat(" ", out[out!=""], " ", sep="\n")}
    
    #----Extract suitability for each location----
    for (i in 1:grid.size^2){
      start_index <- min(which(grid$year==y))
      ii = (i + start_index) -1
      s <- raster::extract(envirosuitability$suitab.raster,grid[ii,c("lon","lat")]) 
      pa <- raster::extract(suitability_PA$pa.raster,grid[ii,c("lon","lat")])
      t <- raster::extract(temp,grid[ii,c("lon","lat")]) 
      grid$suitability[ii] <-  s
      grid$pres[ii] <-  pa
      grid$temp[ii] <-  t
    }
  }
  if(verbose==2) cat("\n")
  
  # summary(grid)
  
  #----Create abundance as a function of the environment----
  if (abund_enviro == "lnorm_low") {
    # SB: values in Ecography paper. I think initially they were based on flounder in EBS but not sure if I edited them
    grid$abundance <- ifelse(grid$pres==1, stats::rlnorm(nrow(grid),2,0.1)*grid$suitability,0)
  }
  if (abund_enviro == "lnorm_high") {
    # EW: I'm cranking up the rlnorm parameters to make it more comparable to wc trawl survey estimates -- these new ones based on arrowtooth
    # SB: rlnorm parameters (6,1) were too large for estimation model. GAMs had explained deviance <10%. Other suggestions?
    grid$abundance <- ifelse(grid$pres==1, stats::rlnorm(nrow(grid),6,1)*grid$suitability,0)
  }
  if (abund_enviro == "poisson") {
    # JS: sample from a Poisson distbn, with suitability proportional to mean (slower, bc it re-samples distbn for each observation)
    maxN <- 20  #max mean abundance at highest suitability
    grid$abundance <- ifelse(grid$pres==1, stats::rpois(nrow(grid),lambda=grid$suitability*maxN),0)
  } 
 
  #give years meaning (instead of 1:n.year)
  grid$year <- grid$year + start.year
  #grid$year <- ifelse(grid$year<=n.fit, grid$year + start.year, grid$year + start.year) 
  
  # The meta data is auto generated. You should not need to edit
  meta=list(
    version=utils::packageVersion("WRAP"),
    func=deparse(as.list(match.call())[[1]]),
    call=deparse( sys.call() ),
    sim.seed=sim.seed,
    grid.dimensions=c(grid.size, grid.size, grid.size^2),
    grid.resolution=c(1, 1),
    grid.extent=c(1, grid.size, 1, grid.size),
    grid.crs="simulated data on a grid",
    grid.unit="not applicable",
    time=c(min(grid$year), max(grid$year)),
    time.unit="year"
  )
  meta <- c(meta, fun.args) #add the function arguments
  
  
  obj <- list(meta=meta, grid=grid)
  
  class(obj) <- "OM"
  
  
  return(obj)
  
}






