#' SDM fit with MLP
#'
#' Build a MLP species distribution model from the output from a operating model (or grid) using the neuralnet package. The model is fit with 3 neurons in the hidden layer (which is good for this particular simulation). If it gets much more complicated, can try different values. But training time increases exponentially with additional neurons. The variables are normalized to a common scale before training the MLPs.
#' Models fit to both the
#' presence (pres column) and log(abundance) are returned.

#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR list with meta$abund_enviro and grid from the operating model (\code{sim$grid}).
#' @param covariates Covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid). If left off, all covariates in x (in x$meta$covariates) are used.
#' @param start.forecast.year The years less will be used for fitting and the years greater than are the forecasted years
#' @param control The control parameters for the `neuralnet::neuralnet` call.
#'
#' @return A list with the presence and abundance fits and the meta data.
#' 
#' @examples
#' sim <- SimulateWorld()
#' # abundance fit
#' fit <- mlp_sdm(sim, "temp")$abundance
#' dat <- fit$data #data used for fit
#' dat$mlpAbun <- predict(fit, dat)
#' plot(dat$temp, dat$mlpAbun)
#' 
#' @export
mlp_sdm <- function(x, covariates=NULL,
                    start.forecast.year=2021,
                    control=list(hidden=c(3), algorithm = "rprop+", threshold = 0.2)){
  
  if(!inherits(x, "OM") & !all(c("meta", "grid") %in% names(x))) stop("Something is wrong. x should be a OM (operating model) object or a list with meta and grid.")
  if(!(c("abund_enviro") %in% names(x$meta))) stop("Something is wrong. x$meta needs 'abund_enviro' value.")
  abund_enviro <- x$meta$abund_enviro
  if(missing(covariates)) covariates <- x$meta$covariates
  
  if(!all(covariates %in% colnames(x$grid))) stop("The operating model does not have all the covariates specified.")
  
  # --- Data set-up section ----
  dat <- x$grid
  
  # If lognormal and response is abundance, make the resp column logged
  if ((abund_enviro == "lnorm_low" || abund_enviro == "lnorm_high")){
    dat$log.abundance <- log(dat$abundance) # log
  }
  if (abund_enviro == "poisson"){
    dat$round.abundance <- round(dat$abundance)
  }
  
  # Covariates must be normalized
  for(i in covariates) dat[,i] <- BBmisc::normalize(dat[,i])
  
  #Create dataframe with historical data
  dat_hist  <- dat[dat$year < start.forecast.year,]

  # --- Model fitting section ----
  
  ## -- The presence fit
  frm.text <- paste("pres ~", paste(covariates, collapse=" + "))
  fit.p <- neuralnet::neuralnet(stats::as.formula(frm.text), 
                                data = dat_hist,
                                hidden = control$hidden, 
                                linear.output = FALSE, 
                                algorithm = control$algorithm, 
                                threshold = control$threshold)
  
  ## -- The abundance fit
  if (abund_enviro == "lnorm_low" | abund_enviro == "lnorm_high"){
    resp <- "log.abundance"
    dat_hist <- dat_hist[dat_hist$abundance>0,] # remove 0s
  }
  if (abund_enviro == "poisson") resp <- "round.abundance"
  frm.text <- paste(resp, "~", paste(covariates, collapse=" + "))
  fit.a <- neuralnet::neuralnet(stats::as.formula(frm.text), 
                                data = dat_hist,
                                hidden = control$hidden, 
                                linear.output = TRUE, 
                                algorithm = control$algorithm, 
                                threshold = control$threshold)
  
  # Add on the meta info from the OM object
  fit <- list(presence=fit.p, abundance=fit.a, 
              meta=c(x$meta, start.forecast.year=start.forecast.year) )
  class(fit) <- c("SDM", "mlp")

  return(fit)
}