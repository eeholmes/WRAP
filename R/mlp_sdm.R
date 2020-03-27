#' SDM fit with MLP
#'
#' Build a MLP species distribution model from the output from a operating model (or grid) using the neuralnet package. The model is fit with 3 neurons in the hidden layer (which is good for this particular simulation). If it gets much more complicated, can try different values. But training time increases exponentially with additional neurons. The variables are normalized to a common scale before training the MLPs.
#'
#' @param x (required) An operating model as output from one of the operating model functions (such as \code{sim <- \link{SimulateWorld}()} OR just the grid from the operating model (\code{sim$grid}).
#' @param covariates (required) covariates to use in the SDM. Must be in the operating model output (in the columns of x$grid).
#' @param start.forecast.year The years equal or less will be used for fitting and the years greater than are the forecasted years.
#'
#' @examples
#' sim <- SimulateWorld()
#' fit <- mlp_sdm(sim, "temp")
#' dat <- fit$data #data used for fit
#' dat$mlpAbun <- predict(fit, dat)
#' plot(dat$temp, dat$mlpAbun)
#' 
#' @export
mlp_sdm <- function(x, covariates,
                    response=c("pres", "abundance"),
                    start.forecast.year=2020){
  # Allow the user to just pass in a grid or an OM object
  if(inherits(x, "OM")) x <- x$grid
  if(!inherits(x, "data.frame")) stop("Something is wrong. x should be a data.frame or a OM (operating model) object.")
  
  resp <- match.arg(response)
  if(!all(covariates %in% colnames(x))) stop("The operating model does not have all the covariates specified.")
  
  abund_enviro <- x$meta$abund_enviro
  
  # --- Data set-up section ----
  # The repsonse variable (pres or abundance) is put in new 'resp' column
  # If needed, resp is transformed (0s removed and logged or rounded)
  dat <- x
  # add a column with name resp to use in the fitting functions
  dat$resp <- dat[,resp]
  
  # If lognormal and resp is abundance, make the resp column logged
  if (resp=="abundance" && str_detect(abund_enviro, "lnorm")){
    dat <- dat[dat$abundance>0,] # remove 0s
    dat$resp <- log(dat$resp) # log
  }
  if (resp=="abundance" && abund_enviro == "poisson"){
    dat$resp <- round(dat$resp)
  }
  
  # Covariates must be normalized
  for(i in covariates) dat[,i] <- BBmisc::normalize(dat[,i])
  
  #Create dataframe with historical/forecast data
  dat_hist <- dat[dat$year<=start.forecast.year,]
  dat_fcast <- dat[dat$year>start.forecast.year,]
  
  # --- Model fitting section ----
  # The response is in the resp column and has been transformed (if needed) already
  
  # The formula as text
  frm.text <- paste("resp ~", paste(covariates, collapse=" + "))
  
  if(resp=="pres"){
    fit <- neuralnet::neuralnet(as.formula(frm.text), 
                                   data = dat_hist,
                                   hidden = c(3), 
                                   linear.output = FALSE, 
                                   algorithm = "rprop+", 
                                   threshold = 0.2)
  }
  
  # else resp was abundance
  # Note all the needed data transformations were done above in the
  # data set-up section
  if(resp=="abundance"){
    fit <- neuralnet::neuralnet(as.formula(frm.text), 
                                data = dat_hist,
                                hidden = c(3), 
                                linear.output = TRUE, 
                                algorithm = "rprop+", 
                                threshold = 0.2)
  }
  
  return(fit)
}