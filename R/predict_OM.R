#' Predict for OM object
#' 
#' Does a hindcast and forecast with \link[=OM_class]{OM} model. Optionally 
#' a previously fitted SDM can be passed in
#'  in to avoid having to refit the SDM. If sdm is not passed in, it will be
#'   fit using
#'  the \code{model} argument. 
#'  Both a hindcast
#'  and a forecast will be returned. If model is "gam", then 95% CIs will 
#'  be returned also.
#'  
#' @return Invisibly returns a data frames with the prediction. If 
#' \code{newdata} is not passed in, then the hindcast and forecast prediction
#' is returned. The "type" column has "hindcast" and "forecast" to indicate
#' which is which. This data frame is the same shape as object$grid except
#' the extra column "type". The first 6 rows of the prediction are printed
#' unless silent=TRUE. If "newdata" is passed in then the prediction for
#' the new data only is returned. "newdata" must be a data frame with
#' the required covariates.
#' 
#' @param object OM object
#' @param model Type of SDM. gam, brt or mlp. Only needed if sdm is not passed in.
#' @param start.forecast.year The years equal or less will be used for fitting and the years greater than are the forecasted years.
#' @param sdm (optional) Previously fitted \link[=SDM_class]{SDM} model
#' @param newdata (optional) Optional new data frame over which to do the prediction.
#' @param alpha The CIs alpha. Default is 1 sigma (alpha = 68\%)
#' @param silent No output printed.
#' @param ... Any extra parameters for the sdm fitting functions, for example k for \code{gam_sdm}
#' 
#' @examples
#' sim <- SimulateWorld(start.year=2015, n.year=20)
#' fit <- mlp_sdm(sim, "temp")
#' pred <- predict(sim, sdm=fit)

#' @method predict OM
#' @export
predict.OM <- function(object, model=c("gam", "brt", "mlp"), 
                       newdata=NULL,
                       start.forecast.year = 2021, 
                       sdm=NULL, 
                       alpha=0.6826895, silent=FALSE, ...) {

  model.passed.in <- !missing(model)
  model <- match.arg(model)
  
  if(!inherits(object, "OM")) stop("predict.OM requires a OM object as returned by one of the simulate functions.")
  abund_enviro <- object$meta$abund_enviro
  ses <- stats::qnorm((1+alpha)/2) #number of sigmas
  
  if(!missing(newdata) && !inherits(newdata, "data.frame"))
    stop("newdata must be a data frame with the covariates in the simulation.")
  if(!missing(newdata) && !all(object$meta$covariates %in% colnames(newdata)))
    stop("newdata is missing some of the covariates. Check object$meta$covariates.")
  
  if(!missing(sdm) && !inherits(sdm, "SDM"))
    stop("sdm must be a SDM object returned from one of the fitting functions. See ?SDM_class.")
  if(!missing(sdm) && !missing(start.forecast.year) && sdm$meta$start.forecast.year != start.forecast.year)
    cat("start.forecast.year does not match the value in the SDM meta data. start.forecast.year from the SDM is being used.")
  if(!missing(sdm)) start.forecast.year <- sdm$meta$start.forecast.year
  
  if(!is.null(sdm)){
    oldmodel <- model
    model <- class(sdm$presence)[1]
    if(model.passed.in && model != oldmodel) cat(paste0("model does not match the SDM. Ignoring model value and using ", model, "."))
    p.sdm <- sdm$presence
    a.sdm <- sdm$abundance
  }

  extras <- list(...)
  if(model=="gam" && "k" %in% names(extras)){ k <- extras$k }else{ k <- NULL }
  
  if(missing(newdata)){
    pred <- object$grid
    n.hind <- sum(pred$year < start.forecast.year)
    n.fore <- sum(pred$year >= start.forecast.year)
    pred <- cbind(pred.type=c(rep("hindcast", n.hind), rep("forecast", n.fore)), pred)
    newdata <- object$grid
  }else{
    pred <- newdata
    pred <- cbind(pred.type=rep("newdata", nrow(pred)), pred)
  }

  # Fit sdm if needed
  if( is.null(sdm) ){
    if(!silent) cat(paste0("Fitting sdm with ", model, "_sdm.\n"))
    fit <- switch(model,
        gam = gam_sdm(object, k=k, start.forecast.year=start.forecast.year),
        brt = brt_sdm(object, start.forecast.year=start.forecast.year),
        mlp = mlp_sdm(object, start.forecast.year=start.forecast.year)
    )
    p.sdm <- fit$presence
    a.sdm <- fit$abundance
  }

  # Do the predictions
  
  if(!silent) cat("Computing predictions.\n")
  
  if(model=="gam"){
    if(abund_enviro!="poisson"){ 
      pred$pred.p <- stats::predict(p.sdm, newdata, type='response')
    }
    pred$pred.a <- predict(a.sdm, newdata, type="response")
  }
  if(model=="brt" | model=="gbm"){
    if(abund_enviro!="poisson"){
      pred$pred.p <- predict(p.sdm, newdata, n.trees=p.sdm$gbm.call$best.trees, 
                               type='response')
    }
    pred$pred.a <- predict(a.sdm, newdata, n.trees=a.sdm$gbm.call$best.trees, 
                               type='response')
  }
  if(model=="mlp" | model=="nn"){
    # The normalize data
    for(i in object$meta$covariates) newdata[,i] <- BBmisc::normalize(newdata[,i])
    if(abund_enviro!="poisson"){
      pred$pred.p <- predict(p.sdm, newdata)
    }
    pred$pred.a <- predict(a.sdm, newdata)
  }
  # Compute the predictions
  if(abund_enviro=="poisson"){
    pred$pred <- pred$pred.a
  }else{  # log-normal  
    pred$pred <- pred$pred.p*exp(pred$pred.a)
  }
  
  #Standard errors 
  if(model=="gam"){
    if(!silent) cat("Computing standared errors (gam only).\n")
    if(abund_enviro!="poisson")
      modCI1.p <- predict(p.sdm, newdata, type="response", se.fit=TRUE)
    
    modCI1.a <- predict(a.sdm, newdata, type="response", se.fit=TRUE)

    if(abund_enviro=="poisson"){
      pred$pred.high <- modCI1.a$fit + ses*(modCI1.a$se.fit)
      pred$pred.low <- modCI1.a$fit - ses*(modCI1.a$se.fit)
    }else{  # log-normal  
      pred$pred.high <- exp((modCI1.a$fit + ses*(modCI1.a$se.fit))) * (modCI1.p$fit + ses*(modCI1.p$se.fit))
      pred$pred.low <- exp((modCI1.a$fit - ses*(modCI1.a$se.fit))) * (modCI1.p$fit - ses*(modCI1.p$se.fit))
    }
    
  }else{
    pred$pred.high <- NA
    pred$pred.low <- NA
  }
  
  if(!silent) cat("\n")
  
  if(!silent){
    print(utils::head(pred))
    cat(paste0(" ", nrow(pred)-6, " more rows...\n"))
  }
  
  class(pred) <- c("POM", class(pred))
  attr(pred, "start.forecast.year") <- start.forecast.year
  
  invisible(pred)
  
}
