#' Predict for SDM object
#' 
#' Does a hindcast and forecast with SDM fit 
#' (sdm returned from one of the sdm
#' fitting functions). If abund_enviro is log-normal, then 
#'  a presence model and an abundance model are fit. If 
#'  abund_enviro is poisson, then only the abundance model is fit. Both a hindcast
#'  and a forecast will be returned. If model is "gam", then 95% CIs will be returned also.
#'  
#' @return Invisibly returns a data frames with the prediction. If newdata is not passed in, then the hindcast and forecast prediction is returned. The "type" column has "hindcast" and "forecast" to indicate which is which. This data frame is the same shape as object$grid except the extra column "type". The first 6 rows of the prediction are printed unless silent=TRUE. If "newdata" is passed in then the prediction for the new data only is returned. "newdata" must be a data frame with the required covariates.
#' 
#' @param object SDM object
#' @param start.forecast.year The years equal or less will be used for fitting and the years greater than are the forecasted years.
#' @param newdata (optional) Optional new data frame over which to do the prediction.
#' @param alpha The CIs alpha. Default is 1 sigma (alpha = 68\%)
#' @param silent No output printed.
#' @param ... Not used.
#' @method predict SDM
#' @export
#' 
#' Adandoning this idea. If lognormal, then the fit does not have all the data. It only has the positive abundances. I could add the original data to the fit object, but that would make it huge.
predict.SDM <- function(object, 
                       newdata=NULL,
                       start.forecast.year = 2021, 
                       alpha=0.6826895, silent=FALSE, ...) {

  if(!inherits(object, "SDM")) stop("predict.SDM requires a SDM object as returned by one of the sdm fitting functions.")
  
  for(j in c("gam", "brt", "mlp")) if(inherits(object$presence, j)) model <- j
  
  if(!missing(newdata) && !inherits(newdata, "data.frame"))
    stop("newdata must be a data frame with the covariates in the simulation.")
  if(!missing(newdata) && !all(object$meta$covariates %in% colnames(newdata)))
    stop("newdata is missing some of the covariates. Check object$meta$covariates.")
  
  abund_enviro <- object$meta$abund_enviro
  ses <- qnorm((1+alpha)/2) #number of sigmas
  
  if(missing(newdata)){
    if(model=="gam") pred <- object$grid
    n.hind <- sum(pred$year < start.forecast.year)
    n.fore <- sum(pred$year >= start.forecast.year)
    pred <- cbind(pred.type=c(rep("hindcast", n.hind), rep("forecast", n.fore)), pred)
    newdata <- object$grid
  }else{
    pred <- newdata
    pred <- cbind(pred.type=rep("newdata", nrow(pred)), pred)
  }

    p.sdm <- object$presence
    a.sdm <- object$abundance

  # Do the predictions
  
  if(!silent) cat("Computing predictions.\n")
  
  if(model=="gam"){
    if(abund_enviro!="poisson"){ 
      pred$pred.p <- predict(p.sdm, newdata, type='response')
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
    print(head(pred))
    cat(paste0(" ", nrow(pred)-6, " more rows...\n"))
  }
  
  invisible(pred)
  
}
