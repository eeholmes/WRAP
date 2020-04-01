#' Predict for OM object
#' 
#' Does a hindcast and forecast with OM model. Optionally previously fitted SDMs can passed
#'  in to avoid having to refit SDMs. If abund_enviro is log-normal, then the
#'  prediction requires a presence model and 
#'  an abundance model. If these are not passed in, they will be estimated. If 
#'  abund_enviro is poisson, then only the abundance model is needed. Both a hindcast
#'  and a forecast will be returned. If model is "gam", then 95% CIs will be returned also.
#'  
#' @return Invisibly returns a data frames with the prediction. If newdata is not passed in, then the hindcast and forecast prediction is returned. The "type" column has "hindcast" and "forecast" to indicate which is which. This data frame is the same shape as object$grid except the extra column "type". The first 6 rows of the prediction are printed unless silent=TRUE. If "newdata" is passed in then the prediction for the new data only is returned. "newdata" must be a data frame with the required covariates.
#' 
#' @param object OM object
#' @param model Type of SDM. gam, brt or mlp
#' @param start.forecast.year The years equal or less will be used for fitting and the years greater than are the forecasted years.
#' @param p.sdm (optional) Previously fitted presence model
#' @param a.sdm (optional) Previously fitted abundance model
#' @param newdata (optional) Optional new data frame over which to do the prediction.
#' @param alpha The CIs alpha. Default is 1 sigma (alpha = 68\%)
#' @param silent No output printed.
#' @param ... Any extra parameters for the sdm fitting functions, for example k for \code{gam_sdm}
#' @method predict OM
#' @export
predict.OM <- function(object, model=c("gam", "brt", "mlp"), 
                       newdata=NULL,
                       start.forecast.year = 2021, 
                       p.sdm=NULL, a.sdm=NULL, 
                       alpha=0.6826895, silent=FALSE, ...) {

  model.passed.in <- !missing(model)
  model <- match.arg(model)
  
  if(!inherits(object, "OM")) stop("predict.OM requires a OM object as returned by one of the simulate functions.")
  abund_enviro <- object$meta$abund_enviro
  ses <- qnorm((1+alpha)/2) #number of sigmas
  
  if(!missing(newdata) && !inherits(newdata, "data.frame"))
    stop("newdata must be a data frame with the covariates in the simulation.")
  if(!missing(newdata) && !all(object$meta$covariates %in% colnames(newdata)))
    stop("newdata is missing some of the covariates. Check object$meta$covariates.")
  
  if(!model.passed.in){ #check that p.sdm and a.sdm match then set model to value in those
    if(!is.null(p.sdm) && !is.null(a.sdm) && class(p.sdm)[1] != class(a.sdm)[1])
      stop("Class of p.sdm does not match class of a.sdm")
    if(!is.null(p.sdm)) model <- class(p.sdm)[1]
    if(!is.null(a.sdm)) model <- class(a.sdm)[1]
  }
  # These warnings will only arise if user passed in model that doesn't match p.sdm or a.sdm
  if(abund_enviro != "poisson" && !is.null(p.sdm) && class(p.sdm)[1] != model){
    warning(paste0("model is ", model, " but p.sdm is class ", class(p.sdm)[1]))
    cat(paste0("Resetting model value to ", class(p.sdm)[1], "to  match p.sdm.\n"))
    model <- class(p.sdm)[1]
  }
  if(!is.null(a.sdm) && class(a.sdm)[1] != model){
    stop(paste0("model is ", model, " but a.sdm is class ", class(a.sdm)[1]))
    warning(paste0("model is ", model, " but a.sdm is class ", class(a.sdm)[1]))
    cat(paste0("Resetting model value to ", class(a.sdm)[1], "to  match a.sdm.\n"))
    model <- class(a.sdm)[1]
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

  # Fit sdms if needed
  if(abund_enviro!="poisson" & is.null(p.sdm)){
    if(!silent) cat(paste0("Fitting presence sdm with ", model, "_sdm.\n"))
    p.sdm <- switch(model,
                    gam = gam_sdm(object, response="pres", k=k),
                    brt = brt_sdm(object, response="pres"),
                    mlp = mlp_sdm(object, response="pres")
    )
  }
  if(is.null(a.sdm)){
    if(!silent) cat(paste0("Fitting abundance sdm with ", model, "_sdm.\n"))
    a.sdm <- switch(model,
                    gam = gam_sdm(object, response="abundance", k=k),
                    brt = brt_sdm(object, response="abundance"),
                    mlp = mlp_sdm(object, response="abundance")
    )
  }
  
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
