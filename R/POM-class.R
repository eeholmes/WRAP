#' POM class
#' 
#' This is the class for output from the `predict.OM` function (\code{\link{predict.OM}}). The POM object is a data from with the predicted presence (pred.a), abundance (pred.a, logged if model is log-normal), and expected abundance (pred = pred.p x exp(pred.a), if log-normal).
#' This allows easy plotting from predictions.
#' 
#' @docType class
#' @name POM_class
NULL