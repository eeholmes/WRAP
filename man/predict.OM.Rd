% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_OM.R
\name{predict.OM}
\alias{predict.OM}
\title{Predict for OM object}
\usage{
\method{predict}{OM}(
  object,
  model = c("gam", "brt", "mlp"),
  newdata = NULL,
  start.forecast.year = 2021,
  sdm = NULL,
  alpha = 0.6826895,
  silent = FALSE,
  ...
)
}
\arguments{
\item{object}{OM object}

\item{model}{Type of SDM. gam, brt or mlp. Only needed if sdm is not passed in.}

\item{newdata}{(optional) Optional new data frame over which to do the prediction.}

\item{start.forecast.year}{The years equal or less will be used for fitting and the years greater than are the forecasted years.}

\item{sdm}{(optional) Previously fitted \link[=SDM_class]{SDM} model}

\item{alpha}{The CIs alpha. Default is 1 sigma (alpha = 68\%)}

\item{silent}{No output printed.}

\item{...}{Any extra parameters for the sdm fitting functions, for example k for \code{gam_sdm}}
}
\value{
Invisibly returns a data frames with the prediction. If 
\code{newdata} is not passed in, then the hindcast and forecast prediction
is returned. The "type" column has "hindcast" and "forecast" to indicate
which is which. This data frame is the same shape as object$grid except
the extra column "type". The first 6 rows of the prediction are printed
unless silent=TRUE. If "newdata" is passed in then the prediction for
the new data only is returned. "newdata" must be a data frame with
the required covariates.
}
\description{
Does a hindcast and forecast with \link[=OM_class]{OM} model. Optionally 
a previously fitted SDM can be passed in
 in to avoid having to refit the SDM. If sdm is not passed in, it will be
  fit using
 the \code{model} argument. 
 Both a hindcast
 and a forecast will be returned. If model is "gam", then 95% CIs will 
 be returned also.
}
\examples{
sim <- SimulateWorld(start.year=2015, n.year=20)
fit <- mlp_sdm(sim, "temp")
pred <- predict(sim, sdm=fit)
}
