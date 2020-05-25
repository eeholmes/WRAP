## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  cache = TRUE,
  fig.width=6,
  fig.height=6,
  message=FALSE,
  collapse=TRUE
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(WRAP)

## -----------------------------------------------------------------------------
abund_enviro <- "lnorm_low"
PA_shape <- "logistic"
temp_spatial <- "matern"
temp_diff <- c(1,4,3,7) 

## ----simworld, results='hide', message=FALSE----------------------------------
sim1 <- SimulateWorld(temp_diff = temp_diff,  
                      temp_spatial = temp_spatial, 
                      PA_shape = PA_shape, 
                      abund_enviro = abund_enviro,
                      n.year=40, start.year=2015) 

## ----print_sim1---------------------------------------------------------------
sim1

## ---- eval=FALSE--------------------------------------------------------------
#  if(!dir.exists(file.path(here::here(), 'Sim1')))
#    dir.create(file.path(here::here(), 'Sim1'))

## ---- eval=FALSE--------------------------------------------------------------
#  saveRDS(sim1, file=file.path(here::here(), "Sim1", "Sim1.rds"))

## ----plot-sim-----------------------------------------------------------------
plot(sim1, start.forecast.year=2020)

## ----fitmodels, results='hide', message=FALSE, fig.show='hide'----------------
gam.fit <- gam_sdm(sim1, covariates="temp")
gam.p <- gam.fit$presence
gam.a <- gam.fit$abundance

brt.fit <- brt_sdm(sim1, covariates="temp")
brt.p <- brt.fit$presence
brt.a <- brt.fit$abundance

mlp.fit <- mlp_sdm(sim1, covariates="temp")
mlp.p <- mlp.fit$presence
mlp.a <- mlp.fit$abundance

## -----------------------------------------------------------------------------
new_dat <- data.frame(temp=seq(0,7,length=100))

## ----alt_pred, results='hide', message=FALSE, fig.show='hide'-----------------
pred.gam <- predict(sim1, sdm=gam.fit, newdata=new_dat)
pred.brt <- predict(sim1, sdm=brt.fit, newdata=new_dat)
pred.mlp <- predict(sim1, sdm=mlp.fit, newdata=new_dat)

## ----plot-pred----------------------------------------------------------------
par(mfrow=c(2,2), mar=c(3,4,4,2))
ylim2 <- 1.05*max(pred.gam$pred, pred.brt$pred, pred.mlp$pred)

#actual TPC
xx <- seq(0, 7, length=100)
#yy must match response function in SimulateWorld function (sim1$meta$response.curve$temp)
yy <- dnorm(xx, mean=4, sd=1)  
plot(xx, yy, type="l", lty=2, main="Actual TPC", col="red", xlim=c(0,8), ylab="suitability", xlab="Temp")
xlim <- round(100*(max(new_dat$temp)/7))
lines(xx[1:xlim], yy[1:xlim], lwd=2)

#gam
plot(new_dat$temp, pred.gam$pred, type="l",
     main="lnorm_low GAM", xlim=c(0,8), col="red", lty=2, ylim=c(0,ylim2), ylab="Abundance", xlab="Temp")
lines(new_dat$temp, pred.gam$pred, lwd=2)

#brt
plot(new_dat$temp, pred.brt$pred, type="l",
     main="lnorm_low BRT", xlim=c(0,8), col="red", lty=2, ylim=c(0,ylim2), ylab="Abundance", xlab="Temp")
lines(new_dat$temp, pred.brt$pred, lwd=2)

#MLP
plot(new_dat$temp, pred.mlp$pred, type="l",
     main="lnorm_low MLP", xlim=c(0,8), col="red", lty=2, ylim=c(0,ylim2), ylab="Abundance", xlab="Temp")
lines(new_dat$temp, pred.mlp$pred, lwd=2)

par(mfrow=c(1,1))

## ----pred_om, results='hide', fig.show='hide'---------------------------------
pred.gam2 <- predict(sim1, model="gam")
pred.brt2 <- predict(sim1, model="brt")
pred.mlp2 <- predict(sim1, model="mlp")

## ----pred_sdm, results='hide', fig.show='hide'--------------------------------
pred.gam2 <- predict(sim1, sdm=gam.fit)
pred.brt2 <- predict(sim1, sdm=brt.fit)
pred.mlp2 <- predict(sim1, sdm=mlp.fit)

## -----------------------------------------------------------------------------
nr <- nrow(pred.gam2)
pred.all <- rbind(pred.gam2, pred.brt2, pred.mlp2)
pred.all$model <- c(rep("gam", nr), rep("brt", nr), rep("mlp", nr))

## ----plott_sim1---------------------------------------------------------------
plot(sim1)

## ----plot-abund1--------------------------------------------------------------
plot_abund(sim1, pred.gam2, pred.mlp2, pred.brt2)

## ----plot-cog1----------------------------------------------------------------
plot_cog(sim1, pred.gam2, pred.mlp2, pred.brt2)

## ----plot-pres1---------------------------------------------------------------
plot_pres(sim1, pred.gam2, pred.mlp2, pred.brt2)

## ----plot-grid1---------------------------------------------------------------
plot_grid(sim1, year=2021, pred.gam, pred.mlp, pred.brt)
# You can also pass in SDMs but the predictions will be computed (slow)
# plot_grid(sim1, year=2021, gam.fit, mlp.fit, brt.fit)

