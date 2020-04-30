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

## ----run_sim_1, message=FALSE, results='hide'---------------------------------
sim <- SimulateWorld()

## ----print_sim----------------------------------------------------------------
sim

## ---- eval=FALSE--------------------------------------------------------------
#  if(!dir.exists(file.path(here::here(), 'Sim1')))
#    dir.create(file.path(here::here(), 'Sim1'))

## ---- eval=FALSE--------------------------------------------------------------
#  saveRDS(sim, file=file.path(here::here(), "Sim1", "Sim.rds"))

## ----plot_obs-----------------------------------------------------------------
data <- sim$grid
plot(aggregate(abundance~year, data[data$year<=2010,], FUN="sum"),type="l",ylab="Abundance")

## ----plot_fore----------------------------------------------------------------
data <- sim$grid
plot(aggregate(abundance~year,data[data$year>2010,], FUN="sum"),type="l", ylab="Abundance")

## ----plot_grid_2001, fig.height=4, fig.width=4, warning=FALSE-----------------
library(raster)
df <- subset(sim$grid, year==2001)
df <- df[, c("lon", "lat", "abundance")]
dfr <- raster::rasterFromXYZ(df)      
plot(dfr)

## ----plot_grid_4, fig.height=6, fig.width=6, warning=FALSE--------------------
library(raster)
par(mfrow=c(2,2), mar=c(2,2,2,2))
for(yr in c(2001, 2025, 2050, 2100)){
df <- subset(sim$grid, year==yr)
df <- df[, c("lon", "lat", "abundance")]
dfr <- raster::rasterFromXYZ(df)      
plot(dfr, axes=FALSE, box=FALSE, legend=FALSE)
title(yr)
}

