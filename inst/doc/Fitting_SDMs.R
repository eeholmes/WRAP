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
sim1 <- SimulateWorld(temp_diff = temp_diff,  temp_spatial = temp_spatial, 
                      PA_shape = PA_shape, abund_enviro = abund_enviro) 

## ----print_sim1---------------------------------------------------------------
sim1

## ---- eval=FALSE--------------------------------------------------------------
#  if(!dir.exists(file.path(here::here(), 'Sim1')))
#    dir.create(file.path(here::here(), 'Sim1'))

## ---- eval=FALSE--------------------------------------------------------------
#  saveRDS(sim1, file=file.path(here::here(), "Sim1", "Sim1.rds"))

## -----------------------------------------------------------------------------
dat <- sim1$grid
dat_hist <- dat[dat$year<=2020,]
dat_fcast <- dat[dat$year>2020,]

## ---- fig.height=6, fig.width=6-----------------------------------------------
#All Years
par(mfrow=c(2,2))
plot(aggregate(suitability~year,dat,FUN="mean"),type="l", lwd=2, ylab="Suitability",col="dark grey")
lines(aggregate(suitability~year,dat[dat$year<=2020,],FUN="mean"),col="blue")
plot(aggregate(pres~year,dat,FUN="mean"),type="l", lwd=2,ylab="Presence",col="dark grey")
lines(aggregate(pres~year,dat[dat$year<=2020,],FUN="mean"),col="blue")
plot(aggregate(abundance~year,dat,FUN="sum"),type="l",  lwd=2,ylab="Abundance", col="dark grey")
lines(aggregate(abundance~year,dat[dat$year<=2020,],FUN="sum"),col="blue")
plot(aggregate(temp~year,dat,FUN="mean"),type="l",ylab="Temperature", col="dark grey")

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

## ----pred_fit1, results='hide', message=FALSE, fig.show='hide'----------------
pred.gam.p <- predict(gam.p, newdata=new_dat, type="response")
pred.gam.a <- predict(gam.a, newdata=new_dat, type="response")

pred.brt.p <- predict(brt.p, newdata=new_dat, type="response",
     n.trees=brt.p$gbm.call$best.trees)
pred.brt.a <- predict(brt.a, newdata=new_dat, type="response",
     n.trees=brt.a$gbm.call$best.trees)

dat_norm <- new_dat
dat_norm$temp <- BBmisc::normalize(dat_norm$temp)
pred.mlp.p <- predict(mlp.p, dat_norm)
pred.mlp.a <- predict(mlp.a, dat_norm)

## ----alt_pred, results='hide', message=FALSE, fig.show='hide'-----------------
pred.gam <- predict(sim1, sdm=gam.fit, newdata=new_dat)
pred.brt <- predict(sim1, sdm=brt.fit, newdata=new_dat)
pred.mlp <- predict(sim1, sdm=mlp.fit, newdata=new_dat)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
nr <- nrow(pred.gam2)
pred.all <- rbind(pred.gam2, pred.brt2, pred.mlp2)
pred.all$model <- c(rep("gam", nr), rep("brt", nr), rep("mlp", nr))

## -----------------------------------------------------------------------------
abund_enviro <- sim1$meta$abund_enviro

# sum up the abundance predictions across cells by year
# model col is the model
abund <- aggregate(pred~year+model, pred.all, FUN="sum")
colnames(abund) <- c("year", "model", "abundance")

# Add true abundance
x <- sim1$grid
if (abund_enviro == "poisson") x$abundance <- round(x$abundance)
tmp <- aggregate(abundance~year, x, FUN="sum")
tmp$model <- "true"
tmp <- tmp[,c("year", "model", "abundance")]

abund <- rbind(abund, tmp)

## -----------------------------------------------------------------------------
p <- ggplot(abund, aes(x=year, y=abundance, color=model)) + 
  geom_line() +
  ggtitle("Comparison of lnorm_low models") +
  geom_vline(xintercept=2020) +
  annotate("text", x=2020, y=max(abund$abundance), label="  forecast", hjust=0) +
  annotate("text", x=2020, y=max(abund$abundance), label="hindcast  ", hjust=1)
p

## -----------------------------------------------------------------------------
# First compute the true cog
library(dplyr)
cog_lat <- x %>% group_by(year) %>% 
  summarize(cog=weighted.mean(x=lat, w=abundance))
cog_lat <- cbind(model="true", cog_lat, stringsAsFactors = FALSE)

# Now add the model cogs
tmp <- pred.all %>% group_by(model, year) %>% 
  summarize(cog=weighted.mean(x=lat, w=pred))

# dplyr uses tibbles and they return a matrix when you use rbind(). ug.
# so use 
cog_lat <- dplyr::bind_rows(cog_lat, tmp)

## -----------------------------------------------------------------------------
p <- ggplot(cog_lat, aes(x=year, y=cog, color=model)) + 
  geom_line() +
  ggtitle("Comparison of lnorm_low models") +
  ylab("Centre of Gravity (deg lat)") +
  geom_vline(xintercept=2020) +
  annotate("text", x=2020, y=max(cog_lat$cog), label="  forecast", hjust=0) +
  annotate("text", x=2020, y=max(cog_lat$cog), label="hindcast  ", hjust=1)

p

## ----surf_pred, fig.height=6, fig.width=6-------------------------------------
#Future
Y = 2021
x <- subset(sim1$grid, year==Y)
#Truth
p1 <- ggplot(x, aes(lon, lat))+
  geom_tile(aes(fill=abundance)) +
  theme_classic() +
  ggtitle("Truth")+
  labs(y="Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  viridis::scale_fill_viridis()

#Gam
x <- subset(pred.all, year==Y & model=="gam")
p2 <- ggplot(x, aes(lon,lat))+
  geom_tile(aes(fill=pred)) +
  theme_classic() +
  ggtitle("GAM")+
  labs(y="Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  viridis::scale_fill_viridis()

#BRT
x <- subset(pred.all, year==Y & model=="brt")
p3 <- ggplot(x, aes(lon,lat))+
  geom_tile(aes(fill=pred)) +
  theme_classic() +
  ggtitle("BRT")+
  labs(y="Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  viridis::scale_fill_viridis()

#Gam
x <- subset(pred.all, year==Y & model=="mlp")
p4 <- ggplot(x, aes(lon,lat))+
  geom_tile(aes(fill=pred)) +
  theme_classic() +
  ggtitle("MLP")+
  labs(y="Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  viridis::scale_fill_viridis()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

