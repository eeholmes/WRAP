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

## ----simworld, results='hide', message=FALSE----------------------------------
sim1 <- SimulateWorld_ROMS(PA_shape = PA_shape, 
                      abund_enviro = abund_enviro,
                      roms.years = 2000:2040) 

## ----print_sim1---------------------------------------------------------------
sim1

## -----------------------------------------------------------------------------
plot(sim1, start.forecast.year=2020)

## -----------------------------------------------------------------------------
library(dplyr) #for the %>% pipe
year_mean <- sim1$grid %>% aggregate(.~year, ., FUN="mean") %>% 
  reshape2::melt(., id.vars=c("year"),
                  measure.vars=c("pres", "suitability", "sst", "abundance"),
                  variable.name="variable",
                  value.name="value")
year_mean$date <- as.Date(paste(year_mean$year, "01-01", sep="-"))

## -----------------------------------------------------------------------------
ggplot(year_mean, aes(x=date, y=value)) + geom_line(col="grey") +
  geom_line(aes(x=date, y=value), col="blue", subset(year_mean, year<=2020)) +
  facet_wrap(~variable, scales = "free_y") +
  xlab("year")

## ---- fig.height=6, fig.width=6-----------------------------------------------
#All Years
dat <- sim1$grid
par(mfrow=c(2,2))
plot(aggregate(suitability~year,dat,FUN="mean"),type="l", lwd=2, ylab="Suitability",col="dark grey")
lines(aggregate(suitability~year,dat[dat$year<=2020,],FUN="mean"),col="blue")
plot(aggregate(pres~year,dat,FUN="mean"),type="l", lwd=2,ylab="Presence",col="dark grey")
lines(aggregate(pres~year,dat[dat$year<=2020,],FUN="mean"),col="blue")
plot(aggregate(abundance~year,dat,FUN="sum"),type="l",  lwd=2,ylab="Abundance", col="dark grey")
lines(aggregate(abundance~year,dat[dat$year<=2020,],FUN="sum"),col="blue")
plot(aggregate(sst~year,dat,FUN="mean"),type="l",ylab="Temperature", col="dark grey")

## ----fitmodels, results='hide', message=FALSE, fig.show='hide'----------------
gam.fit <- gam_sdm(sim1, covariates="sst")
brt.fit <- brt_sdm(sim1, covariates="sst")
mlp.fit <- mlp_sdm(sim1, covariates="sst")

## -----------------------------------------------------------------------------
plot_abund(sim1, gam.fit, mlp.fit, brt.fit)
# You can also pass in predictions, which would be faster

## ----pred, results='hide', message=FALSE, fig.show='hide'---------------------
pred.gam <- predict(sim1, sdm=gam.fit)
pred.brt <- predict(sim1, sdm=brt.fit)
pred.mlp <- predict(sim1, sdm=mlp.fit)

# Assemble into a data frame
nr <- nrow(pred.gam)
pred.all <- rbind(pred.gam, pred.brt, pred.mlp)
pred.all$model <- c(rep("gam", nr), rep("brt", nr), rep("mlp", nr))

## -----------------------------------------------------------------------------
# sum up the abundance predictions across cells by year
# model col is the model
abund <- aggregate(pred~year+model, pred.all, FUN="sum")
colnames(abund) <- c("year", "model", "abundance")

# Add true abundance
x <- sim1$grid
if (sim1$meta$abund_enviro == "poisson") x$abundance <- round(x$abundance)
tmp <- aggregate(abundance~year, x, FUN="sum")
tmp$model <- "true"
tmp <- tmp[,c("year", "model", "abundance")]

abund <- rbind(abund, tmp)

## -----------------------------------------------------------------------------
p <- ggplot(abund, aes(x=year, y=abundance, color=model)) + 
  geom_line() +
  ggtitle("Comparison of models") +
  geom_vline(xintercept=2020) +
  annotate("text", x=2020, y=max(abund$abundance), label="  forecast", hjust=0) +
  annotate("text", x=2020, y=max(abund$abundance), label="hindcast  ", hjust=1)
p

## -----------------------------------------------------------------------------
plot_cog(sim1, pred.gam, pred.mlp, pred.brt)
# You can also pass in SDMs
# plot_cog(sim1, gam.fit, mlp.fit, brt.fit)

## -----------------------------------------------------------------------------
# First compute the true cog
library(dplyr)
cog_lat <- x %>% group_by(year) %>% 
  summarize(cog=weighted.mean(x=lat, w=abundance))
cog_lat <- cbind(model="true", cog_lat, stringsAsFactors = FALSE)

# Now add the model cogs
tmp <- pred.all %>% group_by(model, year) %>% 
  summarize(cog=weighted.mean(x=lat, w=pred))

# dplyr uses tibbles and they return a matrix when you use rbind().
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

## -----------------------------------------------------------------------------
plot_pres(sim1, pred.gam, pred.mlp, pred.brt)
# You can also pass in SDMs
# plot_cog(sim1, gam.fit, mlp.fit, brt.fit)

## -----------------------------------------------------------------------------
plot_grid(sim1, year=2021:2040, pred.gam, pred.mlp, pred.brt)
# You can also pass in SDMs but the predictions will be computed (slow)
# plot_grid(sim1, year=2021:2040, gam.fit, mlp.fit, brt.fit)

## ----surf_pred, fig.height=6, fig.width=6-------------------------------------
#Future
uscale <- 9.5
Y <- 2021:2040
x <- subset(sim1$grid, year%in%Y)
#Truth
p1 <- ggplot(x, aes(x=lon, y=lat))+
  geom_tile(aes(fill=abundance)) +
  theme_classic() +
  ggtitle("Truth")+
  labs(y="Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  viridis::scale_fill_viridis(limits=c(0, uscale))

#Gam
x <- subset(pred.all, year%in%Y & model=="gam")
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
  viridis::scale_fill_viridis(limits=c(0, uscale))

#BRT
x <- subset(pred.all, year%in%Y & model=="brt")
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
  viridis::scale_fill_viridis(limits=c(0, uscale))

#Gam
x <- subset(pred.all, year%in%Y & model=="mlp")
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
  viridis::scale_fill_viridis(limits=c(0, uscale))

gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

