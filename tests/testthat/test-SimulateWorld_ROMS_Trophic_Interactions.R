context("SimulateWorld_ROMS_TrophicInteraction\n")
library(WRAP)
# This runs through all the arg options and makes sure no errors or warnings thrown

PA_shape=c("logistic", "logistic_prev", "linear")
abund_enviro=c("lnorm_low", "lnorm_high", "poisson")

sims <- list()
  for(pa in PA_shape){
    for(ae in abund_enviro){
        test_that(paste("sim", pa, ae), sim <- SimulateWorld_ROMS_TrophicInteraction(PA_shape=pa, abund_enviro=ae, roms.years=1980:1989) )
      sims[[paste(pa, ae)]] <- sim
      cat(pa, ae, "\n")
    }
  }


for(i in 1:length(sims)){
    sim <- sims[[i]]
    test_that(paste("is.OM", names(sims)[i]), expect_is(sim, "OM"))
  }

save(sims, file=file.path(here::here(),"tests/testthat/sims_ROMS_TrophicInteraction.RData"))
