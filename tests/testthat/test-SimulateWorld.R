context("SimulateWorld\n")
library(WRAP)

temp_spatial = c("simple", "matern")
PA_shape = c("logistic", "logistic_prev", "linear")
abund_enviro = c("lnorm_low", "lnorm_high", "poisson")

sims <- list()
for(ts in temp_spatial){
  for(pa in PA_shape){
    for(ae in abund_enviro){
      test_that(paste("sim", ts, pa, ae), sim <- SimulateWorld(n.year=5, temp_spatial=ts, PA_shape=pa, abund_enviro=ae) )
      sims[[paste(ts, pa, ae)]] <- sim
      cat("\n")
    }
  }
}

for(i in 1:length(sims)){
    sim <- sims[[1]]
    test_that(paste("is.OM", names(sims)[i]), expect_is(sim, "OM"))
  }

save(sims, file=file.path(here::here(),"tests/testthat/sims.RData"))
