context("SimulateWorld_ROMS\n")
library(WRAP)
# This runs through all the arg options and makes sure no errors or warnings thrown

covariates=list(c("sst", "chla"), "sst", "chla")
PA_shape=c("logistic", "logistic_prev", "linear")
abund_enviro=c("lnorm_low", "lnorm_high", "poisson")

sims <- list()
for(cov in covariates){
  for(pa in PA_shape){
    for(ae in abund_enviro){
      if(pa=="linear"){
        a=list(a=NULL, b=NULL, species.prevalence=0.4)
        test_that(paste("sim", pa, ae, cov), sim <- SimulateWorld_ROMS(PA_shape=pa, abund_enviro=ae, covariates=cov, roms.years=1980:1989,
        convertToPA.options=a) )
      }else{
      test_that(paste("sim", pa, ae, cov), sim <- SimulateWorld_ROMS(PA_shape=pa, abund_enviro=ae, covariates=cov, roms.years=1980:1989) )
      }
      sims[[paste(pa, ae, paste(cov, collapse=" "))]] <- sim
      cat(pa, ae, cov, "\n")
    }
  }
}

for(i in 1:length(sims)){
    sim <- sims[[i]]
    test_that(paste("is.OM", names(sims)[i]), expect_is(sim, "OM"))
  }

save(sims, file=file.path(here::here(),"tests/testthat/sims_ROMS.RData"))
