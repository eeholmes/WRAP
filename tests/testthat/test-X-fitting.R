context("fitting functions\n")
library(WRAP)

load(file.path(here::here(),"tests/testthat/sims.RData"))

for(i in 1:length(sims)){
  context(paste("SimulateWorld", names(sims)[i]))
  sim <- sims[[i]]
  fit <- mlp_sdm(sim)
  expect_is(fit, "mlp")
  fit <- gam_sdm(sim)
  expect_is(fit, "gam")
  fit <- brt_sdm(sim)
  expect_is(fit, "brt")
}

load(file.path(here::here(),"tests/testthat/sims_ROMS.RData"))

for(i in 1:length(sims)){
  context(paste("SimulateWorld_ROMS", names(sims)[i]))
  sim <- sims[[i]]
  fit <- mlp_sdm(sim)
  expect_is(fit, "mlp")
  fit <- gam_sdm(sim)
  expect_is(fit, "gam")
  fit <- brt_sdm(sim)
  expect_is(fit, "brt")
}

load(file.path(here::here(),"tests/testthat/sims_ROMS_TrophicInteraction.RData"))

for(i in 1:length(sims)){
  context(paste("SimulateWorld_ROMS_TI", names(sims)[i]))
  sim <- sims[[i]]
  fit <- mlp_sdm(sim)
  expect_is(fit, "mlp")
  fit <- gam_sdm(sim)
  expect_is(fit, "gam")
  fit <- brt_sdm(sim)
  expect_is(fit, "brt")
}
