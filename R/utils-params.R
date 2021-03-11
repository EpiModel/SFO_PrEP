## devtools::load_all("../EpiModelHIV-p/")
library(EpiModelHIV)
source("R/utils-prev_funs.R")

orig <- readRDS("out/est/netest.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

full_tx_eff <- c(rep(0.527, 2), 0.5675) # .55 roughly gets 65% supp

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  hiv.test.rate = rep(0.0070, 3),
  hiv.test.late.prob = c(0, 0, 0),
  tx.init.prob = c(0.125, 0.158, 0.164),
  tt.part.supp = 1 - full_tx_eff,
  tt.full.supp = full_tx_eff,
  tt.dur.supp = c(0, 0, 0),
  tx.halt.part.prob = c(9e-04, 9e-04, 8.9e-04),
  tx.halt.full.rr = c(1, 1, 1),
  tx.halt.dur.rr = c(0.5, 0.5, 0.5),
  tx.reinit.part.prob = c(0.00377, 0.0046, 0.0057),
  tx.reinit.full.rr = c(1, 1, 1),
  tx.reinit.dur.rr = c(1, 1, 1),
  max.time.off.tx.full.int = 52 * 15,
  max.time.on.tx.part.int = 52 * 10,
  max.time.off.tx.part.int = 52 * 10,
  aids.mr = 1 / 250,
  trans.scale = c(1.4, 0.85, 0.73333333),
  acts.scale = 1.00,
  acts.aids.vl = 5.75, # above this VL: no more sex
  prep.start = (52 * 65) + 1,
  riskh.start = 52 * 64,
  prep.adhr.dist = c(0.089, 0.127, 0.784),
  prep.adhr.hr = c(0.69, 0.19, 0.01),
  prep.start.prob =  0.00896,
  prep.discont.rate = 0.02589, # 1 - pexp(26, 0.02589) == 0.51
  ## prep.tst.int = 90/7,         # do I need that?
  ## prep.risk.int = 182/7,       # do I need that?
  ## prep.sti.screen.int = 182/7,
  ## prep.sti.prob.tx = 1,
  prep.risk.reassess.method = "year",
  prep.require.lnt = FALSE, # start with random PrEP initiation
  circ.prob = c(0.62, 0.42, 0.55),
  a.rate = 0.00045 # was 0.00052
)

init <- init_msm(
  prev.ugc = 0,
  prev.rct = 0,
  prev.rgc = 0,
  prev.uct = 0
)

param$epi_funs <- lapply(epi_funs_new_base, compiler::cmpfun)

if (lnt) { # PrEP linked to testing
  param$hiv.test.rate <- rep(0.0128, 3)
  param$prep.require.lnt = TRUE
  param$prep.start.prob =  0.685
}
