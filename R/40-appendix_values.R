netparams <- readRDS("out/est/netparams.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

# Section 3.1.1

netparams$main$md.main
netparams$casl$md.casl

netparams$main$concurrent
netparams$casl$concurrent

# Section 3.1.2
netparams$main$nf.race
netparams$casl$nf.race

netparams$main$nf.age.grp
netparams$casl$nf.age.grp

netparams$main$nf.deg.casl
netparams$casl$nf.deg.main

# Section 3.1.3
netparams$main$nm.age.grp
netparams$casl$nm.age.grp

# Section 3.1.5
netparams$main$durs.main.byage # index 0 is non matched
netparams$casl$durs.casl.byage

# Section 3.1.6
netparams$inst$md.inst

# Section 3.1.7
netparams$inst$nf.race
netparams$inst$nf.age.grp
netparams$inst$nf.risk.grp
netparams$inst$nf.deg.tot

# Section 3.1.8
netparams$inst$nm.race_diffF
weighted.mean(
  netparams$inst$nm.age.grp,
  netparams$inst$nf.age.grp / sum(netparams$inst$nf.age.grp)
)

# Section 3.1.9
netparams$all$role.type

# Section 4.1.2
est <- coefficients(epistats$acts.mod)
ci <- confint(epistats$acts.mod)
write.csv(cbind(est, ci), "out/appendix-act_mod.csv") # beware order

# Section 4.1.3
#
# Remake the figure?

# Section 4.2.2
est <- coefficients(epistats$cond.mc.mod)
ci <- confint(epistats$cond.mc.mod)
write.csv(cbind(est, ci), "out/appendix-cond_mc_mod.csv") # beware order

est <- coefficients(epistats$cond.oo.mod)
ci <- confint(epistats$cond.oo.mod)
write.csv(cbind(est, ci), "out/appendix-cond_oo_mod.csv") # beware order

# Section 5.2
#
# In file: https://github.com/EpiModel/ARTnetData/blob/master/inst/RaceDistribution.xlsx
# 80.9 3.8 15.3


# Section 7.1
library(tidyverse)
origin <- readRDS("out/est/restart/restart_sfo.rds")
df <- as_tibble(origin)

df %>%
  filter(time == max(time) - 52) %>%
  summarise(
    pdx.W = i_dx.W / (s.W + i.W),
    pdx.B = i_dx.B / (s.B + i.B),
    pdx.H = i_dx.H / (s.H + i.H)
  )
