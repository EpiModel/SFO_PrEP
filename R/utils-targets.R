targets2013 <- c(
    ## incid            = 405 / 1e5 * num, # 230 - 575 (ic95)
  # B
  prev.B     = 0.318,
  linked3m.B = 0.82,
  tx.B       = 0.84,
  sup_tx.B   = 0.71, #0.714,
  sup_dur.B  = 0.92,
  # H
  prev.H     = 0.273,
  linked3m.H = 0.89,
  tx.H       = 0.86,
  sup_tx.H   = 0.71, #0.709,
  sup_dur.H  = 0.92,
  # W
  prev.W     = 0.24,
  linked3m.W = 0.90,
  tx.W       = 0.88,
  sup_tx.W   = 0.739,
  sup_dur.W  = 0.92
)

targets2017 <- c(
  # B
  dx.B             = 0.94,
  sup_dx.B         = 0.64,
  # H
  dx.H             = 0.94,
  sup_dx.H         = 0.64,
  # W
  dx.W             = 0.94,
  sup_dx.W         = 0.69,
  # PrEP
  prep_retained6m  = 0.51, # 1 - pexp(26, 0.02589)
  prep_any12m      = 0.45
)

targets <- c(targets2013, targets2017)
