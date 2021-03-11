scenarios <- list(
  p_025_sfo = list(),
  p_0375 = list(
    list(
      at = param$prep.start + 52 * 4,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0.5263158
      )
    )
  ),
  p_05 = list(
    list(
      at = param$prep.start + 52 * 4,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0.2702703
      )
    )
  ),
  p_0625 = list(
    list(
      at = param$prep.start + 52 * 4,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0.1
      )
    )
  ),
  p_075 = list(
    list(
      at = param$prep.start + 52 * 4,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0
      )
    )
  ),
  s_0 = list(
    list(
      at = param$prep.start,
      param = list(
        prep.start.prob = 0
      )
    )
  ),
  s_0125 = list(
    list(
      at = param$prep.start,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 2.5
      )
    )
  ),
  s_025_sfo = list(),
  s_0375 = list(
    list(
      at = param$prep.start,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0.5263158
      )
    )
  ),
  s_05 = list(
    list(
      at = param$prep.start,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0.2702703
      )
    )
  ),
  s_0625 = list(
    list(
      at = param$prep.start,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0.1
      )
    )
  ),
  s_075 = list(
    list(
      at = param$prep.start,
      param = list(
        prep.discont.rate = param$prep.discont.rate * 0
      )
    )
  )
)
