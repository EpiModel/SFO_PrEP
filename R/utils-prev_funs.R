epi_funs_by_race <- function(ls_funs, races = 1:3,
                             races_names = c("B", "H", "W"),
                             indiv = TRUE, full = TRUE) {

  if (indiv) {
    epi_funs <- lapply(
      races,
      function(race) lapply(ls_funs, do.call, args = list(r_ind = race))
    )

    epi_funs <- unlist(epi_funs)
    names(epi_funs) <- paste0(
      names(epi_funs), ".",
      unlist(lapply(races_names, rep, times = length(ls_funs)))
    )
  } else {
    epi_funs <- list()
  }

  if (full) {
    epi_funs_full <- lapply(ls_funs, do.call, args = list(r_ind = races))
    epi_funs <- c(epi_funs_full, epi_funs)
  }

  epi_funs
}

epi_s <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 0, na.rm = TRUE)
    })
  }
}

epi_s_neg_test <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
       sum(race %in% r_ind & last.neg.test == at, na.rm = TRUE)
    })
  }
}

epi_lnt_time <- function(weeks) {
  function(r_ind) {
    function(dat, at) {
      with(dat$attr, {
        lnt <- ifelse(is.na(last.neg.test), -Inf, last.neg.test)

        cond <- at - lnt <= weeks
        pop <- race %in% r_ind &
               !(diag.status  %in% 1) &
               at - arrival.time >= weeks

        sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
      })
    }
  }
}

epi_s_prep_elig <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 0 & prepElig == 1, na.rm = TRUE)
    })
  }
}

epi_s_prep <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 0 & prepStat == 1, na.rm = TRUE)
    })
  }
}

epi_s_prep_time <- function(weeks) {
  function(r_ind) {
    function(dat, at) {
      with(dat$attr, {
        sum(race %in% r_ind &
            status == 0 &
            prepStat == 1 &
            prep.period.first == at - weeks,
            na.rm = TRUE)
      })
    }
  }
}

epi_i <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 1, na.rm = TRUE)
    })
  }
}

epi_i_new_dx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & diag.time == at, na.rm = TRUE)
    })
  }
}

epi_i_dx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 1 & diag.status == 1, na.rm = TRUE)
    })
  }
}

epi_i_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 1 & tx.status == 1, na.rm = TRUE)
    })
  }
}

epi_i_sup <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 1 & vl.last.supp == at, na.rm = TRUE)
    })
  }
}

epi_i_sup_dur <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind &
          status == 1 &
          at - vl.last.usupp >= 52,
          na.rm = TRUE)
    })
  }
}

epi_prev <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(race %in% r_ind & status == 1, na.rm = TRUE) /
        sum(race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_dx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(diag.status == 1 & status == 1 & race %in% r_ind, na.rm = TRUE) /
        sum(status == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_linked_time <- function(weeks) {
  function(r_ind) {
    function(dat, at) {
      with(dat$attr, {
        cond <-  tx.init.time - diag.time <= weeks
        pop <- diag.status == 1 & race %in% r_ind

        sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
      })
    }
  }
}

epi_linked_time_this_year <- function(weeks) {
  function(r_ind) {
    function(dat, at) {
      with(dat$attr, {

        cond <- tx.init.time - diag.time <= weeks
        pop <- at - diag.time <= 52 + weeks & # diag this year
               at - diag.time >= weeks & # diag older than `week`
               race %in% r_ind

        sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
      })
    }
  }
}

epi_sup_dx_this_year <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {

      cond <- at - vl.last.supp <= 52
      pop <- (at - diag.time <= 52 + 52 & at - diag.time >= 52 &
                race %in% r_ind)

      sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
    })
  }
}

# Note: This metric as the total nb of diag as denom !
epi_retained_time_this_year <- function(weeks) {
  function(r_ind) {
    function(dat, at) {
      with(dat$attr, {
        cond <- tx.period.last - tx.init.time >= weeks
        pop <- (at - diag.time <= 52 + weeks & at - diag.time >= weeks &
                  race %in% r_ind)

        sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
      })
    }
  }
}

epi_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      cond <- tx.status == 1
      pop <- diag.status == 1 & race %in% r_ind

      sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
    })
  }
}

epi_sup_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(vl <= log10(200) & tx.status == 1 & race %in% r_ind, na.rm = TRUE) /
        sum(tx.status == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_sup_dx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(vl <= log10(200) & diag.status == 1 & race %in% r_ind, na.rm = TRUE) /
        sum(diag.status == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_sup_dur <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      1 - sum(
        at - vl.last.usupp <= 52 & diag.status == 1 &
          vl <= log10(200) & race %in% r_ind, na.rm = TRUE) /
        sum(vl <= log10(200) & diag.status == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_prep_prop <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(prepStat == 1 & race %in% r_ind, na.rm = TRUE) /
        sum(status == 0 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_prep_elig_prev <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(prepStat == 1 & race %in% r_ind, na.rm = TRUE) /
        sum(prepElig == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_prep_elig_prop <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(prepElig == 1 & race %in% r_ind, na.rm = TRUE) /
        sum(status == 0 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_any_prep <- function(weeks) {
  function(r_ind) {
    function(dat, at) {
      with(dat$attr, {
        cond <- at - prep.period.last <= weeks
        pop <- status == 0 & # in the study giving the targets, status is known
               at - arrival.time >= weeks & # arrival older than `week`
               race %in% r_ind

        sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
      })
    }
  }
}

epi_prep_6m <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {

      cond <- prepStat == 1
      pop <- (prep.period.first == at - 26 & race %in% r_ind)

      sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
    })
  }
}

epi_prep_hdr <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(prepStat == 1 & prepClass == 3 & race %in% r_ind, na.rm = TRUE) /
        sum(prepStat == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_mean_dx_delay <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- diag.status == 1 & race %in% r_ind
      mean(diag.time[pop] - inf.time[pop], na.rm = TRUE)
    })
  }
}

epi_mean_time_on_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- tx.status == 1 & race %in% r_ind
      mean(at - tx.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_mean_time_stop_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      # last time on tx was last step (would be current step if still on)
      pop <- at - tx.period.last == 1 & race %in% r_ind
      mean(tx.period.last[pop] - tx.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_mean_time_on_prep <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- prepStat == 1 & race %in% r_ind
      mean(at - prep.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_mean_time_stop_prep <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- at - prep.period.last == 1 & race %in% r_ind
      mean(prep.period.last[pop] - prep.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_mean_test_int <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      mean(at - last.neg.test[diag.status == 0 & race %in% r_ind], na.rm = TRUE)
    })
  }
}

epi_median_time_on_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- tx.status == 1 & race %in% r_ind
      median(at - tx.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_median_time_stop_tx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- at - tx.period.last == 1 & race %in% r_ind
      median(tx.period.last[pop] - tx.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_median_time_on_prep <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- prepStat == 1 & race %in% r_ind
      median(at - prep.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_median_time_stop_prep <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      pop <- at - prep.period.last == 1 & race %in% r_ind
      median(prep.period.last[pop] - prep.period.first[pop], na.rm = TRUE)
    })
  }
}

epi_mean_test_int <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      mean(at - last.neg.test[diag.status == 0 & race %in% r_ind], na.rm = TRUE)
    })
  }
}
epi_med_test_int <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      median(at - last.neg.test[diag.status == 0 & race %in% r_ind],
             na.rm = TRUE)
    })
  }
}

epi_incid_prep <- function(dat, at) {
  with(dat$attr, {

    cond <-  prepStat == 1
    pop <- inf.time == at

    sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
  })
}

epi_dx_prep <- function(dat, at) {
  with(dat$attr, {

    cond <-  prepLastTimeOn == at - 1
    pop <- diag.time == at

    sum(cond & pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
  })
}

epi_inftime_prep <- function(dat, at) {
  with(dat$attr, {

    cond <- at - inf.time
    pop <- prepLastTimeOn == at - 1 & diag.time == at

    sum(cond * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
  })
}

epi_incid_preptime <- function(dat, at) {
  with(dat$attr, {

    cond <- at - prepStartTime
    pop <- prepLastTimeOn == at - 1 & diag.time == at

    sum(cond * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
  })
}


epi_flow_si <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(inf.time == at & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_flow_isupp <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(vl.last.supp == at & vl.last.usupp == at - 1 & race %in% r_ind,
          na.rm = TRUE)
    })
  }
}

epi_flow_suppi <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(vl.last.supp == at - 1 & vl.last.usupp == at & race %in% r_ind,
          na.rm = TRUE)
    })
  }
}

epi_flow_itx <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(tx.period.first == at & race %in% r_ind, na.rm = TRUE)
    })
  }
}

epi_flow_txi <- function(r_ind) {
  function(dat, at) {
    with(dat$attr, {
      sum(at - tx.period.last == 1 & race %in% r_ind, na.rm = TRUE)
    })
  }
}

ls_funs13 <- list(
  s           = epi_s,
  i           = epi_i,
  prev        = epi_prev,
  dx          = epi_dx,
  linked3m    = epi_linked_time(12),
  tx          = epi_tx,
  sup_tx      = epi_sup_tx,
  sup_dur     = epi_sup_dur
)

epi_funs13 <- epi_funs_by_race(ls_funs13, full = FALSE)

ls_funs17 <- list(
  prev           = epi_prev,
  dx             = epi_dx,
  linked1m_new   = epi_linked_time_this_year(4),
  retained6m_new = epi_retained_time_this_year(26),
  sup_dx         = epi_sup_dx,
  sup_tx         = epi_sup_tx,
  prep           = epi_prep_prop,
  ## prep_hdr       = epi_prep_hdr,
  ## prep_any30j    = epi_any_prep(4),
  ## prep_any6m     = epi_any_prep(4),
  prep_any12m    = epi_any_prep(52),
  ## prep_ifelig    = epi_prep_elig_prev,
  ## prep_elig_n    = epi_prep_elig_prop,
  ## mean_test_int  = epi_mean_test_int,
  med_test_int   = epi_med_test_int
)

epi_funs17 <- epi_funs_by_race(ls_funs17)

epi_funsBoth <- unique(names(c(epi_funs17, epi_funs13)))
epi_funsBoth <- c(epi_funs17, epi_funs13)[epi_funsBoth]
epi_funsBoth <- c(
  epi_funsBoth,
  dx_prep = epi_dx_prep,
  incid_prep = epi_incid_prep,
  incid_preptime = epi_incid_preptime,
  inftime_prep = epi_inftime_prep
)

ls_funs_scenarios <- list(
  s                     = epi_s,
  s_neg_test            = epi_s_neg_test,
  s_prep_elig           = epi_s_prep_elig,
  s_prep                = epi_s_prep,
  i                     = epi_i,
  i_new_dx              = epi_i_new_dx,
  i_dx                  = epi_i_dx,
  i_tx                  = epi_i_tx,
  i_sup                 = epi_i_sup,
  linked1m_1y           = epi_linked_time_this_year(4), # linked1m this year
  lnt_1y                = epi_lnt_time(52), # lnt during the year
  prep_any12m           = epi_any_prep(52),
  prep_retained6m       = epi_prep_6m,
  mean_time_on_tx       = epi_mean_time_on_tx,
  mean_time_stop_tx     = epi_mean_time_stop_tx,
  mean_time_on_prep     = epi_mean_time_on_prep,
  mean_time_stop_prep   = epi_mean_time_stop_prep,
  median_time_on_tx     = epi_median_time_on_tx,
  median_time_stop_tx   = epi_median_time_stop_tx,
  median_time_on_prep   = epi_median_time_on_prep,
  median_time_stop_prep = epi_median_time_stop_prep,
  med_test_int          = epi_med_test_int
)

epi_funs_scenarios <- epi_funs_by_race(ls_funs_scenarios, indiv = FALSE)

ls_fun_art_calib <- list(
  linked3m    = epi_linked_time(12),
  tx          = epi_tx,
  i_sup       = epi_i_sup,
  i           = epi_i
)

epi_funs_art_calib <- epi_funs_by_race(ls_fun_art_calib, full = TRUE)

ls_funs_new_base <- list(
  s                     = epi_s,
  s_prep_elig           = epi_s_prep_elig,
  s_prep                = epi_s_prep,
  s_prep_6m             = epi_s_prep_time(26),
  i                     = epi_i,
  i_dx                  = epi_i_dx,
  i_tx                  = epi_i_tx,
  i_sup                 = epi_i_sup,
  i_sup_dur             = epi_i_sup_dur,
  linked3m              = epi_linked_time(13), # linked1m this year
  prep_any12m           = epi_any_prep(52),
  median_time_on_tx     = epi_median_time_on_tx,
  median_time_on_prep   = epi_median_time_on_prep,
  flow_si               = epi_flow_si,
  flow_itx               = epi_flow_itx,
  flow_txi               = epi_flow_txi,
  flow_isupp            = epi_flow_isupp,
  flow_suppi            = epi_flow_suppi
)

epi_funs_new_base <- epi_funs_by_race(ls_funs_new_base, full = TRUE)
