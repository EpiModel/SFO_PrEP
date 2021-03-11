clean_pop_names <- function(nms, excluded = NULL) {
  mapper <- str_replace(nms, "\\.([HBW])", "___\\1")
  names(mapper) <- nms
  mapper <- mapper[!names(mapper) %in% excluded]

  grpd <- mapper[str_detect(mapper, "___[HBW]")]
  ungrpd <- mapper[!mapper %in% grpd]
  n_ungrpd <- names(ungrpd)
  ungrpd <- str_c(ungrpd, "___ALL")
  names(ungrpd) <- n_ungrpd

  mapper <- c(grpd, ungrpd)

  if (!is.null(excluded)) {
    names(excluded) <- excluded
    mapper <- c(mapper, excluded)
  }

  unname(mapper[nms])
}
