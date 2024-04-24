#' @export
check_votes_meta <- function(y) {

  # unlist lists
  if ("list" %in% class(y$votes_meta)) {
    meta <- y$votes_meta[[1]]
  } else {
    meta <- y$votes_meta
  }

  # check the number of votes tables
  n_tables <- length(y$votes[[1]])

  # if the number of rows in meta is larger than in votes
  # the reasons is perhaps that meta contains "Additional information"
  # as the last row
  if (nrow(meta) > n_tables) {
    # check whether the title of meta is "Additional information"
    add_info_idx <- stringr::str_which(
      string = meta$title,
      pattern = stringr::regex(
        pattern = "Additional\\s+information",
        ignore_case = TRUE
      )
    )
    # remove additional information from meta
    if (length(add_info_idx) > 0) {
      meta <- meta[-add_info_idx, ]
    }
  }

  # assign meta back to y$votes_meta
  if ("list" %in% class(y$votes_meta)) {
    y$votes_meta <- list(meta)
  } else {
    y$votes_meta <- meta
  }

  # return y
  return(y)

}