clean_votes_03 <- function(data_dir, verbose) {

  # initial binding of globals
  vote <- vote_type <- yes <- no <- abstain <- NULL

  # canonical data dir
  data_dir <- canonical_data_dir(data_dir = data_dir)

  # load data ---------------------------------------------------------------

  # 1) all votes data (object name: votes)
  load(sprintf("%sparsed/votes.RData", data_dir))

  if (!"vote_type" %in% names(votes)) {
    votes$vote_type <- votes$rcv_etc
  }
  if (!"yes" %in% names(votes)) {
    votes$yes <- NA_integer_
  }
  if (!"no" %in% names(votes)) {
    votes$no <- NA_integer_
  }
  if (!"abstain" %in% names(votes)) {
    votes$abstain <- NA_integer_
  }

  # vote outcome and RCV indicator ------------------------------------------
  # in addtion add the vote split

  # character columns of the dataset
  char_cols <- which(unlist(lapply(X = seq_len(ncol(votes)), FUN = function(y) {
    is.character(dplyr::pull(.data = votes, var = y))
  })))

  # create progress bar
  if (verbose && nrow(votes) > 1) {
    pb <- utils::txtProgressBar(min = 1, max = nrow(votes), style = 3)
  }

  # loop over votes
  votes <- lapply(seq_len(nrow(votes)), function(x) {

    # current row
    c_row <- dplyr::slice(.data = votes, !!x)

    # generate character string
    c_string <- paste0(c_row[, char_cols], collapse = " ")

    # check strings
    check_strings <- c(
      "rollcall vote", "electronic vote", "secret vote", "show of hands"
    )
    if (!c_row$vote_type %in% check_strings) {
      # detect the phrase RCV
      if (stringr::str_detect(string = c_string, pattern = "\\s?/?RCV\\s")) {
        c_row$vote_type <- "rollcall vote"
      } else if (stringr::str_detect(
        string = c_string, pattern = "\\s?/?AN\\s"
      )) {
        # detect the phrase RCV
        c_row$vote_type <- "rollcall vote"
      } else if (stringr::str_detect(
        string = c_string, pattern = "\\s?/?EV\\s"
      )) {
        # detect the phrase EV
        c_row$vote_type <- "electronic vote"
      } else if (stringr::str_detect(
        string = c_string, pattern = "\\s?/?VE\\s"
      )) {
        # detect the phrase VE
        c_row$vote_type <- "electronic vote"
      } else if (stringr::str_detect(
        string = c_string, pattern = "\\s?/?SEC\\s"
      )) {
        # detect the phrase secret vote
        c_row$vote_type <- "secret vote"
      }
    }

    # update progress bar
    if (verbose) {
      utils::setTxtProgressBar(pb = pb, value = x)
    }

    if (is.na(c_row$yes) & is.na(c_row$no) & is.na(c_row$abstain)) {
      # add the vote split
      v_split <- stringr::str_extract(
        string = c_string,
        pattern = "\\d+,\\s?\\d+,\\s?\\d+"
      )

      # yes votes
      c_row$yes <- stringr::str_extract(
        string = v_split, pattern = "^\\d+"
      ) %>%
        as.integer()

      # no votes
      c_row$no <- stringr::str_extract(
        string = v_split, pattern = "(?<=,\\s?).+?(?=,)"
      ) %>%
        as.integer()

      # abstain votes
      c_row$abstain <- stringr::str_extract(
        string = v_split, pattern = "\\d+$"
      ) %>%
        as.integer()
    }

    return(c_row)
  })

  # close progress bar
  if (verbose) {
    close(pb)
  }

  # combine list to tibble
  votes <- dplyr::bind_rows(votes)

  # move variable to is_rcv to the front
  votes <- votes %>%
    dplyr::relocate(vote_type, .before = vote) %>%
    dplyr::relocate(yes, .after = vote) %>%
    dplyr::relocate(no, .after = yes) %>%
    dplyr::relocate(abstain, .after = no)

  # save votes data
  save(... = votes, file = sprintf("%sparsed/votes.RData", data_dir))

}
