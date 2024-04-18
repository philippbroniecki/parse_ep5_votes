clean_votes_02 <- function(data_dir, verbose = TRUE) {

  # initial binding for globals
  split_vote <- vote_date <- row_remove <- vote_item_id <- NULL
  vote_id <- vote_topic_num <- NULL

  # canonical data dir
  data_dir <- canonical_data_dir(data_dir = data_dir)

  # load data ---------------------------------------------------------------

  # 1) all votes data (object name: votes)
  load(sprintf("%sparsed/votes.RData", data_dir))

  # split votes -------------------------------------------------------------

  # add split vote column
  votes <- dplyr::mutate(
    .data = votes,
    split_vote = FALSE,
    row_remove = FALSE
  ) %>%
    dplyr::relocate(split_vote, .before = vote_date) %>%
    dplyr::relocate(row_remove, .after = split_vote)

  # character columns of the dataset
  char_cols <- which(unlist(lapply(X = seq_len(ncol(votes)), FUN = function(y) {
    is.character(dplyr::pull(.data = votes, var = y))
  })))

  # group votes by vote_item_id and loop over vote_items
  votes <- dplyr::group_by(.data = votes, vote_item_id)

  # group indices
  grp_idx <- dplyr::group_indices(.data = votes)

  # number of groups
  n_grps <- dplyr::n_groups(x = votes)

  # ungroup the data
  votes <- dplyr::ungroup(votes)

  # create progress bar
  if (verbose && nrow(votes) > 1) {
    pb <- utils::txtProgressBar(min = 1, max = n_grps, style = 3)
  }

  # loop over rows of votes
  split_votes <- lapply(X = seq_len(n_grps), FUN = function(y) {
    message(y)

    # subset votes to current session
    c_votes <- dplyr::slice(.data = votes, which(grp_idx == y)) %>%
      dplyr::mutate(vote_topic_num = as.integer(stringr::str_extract(
        string = vote_id, pattern = "\\d+$"
      ))) %>%
      dplyr::arrange(vote_topic_num)

    # loop over the rows in the current session
    for (idx in seq_len(nrow(c_votes))) {

      # skip if assigned already
      if (c_votes$split_vote[idx]) next
      # skip last row
      if (idx == nrow(c_votes)) next

      # id string
      id_string <- paste(c_votes[idx, char_cols], collapse = " ")

      # split vote function with phrase: "split"
      c_votes <- split_vote_function(
        data = c_votes,
        idx = idx,
        id_string = id_string,
        phrase = "split"
      )

      # split vote function with phrase: "div"
      c_votes <- split_vote_function(
        data = c_votes,
        idx = idx,
        id_string = id_string,
        phrase = "div"
      )

    } # end of loop over rows in the current session

    # update progress bar
    if (verbose) {
      utils::setTxtProgressBar(pb = pb, value = y)
    }

    # return the current session
    return(c_votes)
  }) # end of loop over votes

  # close progress bar
  if (verbose) {
    close(pb)
  }

  # row-bind list elements
  votes <- dplyr::bind_rows(split_votes) %>%
    dplyr::filter(!row_remove) %>%
    dplyr::select(-row_remove)

  save(votes, file = sprintf("%sparsed/votes.RData", data_dir))

}
