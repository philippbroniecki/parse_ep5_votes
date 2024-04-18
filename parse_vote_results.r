# parse vote results ------------------------------------------------------

#' Call to parsing functions for vote results tables

#' The function "parse_vote_results" assigns a class to current vote and calls
#' paring function.
#' @param x current row of m_data
#' @param data_dir inherits from parse_votes
#' @param verbose inherits from parse_votes

#' @return placeholder.
#' @examples
#' \dontrun{
#' # Not yet
#' }
#'
#' @export
parse_vote_results <- function(x, data_dir, verbose) {

  # initial binding for globals
  minutes_link <- vote_date <- NULL

  # search votes
  votes_out <- search_votes(x = x, data_dir = data_dir, verbose = TRUE)

  # remove additional information from votes_meta
  votes_out <- check_votes_meta(y = votes_out)

  # check that date_tally is consistent in both votes and votes_meta
  votes_out <- chk_date_tally_consistency(y = votes_out)

  # COMMENTED OUT NO MINUTES
  # # add the link to the minutes as an identifier to be used instead of the
  # # date
  # if (length(votes_out$votes) != 1) {
  #   stop("Unexpected number of votes")
  # } else {
  #   # if there are multiple votes in the current session
  #   if ("list" %in% class(votes_out$votes[[1]])) {
  #     # loop over votes tables
  #     for (idx in seq_along(votes_out$votes[[1]])){

  #       # if the votes_table is missing, skip
  #       if (is.null(votes_out$votes[[1]][[idx]])) next

  #       # add minutes link as the session identifier
  #       votes_out$votes[[1]][[idx]] <- votes_out$votes[[1]][[idx]] %>%
  #         dplyr::mutate(minutes_link = !!x$links) %>%
  #         dplyr::relocate(minutes_link, .before = vote_date)
  #     }
  #   }
  #   # if there is one vote in the current session
  #   if ("data.frame" %in% class(votes_out$votes[[1]])) {
  #     votes_out$votes[[1]] <- votes_out$votes[[1]] %>%
  #       dplyr::mutate(minutes_link = !!x$links) %>%
  #       dplyr::relocate(minutes_link, .before = vote_date)
  #   }
  # }
  # return list
  return(votes_out)
}