# chk_date_tally_consistency ----------------------------------------------
# in step parse_votes(), check that the output objects votes_meta and votes are
# cosistent with respect to the date_tally

chk_date_tally_consistency <- function(y = votes_out, verbose = FALSE) {

  # is y$votes_meta a data frame
  if (!"list" %in% class(y$votes_meta)) {
    stop("List expected in vote on ", y$vote_date)
  }

  # is y$votes a list
  if (!"list" %in% class(y$votes)) {
    stop("List expetecd in vote on ", y$vote_date)
  }

  if ("data.frame" %in% class(y$votes[[1]])) {
    if (nrow(y$votes[[1]]) != nrow(y$votes_meta[[1]])) {
      stop("Votes_meta list is not the same length as votes list")
    }
  } else {
    # votes_meta and votes should have the same length
    if (length(y$votes[[1]]) != nrow(y$votes_meta[[1]])) {
      stop("Votes_meta list is not the same length as votes list")
    }
  }

  # loop over votes list and check that the date dally ticks up by one in each
  # iteration
  for (idx in seq_len(nrow(y$votes_meta[[1]]))) {

    # correct date_tally y$votes[[1]] if it is incorrect
    if ("data.frame" %in% class(y$votes[[1]])) {

      if (length(unique(y$votes[[1]]$date_tally)) == 1) {
        if (y$votes_meta[[1]]$date_tally[idx] !=
              unique(y$votes[[1]]$date_tally)) {

          # replace vote_tally in y$votes[[1]][[z]]
          y$votes[[1]]$date_tally <- rep(
            x = y$votes_meta[[1]]$date_tally[idx],
            times = nrow(y$votes[[1]])
          )

        }
      } else {
        if (y$votes_meta[[1]]$date_tally[idx] != unique(
          y$votes[[1]]$date_tally[idx]
        )) {

          # replace vote_tally in y$votes[[1]][[z]]
          y$votes[[1]]$date_tally <- rep(
            x = y$votes_meta[[1]]$date_tally[idx],
            times = nrow(y$votes[[1]])
          )

        }
      }
    } else {

      if (is.null(y$votes[[1]][[idx]])) next

      if (y$votes_meta[[1]]$date_tally[idx] !=
            unique(y$votes[[1]][[idx]]$date_tally)) {

        # replace vote_tally in y$votes[[1]][[z]]
        y$votes[[1]][[idx]]$date_tally <- rep(
          x = y$votes_meta[[1]]$date_tally[idx],
          times = nrow(y$votes[[1]][[idx]])
        )
      }
    }
  }

  return(y)
}