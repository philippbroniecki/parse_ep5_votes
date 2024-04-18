# generic: fill_in_cells --------------------------------------------------
# pull out repeating cell entries to the front as an extra variable -------
# the extra variable is repeated for all rows until a row with another repeating
# cell entry is reached
# @param rows is a list containing a the tibble to be searched for repeating
# entries.
#' @export
fill_in_cells <- function(rows) {
  UseMethod("fill_in_cells", rows)
}


# doc method: fill_in_cells -----------------------------------------------
#' @export
fill_in_cells.doc <- function(rows) {

  # inital binding for globals
  vote <- block_rcv_ev_sep_split <- type <- budget_line <- subject <- NULL
  am_no <- author <- na <- block_rcv_ev_sep_split <- vote <- NULL

  # extract tibble
  rows <- rows$tbl

  # if the "vote" column is called "voting", rename it to "vote"
  if ("voting" %in% names(rows)) {
    names(rows)[names(rows) == "voting"] <- "vote"
  }

  # loop over all rows in the tibble
  out <- lapply(X = seq(from = 1, to = nrow(rows)), FUN = function(y) {

    # extract current row
    y <- dplyr::slice(.data = rows, y)

    # check whether all cells are NA excpet the Subject cell
    if (
      all(is.na(dplyr::select(.data = y, -1))) |
        all(dplyr::select(.data = y, -1) == "", na.rm = TRUE)
    ) {

      # add a new variable
      y <- y %>%
        dplyr::mutate(type = dplyr::pull(.data = ., var = 1)) %>%
        dplyr::relocate(type, .before = 1)

      # replace all cells except the first with an empty string
      y <- y %>%
        dplyr::mutate(
          dplyr::across(.cols = -1, .fns = function(z) z <- "")
        )
    }
    return(y)
  })

  # loop over the table once more to fill in values between
  # the new variable

  # the cell entry to repeat between entries of the type variable

  # for which variable names
  type_repeat <- ""
  subject_repeat <- ""
  amend_no_repeat <- ""
  author_repeat <- ""
  budget_line_repeat <- ""
  block_rcv_ev_sep_split_repeat <- ""

  # iterate over rows of the table
  for (idx in seq_along(out)) {

    # check whether the type variable exists
    type_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = type),
      silent = TRUE
    )

    # check whether the subject variable exists
    subject_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = subject),
      silent = TRUE
    )

    # check whether the am_no variable exists
    amend_no_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = am_no),
      silent = TRUE
    )

    # check whether the author variable exists
    author_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = author),
      silent = TRUE
    )

    # check whether the budget_line variable exists
    budget_line_present <- try(
      exp = dplyr::pull(.data = out[[idx]], var = budget_line),
      silent = TRUE
    )

    # check whether the block_rcv_ev_sep_split variable exists
    block_rcv_ev_sep_split_present <- try(
      exp = dplyr::pull(.data = out[[idx]], var = block_rcv_ev_sep_split),
      silent = TRUE
    )

    # replace subject variable entry NAs
    if (class(subject_present) != "try-error") {
      if (is.na(subject_present)) {
        subject_present <- ""
      }
    }

    # new subject variable entry
    if (subject_present != "") {
      if (class(subject_present) != "try-error") {
        subject_repeat <- subject_present
      } else {
        subject_repeat <- ""
      }
    }

    # replace am_no variable entry NAs
    if (class(amend_no_present) != "try-error") {
      if (is.na(amend_no_present)) {
        amend_no_present <- ""
      }
    }

    # new am_no variable entry
    if (amend_no_present != "") {
      if (class(amend_no_present) != "try-error") {
        amend_no_repeat <- amend_no_present
      } else {
        amend_no_repeat <- amend_no_present <- ""
      }
    }

    # replace author variable entry NAs
    if (class(author_present) != "try-error") {
      if (is.na(author_present)) {
        author_present <- ""
      }
    }

    # new author variable entry
    if (author_present != "") {
      if (class(author_present) != "try-error") {
        author_repeat <- author_present
      } else {
        author_repeat <- ""
      }
    }

    # replace budget_line variable entry NAs
    if (class(budget_line_present) != "try-error") {
      if (is.na(budget_line_present)) {
        budget_line_present <- ""
      }
    }

    # new budget_line variable entry
    if (budget_line_present != "") {
      if (class(budget_line_present) != "try-error") {
        budget_line_repeat <- budget_line_present
      } else {
        budget_line_repeat <- ""
      }
    }

    # replace block_rcv_ev_sep_split variable entry NAs
    if (class(block_rcv_ev_sep_split_present) != "try-error") {
      if (is.na(block_rcv_ev_sep_split_present)) {
        block_rcv_ev_sep_split_present <- ""
      }
    }

    # new block_rcv_ev_sep_split variable entry
    if (block_rcv_ev_sep_split_present != "") {
      if (class(block_rcv_ev_sep_split_present) != "try-error") {
        block_rcv_ev_sep_split_repeat <- block_rcv_ev_sep_split_present
      } else {
        block_rcv_ev_sep_split_repeat <- ""
      }
    }

    # reset variable repeats for new types
    # new type variable entry
    if (class(type_present) != "try-error") {

      # new type
      type_repeat <- type_present

      # reset all the other variables to not overwrite
      subject_repeat <- ""
      amend_no_repeat <- ""
      author_repeat <- ""
      budget_line_repeat <- ""
      block_rcv_ev_sep_split_repeat <- ""
    }

    # check whether na column exists
    if ("na" %in% names(out[[idx]])) {

      # extract variable
      vote_val <- dplyr::pull(.data = out[[idx]], var = na)

      # check whether the variable contains a voting result
      vote_val <- stringr::str_extract(
        string = vote_val, pattern = "\\d+\\s?,\\s?\\d+\\s?,\\s?\\d+"
      )

      # fill the result into the previous column
      if (!is.na(vote_val)) {
        out[[idx]][, (which(names(out[[idx]]) == "na") - 1)] <- vote_val
      }

      # remove column
      out[[idx]]$na <- NULL
    }

    # add entry
    out[[idx]] <- out[[idx]] %>%
      dplyr::mutate(
        type = type_repeat,
        subject = subject_repeat,
        am_no = amend_no_repeat,
        author = author_repeat,
        budget_line = budget_line_repeat,
        block_rcv_ev_sep_split = block_rcv_ev_sep_split_repeat
      ) %>%
      dplyr::relocate(subject, 1) %>%
      dplyr::relocate(type, .before = subject) %>%
      dplyr::relocate(am_no, .after = subject) %>%
      dplyr::relocate(author, .after = am_no) %>%
      dplyr::relocate(budget_line, .before = vote) %>%
      dplyr::relocate(block_rcv_ev_sep_split, .after = budget_line)
  }

  # combine list to current table
  votes <- dplyr::bind_rows(out)

  # filter out rows that have no entry except for the subject variable
  votes <- votes %>%
    dplyr::filter(dplyr::if_any(.cols = -type, ~ .x != ""))

  # repeat votes in block votes
  if ("block_rcv_ev_sep_split" %in% names(votes)) {
    # there has to be at least one entry in the block_rcv_ev_sep_split column
    if (any(votes$block_rcv_ev_sep_split != "")) {
      votes <- votes %>%
        dplyr::group_by(block_rcv_ev_sep_split) %>%
        dplyr::mutate(
          vote = ifelse(
            test = vote == "",
            yes = vote[which(vote != "")[1]],
            no = vote
          )
        ) %>%
        dplyr::ungroup()
    }
  }

  return(votes)
}

# xml & html method: fill_in_cells ----------------------------------------
#' @export
fill_in_cells.xml_html <- function(rows) {

  # initial binding of globals
  author <- budget_line <- am_no <- subject <- NULL
  type <- na <- block_rcv_ev_sep_split <- vote <- NULL

  # extract tibble
  rows <- rows$tbl

  # loop over all rows in the tibble
  out <- lapply(X = seq(from = 1, to = nrow(rows)), FUN = function(y) {

    # initial binding of globals
    type <- . <- NULL

    # extract current row
    y <- dplyr::slice(.data = rows, y)

    # check whether all cells are the same as the first cell
    if (
      all(dplyr::pull(
        .data = y[, names(y)[!is.na(y)]], var = 1
      ) == y[-1][, names(y[-1])[!is.na(y[-1])]])
    ) {

      # add a new variable
      y <- y %>%
        dplyr::mutate(type = dplyr::pull(.data = ., var = 1)) %>%
        dplyr::relocate(type, .before = 1)

      # replace all cells except the first with an empty string
      y <- y %>%
        dplyr::mutate(
          dplyr::across(.cols = -1, .fns = function(z) z <- "")
        )
    }
    return(y)
  })

  # loop over the table once more to fill in values between
  # the new variable

  # the cell entry to repeat between entries of the type variable

  # for which variable names
  type_repeat <- ""
  subject_repeat <- ""
  amend_no_repeat <- ""
  author_repeat <- ""
  budget_line_repeat <- ""
  block_rcv_ev_sep_split_repeat <- ""

  # iterate over rows of the table
  for (idx in seq_along(out)){

    # check whether the type variable exists
    type_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = type),
      silent = TRUE
    )

    # check whether the subject variable exists
    subject_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = subject),
      silent = TRUE
    )

    # check whether the am_no variable exists
    amend_no_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = am_no),
      silent = TRUE
    )

    # check whether the author variable exists
    author_present <- try(
      exp =  dplyr::pull(.data = out[[idx]], var = author),
      silent = TRUE
    )

    # check whether the budget_line variable exists
    budget_line_present <- try(
      exp = dplyr::pull(.data = out[[idx]], var = budget_line),
      silent = TRUE
    )

    # check whether the block_rcv_ev_sep_split variable exists
    block_rcv_ev_sep_split_present <- try(
      exp = dplyr::pull(.data = out[[idx]], var = block_rcv_ev_sep_split),
      silent = TRUE
    )

    # new type variable entry
    if (subject_present != "") {
      if (class(subject_present) != "try-error") {
        subject_repeat <- subject_present
      } else {
        subject_repeat <- ""
      }
    }

    # new am_no variable entry
    if (amend_no_present != "") {
      if (class(amend_no_present) != "try-error") {
        amend_no_repeat <- amend_no_present
      } else {
        amend_no_repeat <- amend_no_present <- ""
      }
    }

    # new author variable entry
    if (author_present != "") {
      if (class(author_present) != "try-error") {
        author_repeat <- author_present
      } else {
        author_repeat <- ""
      }
    }

    # new budget_line variable entry
    if (budget_line_present != "") {
      if (class(budget_line_present) != "try-error") {
        budget_line_repeat <- budget_line_present
      } else {
        budget_line_repeat <- ""
      }
    }

    # new block_rcv_ev_sep_split variable entry
    if (block_rcv_ev_sep_split_present != "") {
      if (class(block_rcv_ev_sep_split_present) != "try-error") {
        block_rcv_ev_sep_split_repeat <- block_rcv_ev_sep_split_present
      } else {
        block_rcv_ev_sep_split_repeat <- ""
      }
    }

    # reset variable repeats for new types
    # new type variable entry
    if (class(type_present) != "try-error") {

      # new type
      type_repeat <- type_present

      # reset all the other variables to not overwrite
      subject_repeat <- ""
      amend_no_repeat <- ""
      author_repeat <- ""
      budget_line_repeat <- ""
      block_rcv_ev_sep_split_repeat <- ""
    }

    # check whether na column exists
    if ("na" %in% names(out[[idx]])) {

      # extract variable
      vote_val <- dplyr::pull(.data = out[[idx]], var = na)

      # check whether the variable contains a voting result
      vote_val <- stringr::str_extract(
        string = vote_val, pattern = "\\d+\\s?,\\s?\\d+\\s?,\\s?\\d+"
      )

      # fill the result into the previous column
      if (!is.na(vote_val)) {
        out[[idx]][, (which(names(out[[idx]]) == "na") - 1)] <- vote_val
      }

      # remove column
      out[[idx]]$na <- NULL
    }

    # add entry
    out[[idx]] <- out[[idx]] %>%
      dplyr::mutate(
        type = type_repeat,
        subject = subject_repeat,
        am_no = amend_no_repeat,
        author = author_repeat,
        budget_line = budget_line_repeat,
        block_rcv_ev_sep_split = block_rcv_ev_sep_split_repeat
      ) %>%
      dplyr::relocate(subject, 1) %>%
      dplyr::relocate(type, .before = subject) %>%
      dplyr::relocate(am_no, .after = subject) %>%
      dplyr::relocate(author, .after = am_no) %>%
      dplyr::relocate(budget_line, .before = vote) %>%
      dplyr::relocate(block_rcv_ev_sep_split, .after = budget_line)
  }

  # combine list to current table
  votes <- dplyr::bind_rows(out)

  # filter out rows that have no entry in the newly created type column
  votes <- votes %>%
    dplyr::filter(dplyr::if_any(.cols = -type, ~ .x != ""))

  # repeat votes in block votes
  if ("block_rcv_ev_sep_split" %in% names(votes)) {
    # there has to be at least one entry in the block_rcv_ev_sep_split column
    if (any(votes$block_rcv_ev_sep_split != "")) {
      votes <- votes %>%
        dplyr::group_by(block_rcv_ev_sep_split) %>%
        dplyr::mutate(
          vote = ifelse(
            test = vote == "",
            yes = vote[which(vote != "")[1]],
            no = vote
          )
        ) %>%
        dplyr::ungroup()
    }
  }

  return(votes)
}