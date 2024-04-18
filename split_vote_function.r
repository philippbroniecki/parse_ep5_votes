
# split vote function  ----------------------------------------------------
split_vote_function <- function(data, id_string, idx, phrase) {

  # is phrase
  if (grepl(pattern = phrase, x = id_string)) {

    # which column ID contais the phrase "split"
    split_col <- which(grepl(pattern = phrase, x = data))

    # if multiple split vote columns, take the one where the string starts
    # with the phrase
    if (length(split_col) > 1) {
      split_col_idx <- lapply(split_col, function(z) {
        stringr::str_detect(
          string = data %>% dplyr::pull(!!z),
          pattern = sprintf("^%s", phrase)
        ) %>%
          any()
      }) %>%
        unlist()
      split_col_idx[is.na(split_col_idx)] <- FALSE
      split_col <- split_col[split_col_idx]
    }

    # if multiple split vote columns, take the one where the string starts
    # with the phrase and ends with the phrase
    # if multiple split vote columns, take the one where the string starts
    # with the phrase
    if (length(split_col) > 1) {
      split_col_idx <- lapply(split_col, function(z) {
        stringr::str_detect(
          string = data %>% dplyr::pull(!!z),
          pattern = sprintf("^%s$", phrase)
        ) %>%
          any()
      }) %>%
        unlist()
      split_col_idx[is.na(split_col_idx)] <- FALSE
      split_col <- split_col[split_col_idx]
    }

    # loop over the split cols
    for (c_col_idx in seq_along(split_col)) {

      # the vector of split votes
      split_vec <- data %>%
        dplyr::slice(idx:nrow(data)) %>%
        dplyr::pull(var = split_col[c_col_idx])

      # check that the 2nd element in the vector starts with the number 1
      if (grepl(x = split_vec[2], pattern = "^1")) {

        # assign split vote to current vote
        data$split_vote[idx] <- TRUE

        # set this row to be removed
        data$row_remove[idx] <- TRUE

        # loop over the vector of split votes and stop if element does not
        # start with a number or if the number is not adjacent to the previous
        # number
        split_stop <- FALSE
        split_idx <- 2L
        while (!split_stop) {
          # element cannot be NA
          if (!is.na(split_vec[split_idx])) {
            # split number detected
            if (stringr::str_detect(
              string = split_vec[split_idx], pattern = "^\\d+"
            )) {
              # assign split vote
              data$split_vote[idx + split_idx - 1] <- TRUE
              # get the split vote number
              split_num <- stringr::str_extract(
                string = split_vec[split_idx],
                pattern = "^\\d+"
              )
              # add the split vote number to the vote subject
              data$vote_subject[idx + split_idx - 1] <- paste0(
                data$vote_subject[idx + split_idx - 1], "/",
                split_num
              )
            } else {
              split_stop <- TRUE
            }
          } else {
            split_stop <- TRUE
          }
          # stop condition if split_idx is the last element in the vector
          if (split_idx == length(split_vec)) split_stop <- TRUE
          split_idx <- split_idx + 1L
        } # end of while loop
      } # end condition: check that the 2nd element in the vector
      # starts with the number 1
    } # end of loop over remaining split vote columns

  } # end condition: is split vote
  return(data)
}