clean_votes_01 <- function(data_dir, overwrite = FALSE, verbose = FALSE) {

  # inital binding for global variables
  vote_id <- oeil_id <- text_tabled_id <- vote_level <- vote_subject <- NULL

  # canonical data_dir
  data_dir <- canonical_data_dir(data_dir = data_dir)

  # load data ---------------------------------------------------------------

  # 1) votes data (object name: votes)
  load(file = sprintf("%sparsed/votes.RData", data_dir))

  # remove the date_tally variable. The session topic is identified by the
  # vote_item_id variable
  votes$date_tally <- NULL

  # check the vote variable in votes -----------------------------------------
  # recode vote variable
  votes$vote[votes$vote == "—"] <- "-"
  votes$vote[votes$vote == "↓"] <- "lapsed"
  votes$vote[votes$vote == "W"] <- "withdrawn"

  # check the votes where the vote variable is not +, -, or lapsed
  chk_ids <- votes$vote_id[!(
    votes$vote %in% c("+", "-", "lapsed", "withdrawn")
  )]
  votes_chk <- votes %>%
    dplyr::filter(vote_id %in% chk_ids)

  # loop over the votes
  votes_chk <- lapply(seq_len(nrow(votes_chk)), FUN = function(y) {

    # current vote to check
    c_chk <- dplyr::slice(.data = votes_chk, !!y)

    # check wehther rcv_etc is missing
    if (is.na(c_chk$rcv_etc)) {
      # check whether the vote is a RCV
      rcv_string <- stringr::str_detect(
        string = c_chk, pattern = stringr::fixed("RCV")
      )
      rcv_string[is.na(rcv_string)] <- FALSE
      c_chk$rcv_etc <- ifelse(
        test = any(rcv_string), yes = "RCV", no = NA_character_
      )
    }

    # check for vote outcome
    vote_string <- stringr::str_detect(
      string = c_chk, pattern = "^\\s*([-+])\\s*$"
    )
    vote_string[is.na(vote_string)] <- FALSE

    # check what is currently stored in the vote variable
    vote_tmp <- c_chk$vote

    # assign the vote outcome to the vote variable if detected
    c_chk$vote <- ifelse(
      test = any(vote_string),
      yes = c_chk[vote_string] %>% unlist,
      no = NA_character_
    )

    # store vote_tmp in extra column
    c_chk$vote_tmp <- vote_tmp

    return(c_chk)
  })

  # combine list to tibble
  votes_chk <- dplyr::bind_rows(votes_chk)

  # replace in votes
  votes <- votes %>%
    dplyr::filter(!vote_id %in% chk_ids) %>%
    dplyr::bind_rows(votes_chk) %>%
    dplyr::arrange(vote_id)

  # add texts tabled and RCVs combined identifiers to votes data ------------
  votes <- votes %>%
    dplyr::mutate(
      oeil_id = NA_character_,
      vote_level = NA_character_,
      vote_subject = NA_character_
    ) %>%
    dplyr::relocate(oeil_id, .after = text_tabled_id) %>%
    dplyr::relocate(vote_level, .after = oeil_id) %>%
    dplyr::relocate(vote_subject, .after = vote_level)

  # character columns of the dataset
  char_cols <- which(unlist(lapply(X = seq_len(ncol(votes)), FUN = function(y) {
    is.character(dplyr::pull(.data = votes, var = y))
  })))

  add_ids_to_votes <- function(votes) {

    # create progress bar
    pb <- utils::txtProgressBar(min = 1, max = nrow(votes), style = 3)

    for (idx in seq_len(nrow(votes))) {

      # OEIL IDs
      # extract columns that contain text id string
      id_string <- paste(votes[idx, char_cols], collapse = " ")
      if ("character" != class(id_string)) {
        stop("id_string is not character at idx ", idx)
      }

      # detect OEIL ID
      if (stringr::str_detect(
        string = id_string,
        pattern = "\\d{4}/\\d{4}\\([A-Z]+\\)"
      )) {
        oeil_id <- stringr::str_extract(
          string = id_string,
          pattern = "\\d{4}/\\d{4}\\([A-Z]+\\)"
        )
        votes[idx, ]$oeil_id <- oeil_id
      }

      # vote type

      # extracts the vote type applying the same categories as in rcv_meta
      c_vote_type <- add_rcv_type_var(desc_text = id_string)

      # add the vote type variable
      votes[idx, ]$vote_level <- c_vote_type

      # vote subject
      c_vote_subject <- NA
      # does the vote pertain to a paragraph?
      if (stringr::str_detect(pattern = "§\\s+[0-9]+", string = id_string)) {
        # column number which contains the paragraph symbol
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "§\\s+[0-9]+"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
        }
      } # end of if condition: detect the paragraph symbol

      # does the vote pertain to a recital?
      if (grepl(pattern = "Recital", x = id_string)) {
        # column number which contains the Recital
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "Recital"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
        }
      } # end of if condition: detect the word Recital

      # does the vote pertain to a After recital?
      if (grepl(pattern = "recital", x = id_string)) {
        # column number which contains the recital
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "recital"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
        }
      } # end of if condition: detect the word recital

      # does the vote pertain to a citation?
      if (grepl(pattern = "Citation\\s+[0-9]+", x = id_string)) {
        # column number which contains the Citation
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "Citation\\s+[0-9]+"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
        }
      } # end of if condition: detect Citation followed by space and digit(s)

      # does the vote pertain to content after a citation?
      if (grepl(pattern = "citation\\s+[0-9]+", x = id_string)) {
        # column number which contains the Citation
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "citation\\s+[0-9]+"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
        }
      } # end of if condition: detect citation lower case plus
      # space and digit(s)

      # does the vote pertain to a block vote?
      if (grepl(pattern = "Block/RCV\\s\\d+", x = id_string)) {
        # column number which contains the block vote
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "Block/RCV\\s\\d+"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
          # remove the string ?RCV
          c_vote_subject <- stringr::str_remove(
            string = c_vote_subject, pattern = "/RCV"
          )
        }
      }

      # does the vote pertain to a separate vote vote?
      if (grepl(pattern = "sep", x = id_string)) {
        # column number which contains the block vote
        c_col <- stringr::str_which(
          string = votes[idx, char_cols], pattern = "sep"
        )
        if (length(c_col) != 0) {
          c_vote_subject <- paste(
            votes[idx, char_cols][, c_col], collapse = " "
          )
          # remove the string ?RCV
          c_vote_subject <- stringr::str_remove(
            string = c_vote_subject, pattern = "/RCV"
          )
        }
      }

      # detect article and point combination
      if (length(c_vote_type) != 0) {
        if (!is.na(c_vote_type)) {
          if (c_vote_type == "article") {
            # does the vote refer to a specific article and point
            if (grepl(pattern = "Article", x = id_string) && grepl(
              pattern = "point", x = id_string
            )) {
              # column number which contains article and point
              c_col1 <- stringr::str_which(
                string = votes[idx, char_cols], pattern = "Article"
              )
              c_col2 <- stringr::str_which(
                string = votes[idx, char_cols], pattern = "point"
              )

              # if both are in the same column
              if (any(c_col1 %in% c_col2)) {
                c_vote_subject <- paste(
                  votes[idx, char_cols][, c_col1[c_col1 %in% c_col2]],
                  collapse = " "
                )
              }
            }
          }
        }
      }

      # detect and attach amendment number if present
      if (grepl(pattern = "^\\d+$", x = votes[idx, ]$am_no)) {
        c_am_no <- stringr::str_extract(
          string = votes[idx, ]$am_no,
          pattern = "^\\d+$"
        )
        c_vote_subject <- paste0(c_vote_subject, ", Am ", c_am_no)
      } # end of if condition: am_no variable contains only digits


      # add vote subject
      votes[idx, ]$vote_subject <- c_vote_subject

      # progress
      utils::setTxtProgressBar(pb = pb, value = idx)

    } # end of loop over votes
    return(votes)

    # close progress bar
    close(pb)

  } # end of add_ids_to_votes() function

  # add ID variables to votes
  votes <- add_ids_to_votes(votes = votes)

  # save votes data
  save(... = votes, file = sprintf("%sparsed/votes.RData", data_dir))

}
