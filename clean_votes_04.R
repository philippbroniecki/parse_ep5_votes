clean_votes_04 <- function(data_dir, verbose = TRUE) {

  # initial binding of globals
  vote_item_id <- vote_subject <- vote_level <- NULL

  # canonical data dir
  data_dir <- canonical_data_dir(data_dir = data_dir)

  # load data ---------------------------------------------------------------

  # 1) all votes data (object name: votes)
  load(sprintf("%sparsed/votes.RData", data_dir))

  # 2) vote requests data (identifies the blocks)
  load(sprintf("%sparsed/vote_requests.RData", data_dir))

  # remove empty tables from vote requests
  vote_requests <- vote_requests[
    sapply(vote_requests, function(x) return(nrow(x) > 0))
  ]
  # combine list elements to tibble
  vote_requests <- dplyr::bind_rows(vote_requests)

  # remove missing vote_item_id rows
  vote_requests <- dplyr::filter(
    vote_requests, !is.na(vote_item_id)
  )

  # group ids_missing data by name and party_group
  votes <- votes %>%
    dplyr::group_by(vote_item_id)

  # group data by vote_item_id
  grp_idx <- dplyr::group_indices(.data = votes)

  # number of groups
  n_grps <- dplyr::n_groups(x = votes)

  # ungroup the data
  votes <- dplyr::ungroup(votes)

  # create progress bar
  if (verbose && n_grps > 1) {
    pb <- utils::txtProgressBar(min = 1, max = n_grps, style = 3)
  }

  # loop over groups (session items)
  votes <- lapply(seq_len(n_grps), function(c_idx) {

    message(c_idx)

    # current group
    c_dat <- dplyr::slice(.data = votes, which(grp_idx == c_idx))

    # remove NAs from the vote subject
    c_dat <- c_dat %>%
      dplyr::mutate(vote_subject = stringr::str_remove(
        string = vote_subject, pattern = "^NA, "
      ))

    # add the vote level if missing
    # Amendment
    c_dat <- c_dat %>%
      dplyr::mutate(vote_level = ifelse(
        test = is.na(vote_level) & stringr::str_detect(
          string = vote_subject, pattern = "^Am"
        ),
        yes = "amendment",
        no = vote_level
      ))

    # corresponding vote request data
    r_dat <- dplyr::filter(
      vote_requests, vote_item_id == c_dat$vote_item_id %>% unique()
    )

    # check the format of the request table
    if (nrow(r_dat) > 0) {
      if ("list" %in% class(r_dat$data[[1]])) {
        r_dat <- r_dat %>%
          tidyr::unnest_longer(col = data)
      }
    }

    # loop over r_dat rows to find the table that identifies block votes
    block_col <- lapply(seq_len(nrow(r_dat)), function(r_idx) {

      # make sure that data is column in r_dat that contains info
      if (is.null(r_dat$data[[r_idx]])) {
        r_dat$data[[r_idx]] <- r_dat %>%
          dplyr::slice(!!r_idx)
      }

      # sequence along the columns of the request
      col_seq <- ncol(r_dat$data[[r_idx]]) %>%
        seq_len()

      # character columns of the dataset
      char_cols <- which(unlist(lapply(X = col_seq, function(y) {
        is.character(dplyr::pull(.data = r_dat$data[[r_idx]], var = y))
      })))

      # check the chracter columns for a reference to a block vote
      is_block <- lapply(char_cols, function(y) {

        # check for the phrase Block \\d =
        block_vote <- stringr::str_detect(
          string = r_dat$data[[r_idx]][, y] %>% paste(collapse = " "),
          pattern = "Block\\s?\\d+\\s?\\="
        )

        # check for the phrase Block \\d+ :
        if (!block_vote) {
          block_vote <- stringr::str_detect(
            string = r_dat$data[[r_idx]][, y] %>% paste(collapse = " "),
            pattern = "Block\\s?\\d+\\s?\\="
          )
        }
        return(block_vote)
      }) %>%
        unlist()

      # there should be only one block vote column
      if (sum(is_block) > 1) {
        stop("More than one block vote column found")
      }

      block_idx <- char_cols[is_block]
      return(block_out = list(
        is_block = any(is_block), block_col = block_idx
      ))
    })

    # table number that contains block vote IDs
    if (length(block_col) > 0) {

      tab_num <- do.call("rbind", block_col)[, "is_block"] %>%
        unlist() %>%
        which()

      # block vote table identified
      if (length(tab_num) > 0) {

        # column number that contains block vote IDs
        col_num <- do.call("rbind", block_col)[, "block_col"] %>%
          unlist()

        # get the block vote identifiers
        block_data <- r_dat$data[[tab_num]][, col_num] %>%
          as.character()

        # separate the block identifiers on block identifiying phrase
        # for now that is "Block"
        block_data <- stringr::str_split(
          string = block_data, pattern = "Block "
        ) %>%
          unlist() %>%
          .[. != ""]

        # loop over the blocks (elements of block_data)
        for (b_idx in seq_along(block_data)) {

          # current block data
          b_dat <- block_data[b_idx]

          # current block number
          b_num <- stringr::str_extract(
            string = b_dat, pattern = "^\\d+"
          ) %>%
            as.integer()

          # remove block number from block data
          b_dat <- b_dat %>%
            stringr::str_remove(pattern = "^\\d+") %>%
            stringr::str_trim(side = "both") %>%
            stringr::str_remove(pattern = "=") %>%
            stringr::str_trim(side = "both")

          if (stringr::str_detect(string = b_dat, pattern = "Amendments\\:")) {
            block_type <- "Am"
          } else {
            stop("Block type not identified")
            # block type
            block_type <- NA_character_
          }

          # individual parts of the block vote
          block_parts <- stringr::str_extract_all(
            string = b_dat, pattern = "\\d+"
          ) %>%
            unlist()

          # character columns of the dataset
          char_cols <- lapply(X = seq_len(ncol(c_dat)), FUN = function(y) {
            is.character(dplyr::pull(.data = c_dat, var = y))
          }) %>%
            unlist() %>%
            which()

          # loop over c_dat and identify the row that contains the current block
          row_idx <- lapply(seq_len(nrow(c_dat)), function(z) {

            # character string of all columns
            char_string <- paste(c_dat[z, char_cols], collapse = " ")

            # check whether the row contains the current block
            is_block <- stringr::str_detect(
              string = char_string, pattern = paste0("\\bBlock ", b_num, "\\b")
            )
            return(is_block)
          }) %>%
            unlist() %>%
            which()

          # add block to vote subjects
          c_dat[row_idx, "vote_subject"] <- sprintf(
            "%s %s", block_type, paste(block_parts, collapse = ", ")
          )

        } # end of loop over blocks
      } # end condition block vote table identified
    } # request table present

    # update progress bar
    if (verbose && n_grps > 1) {
      utils::setTxtProgressBar(pb = pb, value = c_idx)
    }

    return(c_dat)
  }) # end of loop over groups (session items)

  votes <- dplyr::bind_rows(votes)

  # close progress bar
  if (verbose && n_grps > 1) {
    close(pb)
  }

  # save data
  save(votes, file = sprintf("%sparsed/votes.RData", data_dir))
}
