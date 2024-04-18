#' Parse vote data from a directory
#'
#' This function parses vote data from a directory and saves the parsed data.
#'
#' @param data_dir The path to the directory containing the vote data.
#' A character string.
#' @param overwrite A logical value indicating whether to overwrite existing
#' parsed data.
#' @param verbose A logical value indicating whether to print progress messages.
#' @param start_date The start date for filtering the vote data. A character
#' string.
#' @param end_date The end date for filtering the vote data. A character string.
#' @param cores The number of CPU cores to use for parallel processing.
#' An integer.
#' @return Writes parsed vote data to the parsed/votes.RData file, votes_meta
#' data to the parsed/votes_meta.RData file, and vote requests data to the
#' parsed/vote_requests.RData file.
#' @examples
#' parse_votes(
#'  data_dir = "C:/Users/Philipp/Documents/EP 7/",
#'  overwrite = FALSE,
#'  verbose = TRUE,
#'  start_date = "14-07-2009",
#'  end_date = "30-06-2014",
#'  cores = 13)
parse_votes <- function(
  data_dir, overwrite = FALSE, verbose = TRUE,
  start_date = NULL, end_date = NULL, cores = 1
) {

  # initial binding for globals
  `name_raw/votes` <- ep <- filename <- links <- parsed <- vote_date <- NULL # nolint
  vote_id <- date_tally <- vote_item_id <- text_tabled_id <- NULL
  within_item_id <- vote_requests <- votes_new <- votes_meta_new <- NULL
  vote_requests_new <- y <- requests <- NULL

  # Load history ------------------------------------------------------------

  # load list of actual files downloaded
  files_actual <- list.files(
    path = sprintf("%sraw", data_dir),
    pattern = "docx$"
  )

  # Register cores
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)

  # progress bar
  pb <- utils::txtProgressBar(max = length(files_actual), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  # Get all objects in the parent environment
  all_objects <- ls(parent.frame())

  # Find which ones are functions
  f_exp <- all_objects[sapply(
    all_objects, function(x) is.function(get(x, envir = parent.frame()))
  )]

  # loop over votes
  votes <- foreach::foreach(
    x = seq_along(files_actual), .options.snow = opts, .export = f_exp
  ) %dopar% {

    `%>%` <- dplyr::`%>%`

    # data for current loop iteration
    x <- files_actual[x]
    class(x) <- "doc.old"

    # parse vote results
    votes_out <- try(
      expr = parse_vote_results(
        x = x,
        data_dir = data_dir,
        verbose = verbose
      ),
      silent = TRUE
    )

    # new improved xml strucutre
    if ("try-error" %in% class(votes_out)) {
      # xml file extension
      if (
        stringr::str_detect(
          string = x,
          pattern = "xml$"
        )
      ) {
        class(x) <- c(class(x), "xml_v2")
        votes_out <- search_votes(
          x = x,
          data_dir = data_dir,
          verbose = verbose
        )
      }
    }

    # from list generate tables
    # votes meta data
    votes_meta <- votes_out$votes_meta
    if ("list" %in% class(votes_meta)) {
      votes_meta <- votes_meta[[1]]
    }

    # votes (the amendment-level tables)
    votes <- votes_out$votes %>%
      tibble::tibble(data = .) %>%
      tidyr::unnest(cols = data)

    # votes are stored as a list with each session as a data frame
    if (ncol(votes) == 1) {

      # rename column votes to data if it exists
      if (names(votes) == "votes") names(votes) <- "data"

      # votes must contain the same number of rows as votes_meta has topics
      if (nrow(votes_meta) != nrow(votes)) {
        stop(
          "Number of rows in votes_meta and votes do not match in ",
          x
        )
      }

      # each table in votes must contain at least one row
      check_votes <- lapply(seq_len(nrow(votes)), function(y) {
        # row in data has content
        content <- any(!is.na(votes$data[[y]]))
        return(content)
      }) %>%
        unlist()
      if (any(!check_votes)) {
        stop(
          "Some tables in votes do not contain any content in ",
          x
        )
      }
      votes <- tidyr::unnest(data = votes, cols = data)
    } else {
      # unique topics in votes
      vote_topics <- votes$date_tally %>%
        unique()
      # votes meta must contain the same number of topics as votes
      if (nrow(votes_meta) != length(vote_topics)) {
        stop(
          "Number of rows in votes_meta and votes do not match in ",
          x
        )
      }
      # each topic in votes must contain some content
      votes <- votes %>%
        dplyr::group_by(date_tally)
      # group indices
      grp_idx <- dplyr::group_indices(.data = votes)
      # number of groups
      n_grps <- dplyr::n_groups(x = votes)
      # ungroup votes
      votes <- dplyr::ungroup(votes)
      # each topic in votes must contain some content
      check_votes <- lapply(1:n_grps, function(y) {
        # topic in data has content
        content <- dplyr::slice(.data = votes, which(grp_idx == y))
        content <- any(!is.na(content))
        return(content)
      }) %>%
        unlist()
      if (any(!check_votes)) {
        stop(
          "Some tables in votes do not contain any content in ",
          x
        )
      }
    }

    # commented out because requests aren't implemeted for EP 5
    # vote_requests <- tryCatch(
    #   expr = {
    #     votes_out$requests %>%
    #       tibble::tibble(data = .) %>%
    #       tidyr::unnest(cols = data) %>%
    #       tidyr::unnest(cols = data) %>%
    #       dplyr::mutate(vote_item_id = NA_character_)
    #   },
    #   error = function(e) {
    #     votes_out$requests[[1]] %>%
    #       dplyr::mutate(vote_item_id = NA_character_) %>%
    #       dplyr::rename(data = requests)
    #   }
    # )

    # if ("data" %in% colnames(vote_requests)) {
    #   vote_requests <- vote_requests %>%
    #     dplyr::relocate(vote_item_id, .before = data) %>%
    #     dplyr::mutate(ep = unique(votes_meta$ep)) %>%
    #     dplyr::relocate(ep, .after = vote_item_id)

    #   if (any(!is.na(vote_requests$data))) {
    #     # loop over vote_requests and generate vote_item_id
    #     vote_item_id_ex <- lapply(
    #       seq_len(nrow(vote_requests)), function(y) {
    #         c_dat <- dplyr::slice(.data = vote_requests, y)
    #         c_dat <- c_dat$data[[1]]
    #         if ("list" %in% class(c_dat)) {
    #           c_dat <- c_dat[[1]]
    #         }
    #         vote_item_id <- suppressWarnings(
    #           paste0(c_dat$vote_date, "_", c_dat$date_tally) %>%
    #             unique()
    #         )
    #         if (vote_item_id == "_") vote_item_id <- NA_character_
    #         if (is.null(vote_item_id)) {
    #           vote_item_id <- NA_character_
    #         }
    #         return(vote_item_id)
    #       }
    #     ) %>%
    #       unlist()
    #   } else {
    #     vote_item_id_ex <- NA_character_
    #   }
    # } else if ("vote_date" %in% colnames(vote_requests)) {
    #   vote_requests <- vote_requests %>%
    #     dplyr::relocate(vote_item_id, .before = vote_date) %>%
    #     dplyr::mutate(ep = unique(votes_meta$ep)) %>%
    #     dplyr::relocate(ep, .after = vote_item_id)

    #   vote_item_id_ex <- paste0(
    #     vote_requests$vote_date, "_", vote_requests$date_tally
    #   )
    #   vote_item_id_ex[vote_item_id_ex == "NA_NA"] <- NA_character_
    # }

    # # add vote_item_id to vote_requests
    # vote_requests <- vote_requests %>%
    #   dplyr::mutate(vote_item_id = vote_item_id_ex) %>%
    #   dplyr::filter(!is.na(vote_item_id))

    # on error, return current vote date
    if ("try-error" %in% class(votes_out)) {
      stop(x)
    }

    return(votes_out = list(
      votes_meta = votes_meta,
      votes = votes,
      vote_requests = NULL # not implemented for EP 5
    ))

  }
  # De-register cluster
  close(pb)
  parallel::stopCluster

  # data tables combined
  votes_meta <- do.call("rbind", votes)[, "votes_meta"] %>%
    dplyr::bind_rows()
  vote_requests <- do.call("rbind", votes)[, "vote_requests"]
  votes <- do.call("rbind", votes)[, "votes"] %>%
    dplyr::bind_rows()

  # add unique IDs
  votes_meta <- votes_meta %>%
    dplyr::mutate(vote_item_id = paste0(vote_date, "_", date_tally)) %>%
    dplyr::relocate(vote_item_id, .before = ep)

  if (!"text_tabled_id" %in% colnames(votes)) {
    votes <- votes %>%
      dplyr::mutate(text_tabled_id = NA_character_) %>%
      dplyr::relocate(text_tabled_id, .before = vote_date)
  }

  votes <- votes %>%
    dplyr::mutate(vote_item_id = paste0(vote_date, "_", date_tally)) %>%
    dplyr::relocate(vote_item_id, .before = text_tabled_id) %>%
    dplyr::group_by(vote_item_id) %>%
    dplyr::mutate(within_item_id = seq_len(dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vote_id = paste0(vote_item_id, "_", within_item_id)) %>%
    dplyr::relocate(vote_id, .before = vote_item_id) %>%
    dplyr::select(-within_item_id) %>%
    dplyr::mutate(ep = unique(votes_meta$ep)) %>%
    dplyr::relocate(ep, .before = vote_date)

  # always overwrite
  # # load existing parsed votes data
  # if (file.exists(sprintf("%sparsed/votes.RData", data_dir))) {

  #   # copies of newly parsed datasets
  #   votes_new <- votes
  #   votes_meta_new <- votes_meta
  #   vote_requests_new <- vote_requests

  #   # load data (object names "votes", "votes_meta", "vote_requests")
  #   load(file = sprintf("%sparsed/votes.RData", data_dir))
  #   load(file = sprintf("%sparsed/votes_meta.RData", data_dir))
  #   load(file = sprintf("%sparsed/vote_requests.RData", data_dir))

  #   message("Update votes, votes_meta and vote_requests with new data")
  #   if (!overwrite) {

  #     # remove vote_id that from new data that are already in old data
  #     votes_new <- votes_new %>%
  #       dplyr::filter(! vote_id %in% votes$vote_id)

  #     # combine previous session data with current session data
  #     votes <- votes %>%
  #       dplyr::bind_rows(votes_new) %>%
  #       dplyr::distinct()

  #     # remove vote_item_id that from new data that are already in old data
  #     votes_meta_new <- votes_meta_new %>%
  #       dplyr::filter(! vote_item_id %in% votes_meta$vote_item_id)

  #     votes_meta <- votes_meta %>%
  #       dplyr::bind_rows(votes_meta_new) %>%
  #       dplyr::distinct()

  #     # vote requests are a list
  #     # Register cores
  #     cl <- parallel::makeCluster(cores)
  #     doSNOW::registerDoSNOW(cl)

  #     # progress bar
  #     pb <- txtProgressBar(max = length(vote_requests_new), style = 3)
  #     progress <- function(n) setTxtProgressBar(pb, n)

  #     # loop over vote requests
  #     vote_requests_new <- foreach::foreach(y = seq_along(vote_requests_new),
  #       .options.snow = opts, packages = "StREPscrape"
  #     ) %dopar% {

  #       `%>%` <- dplyr::`%>%`

  #       # current vote request
  #       c_request <- vote_requests_new[[y]]

  #       # check whether the date is in the request data
  #       if (!"vote_date" %in% colnames(c_request)) {
  #         c_request <- c_request %>%
  #           dplyr::pull(data) %>%
  #           tibble::tibble(data = .) %>%
  #           tidyr::unnest(cols = data)
  #       }
  #       old_req_date <- c_request$vote_date %>%
  #         .[!is.na(.)] %>%
  #         unique()
  #       # check whether old request date is in new list
  #       is_in_new_data <- suppressWarnings(
  #         lapply(seq_along(vote_requests), function(z) {
  #           # current new vote request
  #           z_request <- vote_requests[[z]]
  #           if (! "vote_date" %in% colnames(z_request)) {
  #             z_request <- z_request %>%
  #               dplyr::pull(data) %>%
  #               tibble::tibble(data = .) %>%
  #               tidyr::unnest(cols = data)
  #           }
  #           new_req_date <- z_request$vote_date %>%
  #             .[!is.na(.)] %>%
  #             unique()
  #           date_found <- old_req_date %in% new_req_date
  #           return(date_found)
  #         }) %>%
  #           unlist() %>%
  #           any()
  #       )
  #       if (is_in_new_data) {
  #         c_request <- NULL
  #       }
  #       if (all(is.na(c_request))) {
  #         c_request <- NULL
  #       }
  #       return(c_request)
  #     }

  #     # close progress bar and de-register cluster
  #     close(pb)
  #     parallel::stopCluster

  #     # combine old and new lists
  #     vote_requests <- c(vote_requests_new, vote_requests) %>%
  #       purrr::discard(is.null)

  #   } else {
  #     # filter out newly parsed content
  #     # if old votes not in newly parsed has 0 rows, only use new votes,
  #     # otherwise combine
  #     if (
  #       votes %>%
  #         dplyr::filter(!vote_id %in% votes_new$vote_id) %>%
  #         nrow() == 0
  #     ) {
  #       votes <- votes_new %>%
  #         dplyr::distinct()
  #     } else {
  #       votes <- votes %>%
  #         dplyr::filter(!vote_id %in% votes_new$vote_id) %>%
  #         dplyr::bind_rows(votes_new) %>%
  #         dplyr::distinct()
  #     }
  #     votes_meta <- votes_meta %>%
  #       dplyr::filter(! vote_item_id %in% votes_meta_new$vote_item_id) %>%
  #       dplyr::bind_rows(votes_meta_new) %>%
  #       dplyr::distinct()

  #     # vote requests are a list
  #     # Register cores
  #     cl <- parallel::makeCluster(cores)
  #     doSNOW::registerDoSNOW(cl)

  #     # progress bar
  #     pb <- txtProgressBar(max = length(vote_requests), style = 3)
  #     progress <- function(n) setTxtProgressBar(pb, n)

  #     # loop over vote requests
  #     vote_requests <- foreach::foreach(
  #       y = seq_along(vote_requests),
  #       .options.snow = opts
  #     ) %dopar% {

  #       `%>%` <- dplyr::`%>%`

  #       # current vote request
  #       c_request <- vote_requests[[y]]

  #       # check whether the date is in the request data
  #       if (!"vote_date" %in% colnames(c_request)) {
  #         c_request <- c_request %>%
  #           dplyr::pull(data) %>%
  #           tibble::tibble(data = .) %>%
  #           tidyr::unnest(cols = data)
  #       }
  #       old_req_date <- c_request$vote_date %>%
  #         .[!is.na(.)] %>%
  #         unique()
  #       # check whether old request date is in new list
  #       is_in_new_data <- suppressWarnings(
  #         lapply(seq_along(vote_requests_new), function(z) {
  #           # current new vote request
  #           z_request <- vote_requests_new[[z]]
  #           if (! "vote_date" %in% colnames(z_request)) {
  #             z_request <- z_request %>%
  #               dplyr::pull(data) %>%
  #               tibble::tibble(data = .) %>%
  #               tidyr::unnest(cols = data)
  #           }
  #           new_req_date <- z_request$vote_date %>%
  #             .[!is.na(.)] %>%
  #             unique()
  #           date_found <- old_req_date %in% new_req_date
  #           return(date_found)
  #         }) %>%
  #           unlist() %>%
  #           any()
  #       )
  #       if (is_in_new_data) {
  #         c_request <- NULL
  #       }
  #       if (all(is.na(c_request))) {
  #         c_request <- NULL
  #       }
  #       return(c_request)
  #     }

  #     # close progress bar and de-register cluster
  #     close(pb)
  #     parallel::stopCluster

  #     # combine old and new lists
  #     vote_requests <- c(vote_requests, vote_requests_new) %>%
  #       purrr::discard(is.null)

  #   }
  #   rm(votes_new, votes_meta_new, vote_requests_new)
  # }

  # check for duplicates ----------------------------------------------------
  # vote tables with completely identical duplicates are removed
  # this needs the minutes
  #votes <- duplicate_votes(votes = votes)

  # save
  save(votes, file = sprintf("%sparsed/votes.RData", data_dir))
  save(votes_meta, file = sprintf("%sparsed/votes_meta.RData", data_dir))
  save(
    vote_requests,
    file = sprintf("%sparsed/vote_requests.RData", data_dir)
  )

}