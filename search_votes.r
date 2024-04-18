# generic: search votes ---------------------------------------------------
#' @export
search_votes <- function(x, data_dir, verbose, ...) {
  UseMethod("search_votes", x)
}


# doc.old method: search votes --------------------------------------------
#' @export
search_votes.doc.old <- function(x, data_dir, verbose, ...) { # nolint

  # initial binding for globals
  vote_date <- date_tally <- NULL

  # current R-session tmp directory
  tmpd <- tempdir()

  # add a process specific sub-directory for parallel processing
  tmpd <- sprintf("%s/%s", tmpd, Sys.getpid())

  # generate directory in R's temporary directory
  suppressWarnings(dir.create(path = tmpd))

  # clean-up
  on.exit({
    unlink(sprintf("%s/docdata", tmpd), recursive = TRUE)
    unlink(sprintf(paste0("%s/", x), tmpd))
  })

  # copy .docx file to tmp dir
  fs::file_copy(
    path = sprintf("%sraw/%s", data_dir, x),
    new_path = tmpd,
    overwrite = TRUE
  )

  # unzip it
  utils::unzip(
    zipfile = sprintf("%s/%s", tmpd, x),
    exdir = sprintf("%s/docdata", tmpd)
  )

  # load xml version of the document
  page <- xml2::read_xml(
    x = sprintf("%s/docdata/word/document.xml", tmpd),
    encoding = "UTF-8"
  )

  # extract the namespace
  ns <- xml2::xml_ns(x = page)

  # all table nodes in the document
  tbls <- page %>%
    xml2::xml_find_all(xpath = ".//w:tbl")

  # prepare docx object for docxtractr::docx_extract_tbl() function
  docx <- list(tbls = tbls, ns = ns, cmnts = NULL, path = NULL, docx = NULL)
  class(docx) <- "docx"

  # meta data ---------------------------------------------------------------

  # hard-coded titles (returns NULL if not hard coded)
  #titles <- hard_coded_titles(x = x)
  titles <- NULL

  # extract the title from an old word document
  if (is.null(titles)) titles <- extract_title_doc_old(page = page)
  if ("list" %in% class(titles)) titles <- unlist(titles)

  # this is no longer needed: Delete it and the function
  # add escape characters to special characters
  # titles_regex <- escape_special_chars(x = titles) # nolint

  # find all tables (node indexes)
  tbl_idxs <- suppressWarnings(
    page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      as.character(.) %>%
      stringr::str_detect(pattern = "w:tbl") %>%
      which
  )

  # extract all nodes that match //w:p or //w:tbl
  t_pos <- page %>%
    xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
    rvest::html_text(trim = TRUE)

  # find exact matches for the titles
  t_pos <- t_pos_fun(titles = titles, t_pos = t_pos, tbl_idxs = tbl_idxs)

  # Attempt to adjudicate between multiple exact matches for the title position
  t_pos <- clean_title_position(y = t_pos)

  # flatten list
  t_pos <- unlist(t_pos)

  # vote descriptions text
  if (exists("vote_descriptions")) rm(vote_descriptions)
  vote_descriptions <- search_descriptions(
    page = page,
    t_pos = t_pos
  )

  # hard coding
  # vote_descriptions <- hard_coded_descriptions(
  #   vote.descriptions = vote_descriptions, x = x
  # )

  # message about number of votes in the document
  if (verbose) {
    message(
      "\n", "Document: ", x,
      " contains ", length(titles), " topics"
    )
  }

  # current date
  #c_date <- dplyr::pull(.data = x, var = date)
  message("date not implemented without minutes")
  c_date <- "not implemented, no minutes"

  # voting meta data
  votes_meta <- dplyr::tibble(
    ep = 5L,
    vote_date = c_date,
    date_tally = seq_along(titles),
    title = titles,
    description = vote_descriptions
  )

  # individual level data ---------------------------------------------------
  # scrape tables following titles

  # containers for vote tables and vote request tables
  votes <- vector(mode = "list", length = nrow(votes_meta))
  requests <- vector(mode = "list", length = nrow(votes_meta))

  # loop over the votes
  for (idx in seq_along(t_pos)) {

    ## which tables are within the current title range
    # not the last title
    if (idx < length(t_pos)) {
      tbl_number <- which(tbl_idxs >= t_pos[idx] & t_pos[idx + 1] > tbl_idxs)
    } else { # otherwise
      tbl_number <- which(tbl_idxs >= t_pos[idx])
    }

    # in case of no match try with second condition >=
    if (length(tbl_number) == 0) {
      # not the last title
      if (idx < length(t_pos)) {
        tbl_number <- which(tbl_idxs >= t_pos[idx] & t_pos[idx + 1] >= tbl_idxs)
      } else { # otherwise
        tbl_number <- which(tbl_idxs >= t_pos[idx])
      }
    }

    if (length(tbl_number) > 1) {

      # check tables for whether they contain the title only
      drop_tables <- lapply(tbl_number, function(y) {

        # extract the table
        z <- doc_table(docx = docx, tbl_number = y, header = TRUE, ns = ns)
        if (nrow(z) == 0) {
          z <- doc_table(docx = docx, tbl_number = y, header = FALSE, ns = ns)
        }

        # table names
        z_names <- names(z)

        # change "Purpose" to "Subject"
        if ("Purpose" %in% z_names) {
          z_names[z_names == "Purpose"] <- "Subject"
          names(z) <- z_names
        }

        # clean table table names
        z_names <- z_names %>%
          stringr::str_remove_all(pattern = "[^\\w]") %>%
          .[. != ""] %>%
          .[!is.na(.)]

        # remove names that are 2 characters
        z_names <- z_names %>%
          .[which(stringr::str_count(.) > 2)]

        title_clean <- titles %>%
          stringr::str_remove_all(pattern = "[^\\w]") %>%
          stringr::str_remove_all(pattern = "[A-Z]+$") %>%
          stringr::str_remove_all(pattern = "[0-9]+$")

        # hard-coded bits
        if (x == "PV-6-2007-02-14-VOT_EN.doc") {
          title_clean[8] <- "PNRSWIFT"
        }
        if (x == "PV-6-2005-07-06-VOT_EN.doc") {
          title_clean[7] <- "ERDF"
          title_clean[9] <- "ESF"
        }
        if (x == "PV-6-2005-07-07-VOT_EN.doc") {
          title_clean[2] <- "Life"
          title_clean[10] <- "FLEGT"
        }
        if (x == "PV-6-2005-11-17-VOT_EN.doc") {
          title_clean[1] <- "Reach"
        }
        if (x == "PV-6-2006-09-27-VOT_EN.doc") {
          title_clean[8] <- "PROGRESS"
        }
        if (x == "PV-6-2006-09-28-VOT_EN.doc") {
          title_clean[4] <- "GALILEO"
        }
        if (x == "PV-6-2006-10-24-VOT_EN.doc") {
          title_clean[10] <- "LIFE"
        }
        if (x == "PV-6-2006-11-30-VOT_EN.doc") {
          title_clean[23] <- "AIDS"
        }
        if (x == "PV-6-2008-01-17-VOT_EN.doc") {
          title_clean[2] <- "EUROPOL"
        }
        if (x == "PV-7-2009-09-17-VOT_EN.doc"
        ) {
          title_clean[3] <- "SWIFT"
        }

        # check whether the title is in table
        table_contains_title <- lapply(title_clean, function(zz) {
          table_contains_title <- any(
            stringr::str_detect(string = z_names, pattern = zz)
          )
          return(table_contains_title)
        }) %>%
          unlist %>%
          any

        # check the other way around
        if (!table_contains_title) {
          table_contains_title <- try(
            expr = {
              lapply(z_names, function(zz) {
                stringr::str_detect(string = title_clean, pattern = zz) %>%
                  any
              }) %>%
                unlist %>%
                any
            }, silent = TRUE
          )
          if ("try-error" %in% class(table_contains_title)) {
            table_contains_title <- FALSE
          }
        }

        # check whether the table also contains the phrases 'Subject' and 'RCV,
        # etc.'
        # loop over the rows the table
        title_row <- lapply(seq_len(nrow(z)), function(a) {

          # detect Subject
          is_subject <- stringr::str_detect(
            string = as.character(z[a, ]),
            pattern = "Subject"
          )

          # detect RCV etc.
          is_rcv_etc <- stringr::str_detect(
            string = as.character(z[a, ]),
            pattern = "RCV,?\\setc."
          )

          # both 'Subject' and 'RCV etc.' detected
          sum(is_subject, is_rcv_etc) == 2
        }) %>%
          unlist %>%
          which

        # check whether the phrases 'Subject' or 'RCVetc' are in the table
        # heading
        res_table <- any(stringr::str_detect(
          string = z_names, pattern = "Subject"
        )) & any(
          stringr::str_detect(string = z_names, pattern = "RCVetc")
        )

        # drop tables if all conditions are met
        drop_table <- table_contains_title == TRUE & length(title_row) == 0 &
          res_table == FALSE

        # remove tables that are about amendments replacing other amendments
        if ("remplacéparoucommentaires" %in% z_names) drop_table <- TRUE
        if ("replacedbyorcomment" %in% z_names) drop_table <- TRUE

        # drop tables with 0 rows
        if (nrow(z) == 0) drop_table <- TRUE

        return(drop_table)
      }) %>%
        unlist()

      # drop tables
      tbl_number <- tbl_number[!drop_tables]
    } # end of condition: length(tbl_number) > 1

    # variable names if the column names and the table content
    # are separate tables
    var_names <- NULL # nolint

    # hard coding document mistakes
    #vote_tables <- hard_coded_votes(x =  x, idx = idx)
    vote_tables <- NULL

    # run extractor if not hard coded
    if (!"data.frame" %in% class(vote_tables[[1]]$votes)) {
      # extract all tables that are in the title range
      vote_tables <- lapply(X = tbl_number, FUN = function(y) {

        # bug fixing counter
        cat(paste(";", "table #", y))

        # extract table
        z <- doc_table(docx = docx, tbl_number = y, header = TRUE, ns = ns)
        if (nrow(z) == 0) {
          z <- doc_table(docx = docx, tbl_number = y, header = FALSE, ns = ns)
        }

        # hard coded table column names
        #z <- hard_coded_table_column_names(x = x, y = y, z = z)

        # change "Purpose" to "Subject"
        if ("Purpose" %in% names(z)) {
          names_z <- names(z)
          names_z[names_z == "Purpose"] <- "Subject"
          names(z) <- names_z
        }

        if ("Objet" %in% names(z)) {
          names_z <- names(z)
          names_z[names_z == "Objet"] <- "Subject"
          names(z) <- names_z
        }

        # change "Voting" to "Vote"
        if ("Voting" %in% names(z)) {
          names_z <- names(z)
          names_z[names_z == "Voting"] <- "Vote"
          names(z) <- names_z
        }

        # try to identify the first row
        if (!"Subject" %in% names(z) & !"RCV etc." %in% names(z) &
              !"Vote" %in% names(z) & !"Voting" %in% names(z) &
              !"RCV/EV - remarks" %in% names(z) &
              !"MODIFICATIONS TO NOMENCLATURE" %in% names(z) &
              !"Amendment" %in% names(z) &
              !"Requests for roll-call votes" %in% names(z) &
              !"Request for roll-call vote" %in% names(z) &
              !"Requests for separate votes" %in% names(z) &
              !"Request for separate vote" %in% names(z) &
              !"Requests for split votes" %in% names(z) &
              !"Request for split vote" %in% names(z) &
              !"Miscellaneous" %in% names(z)) {

          # loop over the rows the table
          title_row <- lapply(seq_len(nrow(z)), function(a) {

            # detect Subject
            is_subject <- stringr::str_detect(
              string = as.character(z[a, ]),
              pattern = "Subject"
            ) |
              stringr::str_detect(
                string = as.character(z[a, ]),
                pattern = "Objet"
              )

            # detect RCV etc.
            is_rcv_etc <- stringr::str_detect(
              string = as.character(z[a, ]),
              pattern = "RCV,?\\setc."
            )

            # both 'Subject' and 'RCV etc.' detected
            sum(is_subject, is_rcv_etc) == 2
          }) %>%
            unlist %>%
            which

          # subset table to start of title row to end of table
          if (length(title_row) > 0) {

            z <- z[title_row:nrow(z), ]

            # set the first row to the names
            z_names <- as.character(z[1, ])
            names(z) <- z_names
            z <- z[2:nrow(z), ]

            # remove empty columns
            if (any(names(z) == "")) {

              # positions fo columns that might be empty
              del_cols <- which(names(z) == "")

              # check whether there is content further down in the table in that
              # column
              del_cols_check <- lapply(del_cols, function(a) {
                all(as.character(z[, a]) == "")
              }) %>%
                unlist

              # delete columns that are completely empty
              z <- z[, -del_cols[del_cols_check]]

            } # end of condition: any(names(z) == '')

          }
        } # end of condition: names of table to not contain
        # the real table heading

        # assign variable names if they were in a previous table
        if (!is.null(var_names)) names(z) <- var_names

        # if the table contains 0 rows check whether it combines with the next
        # table which happpens if the variable names and the variables are
        # separate tables in word
        if (nrow(z) == 0) {

          # extract table
          z_next <- try(
            expr = {
              z <- doc_table(
                docx = docx, tbl_number = y + 1, header = TRUE, ns = ns
              )
            }, silent = TRUE
          )

          if (! "try-error" %in% class(z_next)) {
            # check whether the number of columns is the same
            if (ncol(z) == ncol(z_next)) {
              # check whether z contains "vote" but z_next does not
              if (("Vote" %in% names(z) | "vote" %in% names(z)) &
                (! "Vote" %in% names(z_next) & !"vote" %in% names(
                  z_next
                ))) {
                var_names <- names(z)
                return(NULL)
              } else {
                # return NULL if the table is empty and does not contain
                # variable names
                return(NULL)
              }
            }
          } else {
            # extract table
            z <- z <- doc_table(
              docx = docx, tbl_number = y, header = FALSE, ns = ns
            )
          }

          # if the table contains content (i.e. more than 0 rows)
        } else {
          # do not assume first row contains variable names if it contains
          # duplicated values
          if (any(duplicated(names(z)))) {

            # extract table
            #z <- docxtractr::docx_extract_tbl(
            #  docx = docx, tbl_number = y, header = FALSE)
            z <- doc_table(docx = docx, tbl_number = y, header = FALSE, ns = ns)

            # detect row that contains "Author" and "Vote"
            names_row <- lapply(seq_len(nrow(z)), function(y) {
              y <- dplyr::slice(.data = z, y)
              any(stringr::str_detect(string = y, pattern = "([Ss]ubject)"),
                  na.rm = TRUE)
            }) %>%
              unlist %>%
              which

            # remove rows without content
            if (length(names_row) > 0) {

              # try to get the names of the table
              names_of_z <- as.character(dplyr::slice(.data = z, names_row))

              # detect duplicates
              duplicated_col_pos <- which(duplicated(names_of_z))

              # loop over duplicated names
              for (i in seq_along(duplicated_col_pos)) {
                # add idex number to the column name
                names_of_z[duplicated_col_pos][i] <- paste0(
                  names_of_z[duplicated_col_pos][i], i + 1
                )
              }

              # assign new names to data
              names(z) <- names_of_z

              z <- dplyr::slice(.data = z, ((names_row + 1):nrow(z)))
            }
          }

          # clean column names
          z <- clean_column_names(data_set = z)

          # separate into votes and requests
          if ("subject" %in% names(z) | "am_no" %in% names(z)) {

            # repeated cell entries across rows to fill in multi-row entries
            # pull out the type variable
            z <- list(tbl = z)
            class(z) <- c(class(z), "doc")
            z <- fill_in_cells(rows = z)

            out <- list(votes = z, requests = NA)
          } else {
            out <- list(votes = NA, requests = z)
          }
          return(out)
        }
      })
    } # end of condition: !'data.frame' %in% class(vote_tables)

    # store votes
    votes[[idx]] <- dplyr::tibble(data = vote_tables) %>%
      tidyr::unnest_wider(col = data) %>%
      tidyr::unnest(col = votes) %>%
      dplyr::select(-requests) %>%
      dplyr::mutate(
        vote_date = c_date,
        date_tally = idx
      ) %>%
      dplyr::relocate(vote_date, 1) %>%
      dplyr::relocate(date_tally, .after = vote_date)

    # remove votes column if it exists
    if ("votes" %in% names(votes[[idx]])) {
      votes[[idx]] <- votes[[idx]] %>%
        dplyr::mutate(votes = NULL)
    }

    # store requests
    requests[[idx]] <- dplyr::tibble(data = vote_tables) %>%
      tidyr::unnest_wider(col = data) %>%
      tidyr::unnest(col = requests) %>%
      dplyr::select(-votes)

    # replace empty tibble with NA
    if (all(is.na(requests[[idx]]))) {
      requests[[idx]] <- NA
    } else {
      requests[[idx]] <- requests[[idx]] %>%
        dplyr::mutate(
          vote_date = c_date,
          date_tally = idx
        ) %>%
        dplyr::relocate(vote_date, 1) %>%
        dplyr::relocate(date_tally, .after = vote_date)
    }
  }

  # combine results in one tibble
  votes <- dplyr::tibble(
    ep = 5L,
    vote_date = c_date,
    votes_meta = list(votes_meta),
    votes = list(votes),
    requests = list(requests)
  )

  # check requests (which are not always stored in tables)
  votes$requests <- tryCatch(
    expr = {
      list(check_requests(
        requests = votes$requests,
        t_pos = t_pos,
        page = page,
        vote_date = c_date
      ))
    },
    error = function(e) {
      list(NULL)
      message("Vote requests not properly implemented for EP5")
    }
  )
  # function output
  return(votes)
}


# docx method: search votes ------------------------------------------------
#' @export
search_votes.docx <- function(x, data_dir, verbose, ...) {

  # initial binding for globals
  date_tally <- NULL

  # current R-session tmp directory
  tmpd <- tempdir()

  # add a process specific sub-directory for parallel processing
  tmpd <- sprintf("%s/%s", tmpd, Sys.getpid())

  # generate directory in R's temporary directory
  if (!dir.exists(tmpd)) {
    dir.create(path = tmpd)
  }

  # temporary file name
  tmpf <- tempfile(
    tmpdir = tmpd,
    fileext = ".zip",
    pattern = sprintf("file%s", Sys.getpid())
  )

  # clean-up
  on.exit({
    unlink(tmpf)
    unlink(sprintf("%s/docdata", tmpd), recursive = TRUE)
  })

  # copy .docx to zip
  fs::file_copy(
    path = sprintf("%sraw/votes/%s", data_dir, x$`name_raw/votes`),
    new_path = tmpf,
    overwrite = TRUE
  )

  # unzip it
  unzip(tmpf, exdir = sprintf("%s/docdata", tmpd))

  # read the actual XML document
  page <- xml2::read_xml(sprintf("%s/docdata/word/document.xml", tmpd))

  # extract the namespace
  ns <- xml2::xml_ns(x = page)

  # all table nodes in the document
  tbls <- page %>%
    xml2::xml_find_all(xpath = ".//w:tbl")

  # prepare docx object for docxtractr::docx_extract_tbl() function
  docx <- list(tbls = tbls, ns = ns, cmnts = NULL, path = NULL, docx = NULL)
  class(docx) <- "docx"

  # meta data ---------------------------------------------------------------

  # titles
  titles <- page %>%
    xml2::xml_find_all(xpath = "//w:p") %>%
    xml2::xml_find_all(xpath = paste0(
      ".//w:pStyle[@w:val='VOTEFIRSTTITLE']",
      "|.//w:pStyle[@w:val='VOTETITLE']"
    )) %>%
    xml2::xml_parent() %>%
    xml2::xml_parent() %>%
    rvest::html_text()

  # reading stage and procedure
  # * Consultation procedure
  # ** I Cooperation procedure 1st reading
  # ** II Cooperation procedure 2nd reading
  # *** Assent procedure
  # ***I Codecision 1st reading
  # ***II Codecision 2nd reading
  # ***III Codecision 3rd reading
  proc_and_stage <- lapply(titles, function(z) {
    # codecision 3rd reading
    if (stringr::str_detect(string = z, pattern = "\\*{3}\\s?III")) {
      procedure <- "COD"
      reading_stage <- 3L
    } else if (
      # codecision 2nd reading
      stringr::str_detect(string = z, pattern = "\\*{3}\\s?II")
    ) {
      procedure <- "COD"
      reading_stage <- 2L
    } else if (
      # codecision 1st reading
      stringr::str_detect(string = z, pattern = "\\*{3}\\s?I")
    ) {
      procedure <- "COD"
      reading_stage <- 1L
    } else if (
      # assent procedure
      stringr::str_detect(string = z, pattern = "\\*{3}")
    ) {
      procedure <- "AVC;APP"
      reading_stage <- 1L
    } else if (
      # cooperation procedure 2nd reading
      stringr::str_detect(string = z, pattern = "\\*{2}\\s?II")
    ) {
      procedure <- "SYN"
      reading_stage <- 2L
    } else if (
      # cooperation procedure 1st reading
      stringr::str_detect(string = z, pattern = "\\*{2}\\s?I")
    ) {
      procedure <- "SYN"
      reading_stage <- 1L
    } else if (
      # consultation procedure
      stringr::str_detect(string = z, pattern = "\\*")
    ) {
      procedure <- "CNS"
      reading_stage <- 1L
    } else {
      procedure <- NA_character_
      reading_stage <- NA_integer_
    }
    out <- tibble::tibble(
      procedure = procedure,
      reading_stage = reading_stage
    )
    return(out)
  }) %>%
    dplyr::bind_rows()

  # title positions (suppresses warning of coercion to atomic vector)
  t_pos <- suppressWarnings(
    page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      as.character(.) %>%
      stringr::str_detect(pattern = "VOTEFIRSTTITLE|VOTETITLE") %>%
      which
  )

  # if any titles are empty remove them from titles and t_pos
  if (any(titles == "")) {
    t_pos <- t_pos[-which(titles == "")]
    titles <- titles[-which(titles == "")]
  }

  # vote descriptions text
  vote_descriptions <- page %>%
    xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
    .[t_pos + 1] %>%
    rvest::html_text(trim = TRUE)

  # remove false positives from vote descriptions
  # remove matches of just 1 word
  if (any(stringr::str_count(
    string = vote_descriptions,
    pattern = "\\w+"
  ) == 1)) {
    vote_descriptions <- vote_descriptions[- which(
      stringr::str_count(
        string = vote_descriptions,
        pattern = "\\w+"
      ) == 1
    )]
  }

  # extract links to meta documents from descriptions text
  meta_link <- lapply(X = seq_along(vote_descriptions), FUN = function(y) {
    # extract the reference(s) within each description
    ref <- stringr::str_extract_all(
      string = vote_descriptions[y],
      pattern = "[A-Z]+[0-9]?[-][0-9]{4}/[0-9]{4}"
    )
    # operate on each reference within a single description field
    lapply(X = ref, FUN = function(z) {
      # character(0) fields to NA
      if (length(z) == 0) {
        meta_link <- NA
      } else {
        # remove white spaces
        z <- trimws(x = z, which = "both")
        # separate out the year
        year <- stringr::str_extract(string = z, pattern = "(?<=/)[\\s\\S]*")
        # remove the year from the reference string
        ref <- stringr::str_remove(string = z, pattern = "(?<=/)[\\s\\S]*")
        # remove white spaces from reference string
        ref <- trimws(x = ref, which = "both")
        # remove the slash from reference string
        ref <- stringr::str_remove(string = ref, pattern = "/")
        # separate out the running id
        r_id <- stringr::str_extract(string = ref, pattern = "(?<=-)[\\s\\S]*")
        # remove the running id from the reference string
        ref <- stringr::str_remove(string = ref, pattern = "(?<=-)[\\s\\S]*")
        # separate letter and number by a -
        ref <- paste(
          stringr::str_extract(string = ref, pattern = "[A-Z]+"),
          stringr::str_extract(string = ref, pattern = "[0-9]+"),
          sep = "-"
        )
        # put together the link
        meta_link <- sprintf(
          "https://www.europarl.europa.eu/doceo/document/%s-%s-%s_EN.html",
          ref, year, r_id
        )
      }
      return(meta_link)
    }) %>%
      unlist
  }) %>%
    unlist() %>%
    paste(., collapse = "; ")

  # extract OEIL links from descriptions text
  oeil_link <- lapply(X = seq_along(vote_descriptions), FUN = function(y) {
    # extract the reference(s) within each description
    ref <- stringr::str_extract_all(
      string = vote_descriptions[y],
      pattern = "[0-9]{4}/[0-9]{4}\\([A-Z]{3}\\)"
    )
    # operate on each reference within a single description field
    lapply(X = ref, FUN = function(z) {
      # character(0) fields to NA
      if (length(z) == 0) {
        oeil_link <- NA
      } else {
        # remove white spaces
        z <- trimws(x = z, which = "both")
        # put together the link
        oeil_link <- paste0(
          "https://oeil.secure.europarl.europa.eu/oeil/popups/",
          "ficheprocedure.do?reference=",
          z, "&l=en"
        )
      }
      return(oeil_link)
    }) %>%
      unlist
  }) %>%
    unlist() %>%
    paste(., collapse = "; ")

  # extract vote date
  vote_date <- dplyr::pull(.data = x, var = date)

  # voting meta data
  votes_meta <- tibble::tibble(
    ep = x$ep,
    vote_date = vote_date,
    date_tally = seq_along(titles),
    title = titles,
    description = vote_descriptions,
    meta_link = meta_link,
    oeil_link = oeil_link
  )

  # add procedure and stage
  votes_meta <- dplyr::bind_cols(votes_meta, proc_and_stage) %>%
    dplyr::relocate(procedure, .before = title) %>%
    dplyr::relocate(reading_stage, .before = title)

  # remove false positive matches which are empty strings on the title variable
  votes_meta <- votes_meta %>%
    dplyr::filter(title != "") %>%
    dplyr::mutate(date_tally = seq(seq_len(nrow(.))))

  # message about number of votes in the document
  if (verbose) {
    message(
      "Document: ", x$`name_raw/votes`, " contains ", nrow(votes_meta),
      " topics"
    )
  }

  # individual level data ---------------------------------------------------
  # scrape tables following titles

  # find all tables (node indexes)
  tbl_idxs <- suppressWarnings(
    page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      as.character(.) %>%
      stringr::str_detect(pattern = "w:tbl") %>%
      which
  )

  # containers for vote tables and vote request tables
  votes <- vector(mode = "list", length = nrow(votes_meta))
  requests <- vector(mode = "list", length = nrow(votes_meta))

  # loop over the votes
  for (idx in seq_along(t_pos)){

    ## which tables are within the current title range
    # not the last title
    if (idx < length(t_pos)) {
      tbl_number <- which(tbl_idxs > t_pos[idx] & t_pos[idx + 1] > tbl_idxs)
    } else { # otherwise
      tbl_number <- which(tbl_idxs > t_pos[idx])
    }

    # extract all tables that are in the title range
    vote_tables <- lapply(X = tbl_number, FUN = function(y) {

      # extract table
      z <- docxtractr::docx_extract_tbl(docx = docx, tbl_number = y)

      # clean column names
      z <- clean_column_names(data_set = z)

      # separate into votes and requests
      if ("subject" %in% names(z) | "am_no" %in% names(z)) {

        # repeated cell entries across rows to fill in multi-row entries
        # pull out the type variable
        z <- list(tbl = z)
        class(z) <- c(class(z), "doc")
        z <- fill_in_cells(rows = z)

        out <- list(votes = z, requests = NA)
      } else {
        out <- list(votes = NA, requests = z)
      }
      return(out)
    })

    # If no vote tables were matched, skip ahead to the next loop iteration
    if (length(vote_tables) == 0) next

    # store votes
    votes[[idx]] <- dplyr::tibble(data = vote_tables) %>%
      tidyr::unnest_wider(col = data) %>%
      tidyr::unnest(col = votes) %>%
      dplyr::select(-requests) %>%
      dplyr::mutate(
        vote_date = vote_date,
        date_tally = idx
      ) %>%
      dplyr::relocate(vote_date, 1) %>%
      dplyr::relocate(date_tally, .after = vote_date)

    # remove votes column if it exists
    if ("votes" %in% names(votes[[idx]])) {
      votes[[idx]] <- votes[[idx]] %>%
        dplyr::mutate(votes = NULL)
    }

    # store requests
    requests[[idx]] <- do.call("rbind", vote_tables)[, "requests"]

    # identify list elements that contain content
    keep_elements <- lapply(seq_along(requests[[idx]]), function(xx) {
      !is.na(requests[[idx]][xx])
    }) %>% unlist

    # keep rows with content only
    requests[[idx]] <- requests[[idx]][keep_elements]

    # add vote date and topic tally
    requests[[idx]] <- lapply(requests[[idx]], function(xx) {
      xx <- xx %>%
        dplyr::mutate(
          vote_date = vote_date,
          date_tally = idx
        ) %>%
        dplyr::relocate(vote_date, 1) %>%
        dplyr::relocate(date_tally, .after = vote_date)
    })
  }

  # remove NULL values from votes list
  votes[sapply(X = votes, FUN = is.null)] <- NULL

  # remove NULL values from requests list
  requests[sapply(X = requests, FUN = is.null)] <- NULL

  # combine results in one tibble
  votes <- dplyr::tibble(
    ep = x$ep,
    vote_date = x$date,
    votes_meta = list(votes_meta),
    votes = list(votes),
    requests = list(requests)
  )

  # function output
  return(votes)

}

# doc method: search votes ------------------------------------------------
#' @export
search_votes.doc <- function(x, data_dir, verbose, ...) {

  # inital binding for globals
  objet <- date_tally <- NULL

  # current R-session tmp directory
  tmpd <- tempdir()

  # add a process specific sub-directory for parallel processing
  tmpd <- sprintf("%s/%s", tmpd, Sys.getpid())

  # generate directory in R's temporary directory
  dir.create(path = tmpd)

  # clean-up
  on.exit({
    unlink(sprintf("%s/docdata", tmpd), recursive = TRUE)
    unlink(sprintf(paste0("%s/", x$`name_raw/votes`, "x"), tmpd))
  })

  # copy .docx file to tmp dir
  fs::file_copy(
    path = sprintf("%sraw/votes/%sx", data_dir, x$`name_raw/votes`),
    new_path = tmpd,
    overwrite = TRUE
  )

  # unzip it
  utils::unzip(
    zipfile = sprintf("%s/%sx", tmpd, x$`name_raw/votes`),
    exdir = sprintf("%s/docdata", tmpd)
  )

  # load xml version of the document
  page <- xml2::read_xml(
    x = sprintf("%s/docdata/word/document.xml", tmpd),
    encoding = "UTF-8"
  )

  # extract the namespace
  ns <- xml2::xml_ns(x = page)

  # all table nodes in the document
  tbls <- page %>%
    xml2::xml_find_all(xpath = ".//w:tbl")

  # prepare docx object for docxtractr::docx_extract_tbl() function
  docx <- list(tbls = tbls, ns = ns, cmnts = NULL, path = NULL, docx = NULL)
  class(docx) <- "docx"

  # meta data ---------------------------------------------------------------

  # titles
  titles <- page %>%
    xml2::xml_find_all(xpath = "//w:p") %>%
    xml2::xml_find_all(
      xpath = paste0(
        ".//w:pStyle[@w:val='VOTEFIRSTTITLE']|",
        ".//w:pStyle[@w:val='VOTETITLE']"
      )
    ) %>%
    xml2::xml_parent() %>%
    xml2::xml_parent() %>%
    rvest::html_text()

  # message about number of votes in the document
  if (verbose) {
    message(
      "Document: ", x$`name_raw/votes`, " contains ",
      length(titles), " topics"
    )
  }

  # title positions (suppresses warning of coercion to atomic vector)
  t_pos <- suppressWarnings(
    page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      as.character(.) %>%
      stringr::str_detect(pattern = "VOTEFIRSTTITLE|VOTETITLE") %>%
      which
  )

  # vote descriptions text
  vote_descriptions <- page %>%
    xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
    .[t_pos + 1] %>%
    rvest::html_text(trim = TRUE)

  # extract links to meta documents from descriptions text
  meta_link <- lapply(X = seq_along(vote_descriptions), FUN = function(y) {
    # extract the reference(s) within each description
    ref <- stringr::str_extract_all(
      string = vote_descriptions[y],
      pattern = "[A-Z]+[0-9]?[-][0-9]{4}/[0-9]{4}"
    )
    # operate on each reference within a single description field
    lapply(X = ref, FUN = function(z) {
      # character(0) fields to NA
      if (length(z) == 0) {
        meta_link <- NA
      } else {
        # remove white spaces
        z <- trimws(x = z, which = "both")
        # separate out the year
        year <- stringr::str_extract(string = z, pattern = "(?<=/)[\\s\\S]*")
        # remove the year from the reference string
        ref <- stringr::str_remove(string = z, pattern = "(?<=/)[\\s\\S]*")
        # remove white spaces from reference string
        ref <- trimws(x = ref, which = "both")
        # remove the slash from reference string
        ref <- stringr::str_remove(string = ref, pattern = "/")
        # separate out the running id
        r_id <- stringr::str_extract(string = ref, pattern = "(?<=-)[\\s\\S]*")
        # remove the running id from the reference string
        ref <- stringr::str_remove(string = ref, pattern = "(?<=-)[\\s\\S]*")
        # separate letter and number by a -
        ref <- paste(
          stringr::str_extract(string = ref, pattern = "[A-Z]+"),
          stringr::str_extract(string = ref, pattern = "[0-9]+"),
          sep = "-"
        )
        # put together the link
        meta_link <- paste0(
          "https://www.europarl.europa.eu/doceo/document/",
          ref,
          "-",
          year,
          "-",
          r_id,
          "_EN.html"
        )
      }
      return(meta_link)
    }) %>%
      unlist
  }) %>%
    unlist() %>%
    paste(., collapse = "; ")

  # extract OEIL links from descriptions text
  oeil_link <- lapply(X = seq_along(vote_descriptions), FUN = function(y) {
    # extract the reference(s) within each description
    ref <- stringr::str_extract_all(
      string = vote_descriptions[y],
      pattern = "[0-9]{4}/[0-9]{4}\\([A-Z]{3}\\)"
    )
    # operate on each reference within a single description field
    lapply(X = ref, FUN = function(z) {
      # character(0) fields to NA
      if (length(z) == 0) {
        oeil_link <- NA
      } else {
        # remove white spaces
        z <- trimws(x = z, which = "both")
        # put together the link
        oeil_link <- paste0(
          paste0(
            "https://oeil.secure.europarl.europa.eu/oeil/popups/",
            "ficheprocedure.do?reference="
          ), z, "&l=en"
        )
      }
      return(oeil_link)
    }) %>%
      unlist
  }) %>%
    unlist() %>%
    paste(., collapse = "; ")

  # voting meta data
  votes_meta <- dplyr::tibble(
    ep = x$ep,
    vote_date = x$date,
    date_tally = seq_along(titles),
    title = titles,
    description = vote_descriptions,
    meta_link = meta_link,
    oeil_link = oeil_link
  )


  # individual level data ---------------------------------------------------
  # scrape tables following titles

  # find all tables (node indexes)
  tbl_idxs <- suppressWarnings(
    page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      as.character(.) %>%
      stringr::str_detect(pattern = "w:tbl") %>%
      which
  )

  # containers for vote tables and vote request tables
  votes <- vector(mode = "list", length = nrow(votes_meta))
  requests <- vector(mode = "list", length = nrow(votes_meta))

  # loop over the votes
  for (idx in seq_along(t_pos)){

    ## which tables are within the current title range
    # not the last title
    if (idx < length(t_pos)) {
      tbl_number <- which(tbl_idxs > t_pos[idx] & t_pos[idx + 1] > tbl_idxs)
    } else { # otherwise
      tbl_number <- which(tbl_idxs > t_pos[idx])
    }

    # extract all tables that are in the title range
    vote_tables <- lapply(X = tbl_number, FUN = function(y) {

      # extract table
      #z <- docxtractr::docx_extract_tbl(docx = docx, tbl_number = y) #nolint
      z <- doc_table(docx = docx, tbl_number = y, header = TRUE, ns = ns)

      # clean column names
      z <- clean_column_names(data_set = z)

      # if subject is called 'objet', rename it
      if ("objet" %in% names(z)) z <- dplyr::rename(
        .data = z, subject = objet
      )

      # separate into votes and requests
      if ("subject" %in% names(z) | "am_no" %in% names(z)) {

        # repeated cell entries across rows to fill in multi-row entries
        # pull out the type variable
        z <- list(tbl = z)
        class(z) <- c(class(z), "doc")
        z <- fill_in_cells(rows = z)

        out <- list(votes = z, requests = NA)
      } else {
        out <- list(votes = NA, requests = z)
      }
      return(out)
    })

    # store votes
    votes[[idx]] <- dplyr::tibble(data = vote_tables) %>%
      tidyr::unnest_wider(col = data) %>%
      tidyr::unnest(col = votes) %>%
      dplyr::select(-requests) %>%
      dplyr::mutate(
        vote_date = x$date,
        date_tally = idx
      ) %>%
      dplyr::relocate(vote_date, 1) %>%
      dplyr::relocate(date_tally, .after = vote_date)

    # remove votes column if it exists
    if ("votes" %in% names(votes[[idx]])) {
      votes[[idx]] <- votes[[idx]] %>%
        dplyr::mutate(votes = NULL)
    }

    # store requests
    requests[[idx]] <- dplyr::tibble(data = vote_tables) %>%
      tidyr::unnest_wider(col = data) %>%
      tidyr::unnest(col = requests) %>%
      dplyr::select(-votes)

    # extract vote date
    vote_date <- dplyr::pull(.data = x, var = date)

    # replace empty tibble with NA
    if (all(is.na(requests[[idx]]))) {
      requests[[idx]] <- NA
    } else {
      requests[[idx]] <- requests[[idx]] %>%
        dplyr::mutate(
          vote_date = vote_date,
          date_tally = idx
        ) %>%
        dplyr::relocate(vote_date, 1) %>%
        dplyr::relocate(date_tally, .after = vote_date)
    }
  }

  # combine results in one tibble
  votes <- dplyr::tibble(
    ep = x$ep,
    vote_date = x$date,
    votes_meta = list(votes_meta),
    votes = list(votes),
    requests = list(requests)
  )

  # function output
  return(votes)
}

# xml_v2 method: search votes ---------------------------------------------
#' As of March 2024, this function works for the xml files from 2024.
#' All other xmls are parsed using search_votes.xml
#' @export
search_votes.xml_v2 <- function(x, data_dir, verbose, ...) {

  # global variables
  split_vote <- NULL

  # load the file
  page <- xml2::read_xml(
    x = sprintf("%sraw/votes/%s", data_dir, x$`name_raw/votes`),
    encoding = "UTF-8"
  )

  #-------------------------------------------------------------------------
  # meta data
  #-------------------------------------------------------------------------

  # sitting date
  sitting <- page %>%
    xml2::xml_find_all(xpath = "//sitting") %>%
    xml2::xml_attr("date") %>%
    stringr::str_trim(side = "both") %>%
    as.Date("%Y-%m-%d")

  # topic titles
  titles <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_find_first(xpath = ".//title") %>%
    xml2::xml_text() %>%
    stringr::str_trim(side = "both")

  # database ID
  dlvID <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_attr("dlvId")

  # list index
  list_index <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_attr("listIndex")

  # topic type
  type <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_attr("type") %>%
    stringr::str_to_title()

  # description
  description <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_find_first(xpath = ".//label") %>%
    xml2::xml_text() %>%
    stringr::str_trim(side = "both")

  # author
  author <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_attr("author") %>%
    stringr::str_trim(side = "both")
  # replace NAs with ""
  author[is.na(author)] <- ""

  # committee
  ctte <- page %>%
    xml2::xml_find_all(xpath = "//votes") %>%
    xml2::xml_find_all(xpath = ".//vote") %>%
    xml2::xml_attr("committee") %>%
    stringr::str_trim(side = "both")
  # replace NAs with ""
  ctte[is.na(ctte)] <- ""
  # remove the phrase "Committee: "
  ctte <- ctte %>%
    stringr::str_remove("^Committee\\:\\s+") %>%
    stringr::str_trim()

  # voting meta data
  votes_meta <- dplyr::tibble(
    ep = x$ep,
    text_tabled_id = NA_character_,
    vote_date = sitting,
    date_tally = seq_along(titles),
    procedure = NA_character_,
    reading_stage = NA_integer_,
    text_type = NA_character_,
    title = titles,
    description = description,
    topic_type = type,
    rapporteur = NA_character_,
    subject_author = author,
    ctte_long = ctte,
    meta_link = x$links,
    oeil_link = "",
    dlvID = dlvID,
    list_index = list_index
  )

  # remove info from table
  if (any(votes_meta$topic_type == "Info")) {
    info_row <- which(votes_meta$topic_type == "Info")
    # remove that row from the table
    votes_meta <- dplyr::slice(.data = votes_meta, -info_row)
  }

  # message about number of votes in the document
  if (verbose) {
    message("Document: ", x$`name_raw/votes`, " contains ",
      nrow(votes_meta), " topics"
    )
  }

  #-------------------------------------------------------------------------
  # individual votes
  #-------------------------------------------------------------------------

  # loop over topics
  votes <- lapply(seq_len(nrow(votes_meta)), function(y) {

    # current vote
    list_index <- votes_meta %>%
      dplyr::slice(y) %>%
      dplyr::pull(var = "list_index")

    # construct xpath
    current_x_path <- sprintf(".//vote[@listIndex='%s']", list_index)

    # general content relating to all votings in the vote
    vote_info <- page %>%
      xml2::xml_find_all(xpath = ".//votes") %>%
      xml2::xml_find_all(xpath = current_x_path)

    # title
    vote_title <- vote_info %>%
      xml2::xml_find_first(xpath = ".//title") %>%
      xml2::xml_text()

    # reading stage and procedure
    # * Consultation procedure
    # ** I Cooperation procedure 1st reading
    # ** II Cooperation procedure 2nd reading
    # *** Assent procedure
    # ***I Codecision 1st reading
    # ***II Codecision 2nd reading
    # ***III Codecision 3rd reading


    # codecision 3rd reading
    if (stringr::str_detect(string = vote_title, pattern = "\\*{3}\\s?III")) {
      procedure <- "COD"
      reading_stage <- 3L
    } else if (
      # codecision 2nd reading
      stringr::str_detect(string = vote_title, pattern = "\\*{3}\\s?II")
    ) {
      procedure <- "COD"
      reading_stage <- 2L
    } else if (
      # codecision 1st reading
      stringr::str_detect(string = vote_title, pattern = "\\*{3}\\s?I")
    ) {
      procedure <- "COD"
      reading_stage <- 1L
    } else if (
      # assent procedure
      stringr::str_detect(string = vote_title, pattern = "\\*{3}")
    ) {
      procedure <- "AVC;APP"
      reading_stage <- 1L
    } else if (
      # cooperation procedure 2nd reading
      stringr::str_detect(string = vote_title, pattern = "\\*{2}\\s?II")
    ) {
      procedure <- "SYN"
      reading_stage <- 2L
    } else if (
      # cooperation procedure 1st reading
      stringr::str_detect(string = vote_title, pattern = "\\*{2}\\s?I")
    ) {
      procedure <- "SYN"
      reading_stage <- 1L
    } else if (
      # consultation procedure
      stringr::str_detect(string = vote_title, pattern = "\\*")
    ) {
      procedure <- "CNS"
      reading_stage <- 1L
    } else {
      procedure <- NA_character_
      reading_stage <- NA_integer_
    }

    # text_tabled_id and description text
    vote_desc <- vote_info %>%
      xml2::xml_find_first(xpath = ".//label") %>%
      xml2::xml_text()

    # split string on colon
    vote_desc <- stringr::str_split(string = vote_desc, pattern = ":") %>%
      unlist() %>%
      stringr::str_trim(side = "both")

    # text_type
    text_type <- vote_desc[1]

    # text_tabled_id
    tt_id <- vote_desc[2] %>%
      stringr::str_extract(pattern = "\\(([^)]+)\\)") %>%
      stringr::str_remove_all(pattern = "[\\(\\)]")

    # rapporteur
    rapporteur <- vote_desc[2] %>%
      stringr::str_extract(pattern = "^[^(]+") %>%
      stringr::str_trim(side = "both")

    # add to votes meta
    votes_meta[y, "reading_stage"] <- reading_stage
    votes_meta[y, "procedure"] <- procedure
    votes_meta[y, "rapporteur"] <- rapporteur
    votes_meta[y, "text_tabled_id"] <- tt_id

    # node to the current vote
    vote_node <- page %>%
      xml2::xml_find_all(xpath = "//votes") %>%
      xml2::xml_find_all(xpath = current_x_path) %>%
      xml2::xml_find_all(xpath = ".//votings")

    # rows
    rows <- vote_node %>%
      xml2::xml_find_all(xpath = ".//voting")

    # loop over the rows
    individual_votes <- lapply(seq_along(rows), function(idx) {

      # move on to the next loop iteration in case of a title block
      if (
        xml2::xml_attr(x = rows[[idx]], attr = "type") == "TITLE_BLOCK"
      ) {
        out <- NULL
      } else {
        # extract the vote level
        # note that the source will report anything as an amendment
        # for now, we don't use the the following
        vote_level <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("type") %>%
          stringr::str_trim(side = "both") %>%
          stringr::str_to_lower()

        # extract the outcome
        vote <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("result")
        if (!is.na(vote)) {
          if (vote == "ADOPTED") {
            vote <- "+"
          } else if (vote == "REJECTED") {
            vote <- "-"
          } else {
            vote <- "lapsed"
          }
        }

        # extract the vote type
        vote_type <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("resultType")
        if (vote_type == "ROLL_CALL") {
          vote_type <- "rollcall vote"
        } else if (vote_type == "RAISE_HAND") {
          vote_type <- "show of hands"
        } else if (vote_type == "ELECTRONICAL") {
          vote_type <- "electronic vote"
        } else {
          stop("no vote type")
        }

        # yes votes
        yes <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("numberFavor") %>%
          stringr::str_trim(side = "both") %>%
          as.integer()

        # no votes
        no <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("numberAgainst") %>%
          stringr::str_trim(side = "both") %>%
          as.integer()

        # abstentions
        abstain <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("numberAbstention") %>%
          stringr::str_trim(side = "both") %>%
          as.integer()

        # number of voters
        n_voters <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("votersCount") %>%
          stringr::str_trim(side = "both") %>%
          as.integer()

        # vote date
        timestamp <- rows %>%
          .[[idx]] %>%
          xml2::xml_attr("voteTimestamp") %>%
          # remove the fractional seconds
          sub(pattern = "\\.\\d+", replacement = "", x = .) %>%
          # remove the colon from the timezone offset
          sub(
            pattern = "(\\+|-)(\\d\\d):(\\d\\d)",
            replacement = "\\1\\2\\3",
            x = .
          ) %>%
          # convert the modified timestamp to a POSIXct object
          as.POSIXct(x = ., format = "%Y-%m-%dT%H:%M:%S%z")

        # vote_date variable (excluding time)
        vote_date <- as.Date(timestamp)

        # type variable in our data
        type <- rows %>%
          .[[idx]] %>%
          xml2::xml_find_all(xpath = ".//*[contains(name(), 'Subject')]") %>%
          xml2::xml_text()

        # add title if type is empty
        if (type == "") {
          type <- rows %>%
            .[[idx]] %>%
            xml2::xml_find_all(xpath = "title") %>%
            xml2::xml_text() %>%
            stringr::str_trim(side = "both")
        }

        # amendment number in our data
        # note that this can be a paragraph as well
        am_no <- rows %>%
          .[[idx]] %>%
          xml2::xml_find_all(xpath = ".//amendmentNumber") %>%
          xml2::xml_text()
        # remove paragraph symbol
        if (am_no == "§") am_no <- ""

        # split vote
        split_vote <- rows %>%
          .[[idx]] %>%
          xml2::xml_find_all(xpath = ".//rcv") %>%
          xml2::xml_find_all(xpath = "value") %>%
          xml2::xml_text() %>%
          stringr::str_trim(side = "both")
        if (length(split_vote) == 0) split_vote <- ""
        if (
          stringr::str_detect(
            string = split_vote,
            pattern = "^\\d+/EV$"
          )
        ) {
          split_vote <- stringr::str_extract(
            string = split_vote,
            pattern = "^\\d+"
          )
        }

        # author
        vote_author <- rows %>%
          .[[idx]] %>%
          xml2::xml_find_all(xpath = ".//*[contains(name(), 'Author')]") %>%
          xml2::xml_text()

        # description text
        description <- rows %>%
          .[[idx]] %>%
          xml2::xml_find_all(xpath = ".//label") %>%
          xml2::xml_text()

        # return dataset
        out <- tibble::tibble(
          title = vote_title,
          text_type = text_type,
          text_tabled_id = tt_id,
          rapporteur = rapporteur,
          vote_date = vote_date,
          timestamp = timestamp,
          date_tally = y,
          subject_author = votes_meta %>%
            dplyr::slice(y) %>%
            dplyr::pull(var = "subject_author"),
          author = vote_author,
          am_no = am_no,
          split_vote = split_vote,
          type = type,
          vote_type = vote_type,
          vote = vote,
          yes = yes,
          no = no,
          abstain = abstain,
          n_voters = n_voters,
          description = description
        )
      }
      return(out)
    })

    # list of individual votes to a data frame
    individual_votes <- dplyr::bind_rows(individual_votes)

    # deal with split votes
    individual_votes <- individual_votes %>%
      dplyr::filter(split_vote != "split") %>%
      dplyr::mutate(
        type = dplyr::case_when(
          stringr::str_detect(string = split_vote, pattern = "^\\d+$") ~
            paste0(type, "/", split_vote),
          TRUE ~ type
        ),
        am_no = dplyr::case_when(
          stringr::str_detect(string = split_vote, pattern = "^\\d+$") &
            am_no != "" ~
            paste0(am_no, "/", split_vote),
          TRUE ~ am_no
        )
      ) %>%
      dplyr::select(-split_vote)

    #-------------------------------------------------------------------------
    # requests
    #-------------------------------------------------------------------------

    # node to the current vote
    request_node <- page %>%
      xml2::xml_find_all(xpath = "//votes") %>%
      xml2::xml_find_all(xpath = current_x_path) %>%
      xml2::xml_find_all(xpath = ".//remarks")

    # rcv_requests
    rcv_node <- request_node %>%
      xml2::xml_find_all(xpath = ".//remarkRollCalls") %>%
      xml2::xml_find_all(xpath = ".//RemarkRollCallSeparated")

    # loop over rcv requests
    rcv_requests <- lapply(seq_along(rcv_node), function(z) {

      # current node
      c_rcv <- rcv_node[[z]]

      # current group
      epgs_rcv <- c_rcv %>%
        xml2::xml_find_all(xpath = ".//politicalGroup") %>%
        xml2::xml_attr("code")

      # sometimes MEPs request instead of groups
      if (length(epgs_rcv) == 0) {
        epgs_rcv <- c_rcv %>%
          xml2::xml_find_all(xpath = ".//rcvTitle") %>%
          xml2::xml_text() %>%
          stringr::str_remove(pattern = ":$") %>%
          stringr::str_trim(side = "both")
      }

      # request subject
      subject_rcv <- c_rcv %>%
        xml2::xml_find_all(xpath = ".//translation") %>%
        xml2::xml_text() %>%
        stringr::str_trim(side = "both")

      rcv_out <- tibble::tibble(
        vote_date = !!x$date,
        date_tally = y,
        request_for_roll_calls = subject_rcv,
        epg = epgs_rcv
      )

      return(rcv_out)
    }) %>%
      dplyr::bind_rows()

    # requests for separate votes
    sep_node <- request_node %>%
      xml2::xml_find_all(xpath = ".//remarkSeparateds") %>%
      xml2::xml_find_all(xpath = ".//RemarkRollCallSeparated")

    # loop over separate votes
    sep_requests <- lapply(seq_along(sep_node), function(z) {

      # current node
      c_sep <- sep_node[[z]]

      # current group
      epgs_sep <- c_sep %>%
        xml2::xml_find_all(xpath = ".//politicalGroup") %>%
        xml2::xml_attr("code")

      # sometimes MEPs request instead of groups
      if (length(epgs_sep) == 0) {
        epgs_sep <- c_sep %>%
          xml2::xml_find_all(xpath = ".//sepTitle") %>%
          xml2::xml_text() %>%
          stringr::str_remove(pattern = ":$") %>%
          stringr::str_trim(side = "both")
      }

      # request subject
      subject_sep <- c_sep %>%
        xml2::xml_find_all(xpath = ".//translation") %>%
        xml2::xml_text() %>%
        stringr::str_trim(side = "both")

      sep_out <- tibble::tibble(
        vote_date = !!x$date,
        date_tally = y,
        request_for_separate_votes = subject_sep,
        epg = epgs_sep
      )

      return(sep_out)
    }) %>%
      dplyr::bind_rows()

    # requests for split votes
    split_node <- request_node %>%
      xml2::xml_find_all(xpath = ".//remarkSplitVotes") %>%
      xml2::xml_find_all(xpath = ".//remarkSplitVote")

    # loop over the split nodes
    split_requests <- lapply(seq_along(split_node), function(z) {

      # current node
      c_split <- split_node[[z]]

      # current group
      epgs_split <- c_split %>%
        xml2::xml_find_all(xpath = ".//politicalGroup") %>%
        xml2::xml_attr("code")

      # sometimes MEPs request instead of groups
      if (length(epgs_split) == 0) {
        epgs_split <- c_split %>%
          xml2::xml_find_all(xpath = ".//splitTitle") %>%
          xml2::xml_text() %>%
          stringr::str_remove(pattern = ":$") %>%
          stringr::str_trim(side = "both")
      }

      # request subject
      subject_split <- c_split %>%
        xml2::xml_find_all(xpath = ".//items") %>%
        xml2::xml_find_all(xpath = ".//title") %>%
        xml2::xml_text() %>%
        stringr::str_trim(side = "both")

      split_out <- tibble::tibble(
        vote_date = !!x$date,
        date_tally = y,
        request_for_split_votes = subject_split,
        epg = epgs_split
      )

      return(split_out)
    }) %>%
      dplyr::bind_rows()

    # return individual votes
    return(
      list(
        votes = individual_votes,
        requests = list(
          rcv_requests = rcv_requests,
          sep_requests = sep_requests,
          split_requests = split_requests
        )
      )
    )

  }) # end of loop over topics

  # requests tables
  requests <- tibble::tibble(
    data = votes
  ) %>%
    tidyr::unnest_wider(data) %>%
    dplyr::select(requests)

  # votes tables
  votes <- do.call(rbind, votes)[, "votes"] %>%
    dplyr::bind_rows()

  # combine results in one tibble
  votes <- dplyr::tibble(
    ep = x$ep,
    vote_date = x$date,
    votes_meta = list(votes_meta),
    votes = list(votes),
    requests = list(requests)
  )

  return(votes)

}

# xml method: search votes ------------------------------------------------
#' This is the code for xml votes up until 2024. The EP may change this
#' in the future. For new xml code, use the function search_votes.xml_v2
#' @export
search_votes.xml <- function(x, data_dir, verbose, ...) {

  vote_date <- date_tally <- type <- text_tabled_id <- procedure <- NULL
  reading_stage <- NULL

  # load the file
  page <- xml2::read_xml(
    x = sprintf("%sraw/votes/%s", data_dir, x$`name_raw/votes`),
    encoding = "UTF-8"
  )

  #-------------------------------------------------------------------------
  # meta data
  #-------------------------------------------------------------------------

  # EP Number
  ep_number <- page %>%
    xml2::xml_find_all(xpath = ".//PV.Vote.Results") %>%
    xml2::xml_attr(attr = "EP.Number")

  # EP Reference
  ep_ref <- page %>%
    xml2::xml_find_all(xpath = ".//PV.Vote.Results") %>%
    xml2::xml_attr(attr = "EP.Reference")

  # Sitting Date
  sitting_date <- page %>%
    xml2::xml_find_all(xpath = ".//PV.Vote.Results") %>%
    xml2::xml_attr(attr = "Sitting.Date") %>%
    stringr::str_trim(side = "both") %>%
    as.Date("%Y-%m-%d")

  # titles
  titles <- page %>%
    xml2::xml_find_all(xpath = ".//Vote.Result.Text.Title") %>%
    rvest::html_text()

  # reading stage and procedure
  # * Consultation procedure
  # ** I Cooperation procedure 1st reading
  # ** II Cooperation procedure 2nd reading
  # *** Assent procedure
  # ***I Codecision 1st reading
  # ***II Codecision 2nd reading
  # ***III Codecision 3rd reading
  proc_and_stage <- lapply(titles, function(z) {
    # codecision 3rd reading
    if (stringr::str_detect(string = z, pattern = "\\*{3}\\s?III")) {
      procedure <- "COD"
      reading_stage <- 3L
    } else if (
      # codecision 2nd reading
      stringr::str_detect(string = z, pattern = "\\*{3}\\s?II")
    ) {
      procedure <- "COD"
      reading_stage <- 2L
    } else if (
      # codecision 1st reading
      stringr::str_detect(string = z, pattern = "\\*{3}\\s?I")
    ) {
      procedure <- "COD"
      reading_stage <- 1L
    } else if (
      # assent procedure
      stringr::str_detect(string = z, pattern = "\\*{3}")
    ) {
      procedure <- "AVC;APP"
      reading_stage <- 1L
    } else if (
      # cooperation procedure 2nd reading
      stringr::str_detect(string = z, pattern = "\\*{2}\\s?II")
    ) {
      procedure <- "SYN"
      reading_stage <- 2L
    } else if (
      # cooperation procedure 1st reading
      stringr::str_detect(string = z, pattern = "\\*{2}\\s?I")
    ) {
      procedure <- "SYN"
      reading_stage <- 1L
    } else if (
      # consultation procedure
      stringr::str_detect(string = z, pattern = "\\*")
    ) {
      procedure <- "CNS"
      reading_stage <- 1L
    } else {
      procedure <- NA_character_
      reading_stage <- NA_integer_
    }
    out <- tibble::tibble(
      procedure = procedure,
      reading_stage = reading_stage
    )
    return(out)
  }) %>%
    dplyr::bind_rows()

  # Descriptions
  descriptions <- page %>%
    xml2::xml_find_all(xpath = ".//Vote.Result.Description.Text") %>%
    rvest::html_text()

  # split string on colon
  vote_desc <- stringr::str_split(string = descriptions, pattern = ":") %>%
    unlist() %>%
    stringr::str_trim(side = "both")

  # text_type
  text_type <- vote_desc[1]

  # text_tabled_id
  if (
    stringr::str_detect(
      string = vote_desc[2],
      pattern = "[RC]?-?[AB]?-?[0-9]+/[0-9]{4}"
    )
  ) {
    tt_id <- stringr::str_extract(
      string = vote_desc[2],
      pattern = "[RC]?-?[AB]?[0-9]?-?[0-9]+/[0-9]{4}"
    )
    vote_desc <- vote_desc[2] %>%
      stringr::str_remove(
        pattern = stringr::fixed(pattern = tt_id)
      )
  }

  # rapporteur
  rapporteur <- vote_desc[2] %>%
    stringr::str_extract(pattern = "^[^(]+") %>%
    stringr::str_trim(side = "both")
  if (length(rapporteur) == 0) rapporteur <- NA_character_


  # Votes meta link (both roll-calls and votes link to these documents)
  # there may be multiple links per description
  meta_link <- lapply(seq_along(descriptions), function(y) {
    meta_link <- page %>%
      xml2::xml_find_all(xpath = ".//Vote.Result.Description.Text") %>%
      .[y] %>%
      xml2::xml_find_all(xpath = ".//a") %>%
      xml2::xml_attr(attr = "href")
    # find the position of the link to internal docs
    if (length(meta_link) > 0) {
      meta_pos <- stringr::str_which(string = meta_link, pattern = "reds:iPl")
      if (length(meta_pos) != 0) {
        meta_link <- paste0(
          "https://www.europarl.europa.eu/doceo/document/",
          stringr::str_extract_all(
            string = meta_link,
            pattern = "(?<=/)[\\s\\S]*"
          ),
          "_EN.html"
        )
      } else {
        meta_link <- NA
      }
    } else {
      meta_link <- NA
    }
    meta_link <- unlist(meta_link)
  }) %>%
    unlist() %>%
    paste(., collapse = "; ")

  # link to OEIL
  oeil_link <- lapply(seq_along(descriptions), function(y) {
    oeil_link <- page %>%
      xml2::xml_find_all(xpath = ".//Vote.Result.Description.Text") %>%
      .[y] %>%
      xml2::xml_find_all(xpath = ".//a") %>%
      xml2::xml_attr(attr = "href")
    if (length(oeil_link) > 0) {
      oeil_pos <- stringr::str_which(
        string = oeil_link, pattern = "DirContProc"
      )
      if (length(oeil_pos) != 0) {
        oeil_ref <- stringr::str_extract_all(
          string = oeil_link[oeil_pos],
          pattern = "(?<=/)[\\s\\S]*"
        )
        # re-arrange to construct the link
        proc_code <- stringr::str_extract(string = oeil_ref, pattern = "[A-Z]+")
        proc_year <- stringr::str_extract(
          string = oeil_ref, pattern = "^[^\\d]*(\\d+)"
        )
        proc_year <- stringr::str_extract(
          string = proc_year, pattern = "[0-9]{4}"
        )
        proc_nbr <- stringr::str_extract(string = oeil_ref, pattern = "[0-9]+$")
        oeil_link <- paste0(
          "https://oeil.secure.europarl.europa.eu/oeil/popups/",
          "ficheprocedure.do?lang=en&reference=",
          proc_year, "/", proc_nbr, "(", proc_code, ")"
        )
      } else {
        oeil_link <- NA
      }
    } else {
      oeil_link <- NA
    }
    return(oeil_link)
  }) %>%
    unlist() %>%
    paste(., collapse = "; ")

  # compare length titles and length descriptions
  if (length(titles) != length(descriptions)) {
    descriptions[length(titles)] <- ""
  }

  # voting meta data
  votes_meta <- dplyr::tibble(
    ep = x$ep,
    ep_number = ep_number,
    ep_ref = ep_ref,
    vote_date = sitting_date,
    date_tally = seq_along(titles),
    text_type = text_type,
    title = titles,
    rapporteur = rapporteur,
    description = descriptions,
    meta_link = meta_link,
    oeil_link = oeil_link
  )

  # add procedure and reading stage
  votes_meta <- dplyr::bind_cols(votes_meta, proc_and_stage) %>%
    dplyr::relocate(procedure, .before = title) %>%
    dplyr::relocate(reading_stage, .before = title)

  # message about number of votes in the document
  if (verbose) {
    message("Document: ", x$`name_raw/votes`, " contains ",
      length(titles), " topics"
    )
  }

  #-------------------------------------------------------------------------
  # individual votes
  #-------------------------------------------------------------------------

  # vote results nodes
  vote_results <- page %>%
    xml2::xml_find_all(xpath = ".//Vote.Result")

  # check whether the number of results corresponds to the meta_data titles
  if (length(vote_results) != nrow(votes_meta)) {
    stop("Meta data does not correspond to individual results data")
  }

  # loop over vote results
  results <- lapply(X = seq_along(vote_results), FUN = function(y) {

    # loop idx
    idx_result <- y

    # extract current vote result
    y <- vote_results[[y]]

    # References
    ref <- try(
      expr = {
        y %>%
          xml2::xml_find_all(xpath = ".//Vote.Result.Description.Text") %>%
          xml2::xml_find_all(xpath = ".//a") %>%
          xml2::xml_attr(attr = "href") %>%
          basename()
      },
      silent = TRUE
    )

    if (length(ref) == 0) {
      ref <- y %>%
        xml2::xml_find_all(xpath = ".//Vote.Result.Description.Text") %>%
        xml2::xml_text()
      # extraxt text between round brackets from ref
      ref <- stringr::str_extract(
        string = ref, pattern = "(?<=\\()[^\\)]+"
      )
      if (length(ref) == 0) {
        ref <- NA
      }
      if (!is.na(ref)) {
        # detect whether ref contains a forward slash
        if (stringr::str_detect(string = ref, pattern = "/")) {
          # extract the year
          ref_year <- basename(ref)
          # remove the year from the ref
          ref <- stringr::str_remove(
            string = ref,
            pattern = stringr::fixed(pattern = paste0("/", ref_year))
          )
          # extract the number after the hyphen
          ref_nbr <- stringr::str_extract(
            string = ref,
            pattern = "(?<=-)[0-9]+"
          )
          # remove ref_nbr from ref
          ref <- stringr::str_remove(
            string = ref,
            pattern = stringr::fixed(pattern = paste0("-", ref_nbr))
          )
          # extract the first letter from ref
          ref_letter <- stringr::str_extract(
            string = ref,
            pattern = "^[A-Z]"
          )
          # extract the reference term
          ref_term <- stringr::str_extract(
            string = ref,
            pattern = "\\d+"
          )
          # re-arrange the ref
          ref <- paste0(ref_letter, "-", ref_term, "-",
            ref_year, "-", ref_nbr
          )
        }
      }
    }

    # vote requests
    vote_requests <- y %>%
      xml2::xml_find_all(xpath = ".//Vote.Result.Table.Requests")

    # rows of the voting table
    vote_table <- y %>%
      xml2::xml_find_all(xpath = ".//Vote.Result.Table.Results") %>%
      xml2::xml_find_all(xpath = ".//TABLE")

    # extract xml table
    if (length(vote_table) != 0) {

      rows <- xml_table(xml_nodeset = vote_table, row_1_var_names = TRUE)

      if (nrow(rows) != 0) {

        # clean column names
        rows <- clean_column_names(data_set = rows)

        # repeated cell entries across rows to fill in multi-row entries
        # pull out the type variable
        rows <- list(tbl = rows)
        class(rows) <- c(class(rows), "xml_html")
        if (nrow(rows$tbl) > 0) {
          votes <- fill_in_cells(rows = rows)
        }

        # check whether table has more than one row
        if (nrow(votes) == 0) {
          # extract xml table
          rows <- xml_table(xml_nodeset = vote_table, row_1_var_names = TRUE)
          # clean column names
          votes <- clean_column_names(data_set = rows)
          if (!"type" %in% names(votes)) votes$type <- NA_character_
        }
      } else {
        votes <- tibble::tibble(
          type = "",
          subject = "",
          am_no = "",
          author = "",
          rcv_etc = "",
          budget_line = "",
          block_rcv_ev_sep_split = "",
          vote = "",
          rcv_ev_remarks = ""
        )
      }
    } else {
      votes <- tibble::tibble(
        type = "",
        subject = "",
        am_no = "",
        author = "",
        rcv_etc = "",
        budget_line = "",
        block_rcv_ev_sep_split = "",
        vote = "",
        rcv_ev_remarks = ""
      )
    }

    # add id and date columns to table
    votes <- votes %>%
      dplyr::mutate(
        text_type = text_type,
        rapporteur = rapporteur,
        text_tabled_id = paste(ref, collapse = "; "),
        vote_date = x$date,
        date_tally = idx_result
      ) %>%
      dplyr::relocate(vote_date, .before = type) %>%
      dplyr::relocate(date_tally, .after = vote_date) %>%
      dplyr::relocate(text_tabled_id, .before = vote_date)

    ## vote requests
    # iterate over vote requests

    requests <- lapply(vote_requests, function(z) {

      # request table at the <TABLE> tag level
      request_table <- z %>%
        xml2::xml_find_all(xpath = ".//TABLE")

      # transform requests into a table
      if (length(request_table) != 0) {

        # get the table
        requests <- xml_table(
          xml_nodeset = request_table,
          row_1_var_names = FALSE,
          is.requests.table = TRUE
        )

        # check whether the first row contains the variable names
        if (names(requests)[1] == "...1") {
          names(requests) <- unlist(requests[1, ])
          requests <- clean_column_names(data_set = requests)
          requests <- dplyr::slice(.data = requests, -1)
        }

      } else {
        requests <- NULL
      }

      # add id and date columns to table
      if (length(requests) > 0) {
        requests <- requests %>%
          dplyr::mutate(
            vote_date = !!x$date,
            date_tally = idx_result
          ) %>%
          dplyr::relocate(vote_date, .before = 1) %>%
          dplyr::relocate(date_tally, .after = vote_date)
      }
      return(requests)
    })

    # object to return in loop over voting results
    return(list(votes = votes, requests = requests))

  }) # end of loop over voting results

  # votes
  votes <- dplyr::tibble(data = results) %>%
    tidyr::unnest_wider(col = data) %>%
    dplyr::pull(var = votes)

  # check whether requests column exists
  requests_exist <- dplyr::tibble(data = results) %>%
    tidyr::unnest_wider(col = data) %>%
    exists(x = "requests", where = .)

  # requests and miscellaneous
  if (requests_exist) {
    requests <- dplyr::tibble(data = results) %>%
      tidyr::unnest_wider(col = data) %>%
      dplyr::pull(var = requests)
  } else {
    requests <- NULL
  }

  # remove null values from requests list
  requests[sapply(X = requests, FUN = is.null)] <- NULL

  # combine results in one tibble
  votes <- dplyr::tibble(
    ep = x$ep,
    vote_date = x$date,
    votes_meta = list(votes_meta),
    votes = list(votes),
    requests = list(requests)
  )

  # function output
  return(votes)

}


# html method: search votes -----------------------------------------------
#' @export
search_votes.html <- function(x, data_dir, verbose, ...) { # nolint

  # initial binding for gloabls
  description <- type <- X1 <- X2 <- NULL # nolint

  # load the file
  page <- xml2::read_html(
    x = sprintf("%sraw/votes/%s", data_dir, x$`name_raw/votes`),
    encoding = "UTF-8"
  )

  #-------------------------------------------------------------------------
  # meta data
  #-------------------------------------------------------------------------

  # titles of the votes
  titles <- page %>%
    rvest::html_nodes(css = ".doc_title td") %>%
    rvest::html_text(trim = TRUE) %>%
    .[. != ""]

  # remove duplicates
  titles <- unique(titles)

  # vote descriptions
  vote_descriptions <- page %>%
    rvest::html_elements(
      css = "p[style='font-style:italic; margin-bottom:0px']"
    ) %>%
    rvest::html_text()

  # titles as tibble and add in ep, date, and an id that is a running variable
  titles <- dplyr::tibble(titles) %>%
    dplyr::rename(title = titles) %>%
    dplyr::mutate(
      date_tally = seq(seq_len(nrow(.))),
      vote_date = x$date,
      ep = x$ep,
      description = vote_descriptions
    ) %>%
    dplyr::relocate(description, .after = title)

  # message about number of votes in the document
  if (verbose) {
    message(
      "Document: ", x$`name_raw/votes`, " contains ",
      length(titles), " topics"
    )
  }

  #-------------------------------------------------------------------------
  # individual votes
  #-------------------------------------------------------------------------

  # find relevant tables (that include individual vote choices)
  table_idx <- page %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_attr("style") %>%
    stringr::str_detect(pattern = "border-collapse:collapse") %>%
    which

  # parse tables
  vote_tables <- page %>%
    rvest::html_nodes(css = "table") %>%
    .[table_idx] %>%
    rvest::html_table()

  # vote IDs counter (relates back to the titles)
  vote_ids <- 0

  # container for remarks below tables such as roll-call requests
  vote_miscellaneous <- vector(
    mode = "list",
    length = (length(vote_tables) - 1)
  )

  # container for vote table results
  votes <- vector(mode = "list", length = (length(vote_tables) - 1))

  # loop over the vote tables (skip first which contains explanations
  # of symbols)
  for (idx in 2:length(vote_tables)){

    # clean tables ------------------------------------------------------------

    # check for new votes by looking for the word subject in column 1, row 2
    new_vote <- vote_tables[[idx]] %>%
      dplyr::pull(var = 1) %>%
      stringr::str_detect(pattern = "Subject") %>%
      .[2]

    # increase vote ID for new votes
    if (new_vote) {

      vote_ids <- vote_ids + 1

      # check whether the first row of the table is empty
      if (all(unlist(dplyr::slice(.data = vote_tables[[idx]], 1)) == "")) {

        # remove the first row from the table
        vote_tables[[idx]] <- dplyr::slice(.data = vote_tables[[idx]], -1)
      }

      # the first row of the table becomes the variable names
      names(vote_tables[[idx]]) <- vote_tables[[idx]] %>%
        dplyr::slice(1) %>%
        unlist

      # clean column names
      vote_tables[[idx]] <- clean_column_names(data_set = vote_tables[[idx]])

      # remove the row that contains the variable names
      vote_tables[[idx]] <- vote_tables[[idx]] %>%
        dplyr::slice(-1)

      # repeated cell entries across rows to fill in multi-row entries
      # pull out the type variable
      vote_tables[[idx]] <- list(tbl = vote_tables[[idx]])
      class(vote_tables[[idx]]) <- c(class(vote_tables[[idx]]), "xml_html")
      out <- fill_in_cells(rows = vote_tables[[idx]])

      # add vote id columns
      out <- dplyr::mutate(
        .data = out,
        vote_date = x$date,
        date_tally = vote_ids
      )

    } # end of new vote condition

    # extract voting results from tables --------------------------------------

    # take the table out of the list container
    if ("tbl" %in% names(vote_tables[[idx]])) {
      vote_tables[[idx]] <- vote_tables[[idx]]$tbl
    }

    # check whether the tibble contains a "Vote" or a "vote" column
    if (("Vote" %in% names(vote_tables[[idx]]) || "vote" %in% names(
      vote_tables[[idx]]
    ))) {

      # save voting tables result
      votes[[idx]] <- out

    } else {

      # remove empty rows in remarks below votes
      out <- vote_tables[[idx]]

      # remove NA columns
      out <- out %>%
        purrr::map(~.x) %>%
        purrr::discard(~all(is.na(.x))) %>%
        purrr::map_df(~.x)

      # check whether the first row of the table is empty
      if (all(unlist(dplyr::slice(.data = out, 1)) == "")) {
        # remove the first row from the table
        out <- dplyr::slice(.data = out, -1)
      }

      ## remove repeated cell entries
      # check whether the table has at least two columns
      if (ncol(out) > 1) {
        # loop over all rows in the tibble
        out <- lapply(X = seq(from = 1, to = nrow(out)), FUN = function(y) {

          # extract current row
          y <- dplyr::slice(.data = out, y)

          # check whether all cells are the same as the first cell
          if (all(dplyr::pull(.data = y, var = 1) == y[-1])) {

            # add a new variable
            y <- y %>%
              dplyr::mutate(type = X1) %>%
              dplyr::relocate(type, .before = X1)

            # replace all cells except the first with an empty string
            y <- y %>%
              dplyr::mutate(
                dplyr::across(.cols = -1, .fns = function(z) z <- "")
              )
          }
          return(y)
        })

        # combine list to current table
        out <- dplyr::bind_rows(out)

        # if type is empty except for the first entry then add
        # it to the remaining rows
        out <- out %>%
          dplyr::mutate(type = ifelse(
            test = is.na(type),
            yes = dplyr::pull(.data = ., var = 1)[1],
            no = type
          ))

        # remove rows where X1 and X2 columns are empty
        out <- out %>%
          dplyr::filter(X1 != "" & X2 != "")

      } # end of table has at least 2 columns check

      # add id date columns to table
      out <- out %>%
        dplyr::mutate(
          vote_date = x$date,
          date_tally = vote_ids
        )

      # save
      vote_miscellaneous[[idx]] <- out

    } # end of no vote column in table check

    # remove out
    rm(out)
  } # end of loop over the vote tables

  # remove null values from the votes list
  votes[sapply(X = votes, FUN = is.null)] <- NULL

  # remove null values from miscellaneous list
  vote_miscellaneous[sapply(X = vote_miscellaneous, FUN = is.null)] <- NULL

  # if there are no miscellaneous additions set vote_miscellaneous NULL
  if (length(vote_miscellaneous) == 0) vote_miscellaneous <- NULL

  # remove empty tables from the miscellaneous list
  if (!is.null(vote_miscellaneous)) {
    vote_miscellaneous[sapply(X = vote_miscellaneous, FUN = function(y) {
      if (dplyr::is.tbl(y)) {
        nrow(y) == 0
      }
    })] <- NULL

  }

  # combine results in one tibble
  votes <- dplyr::tibble(
    ep = x$ep,
    vote_date = x$date,
    votes_meta = list(titles),
    votes = list(votes),
    requests = (list(vote_miscellaneous))
  )

  # function output
  return(votes)
}