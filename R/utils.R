# check date format for character dates -----------------------------------

#' Catches user input errors for character stop dates
#'
#' \code{character_date_format()} checks for date format entry in the form
#' "%d-%m-%y".
#'
#' @export
character_date_format <- function(usr_date, date_format = "%d-%m-%Y") {

  # does the user supplied date convert into Date class with
  # the format "%d-%m-%Y"?
  date_error <- try(
    expr = !is.na(as.Date(usr_date, date_format)),
    silent = TRUE
  )

  # to logical
  date_error <- !"try-error" %in% class(date_error)

  return(date_error)
}


# Suppress cat ------------------------------------------------------------

#' Suppress cat in external package
#'
#' \code{quiet()} suppresses cat output.
#'
#' @param x Input. It can be any kind.

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


# Extracts votes from roll call node --------------------------------------
extract_votes <- function(x, vote) {

  # loop over all parties
  party_level_out <- lapply(x, function(y) {

    # party name
    party_group <- y %>%
      xml2::xml_attr(attr = "Identifier")

    # voter names
    names <- y %>%
      xml2::xml_children() %>%
      xml2::xml_text()

    # MEP IDs
    mep_ids <- y %>%
      xml2::xml_children() %>%
      xml2::xml_attr(attr = "MepId") %>%
      as.numeric

    # Personal IDs
    personal_id <- y %>%
      xml2::xml_children() %>%
      xml2::xml_attr(attr = "PersId") %>%
      as.numeric

    # data frame
    out <- dplyr::tibble(
      mep_id = mep_ids,
      personal_id = personal_id,
      party_group = party_group,
      name = names,
      vote = vote,
      vote_correction = 0
    )

    return(out)
  })

  # combine party level results list to tibble
  out <- dplyr::bind_rows(party_level_out)
  return(out)
}


# Extracts IDs and names from correction votes ----------------------------

extract_correction_votes <- function(x, vote) {

  # MEP id
  mep_id <- x %>%
    xml2::xml_children() %>%
    xml2::xml_attr(attr = "MepId") %>%
    as.numeric

  # Personal id
  pers_id <- x %>%
    xml2::xml_children() %>%
    xml2::xml_attr(attr = "PersId") %>%
    as.numeric

  # Name
  name <- x %>%
    xml2::xml_children() %>%
    xml2::xml_text()

  # combine to tibble
  out <- dplyr::tibble(
    mep_id = mep_id,
    personal_id = pers_id,
    party_group = NA,
    name = name,
    vote = vote,
    vote_correction = 1
  )

  return(out)
}


# download 1) attendance lists; 2) votes; 3) roll-calls -------------------
download_targets <- function(
  what, where_to, indexes, types, nodeset,
  libre_office_path = NULL, data_dir = data_dir
) {

  # check whether file extension exists in nodeset
  if (any(grepl(pattern = what, x = types, ignore.case = TRUE))) {

    # find node
    idx <- types %>%
      stringr::str_detect(
        pattern = stringr::fixed(what, ignore_case = TRUE)
      ) %>%
      which()
    out <- nodeset[[indexes[idx]]]

    # extract link
    link <- rvest::html_attr(x = out, name = "href")

    # make sure to recognize word
    if (grepl(pattern = "word", x = what, ignore.case = TRUE)) {
      # extract doc or docx
      what <- stringr::str_extract(string = link, pattern = "docx?$")
    }

    # extract file name from link
    file_name <- link %>%
      strsplit(split = "/") %>%
      unlist %>%
      grep(pattern = paste("*.", what, sep = ""),
           value = TRUE, ignore.case = TRUE)

    # construct complete URL
    url <- paste("https://www.europarl.europa.eu", link, sep = "")

    if (stringr::str_detect(string = url, pattern = "_EN\\.")) {
      # download file
      out_file <- try(expr = {
        # html
        if (tolower(x = what) == "html") {
          xml2::download_html(
            url = url,
            file = sprintf("%s%s/%s", data_dir, where_to, file_name),
            quiet = FALSE
          )
          # xml or doc
        } else {
          xml2::download_xml(
            url = url,
            file = sprintf("%s%s/%s", data_dir, where_to, file_name),
            quiet = FALSE
          )
        }
        closeAllConnections()
      }, silent = TRUE)
      if ("try-error" %in% class(out_file)) {
        warning("Download of, ", what, " failed")
      }

      # output data
      out <- dplyr::tibble(
        name = file_name,
        url = url,
        format = what,
        folder = where_to
      )
    } else {
      # output data if source is not available in English
      out <- dplyr::tibble(
        name = NA_character_,
        url = NA_character_,
        format = what,
        folder = where_to
      )
    }
  } else {
    # output data
    out <- dplyr::tibble(
      name = NA_character_,
      url = NA_character_,
      format = what,
      folder = where_to
    )
  }
  return(out)
}


# download 1) attendance lists; 2) votes; 3) roll-calls -------------------
download_french_targets <- function(
  what, where_to, indexes, types, nodeset,
  data_dir = data_dir
) {

  # check whether file extension exists in nodeset
  if (any(grepl(pattern = what, x = types, ignore.case = TRUE))) {

    # find node
    idx <- types %>%
      stringr::str_detect(
        pattern = stringr::fixed(what, ignore_case = TRUE)
      ) %>%
      which()
    out <- nodeset[[indexes[idx]]]

    # extract link
    link <- rvest::html_attr(x = out, name = "href")

    # extract file name from link
    file_name <- link %>%
      strsplit(split = "/") %>%
      unlist %>%
      grep(pattern = paste("*.", what, sep = ""), value = TRUE,
           ignore.case = TRUE)

    # construct complete URL
    url <- paste("https://www.europarl.europa.eu", link, sep = "")

    # download file
    out_file <- try(expr = {
      xml2::download_xml(
        url = url,
        file = sprintf("%s%s/%s", data_dir, where_to, file_name),
        quiet = FALSE
      )
      closeAllConnections()
    }, silent = TRUE)
    if ("try-error" %in% class(out_file)) {
      warning("Download of ", file_name, " failed")
    }

    # check whether the downloaded file exists
    if (!file.exists(paste(data_dir, where_to, file_name, sep = "/"))) {

      # check whether a curltmp file exists
      if (file.exists(paste(data_dir, where_to,
                            paste0(file_name, ".curltmp"), sep = "/"))) {
        # delete the curltmp file
        file.remove(paste(data_dir, where_to,
                          paste0(file_name, ".curltmp"), sep = "/"))
      }

      # download once more

      # download file
      out_file <- try(expr = {
        xml2::download_xml(
          url = url,
          file = sprintf("%s%s/%s", data_dir, where_to, file_name),
          quiet = FALSE
        )
        closeAllConnections()
      }, silent = TRUE)
    }

    # check file for output tibble
    if (file.exists(paste(data_dir, where_to, file_name, sep = "/"))) {
      # output data
      out <- dplyr::tibble(
        name = file_name,
        url = url,
        format = what,
        folder = where_to
      )
    } else {
      # output data
      out <- dplyr::tibble(
        name = NA_character_,
        url = NA_character_,
        format = what,
        folder = where_to
      )
    }
  } else {
    # output data
    out <- dplyr::tibble(
      name = NA_character_,
      url = NA_character_,
      format = what,
      folder = where_to
    )
  }
  return(out)
}


# extract xml table -------------------------------------------------------
# @param xml_nodeset The xml nodeset at the <TABLE> tag level. An xml2 nodeset.
# @param row_1_var_nams Whether to turn the values in the first row into
# variable names
xml_table <- function(xml_nodeset, row_1_var_names, is.requests.table = FALSE) {

  # extract all rows from the table
  xml_nodeset <- xml_nodeset %>%
    xml2::xml_find_all(xpath = ".//TR")

  # find the total number of columns
  column_n_max <- lapply(xml_nodeset, function(y) {
    y %>%
      xml2::xml_find_all(xpath = ".//TD") %>%
      length
  }) %>%
    unlist %>%
    max

  # check the second row also
  column_n_max_row2 <- try(expr = {
    xml_nodeset[[2]] %>%
      xml2::xml_find_all(xpath = ".//TD") %>%
      length
  }, silent = TRUE)
  if ("try-error" %in% class(column_n_max_row2)) column_n_max_row2 <- 0
  column_n_max <- max(column_n_max, column_n_max_row2)

  # iterate over the rows in the vote table
  rows <- lapply(X = xml_nodeset, FUN = function(row) {

    # column node sets
    row_columns <- row %>%
      xml2::xml_find_all(xpath = ".//TD")

    # extract the number of columns
    column_n <- row_columns %>%
      xml2::xml_attr(attr = "COLNAME") %>%
      stringr::str_extract(pattern = "\\d+") %>%
      as.integer

    # return NULL if no columns found
    if (all(is.na(column_n))) {
      columns <- NULL
    } else {
      # which columns actually exist
      columns_exist <- which(seq(from = 1, to = max(column_n)) %in% column_n)

      # columns container
      columns <- vector(mode = "list", length = max(column_n))

      # column width container
      colspan <- vector(mode = "list", length = max(column_n))

      # iterate over the columns in the vote table
      for (col_idx in 1:max(column_n)) {

        # check wheter loop iteration refers to an existing column
        if (col_idx %in% columns_exist) {

          # extract the current column
          column <- row_columns[[which(col_idx == columns_exist)]]

          # extract the column number
          col_number <- column %>%
            xml2::xml_attr(attr = "COLNAME") %>%
            stringr::str_extract(patter = "\\d+") %>%
            as.integer

          # check if the current column number corresponds to the loop index
          if (col_idx == col_number) {

            # how many columns does this column span
            colspan[[col_idx]] <- column %>%
              xml2::xml_attr(attr = "COLSPAN") %>%
              as.integer

            # set colspan to 1 if the argument is not used
            if (is.na(colspan[[col_idx]])) colspan[[col_idx]] <- 1

            # also set colspan to 1 if the number of columns that
            # still have to be filled is equal or greater to the
            # number of columns in the table
            if (((col_idx - 1) + length(columns_exist)) >= column_n_max) {
              colspan[[col_idx]] <- 1
            }

            # columns already filled
            cols_already_filled <- sum(unlist(colspan[-col_idx]))

            # columns that still need to be filled
            cols_need_filling <- ifelse(
              test = col_idx == max(columns_exist),
              yes = 0,
              no = sum(sapply(columns, is.null)) - 1
            )

            # check whether colspan is too wide given the number
            # of remaining columns
            if (cols_already_filled + colspan[[col_idx]] +
                  cols_need_filling > column_n_max) {

              # available budget
              colspan[[col_idx]] <- column_n_max -
                cols_already_filled - cols_need_filling
            }

            # if colpsan is greater than the maximum number of
            # columns, set it to the maximum number of columns
            if (colspan[[col_idx]] > column_n_max) {
              colspan[[col_idx]] <- column_n_max
            }

            # check whether the columns contain line breaks
            line_breaks <- column %>%
              rvest::html_elements(css = "BR") %>%
              length

            # line breaks found
            if (line_breaks != 0) {

              # content of the column
              content <- column %>%
                xml2::xml_find_all(xpath = "./P") %>%
                xml2::xml_find_all(xpath = ".//text()")

              # check whether content length corresponds to
              # number of line breaks
              if ((length(line_breaks) + 1) == length(content)) {

                # insert line breaks
                content <- paste(content, collapse = "\n")
              }
            } else {

              # content of the column
              content <- column %>%
                xml2::xml_children() %>%
                xml2::xml_text(trim = TRUE)
            }

            # attempt to extract for votes
            for_votes <- column %>%
              xml2::xml_find_all(
                xpath = ".//Vote.Result.Table.TotalVote.For"
              ) %>%
              xml2::xml_text(trim = TRUE) %>%
              as.integer()

            # attempt to extract against votes
            against_votes <- column %>%
              xml2::xml_find_all(
                xpath = ".//Vote.Result.Table.TotalVote.Against"
              ) %>%
              xml2::xml_text(trim = TRUE) %>%
              as.integer()

            # attempt to extract abstentions
            abstention_votes <- column %>%
              xml2::xml_find_all(
                xpath = ".//Vote.Result.Table.TotalVote.Abstention"
              ) %>%
              xml2::xml_text(trim = TRUE) %>%
              as.integer()

            # add votes separated by comma if they are available
            if (
              sum(
                length(for_votes), length(against_votes),
                length(abstention_votes)
              ) > 0
            ) {
              columns[[col_idx]] <- as.list(
                rep(
                  x = paste(
                    for_votes, against_votes, abstention_votes, sep = ","
                  ), time = colspan[[col_idx]]
                )
              )
            } else { 
              columns[[col_idx]] <- as.list(
                rep(
                  x = paste(content, collapse = " "), time = colspan[[col_idx]]
                )
              )
            }

            # if content is character(0), add empty string ""
            if (length(content) == 0) {
              columns[[col_idx]] <- as.list(
                rep(x = "", time = colspan[[col_idx]])
              )
            }

          } else {
            # bug fixing
            #stop ("Current loop iteration does not correspond to column name")
            next
          }
        } else {
          columns[[col_idx]] <- as.list("")
          colspan[[col_idx]] <- 1
        }
      } # end of loop over columns

      # in requests table duplicates can be removed
      if (is.requests.table) {
        for (ii in seq_along(unlist(columns))) {
          if (ii > 1) {
            if (unlist(columns)[[ii]] == unlist(columns)[[ii - 1]]) {
              columns[[1]][[ii]] <- ""
            }
          }
        }
      }

      # column bind elements and suppress the missing column names message
      columns <- suppressMessages(dplyr::bind_cols(columns))
      return(columns)
      rm(columns)
    }
  }) # end of loop over rows of the table

  # row bind results
  rows <- dplyr::bind_rows(rows)

  # variable names are in the first row of the table
  if (row_1_var_names) {

    # the first row of the table becomes the variable names
    table_names <- rows %>%
      dplyr::slice(1) %>%
      unlist

    # replace empty strings with placeholders
    if (any(table_names == "") || any(is.na(table_names))) {
      table_names[which(table_names == "" | is.na(table_names))] <-
        paste0("V", seq(
          from = 1, to = sum(table_names == "" | is.na(table_names))
        ))
    }

    # assign the names
    names(rows) <- table_names

    # remove the row that contains the variable names
    rows <- rows %>%
      dplyr::slice(-1)
  }

  return(rows)
}


# extract doc table -------------------------------------------------------
# Extracts a table from a .doc document
#' @export
doc_table <- function(docx, tbl_number, header = TRUE, ns = ns) {

  # extract the table
  tbl <- docx$tbls[[tbl_number]]

  # find all rows in the table
  xml_nodeset <- xml2::xml_find_all(tbl, "./w:tr", ns = ns)

  # maximum number of columns
  max_columns <- sapply(xml_nodeset, function(y) {

    # first check for rows within row (should not happen)
    row_exception <- y %>%
      xml2::xml_find_all(xpath = ".//w:tr", ns = ns)
    if (length(row_exception) == 0) {
      n <- y %>%
        xml2::xml_find_all(xpath = ".//w:tc", ns = ns) %>%
        length
    } else {
      n <- sapply(row_exception, function(z) {
        # first check for rows within row (should not happen)
        row_exception2 <- z %>%
          xml2::xml_find_all(xpath = ".//w:tr", ns = ns)
        if (length(row_exception2) == 0) {
          n <- z %>%
            xml2::xml_find_all(xpath = ".//w:tc", ns = ns) %>%
            length
        } else {
          n <- sapply(row_exception2, function(z2) {
            n <- z2 %>%
              xml2::xml_find_all(xpath = ".//w:tc", ns = ns) %>%
              length
            return(n)
          }) %>%
            max()
        }
        return(n)
      }) %>%
        max()
    }
    return(n)
  }) %>%
    max()

  # get colspan for each cell
  colspan <- get_colspan(
    xml_nodeset = xml_nodeset,
    max_columns = max_columns, ns = ns
  )

  # loop over all rows
  rows <- lapply(X = seq_along(xml_nodeset), FUN = function(row) {

    # colspan
    row_colspan <- colspan[[row]]

    # column node sets
    row_columns <- xml_nodeset[[row]] %>%
      xml2::xml_find_all(xpath = ".//w:tc", ns = ns)

    # columns container
    columns <- vector(mode = "list", length = max(max_columns))

    # counts actual columns with content
    col_content_idx <- 0

    skip_cols <- 0

    # iterate over the columns in the vote table
    for (col_idx in 1:max(max_columns)) {

      if (skip_cols > 0) {
        skip_cols <- skip_cols - 1
        columns[[col_idx]] <- ""
        next
      }

      col_content_idx <- col_content_idx + 1

      # extract the current column
      column <- row_columns[[col_content_idx]]

      # how many columns does this column span
      cell_span <- row_colspan[[col_content_idx]]
      if (cell_span > 1) skip_cols <- cell_span - 1

      # find line breaks
      line_breaks <- column %>%
        xml2::xml_find_all(xpath = ".//w:br", ns = ns) %>%
        length()

      # extract content and keep line breaks intact
      if (line_breaks != 0) {
        content <- column %>%
          xml2::xml_find_all(xpath = ".//w:t", ns = ns) %>%
          xml2::xml_text()
        if (length(content) == 0) {
          content <- column %>%
            xml2::xml_find_all(xpath = ".//w:sym", ns = ns) %>%
            xml2::xml_attr("char")
          if (length(content) == 1) {
            if (content == "f0af") {
              content <- "lapsed"
            }
          }
        }
        content <- paste(content, collapse = "\n")
        content <- stringr::str_trim(string = content, side = "both")
      } else {
        content <- column %>%
          xml2::xml_find_all(xpath = ".//w:t", ns = ns) %>%
          xml2::xml_text()
        content <- paste(content, collapse = " ")
        content <- stringr::str_trim(string = content, side = "both")
      }

      # store content
      columns[[col_idx]] <- content

    }

    # column bind elements and suppress the missing column names message
    columns <- suppressMessages(dplyr::bind_cols(columns))
    return(columns)
    rm(columns)

  })

  # row bind results
  rows <- dplyr::bind_rows(rows)

  # variable names are in the first row of the table
  if (header) {

    # the first row of the table becomes the variable names
    table_names <- rows %>%
      dplyr::slice(1) %>%
      unlist

    # replace empty strings with placeholders
    if (
      any(table_names == "") ||
        any(is.na(table_names)) ||
        any(table_names == "*")
    ) {
      table_names[
        which(table_names == "" | is.na(table_names) | table_names == "*")
      ] <- paste0(
        "V", seq(
          from = 1,
          to = sum(table_names == "" | is.na(table_names) | table_names == "*")
        )
      )
    }

    # assign the names
    names(rows) <- table_names

    # remove the row that contains the variable names
    rows_ <- try(expr = {
      rows %>%
        dplyr::slice(-1)
    }, silent = TRUE)

    if ("try-error" %in% class(rows_)) {
      rows <- rows[-1, ]
    } else {
      rows <- rows_
    }
  }

  return(rows)
}



# guess colspan of each cell ----------------------------------------------
#' Finds the colspan of each cell
#' xml_nodeset The table nodeset. An xml2 nodeset.
get_colspan <- function(xml_nodeset, max_columns, ns) {

  # loop over the rows of the table
  colspan <- lapply(xml_nodeset, function(row) {

    # column node sets
    row_columns <- row %>%
      xml2::xml_find_all(xpath = ".//w:tc", ns = ns)

    # loop over cells
    row_dim <- sapply(row_columns, function(cell) {

      # coll-span
      colspan <- cell %>%
        xml2::xml_find_first(xpath = ".//w:gridSpan") %>%
        xml2::xml_attr(attr = "val") %>%
        as.integer
      colspan <- ifelse(test = is.na(colspan), yes = 1, no = colspan)
    })
    return(row_dim)
  })

  # row-sums of colspan for each cell
  row_sums <- sapply(colspan, function(x) sum(x, na.rm = TRUE))

  # return colspan unchanged if the row-sums correspond to max number of columns
  if (all(row_sums == max_columns)) return(colspan)

  # correct colspan if row-sums do NOT correspond to the max number of columns
  if (any(row_sums != max_columns)) {

    # loop over rows
    colspan <- lapply(colspan, function(x) {

      # each column spans exactly one column if the number of cells is the same
      # as the max number of columns
      if (length(x) == max_columns) {
        x <- rep(x = 1, times = max_columns)
      }

      # if there is just one cell per row, let it span the max number of columns
      if (length(x) == 1) x <- max_columns

      # if the number of cells is greater than one but less than the max number
      # of columns, greedily span rows as far as possible
      if (length(x) > 1 & length(x) < max_columns) {

        # number of cells to fill
        n_cells <- length(x)

        # loop over cells
        for (idx_cell in 1:n_cells){

          # how many more cells to fill after this one
          cells_left <- n_cells - idx_cell

          # columns already filled
          already_filled <- ifelse(test = idx_cell > 1,
                                   yes = sum(x[1:(idx_cell - 1)]), no = 0)

          # maximum colspan of cell
          max_span <- max_columns - already_filled - cells_left

          # fill cells greedily
          x[idx_cell] <- ifelse(
            test = x[idx_cell] > max_span,
            yes = max_span,
            no = x[idx_cell]
          )
        }
      }
      return(x)
    })
  }
  return(colspan)
}


# save a .doc file as a new .docx file, using LibreOffice command  --------
# Function origin: docxtractr/R/utils.r
# https://CRAN.R-project.org/package=docxtractr
convert_doc_to_docx <- function(docx_dir, doc_file) {

  # initial binding for globals
  lo_path_missing <- NULL

  # libre office file path
  lo_path <- getOption("path.to.libreoffice")

  # if no path to libre office option is set
  if (is.null(lo_path)) {
    stop(lo_path_missing, call. = FALSE)
  }

  # convert doc to docx
  if (Sys.info()["sysname"] == "Windows") {
    convert_win(lo_path = lo_path, docx_dir = docx_dir, doc_file = doc_file)
  } else {
    convert_osx(lo_path = lo_path, docx_dir = docx_dir, doc_file = doc_file)
  }
}

# .docx to .doc convertion for Windows
# Function origin: docxtractr/R/utils.r
# https://CRAN.R-project.org/package=docxtractr
convert_win <- function(
  lo_path, docx_dir, doc_file, convert_to = 'docx:"MS Word 2007 XML"'
) {

  # initial binding for globals
  cmd <- libre_office_path <- docx_dir <- doc_file <- NULL

  cmd <- sprintf(
    '"%s" --convert-to %s -headless -outdir "%s" "%s"',
    libre_office_path,
    convert_to,
    docx_dir,
    doc_file
  )
  return(cmd)
}

# .docx to .doc convertion for OSX
# Function origin: docxtractr/R/utils.r
# https://CRAN.R-project.org/package=docxtractr
convert_osx <- function(lo_path, docx_dir, doc_file,
                        convert_to = 'docx:"MS Word 2007 XML"') {
  cmd <- sprintf(
    '"%s" --convert-to %s --headless --outdir "%s" "%s"',
    lo_path,
    convert_to,
    docx_dir,
    doc_file
  )

  res <- system(cmd, intern = TRUE)
  return(res)
}


#' Point to Local soffice.exe File
#'
#' Function to set an option that points to the local LibreOffice file
#' \code{soffice.exe}.
#'
#' @param path path to the LibreOffice soffice file
#'
#' @details For a list of possible file path locations for \code{soffice.exe},
#'  see
#' \url{https://github.com/hrbrmstr/docxtractr/issues/5#issuecomment-233181976}
#'
#' @return Returns nothing, function sets the option variable
#'  \code{path_to_libreoffice}.
#' @export
#'
#' @examples \dontrun{
#' set_libreoffice_path("local/path/to/soffice.exe")
#' }
set_libreoffice_path <- function(path) {
  stopifnot(is.character(path))

  if (!file.exists(path)) stop(sprintf("Cannot find '%s'", path), call. = FALSE)
  options("path.to.libreoffice" = path)
}



# Clean column names ------------------------------------------------------

#' Clean column names
#'
#' Function that cleans column names by lower casing them, removing trailing
#' full stops, replaces repeated full stops with one full stop and finally
#' replaces full stops with underscores.
#' @export
clean_column_names <- function(data_set) {

  # rename repeated columns
  if (any(duplicated(x = names(data_set)))) {

    # which columns names are repeated
    dup_idx <- which(duplicated(x = names(data_set)))

    # just one variable to rename
    if (length(dup_idx) == 1) {
      names(data_set)[dup_idx] <- paste0(names(data_set[dup_idx]), "2")
    } else {
      names(data_set)[dup_idx] <- paste0(
        names(data_set[dup_idx]),
        seq(from = 1, to = length(dup_idx))
      )
    }
  }

  # replace special characters
  names(data_set) <- make.names(names = names(data_set))

  # lower_case all variable names
  data_set <- data_set %>%
    dplyr::rename_with(
      .fn = function(x) tolower(x), .cols = dplyr::everything()
    )

  # replace all spaces and punctuation with full stops
  data_set <- data_set %>%
    dplyr::rename_with(.fn = function(x)
      stringr::str_replace_all(string = x, pattern = "[[:punct:][:space:]]", replacement = "\\."))

  # remove trailing full stops
  data_set <- data_set %>%
    dplyr::rename_with(.fn = function(x)
      stringr::str_remove(string = x, pattern = "\\.$"), .cols = dplyr::everything())

  # replace multiple full stops with 1 full stop
  data_set <- data_set %>%
    dplyr::rename_with(.fn = function(x)
      stringr::str_replace_all(string = x, pattern = "\\.\\.+", replacement = "\\."))

  # replace full stops with underscores
  data_set <- data_set %>%
    dplyr::rename_with(.fn = function(x)
      stringr::str_replace_all(string = x, pattern = "\\.", replacement = "_"))

  return(data_set)
}


# Match title position ----------------------------------------------------
#' Matches the position of titles within the xml tree structure
#' relative to all occurences of "//w:p|//w:tbl"
match_title <- function(string, pattern) {

  # matching nodes
  c_nodes <- xml2::xml_find_all(x = string, xpath = "//w:p|//w:tbl")

  # attempt to match the title
  c_match <- try(expr = {
    suppressWarnings(stringr::str_detect(string = c_nodes, pattern = pattern))
  }, silent = TRUE)

  # return the position of the first match if matched
  if (class(c_match) == "try-error") {
    return(c_match)
  } else {
    return(suppressWarnings(min(which(c_match))))
  }
}


# headings method: search descriptions ------------------------------------
#' Searches for the descriptions of the votes below the title
#' @export
search_descriptions <- function(page, t_pos) {

  # loop over title positions
  vote_descriptions <- lapply(X = t_pos, FUN = function(x) {

    # the current title
    c_title <- page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      .[x] %>%
      rvest::html_text(trim = TRUE)

    # if title contains Report and name of person
    if (stringr::str_detect(
      string = c_title, pattern = "(?<=F?i?n?a?l?\\s?[Rr]eport:).*")) {

      # does it contain the phrase "Final report"
      if (stringr::str_detect(string = c_title, pattern = "Final report:")) {
        the_pattern <- "(?<=Final [Rr]eport:).*"
      } else {
        the_pattern <- "(?<=[Rr]eport:).*"
      }

      # extract Report and name of person if it is in the title
      reports <- stringr::str_extract_all(
        string = c_title, pattern = the_pattern)

      # if the title contains a reference number as well
      if (stringr::str_detect(
        string = c_title, pattern = "[ABC]\\d+-\\d+/\\d{4}")) {

        # take the description from the current title if it already contains a
        # reference number
        ref_numbers <- stringr::str_extract_all(
          string = c_title, pattern = "[ABC]\\d+-\\d+/\\d{4}")

        # remove the reference numbers from the string and brackets
        reports <- reports %>%
          stringr::str_remove_all(pattern = "[ABC]\\d+-\\d+/\\d{4}") %>%
          stringr::str_remove_all(pattern = "\\(\\)$")

        # combine the multiple reference numbers
        ref_numbers <- paste(ref_numbers, collapse = "; ")

        # combine report name and reference number
        vote_descriptions <- paste("Report:", reports, ref_numbers)

        # end of condition: if the title contains a reference number as well
      } else {
        vote_descriptions <- paste("Report:", reports)
      } # end of else condition: if the title contains a reference number
      # as well
      # following: end of condition: title contains report and name of person
    } else {
      # take the description from the current title if it already contains a
      # reference number
      if (stringr::str_detect(
        string = c_title, pattern = "[ABC]\\d+-\\d+/\\d{4}")) {

        # extract reference number from title
        ref_numbers <- stringr::str_extract_all(
          string = c_title, pattern = "[ABC]\\d+-\\d+/\\d{4}")

        # combine the multiple reference numbers
        ref_numbers <- paste(ref_numbers, collapse = "; ")

        # combine report name and reference number
        vote_descriptions <- ref_numbers

      } # end of condition: reference number in title
    } # end of else condition: length(reports) != 0

    # loop over the 10 nodes following the title
    if (!exists("vote_descriptions")) {
      for (idx in 1:10) {

        # attempt to extract the description
        vote_descriptions <- page %>%
          xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
          .[x + idx] %>%
          rvest::html_text(trim = TRUE)

        # stop (break from loop if the heading of the votes table is reached)
        if (stringr::str_detect(
          string = vote_descriptions, pattern = "^SubjectRCV,")) {
          vote_descriptions <- ""
          break
        }

        # break from the loop if some string was extracted
        if (vote_descriptions != "" & vote_descriptions != c_title &
          vote_descriptions != "***") {

          # check whether the last character is a colon
          if (stringr::str_detect(
            string = vote_descriptions[[1]], pattern = "(:)$")) {

            # extract the next line as well
            vote_descriptions_2 <- page %>%
              xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
              .[x + idx + 1] %>%
              rvest::html_text(trim = TRUE)

            # extract the next next line as well
            vote_descriptions_3 <- page %>%
              xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
              .[x + idx + 2] %>%
              rvest::html_text(trim = TRUE)

            # extract the next next next line as well
            vote_descriptions_4 <- page %>%
              xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
              .[x + idx + 3] %>%
              rvest::html_text(trim = TRUE)


            # combine all for strings if none are empty
            if (vote_descriptions_4 != "" & vote_descriptions_3 != "") {

              # combine all 4 strings
              vote_descriptions <- paste(
                vote_descriptions, vote_descriptions_2, vote_descriptions_3,
                vote_descriptions_4, sep = " ")

            } else if (vote_descriptions_4 == "" & vote_descriptions_3 != "") {

              # combine all 3 strings
              vote_descriptions <- paste(
                vote_descriptions, vote_descriptions_2, vote_descriptions_3,
                sep = " ")

            } else {
              # combine both strings
              vote_descriptions <- paste(
                vote_descriptions, vote_descriptions_2, sep = " ")
            }
          }
          break
        } # end of break from loop if some string was detected
      } # end of loop over the 10 nodes following the title
    } # end of condition: !exists("vote_descriptions")
    return(vote_descriptions)
  })
  return(unlist(vote_descriptions))
}



# Clean title positions matches -------------------------------------------
#' Filters out multiple exact matches
#' @export
clean_title_position <- function(y) {

  # matches per title
  n_matches <- sapply(y, length)

  # clean if there is more than 1 match per title
  if (any(n_matches > 1)) {

    # check if there are 2 elements with more than 1 match
    if (length(y[which(n_matches > 1)]) == 2) {

      # the first element gets the first match
      y[which(n_matches > 1)][[1]] <- min(y[which(n_matches > 1)][[1]])

      # the second element gets the second match
      y[which(n_matches > 1)][[2]] <- max(y[which(n_matches > 1)][[2]])
    } else {

      # more than 2 elements with more than 1 match

      # the title positions
      indexes <- lapply(y[which(n_matches > 1)], sort)

      for (idx in seq_along(which(n_matches > 1))) {
        y[which(n_matches > 1)][[idx]] <- indexes[[idx]][1]
      }
    }
  } # end of condition more than 1 match
  return(y)
}



# Extract amendment number and add to rcv meta data -----------------------
#' Adds the amendment number of an rcv to the rcv meta data
add_ammendment_number <- function(x) {

  # loop over the data line by line
  am_no <- lapply(x$rcv_desc, function(y) {

    # everything after a dash starting from the end of the string
    after_dash <- stringr::str_extract(string = y, pattern = "(?:-[^-\r\n]*)$")

    # remove the dash
    after_dash <- stringr::str_remove(string = after_dash, pattern = "-")

    # separate number and type
    no <- stringr::str_extract(
      string = after_dash,
      pattern = "[\\d|\\w]*[/]*[\\d]+[\\s]*[\\w]*[/]*[\\d]*")
    after_dash <- stringr::str_remove(
      string = after_dash,
      pattern = "[\\d]*[/]*[\\d]+")

    # trim white spaces
    no <- stringr::str_trim(string = no, side = "both")
    after_dash <- stringr::str_trim(string = after_dash, side = "both")

    return(am_no = list(type = after_dash, number = no))
  })

  # un-nest data
  am_no <- am_no %>%
    dplyr::tibble(data = .) %>%
    tidyr::unnest_wider(col = data)

  # attach the variable
  x <- dplyr::bind_cols(x, am_no)

}


# add aggregate vote results to rcv meta data -----------------------------
add_agg_rcv_votes <- function(rcv_votes, rcv_meta) {

  # initial binding for globals
  rcv_id <- vote <- NULL

  # aggregate votes
  agg_votes <- rcv_votes %>%
    dplyr::group_by(rcv_id) %>%
    dplyr::summarise(
      yay = sum(vote == "+"),
      nay = sum(vote == "-"),
      abstention = sum(vote == "0"), .groups = "drop")

  # merge into meta data
  rcv_meta <- dplyr::left_join(x = rcv_meta, y = agg_votes, by = "rcv_id")

  return(rcv_meta)
}



# extract from doc_subtitle_level2 class ----------------------------------
#' function extracts all information organised in a table tag with
#' the class: "doc_subtitle_level2"
level2_info_fun <- function(page) {

  # table containing the doc_subtitle_level2 class
  level2_info <- page %>%
    rvest::html_elements(css = "table[class='doc_subtitle_level2']") %>%
    rvest::html_text2()

  # use the p tag if level2_info is character(0)
  if (length(level2_info) == 0) {
    level2_info <- page %>%
      rvest::html_elements(css = "p[class='doc_subtitle_level2']") %>%
      rvest::html_text()
  }

  # separate on new lines
  level2_info <- stringr::str_split(string = level2_info, pattern = "\n")

  # remove empty elements
  level2_info <- lapply(X = level2_info, FUN = function(z) {
    z[which(z != "")]
  })

  return(level2_info)
} # end of level2_info_fun()



# extract from doceo_notice_mainbloc class --------------------------------
level_doceo_notice_mainbloc_fun <- function(page) {

  # table containing the "doceo_notice_mainbloc" class
  doceo_notice_mainbloc <- page %>%
    rvest::html_elements(css = "table[class='doceo_notice_mainbloc']") %>%
    rvest::html_text2()

  # separate on new lines
  doceo_notice_mainbloc <- stringr::str_split(
    string = doceo_notice_mainbloc, pattern = "\n"
  )

  # remove empty elements
  doceo_notice_mainbloc <- lapply(X = doceo_notice_mainbloc, FUN = function(z) {
    z[which(z != "")]
  })

  return(doceo_notice_mainbloc)

}


# extract from level_doceo_notice_mainbloc class --------------------------
level_doc_subtitle_level1_fun <- function(page) {

  # column containing the "doc_subtitle_level1" class
  doc_subtitle_level1 <- page %>%
    rvest::html_elements(css = "td[class='doc_subtitle_level1']") %>%
    rvest::html_text2()

  # separate on new lines
  doc_subtitle_level1 <- stringr::str_split(
    string = doc_subtitle_level1, pattern = "\n"
  )

  # remove empty elements
  doc_subtitle_level1 <- lapply(X = doc_subtitle_level1, FUN = function(z) {
    z[which(z != "")]
  })

  return(doc_subtitle_level1)

}


# extract from doc_box_header class ---------------------------------------
level_doc_box_header_fun <- function(page) {

  # table containing the "doc_box_header" class
  doc_box_header <- page %>%
    rvest::html_elements(css = "table[class='doc_box_header']") %>%
    rvest::html_text2()

  # separate on new lines
  doc_box_header <- stringr::str_split(string = doc_box_header, pattern = "\n")

  # remove empty elements
  doc_box_header <- lapply(X = doc_box_header, FUN = function(z) {
    z[which(z != "")]
  })
}



# extract lead committee and rapporteur -----------------------------------
#' function extracts lead committee and lead rapporteur from level2_info
#' and removes these entries from level2_info
level2_lead_extract <- function(x) {

  if (is.null(x)) {
    ctte <- NA
    rap <- NA
  } else {
    # check whether x is a list
    if (is.list(x)) {
      # unlist x if it is a list
      x <- unlist(x)
    }

    # check whether x is a character vector
    if (is.character(x)) {

      ## lead committee
      # search for <Commission> and </Commission> tags
      if (any(grepl(pattern = "<Commission>", x = x, fixed = TRUE))) {
        # position of the opening tag
        open_tag_pos <- grep(pattern = "<Commission>", x = x, fixed = TRUE)
        # position of the closing tag
        close_tag_pos <- grep(pattern = "</Commission>", x = x, fixed = TRUE)
        # extract the lead committee(s)
        ctte <- x[open_tag_pos:close_tag_pos]
        # combine the tags if there are multiple tags
        if (length(ctte) > 1) {
          ctte <- paste(ctte, collapse = "; ")
        }
        # remove the opening tag
        ctte <- sub(
          pattern = "<Commission>",
          replacement = "",
          fixed = TRUE,
          x = ctte
        )
        # remove the closing tag
        ctte <- sub(
          pattern = "</Commission>",
          replacement = "",
          fixed = TRUE,
          x = ctte
        )
        # check whether ctte object contains committee abbreviation in
        # curly braces
        if (stringr::str_detect(string = ctte, pattern = "\\{\\w+\\}")) {
          ctte <- stringr::str_replace(
            string = ctte,
            pattern = "\\{\\w+\\}",
            replacement = ""
          )
        }
        # remove the element with the lead committee from the vector
        if (open_tag_pos != close_tag_pos) {
          x <- x[c(-open_tag_pos, -close_tag_pos)]
        } else {
          x <- x[-open_tag_pos]
        }
      } else {
        # search for an element with a match on 'Committee"
        ctte_pos <- stringr::str_which(
          string = x,
          pattern = stringr::regex(pattern = "committee", ignore_case = TRUE)
        )
        # match on the first match only (the lead committee is always mentioned
        # before the associated committees)
        ctte_pos <- ctte_pos[1]
        # remove white spaces
        ctte <- trimws(x = x[ctte_pos], which = "both")
        # check whether the ctte object contains <Commission> tags and remove
        # if so
        if (grepl(pattern = "<Commission>", x = ctte, fixed = TRUE)) {
          ctte <- sub(
            pattern = "<Commission>", replacement = "", fixed = TRUE, x = ctte
          )
          ctte <- sub(
            pattern = "</Commission>", replacement = "", fixed = TRUE, x = ctte
          )
        }
        # check whether ctte object contains committee abbreviation in curly
        # braces
        if (grepl(pattern = "\\{[A-Z]+||}", x = ctte, perl = TRUE)) {
          # remove everything within and including the curly braces
          ctte <- sub(
            pattern = "\\{[A-Z]+\\}", replacement = "", perl = TRUE, x = ctte
          )
        }
        # remove the element with the lead committee from the vector
        x <- x[-ctte_pos]
      }

      ## lead rapporteur
      # element with the pattern "Rapporteur" or "Co-Rapporteurs" or
      # "Co-rapporteurs"
      rap_pos <- stringr::str_which(
        string = x,
        pattern = stringr::regex(pattern = "rapporteur", ignore_case = TRUE)
      )
      # match the first match (same reason as above)
      rap_pos <- rap_pos[1]
      # extract the name which is everything after the colon:
      rap <- stringr::str_extract(
        string = x[rap_pos], pattern = "(?<=\\:)[\\s\\S]*"
      )
      # remove white spaces
      rap <- trimws(x = rap, which = "both")
      # check whether the rap object contains <Depute> tags
      if (grepl(pattern = "<Depute>", x = rap, fixed = TRUE)) {
        # remove opening and closing Depute tags
        rap <- sub(
          pattern = "<Depute>", replacement = "", x = rap, fixed = TRUE
        )
        rap <- sub(
          pattern = "</Depute>", replacement = "", x = rap, fixed = TRUE
        )
      }
      # check in ctte whether there are multiple lead committees
      if (grepl(pattern = ";", x = ctte, fixed = TRUE)) {
        # count number of committee delimiters (ie number of Committees -1)
        ctte_nbr <- stringr::str_count(string = ctte, pattern = ";")
        # count number of rapporteur delimiters (ie number of rapporteurs -1)
        rap_nbr <- stringr::str_count(string = rap, pattern = ",")
        if (ctte_nbr != rap_nbr) message(
          "Multiple rapporteurs and committees matched",
          " but the number of rapporteurs does not",
          " match the number of committees."
        )
        # replace the delimiter in rapporteurs (the comma) with a semicolon
        rap <- sub(pattern = ",", replacement = ";", x = rap, fixed = TRUE)
      } else if (grepl(pattern = ",", x = rap, fixed = TRUE)) {
        # replace the comma with a semicolon in rapportuers field
        rap <- sub(pattern = ",", replacement = ";", x = rap, fixed = TRUE)
      } # end of multiple rapporteurs from the same committee
      # remove the element with the lead rapporteur from the vector
      x <- x[-rap_pos]

    } # end of is.character(x) condition
  }

  return(list(
    level2_info = x,
    ctte = trimws(x = ctte, which = "both"),
    rap = trimws(x = rap, which = "both")))
} # end of level2_lead() extract function



# extract associated committee(s) and rapporteur(s) -----------------------
#' function extracts associated committee(s) and associated rapporteur(s)
level2_assoc_extract <- function(x) {

  # if x has 0 length, then there is no additional information about
  # associated committee and associated raporteur
  if (length(x) == 0) {
    assoc_rapporteur <- NA
    assoc_ctte <- NA
  } else {

    # find a string with the word: "Rapporteur" or "Co-Rapporteurs"
    rap_pos <- stringr::str_which(
      string = x,
      pattern = stringr::regex(pattern = "rapporteur", ignore_case = TRUE))
    rap <- x[rap_pos]

    if (length(rap) != 0) {
      # extract everything after the colon if there is a colon in the string
      if (stringr::str_detect(string = rap, pattern = ":")) {
        rap <- stringr::str_extract(string = rap, pattern = "(?<=:)[\\s\\S]*")
      }
    }
    # trim white spaces in case only white spaces are matched
    rap <- trimws(x = rap, which = "both")
    # if object rap is string-length 0, then the assoc. rapporteur\
    # is in the next line
    if (length(rap) != 0) {
      if (stringr::str_length(string = rap) == 0) {
        rap <- x[rap_pos + 1]
        # check whether the word "committee" is matched as well
        is_committee <- stringr::str_detect(
          string = rap,
          pattern = stringr::regex(pattern = "committee", ignore_case = TRUE))
        if (is_committee) {
          # extract everything before the word committee
          rap <- stringr::str_extract(
            string = rap,
            pattern = ".+?(?=[Cc]ommittee)")
        }
      }
    }
    # if object rap is character(0), there is not info on assoc. rapporteur
    if (length(rap) == 0) {
      assoc_rapporteur <- NA
    } else {
      # if the extracted string is empty, use the next vector element
      if (rap == "") {
        # extract information from following vector element
        rap <- x[rap_pos + 1]
        # check whether the string contains the committee as well
        two_in_one <- grepl(pattern = "Committee", x = rap, fixed = TRUE)
        # extract both committee and rapporteur from the same string
        if (two_in_one) {
          # split text on the first comma only
          x_split <- unlist(strsplit(
            x = sub(pattern = ",\\s*", replacement =  "NeVerMatCh", x = rap),
            split = "NeVerMatCh"))
          # extract the associated committee by matching on the exact
          # string "Committee"
          assoc_ctte <- x_split[grep(
            pattern = "Committee", fixed = TRUE, x = x_split)]
          assoc_ctte <- trimws(x = assoc_ctte, which = "both")
          # extract the associated rapporteur by using remaining element
          # of x_split
          assoc_rapporteur <- x_split[-grep(
            pattern = "Committee", fixed = TRUE, x = x_split)]
          assoc_rapporteur <- trimws(x = assoc_rapporteur, which = "both")
        } # end of two_in_one == TRUE condition
        # remove elements from x that were used
        if (two_in_one) {
          x <- x[c(-rap_pos, -(rap_pos + 1))]
        } else {
          x <- x[-rap_pos]
        }
      } else { # end of rap == "" condition and start of the alternative
        # if rapporteur also contains the committee
        if (any(grepl(pattern = "Committee", x = rap, fixed = TRUE))) {
          # check whether the string contains the committee as well
          two_in_one <- grepl(pattern = "Committee", x = rap, fixed = TRUE)
          # extract both committee and rapporteur from the same string
          if (two_in_one) {
            # split text on the first comma only
            x_split <- unlist(strsplit(
              x = sub(pattern = ",\\s*", replacement =  "NeVerMatCh", x = rap),
              split = "NeVerMatCh"))
            # extract the associated committee by matching on the
            # exact string "Committee"
            assoc_ctte <- x_split[grep(
              pattern = "Committee", fixed = TRUE, x = x_split)]
            assoc_ctte <- trimws(x = assoc_ctte, which = "both")
            # extract the associated rapporteur by using remaining element
            # of x_split
            assoc_rapporteur <- x_split[-grep(
              pattern = "Committee", fixed = TRUE, x = x_split)]
            assoc_rapporteur <- trimws(x = assoc_rapporteur, which = "both")
          } # end of two_in_one == TRUE condition
        } else { # end of rap object contains the commission as well
          # rap object contains the rapporteur only
          assoc_rapporteur <- rap
          # search for <Depute> tags
          if (any(grepl(
            pattern = "<Depute>", x = assoc_rapporteur, fixed = TRUE))) {
            # remove opening and closing Depute tags
            assoc_rapporteur <- sub(
              pattern = "<Depute>",
              replacement = "", x = assoc_rapporteur, fixed = TRUE)
            assoc_rapporteur <- sub(
              pattern = "</Depute>",
              replacement = "", x = assoc_rapporteur, fixed = TRUE)
            assoc_rapporteur <- trimws(x = assoc_rapporteur, which = "both")
          } # end of <Depute> condition
        } # end of else condition for rap object contains the commission as well
      } # end of rap != "" condition
    } # end of else condition of length(x) == 0 condition

    # find a match for the exact phrase "Committee"
    if (any(grepl(pattern = "Committee", x = x, fixed = TRUE))) {
      # check whether assoc_ctte is not defined yet
      if (!exists("assoc_ctte")) {
        ctte_pos <- grep(pattern = "Committee", x = x, fixed = TRUE)
        ctte <- x[ctte_pos]
        # check whether the string contains (*)
        ctte <- lapply(X = seq_along(ctte), function(z) {
          if (stringr::str_detect(string = ctte[z], pattern = "\\(\\*\\)")) {
            # remove (*) from the string
            ctte <- sub(
              pattern = "(*)", replacement = "", x = ctte[z], fixed = TRUE)
          } else {
            ctte <- ctte[z]
          }
          return(ctte)
        })# end of string contains (*) condition
        # remove white spaces
        assoc_ctte <- lapply(X = ctte, function(z) {
          assoc_ctte <- stringr::str_trim(string = z, side = "both")
          return(assoc_ctte)
        })
        # unlist
        assoc_ctte <- unlist(assoc_ctte)
        # collapse if length is > 1
        if (length(assoc_ctte) > 1) {
          assoc_ctte <- paste(assoc_ctte, collapse = "; ")
        }
      } # end of !exists("assoc_ctte") condition

      # extract everything from and including the word committee
      assoc_ctte <- stringr::str_extract(
        string = assoc_ctte,
        pattern = "([Cc])(.*)")

    } else {
      # if assoc_ctte was not defined before, set it to NA
      if (!exists("assoc_ctte")) {
        assoc_ctte <- NA
      } # end of !exists("assoc_ctte") condition
    } # end of else condition of match "Committee" phrase
  } # end of else condition of length(rap) == 0 condition

  return(list(assoc_ctte = assoc_ctte, assoc_rapporteur = assoc_rapporteur))
} # end of level2_assoc_extract() function


# extract from doc_subtitle_level1 ----------------------------------------
#' function extracts all information organised in a table tag with
#' the class: "doc_subtitle_level1"
level1_info_fun <- function(page) {

  # table containing the doc_subtitle_level1 class
  level1_info <- page %>%
    rvest::html_nodes(css = "table[class='doc_subtitle_level1']") %>%
    rvest::html_text2()

  # use the p tag if level1_info is character(0)
  if (length(level1_info) == 0) {
    level1_info <- page %>%
      rvest::html_nodes(css = "p[class='doc_subtitle_level1']") %>%
      rvest::html_text()
  }

  # separate on new lines
  level1_info <- stringr::str_split(string = level1_info, pattern = "\n")

  # remove empty elements
  level1_info <- lapply(X = level1_info, FUN = function(z) {
    z[which(z != "")]
  })

  return(level1_info)
} # end of level1_info_fun()



# extracts lead rapporteur and committee from level1 info ------------------
#' function extracts lead committee and lead rapporteur from level1_info
#' and removes these entries from level1_info
level1_lead_extract <- function(x) {

  # check whether x is a list
  if (is.list(x)) {
    # unlist x if it is a list
    x <- unlist(x)
  }

  # check whether x is a character vector
  if (is.character(x)) {

    ## Group on behalf of which the DOC_TYPE was called
    # search for <Commission> and </Commission> tags
    if (any(grepl(pattern = "<Commission>", x = x, fixed = TRUE))) {

      # position of the opening tag <Commission>
      # there may be multiple opening tags in the same line
      open_tag_pos <- NA
      for (idx in seq_along(x)) {
        open_tag_pos <- c(open_tag_pos, rep(
          x = idx,
          times = ncol(stringr::str_extract_all(
            string = x[idx], pattern = "<Commission>", simplify = TRUE))))
      }
      open_tag_pos <- open_tag_pos[-1]

      # position of the closing tag </Commission>
      # there may be multiple opening tags in the same line
      close_tag_pos <- NA
      for (idx in seq_along(x)) {
        close_tag_pos <- c(close_tag_pos, rep(
          x = idx,
          times = ncol(stringr::str_extract_all(
            string = x[idx], pattern = "</Commission>", simplify = TRUE))))
      }
      close_tag_pos <- close_tag_pos[-1]

      # loop in the case of multiple opening and closing tags
      epg_info <- lapply(X = seq(1, length(open_tag_pos)), FUN = function(y) {
        # extract the lead committee(s)
        epg <- x[open_tag_pos[y]:close_tag_pos[y]]
        # combine the tags if there are multiple tags
        if (length(epg) > 1) {
          epg <- paste(epg, collapse = "; ")
        }

        # match abbreviation in curly braces
        if (grepl(pattern = "\\{[\\s\\S]+\\}", x = epg, perl = TRUE)) {
          # extract the group abbreviation in curly braces
          epg_long <- stringr::str_extract(
            string = epg, pattern = "(?<=\\})[\\s\\S]*[\\s\\S]*(?=\\<)")
          epg <- stringr::str_extract(
            string = epg, pattern = "(?<=\\{)[\\s\\S]*[\\s\\S]*(?=\\})")
        } else {
          # extract the long-form group (this contains roles such as
          # "chair" as well)
          epg_long <- stringr::str_extract(
            string = epg, pattern = "(?<=\\>)[\\s\\S]*[\\s\\S]*(?=\\<)")
          epg <- NA
        }

        # trim white spaces
        if (!is.na(epg)) epg <- trimws(x = epg, which = "both")
        if (!is.na(epg_long)) epg_long <- trimws(x = epg_long, which = "both")

        return(list(epg = epg, epg_long = epg_long))
      }) # end of loop over multiple opening tags
      epg <- paste0(unlist(lapply(
        epg_info, function(y) y$epg)), collapse = "; ")
      epg_long <- paste0(unlist(lapply(
        epg_info, function(y) y$epg_long)), collapse = "; ")

      # remove the element with the calling party group from the vector
    } else { # end of <Commission> tag found condition
      epg <- NA
      epg_long <- NA
    } # end of <Commission> tag NOT found condition


    ## MEPs who requested
    # search for <Depute> tag
    if (any(grepl(pattern = "<Depute>", x = x, fixed = TRUE))) {

      # position of the opening tag <Depute>
      # there may be multiple opening tags in the same line
      open_tag_pos <- NA
      for (idx in seq_along(x)) {
        open_tag_pos <- c(open_tag_pos, rep(
          x = idx,
          times = ncol(stringr::str_extract_all(
            string = x[idx], pattern = "<Depute>", simplify = TRUE))))
      }
      open_tag_pos <- open_tag_pos[-1]

      # position of the closing tag </Depute>
      # there may be multiple opening tags in the same line
      close_tag_pos <- NA
      for (idx in seq_along(x)) {
        close_tag_pos <- c(close_tag_pos, rep(
          x = idx,
          times = ncol(stringr::str_extract_all(
            string = x[idx], pattern = "</Depute>", simplify = TRUE))))
      }
      close_tag_pos <- close_tag_pos[-1]

      # check whether the number of opening tags is the same as
      # the number of closing tags
      if (length(open_tag_pos) != length(close_tag_pos)) {

        # check whether there are opening tags than closing tags
        if (length(open_tag_pos) > length(close_tag_pos)) {

          # insert missing closing tags on the same line where the tag opens
          close_tag_pos <- R.utils::insert(
            x = close_tag_pos,
            ats = which(!open_tag_pos %in% close_tag_pos),
            values = open_tag_pos[which(!open_tag_pos %in% close_tag_pos)])

        } else {
          stop("More closing tags than opening tags found.")
        }
      }

      # loop in the case of multiple opening and closing tags
      meps_info <- lapply(X = seq(1, length(open_tag_pos)), FUN = function(y) {

        # extract the lead committee(s)
        meps <- x[open_tag_pos[y]:close_tag_pos[y]]

        # combine the tags if there are multiple tags
        if (length(meps) > 1) {
          meps <- paste(meps, collapse = "; ")
        }

        # extract the meps within the <Depute> and </Depute> tags
        meps_c <- stringr::str_extract(
          string = meps,
          pattern = "(?<=<Depute>)[\\s\\S]*[\\s\\S]*(?=\\</Depute>)")

        # if meps is NA
        if (is.na(meps_c)) {
          meps <- stringr::str_extract(
            string = meps, pattern = "(?<=<Depute>)[\\s\\S]*")
        } else {
          meps <- meps_c
        }

        # remove <Depute> and </Depute> tags which may still be in the
        # string if there were multiple tags
        meps <- sub(
          pattern = "<Depute>",
          replacement = "", x = meps, fixed = TRUE)
        meps <- sub(
          pattern = "</Depute>",
          replacement = "", x = meps, fixed = TRUE)

        # trim white spaces
        meps <- trimws(x = meps, which = "both")

        return(meps)
      }) # end of loop over multiple opening tags
      meps <- paste0(unlist(meps_info), collapse = "; ")

      # remove duplicates
      meps <- unlist(strsplit(x = meps, split = "; "))
      meps <- unique(meps)
      meps <- paste0(meps, collapse = "; ")

      # remove the element with the calling party group from the vector
      x <- x[c(-open_tag_pos, -close_tag_pos)]

    } else { # end of <Depute> tag found condition
      meps <- NA
    } # end of <Depute> tag NOT found condition
  } # end of is.character(x) condition

  return(list(
    level2_info = x,
    epg_abbr = epg,
    group_or_function = epg_long,
    meps = meps))

} # end of level1_lead_extract() function



# re-assemble id numbers --------------------------------------------------

# function re_assembles the id numbers
# from: "B9-0012/2019" to: "B-9-2019-0012"
re_assemble_ids <- function(ids) {

  # check whether ID is already in the correct format
  if (
    stringr::str_detect(
      string = ids,
      pattern = "^[A-Z]{1}-\\d+-\\d{4}-\\d+"
    )
  ) {
    return(ids)
  } else {
    ## dis-assemble id into its components
    # the leading letter from the id number
    id_letter <- stringr::str_extract(string = ids, pattern = "^[A-Z]{1}")
    ids <- stringr::str_remove(string = ids, pattern = "^[A-Z]{1}")

    # the number following the id letter
    id_letter_nbr <- stringr::str_extract(string = ids, pattern = "^[0-9]{1}")

    # the id nbr
    id_nbr <- stringr::str_extract(
      string = ids, pattern = "(?<=-)[\\s\\S]*[\\s\\S]*(?=/)"
    )

    # the id year
    id_year <- stringr::str_extract(string = ids, pattern = "[0-9]{4}$")

    # re-assemble the id number
    ids <- paste0(
      id_letter, "-", id_letter_nbr, "-",
      id_year, "-", id_nbr
    )
  }
  return(ids)
}



# legislative procedure: Yes or No ----------------------------------------
legislative_proc <- function(procedure_id) {

  legislative <- lapply(seq_along(procedure_id), function(y) {

    # extract text within brackets
    proc_type <-  stringr::str_extract(
      string = procedure_id[y],
      pattern = "(?<=\\()[\\s\\S]*[\\s\\S]*(?=\\))")

    if (proc_type %in% c("COD", "CNS", "OCM", "ACC", "APP",
                         "AVC", "BUD", "DEC", "RSP", "ADO",
                         "SYN")) {
      legislative <- TRUE
    } else {
      legislative <- FALSE
    }
    return(legislative)
  })

  # unlist
  legislative <- unlist(legislative)

  return(legislative)
}



# scrape combined roll-call votes -----------------------------------------

#' Scrapes RC documents.
#'
#' RC documents are used to reference roll-call votes that combine several
#' requests into one roll-call.

scrape_combined_rcvs <- function(data_dir, verbose = TRUE, sleep = 1, links) {

  if (length(links) > 0) {
    # download links
    combined_rcvs <- lapply(X = seq_len(nrow(links)), FUN = function(x) {

      # attempt to download in English first
      out_en <- try(expr = {
        # verbose = TRUE screen output
        if (verbose) {
          message("\nRequesting: ", links$english[x])
        }
        # html
        xml2::download_html(
          url = links$english[x],
          file = sprintf("%sraw/combined_rcvs/%s", data_dir, links$filename[x]),
          quiet = FALSE)
        closeAllConnections()
      }, silent = TRUE)

      if ("try-error" %in% class(out_en)) {
        warning("Download of ", links$english[x], " failed")
      }

      combined_rcvs <- tibble::tibble(
        link = links$english[x])

      return(combined_rcvs)
    })

    # combining list to tibble
    combined_rcvs <- combined_rcvs %>%
      dplyr::bind_rows()

    # saving to parsed data
    if (file.exists(sprintf("%sraw/combined_rcvs/links.RData", data_dir))) {
      new <- combined_rcvs
      load(file = sprintf("%sraw/combined_rcvs/links.RData", data_dir))
      combined_rcvs <- combined_rcvs %>%
        dplyr::bind_rows(new) %>%
        dplyr::distinct()
      save(combined_rcvs,
        file = sprintf("%sraw/combined_rcvs/links.RData", data_dir))
    } else {
      save(combined_rcvs,
        file = sprintf("%sraw/combined_rcvs/links.RData", data_dir))
    }

    # re-naming curltmps ------------------------------------------------------
    # search minutes folder for existing minutes files
    filenames <- list.files(
      path = sprintf("%sraw/combined_rcvs", data_dir),
      pattern = "\\.html$")

    # check whether downloaded files include .curltmp files
    if (any(stringr::str_detect(
        string = filenames, pattern = "\\.curltmp"))) {

      # re-name .curltmp's to html if they can be parsed and otherwise, throw a
      # warning to redo scraping for these dates

      # find .curltmp files
      curltemps <- filenames[
        stringr::str_detect(string = filenames, pattern = "\\.curltmp")]

      # try to parse .curltmp documents
      pages <- lapply(curltemps, function(y) {
        pages <- try(expr = {
          xml2::read_html(x = sprintf("%sraw/combined_rcvs", data_dir),
          encoding = "UTF-8")
        }, silent = TRUE)
        class(pages)
      })

      # error message if documents cannot be parsed
      if ("try-error" %in% unlist(pages)) {
        stop(
          "There are .curltmp documents in the download folder which cannot",
          " be parsed. Delete those files from",
          sprintf("%sraw/combined_rcvs", data_dir),
          "and re-run scrape_combined_rcvs().")
      } else {
        # new file names
        htmls <- stringr::str_remove(string = curltemps, pattern = "\\.curltmp")
        # re-name files
        quiet(
          suppressMessages(
            file.rename(
              from = sprintf("%sraw/combined_rcvs/%s", data_dir, curltemps),
              to = sprintf("%sraw/combined_rcvs/%s", data_dir, htmls))
          )
        )
      }
    }
  }
} # end of function


# Scrape texts tabled -----------------------------------------------------

scrape_texts_tabled <- function(data_dir, links, verbose = TRUE, sleep = 3) {

  # download links
  texts_tabled <- lapply(X = seq_len(nrow(links)), FUN = function(x) {

    # attempt to download in English first
    try(expr = {
      # verbose = TRUE screen output
      if (verbose) {
        message("\nRequesting: ", links$english[x])
      }
      # html
      xml2::download_html(
        url = links$english[x],
        file = sprintf("%sraw/texts_tabled/%s", data_dir, links$filename[x]),
        quiet = FALSE)
      closeAllConnections()
    }, silent = TRUE)

    # link
    texts_tabled <- tibble::tibble(
      link = links$english[x])

    return(texts_tabled)
  })

  # combining list to tibble
  texts_tabled <- texts_tabled %>%
    dplyr::bind_rows()

  # saving to parsed data

  if (file.exists(sprintf("%sraw/texts_tabled/links.RData", data_dir))) {
    new <- texts_tabled
    load(file = sprintf("%sraw/texts_tabled/links.RData", data_dir))
    texts_tabled <- texts_tabled %>%
      dplyr::bind_rows(new) %>%
      dplyr::distinct()
    save(texts_tabled,
      file  = sprintf("%sraw/texts_tabled/links.RData", data_dir))
  } else {
    save(texts_tabled,
      file = sprintf("%sraw/texts_tabled/links.RData", data_dir))
  }

  # re-naming curltmps ------------------------------------------------------
  # search minutes folder for existing minutes files
  filenames <- list.files(
    path = sprintf("%sraw/texts_tabled", data_dir),
    pattern = "\\.html$")

  # check whether downloaded files include .curltmp files
  if (any(stringr::str_detect(string = filenames, pattern = "\\.curltmp"))) {

    # re-name .curltmp's to html if they can be parsed and otherwise, throw a
    # warning to redo scraping for these dates

    # find .curltmp files
    curltemps <- filenames[stringr::str_detect(
      string = filenames, pattern = "\\.curltmp")]

    # try to parse .curltmp documents
    pages <- lapply(curltemps, function(y) {
      pages <- try(expr = {
        xml2::read_html(
          x = sprintf("%sraw/texts_tabled/%s", data_dir, y),
          encoding = "UTF-8")
      }, silent = TRUE)
      class(pages)
    })

    # error message if documents cannot be parsed
    if ("try-error" %in% unlist(pages)) {
      stop(
        "There are .curltmp documents in the download folder which cannot",
        " be parsed. Delete those files from",
        sprintf("%sraw/texts_tabled", data_dir),
        "and re-run scrape_texts_tabled().")
    } else {
      # new file names
      htmls <- stringr::str_remove(string = curltemps, pattern = "\\.curltmp")
      # re-name files
      quiet(
        suppressMessages(
          file.rename(
            from = sprintf("%sraw/texts_tabled/%s", data_dir, curltemps),
            to = sprintf("%sraw/texts_tabled/%s", data_dir, htmls)))
        )
    }
  }
} # end of scrape_texts_tabled() function


# Scrape oeils ------------------------------------------------------------
scrape_oeils <- function(data_dir, links, verbose = TRUE, sleep = 3) {

  # screen output
  if (verbose) {
    message(paste0("\nAttempting to download ", nrow(links), " files."))
  }

  # download links
  lapply(X = seq_len(nrow(links)), FUN = function(x) {

    # idle time before requesting document
    Sys.sleep(time = sleep)

    # attempt to download in English first
    try(expr = {
      # verbose = TRUE screen output
      if (verbose) {
        message("\nRequesting: ", links$english[x])
      }
      # html
      xml2::download_html(
        url = links$english[x],
        file = sprintf("%sraw/procedures/%s", data_dir, links$filename[x]),
        quiet = FALSE)
      closeAllConnections()
    }, silent = TRUE)
  })

  # re-naming curltmps ------------------------------------------------------
  # search minutes folder for existing minutes files
  filenames <- list.files(
    path = sprintf("%sraw/procedures", data_dir),
    pattern = "\\.html$")

  # check whether downloaded files include .curltmp files
  if (any(stringr::str_detect(string = filenames, pattern = "\\.curltmp"))) {

    # re-name .curltmp's to html if they can be parsed and otherwise, throw a
    # warning to redo scraping for these dates

    # find .curltmp files
    curltemps <- filenames[stringr::str_detect(
      string = filenames, pattern = "\\.curltmp")]

    # try to parse .curltmp documents
    pages <- lapply(curltemps, function(y) {
      pages <- try(expr = {
        xml2::read_html(x = sprintf(
          "%sraw/procedures/%s", data_dir, y), encoding = "UTF-8")
      }, silent = TRUE)
      class(pages)
    })

    # error message if documents cannot be parsed
    if ("try-error" %in% unlist(pages)) {
      stop(
        "There are .curltmp documents in the download folder",
        " which cannot be parsed. Delete those files from",
        sprintf("%sraw/procedures", data_dir), "and re-run",
        " scrape_oeils().")
    } else {
      # new file names
      htmls <- stringr::str_remove(string = curltemps, pattern = "\\.curltmp")
      # re-name files
      quiet(
        suppressMessages(
          file.rename(
            from = sprintf("%sraw/procedures/%s", data_dir, curltemps),
            to = sprintf("%sraw/procedures/%s", data_dir, htmls))
        )
      )
    }
  }
} # end of scrape_oeils() function


# Decide among multiple matches -------------------------------------------

adjudicate_bw_matches <- function(candidates, string) {

  # attempt exact match first
  best_match <- which(candidates == string)

  # match on string distance
  if (length(best_match) == 0) {
    str_dist <- stringdist::stringdist(
      a = string, b = candidates)
    best_match <- which.min(str_dist)
  }

  # if both matches are equally likely, return the first
  # with a warning message
  if (length(best_match) > 1) {
    best_match <- best_match[1]
    warning("All candidates are equally likely matches")
  }

  return(best_match)
}


# comma separated elements as well elements separated by and --------------
separate_string_enumeration <- function(x) {

  # separate elements on ","
  # separate elements
  x <- stringr::str_split(x, pattern = ",(?!\\s*point\\b)") %>%
    unlist()

  # separate elements on ";"
  x <- unlist(stringr::str_split(string = x, pattern = ";"))

  # separate elements on "and"
  x <- unlist(stringr::str_split(string = x, pattern = "and"))

  # remove leading and trailing white spaces
  x <- stringr::str_trim(string = x, side = "both")

}



# element types -----------------------------------------------------------
# function adds types of subject elements, e.g. amendments, paragraphs or blocks
element_types <- function(x) {

  # was content withdrawn (any means all content has been withdrawn)
  withdrawn_content <- any(
    stringr::str_detect(string = x, pattern = "withdrawn"))

  # Skip if x is NA
  if (all(!is.na(x))) {

    # is there a number in the text
    x <- lapply(X = x, FUN = function(x) {
      if (stringr::str_detect(string = x, pattern = "\\d+\\b") & !is.na(x)) {
        # check whether the text contains number and then the word "point"
        if (stringr::str_detect(
          string = x, pattern = "^.*\\d+\\spoint\\s[a-z]{1,2}")) {
          # keep everything including point and the point letter
          x <- stringr::str_extract(
            string = x, pattern = "^.*\\d+\\spoint\\s[a-z]{1,2}")
        } else {
          # keep text only up until the number
          x <- stringr::str_extract(string = x, pattern = "^.*\\d+")
        }
      }
      return(x)
    })
    x <- unlist(x)

    # iterate over elements
    for (m in seq_along(x)) {

      # does this element contain only a number
      num_only <- stringr::str_detect(string = x[m], pattern = "^\\d+$")
      if (is.na(num_only)) next

      # if the element contains text besides numbers, check the text
      if (!num_only) {

        # check for amendments
        if (stringr::str_detect(string = x[m], pattern = "Am")) {
          x[m] <- paste0("Am ",
                         stringr::str_extract(string = x[m], pattern = "\\d+"))
        } # end of condition: amendments

      } else { # end of condition: element contains text

        # use the previous type
        x[m] <- paste0(
          stringr::str_extract(string = x[m - 1], pattern = "(^\\w+|^§)"),
          " ", x[m])
      } # end of condition element does not contain text

    } # end of loop over elements

  }

  # end withdrawn (if content hat been withdrawn)
  x <- list(elements = x, withdrawn = withdrawn_content)

  return(x)
}


# get voting threshold ----------------------------------------------------
# called from parse_vote_descriptions()
# It finds the voting threshold given the contents of the html

get_voting_threshold <- function(y) {

  vt <- NA_character_

  ############## Method 1
  # match on the phrase "majority required" followed by a colon and a number
  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = "majority\\s+required\\:\\s+\\d+", ignore_case = TRUE)))) {

    # match on "votes cast" followed by colon and number
    if (any(stringr::str_detect(string = y, pattern = stringr::regex(
      pattern = "votes\\s+cast\\:\\s+\\d+", ignore_case = TRUE)))) {

      # extract majority required
      vt1 <- stringr::str_extract(
        string = y,
        pattern = stringr::regex(
          pattern = "majority\\s+required\\:\\s+\\d+", ignore_case = TRUE))
      # remove NAs
      vt1 <- vt1[!is.na(vt1)]
      # extract the number
      vt1 <- as.integer(stringr::str_extract(
        string = vt1,
        pattern = "\\b\\d+\\b"))

      # extract votes cast
      vc <- stringr::str_extract(
        string = y,
        pattern = stringr::regex(
          pattern = "votes\\s+cast\\:\\s+\\d+", ignore_case = TRUE))

      # remove NAs
      vc <- vc[!is.na(vc)]

      # extract the number
      vc <- as.integer(stringr::str_extract(
        string = vc,
        pattern = "\\b\\d+\\b"))

      # determine the threshold
      vt1 <- ifelse(
        test = ((vt1 - 1) / vt1) < 0.5,
        yes = "simple majority", no = "qualified majority")

      # remove duplicates
      vt1 <- unique(vt1)
    }
  }

  if (exists("vt1")) vt <- vt1

  ############## Method 2
  # match on the phrase "simple majority"
  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = "simple\\s+majority", ignore_case = TRUE)))) {

    # get the row that contains the phrase
    vt2 <- stringr::str_detect(string = y, pattern = stringr::regex(
      pattern = "simple\\s+majority", ignore_case = TRUE))

    # subset the string by the matched position
    vt2 <- y[vt2]

    # detect line break
    if (any(stringr::str_detect(string = vt2, pattern = "\n"))) {

      # split string on line break
      vt2 <- unlist(stringr::str_split(string = vt2, pattern = "\n"))

      # get element that contains the phrase "simple\\s+majority"
      vt_pos <- stringr::str_detect(string = vt2, pattern = stringr::regex(
        pattern = "simple\\s+majority", ignore_case = TRUE))

      # subset the vector by the matched string above
      vt2 <- vt2[vt_pos]

      # remove leading round bracket
      vt2 <- stringr::str_remove(string = vt2, pattern = "^\\(")

      # remove trailing round bracket
      vt2 <- stringr::str_remove(string = vt2, pattern = "\\)$")

      # lower case string
      vt2 <- stringr::str_to_lower(string = vt2, locale = "en")
    }
  }

  if (exists("vt2")) {
    if (all(is.na(vt))) vt <- vt2
    else vt <- c(vt, vt2)
  }

  ############## Method 3
  # match on the phrase "qualified majority"
  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = "qualified\\s+majority", ignore_case = TRUE)))) {

    # get the row that contains the phrase
    vt3 <- stringr::str_detect(string = y, pattern = stringr::regex(
      pattern = "qualified\\s+majority", ignore_case = TRUE))

    # subset the string by the matched position
    vt3 <- y[vt3]

    # detect line break
    if (any(stringr::str_detect(string = vt3, pattern = "\n"))) {

      # split string on line break
      vt3 <- unlist(stringr::str_split(string = vt3, pattern = "\n"))

      # get element that contains the phrase "qualified\\s+majority"
      vt_pos <- stringr::str_detect(string = vt3, pattern = stringr::regex(
        pattern = "qualified\\s+majority", ignore_case = TRUE))

      # subset the vector by the matched string above
      vt3 <- vt3[vt_pos]

      # if the phrase qualified majority is surrounded by brackets
      vt3 <- lapply(vt3, function(z) {
        if (stringr::str_detect(
          string = z,
          pattern = stringr::regex(
            pattern = "\\(qualified\\s+majority\\)", ignore_case = TRUE))) {

          # set to qualified majority
          vt3 <- "qualified majority"
        } else {
          vt3 <- z
        }
        return(vt3)
      }) %>%
        unlist

      # remove leading round bracket
      vt3 <- stringr::str_remove(string = vt3, pattern = "^\\(")

      # remove trailing round bracket
      vt3 <- stringr::str_remove(string = vt3, pattern = "\\)$")

      # remove round brackets
      vt3 <- stringr::str_remove_all(string = vt3, pattern = "\\)")

      # lower case string
      vt3 <- stringr::str_to_lower(string = vt3, locale = "en")
    }
  }

  if (exists("vt3")) {
    if (all(is.na(vt))) vt <- vt3
    else vt <- c(vt, vt3)
  }

  ############## Method 4
  # match on the phrase "Majority of Parliament's component Members and three
  # fifths of the votes cast required"
  try_pattern <- paste0(
    "Majority\\s+of\\s+Parliament's\\s+component\\s+Members\\s+and\\s+three",
    "\\s+fifths of the\\s+votes\\s+cast\\s+required")

  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = try_pattern, ignore_case = TRUE)))) {

    # set the phrase
    vt4 <- paste0("Majority of Parliament's component Members and three",
                  " fifths of the votes cast")
  }

  if (exists("vt4")) {
    if (all(is.na(vt))) vt <- vt4
    else vt <- c(vt, vt4)
  }

  ############## Method 5
  # match on the phrase "Majority of Members and 3/5 of votes cast required"
  try_pattern <- paste0("Majority\\s+of\\s+Members\\s+and\\s+3/5\\s+of",
                        "\\s+votes\\s+cast\\s+required")

  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = try_pattern, ignore_case = TRUE)))) {

    # set the phrase
    vt5 <- paste0("Majority of Parliament's component Members and three",
                  " fifths of the votes cast")
  }

  if (exists("vt5")) {
    if (all(is.na(vt))) vt <- vt5
    else vt <- c(vt, vt5)
  }

  ############## Method 6
  # match on the phrase "Majority of Parliament's component Members required for
  # adoption of the amendments"

  try_pattern <- paste0(
    "Majority\\s+of\\s+Parliament's\\s+component\\s+Members\\s+required\\s+for",
    "\\s+adoption\\s+of\\s+the\\s+amendments")

  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = try_pattern, ignore_case = TRUE)))){

    # set the phrase
    vt6 <- "Majority of Parliament's component Members"
  }

  if (exists("vt6")) {
    if (all(is.na(vt))) vt <- vt6
    else vt <- c(vt, vt)
  }

  ############## Method 7
  # match on the phrase "(Majority of Parliament’s component Members required)"

  try_pattern <- paste0(
    "\\(Majority\\s+of\\s+Parliament’s\\s+component\\s+Members\\s+required\\)")

  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = try_pattern, ignore_case = TRUE)))) {

    # set the phrase
    vt7 <- "Majority of Parliament’s component Members"

  }

  if (exists("vt7")) {
    if (all(is.na(vt))) vt <- vt7
    else vt <- c(vt, vt7)
  }

  ############## Method 8
  # match on the phrase "Majority of Parliament's component Members required for
  # the joint text to be rejected"

  try_pattern <- paste0(
    "\\Majority\\s+of\\s+Parliament's\\s+component\\s+Members\\s+required",
    "\\s+for\\s+the\\s+joint\\s+text\\s+to\\s+be\\s+rejected")

  if (any(stringr::str_detect(string = y, pattern = stringr::regex(
    pattern = try_pattern, ignore_case = TRUE)))) {

    # set the phrase
    vt8 <- paste0("Majority of Parliament's component Members required for",
                  "the joint text to be rejected")

  }

  if (exists("vt8")) {
    if (all(is.na(vt))) vt <- vt
    else vt <- c(vt, vt8)
  }

  return(paste(vt, collapse = "; "))
}



# get item number ---------------------------------------------------------
# called from parse_vote_descriptions()
# It finds the item number given the contents of the html

get_item_number <- function(y) {

  ############## Method 1
  # match on the phrase "item number" followed by a number
  if (any(stringr::str_detect(
    string = y, pattern = stringr::regex(
      pattern = "item\\s\\d+", ignore_case = TRUE)))) {

    # extract phrase
    it <- stringr::str_extract(string = y, pattern = stringr::regex(
      pattern = "item\\s\\d+\\.?\\d?", ignore_case = TRUE))

    # remove NA's
    it <- it[!is.na(it)]

    # multiple matches with different numbers
    if (length(unique(it)) > 1) {

      # detect phrase which contains item and number
      it <- stringr::str_detect(string = y, pattern = stringr::regex(
        pattern = "item\\s\\d+\\.?\\d?", ignore_case = TRUE))

      # extract the line
      it <- y[it]

      # find the phrase voting record
      it <- it[stringr::str_detect(
        string = it,
        pattern = stringr::regex(
          pattern = "Results\\s+of\\s+votes", ignore_case = TRUE))]

      # extract phrase
      it <- stringr::str_extract(string = it, pattern = stringr::regex(
        pattern = "item\\s\\d+\\.?\\d?", ignore_case = TRUE))

      # remove duplicates
      it <- unique(it)

      # set NA if no match found
      if (length(it) == 0) {
        it <- NA
      }
    }

    # extract the number
    it <- as.numeric(stringr::str_extract(
      string = it, pattern = "\\b\\d+\\.?\\d?\\d?\\d?\\b"))

    # remove duplicates
    it <- unique(it)

  }

  ############## Method 2
  # match on the phrase "annex" followed by a number

  if (!exists("it")) {
    if (any(stringr::str_detect(
      string = y, pattern = stringr::regex(
        pattern = "annex\\s\\d+\\.?\\d?\\d?", ignore_case = TRUE)))) {

      # extract phrase
      it <- stringr::str_extract(string = y, pattern = stringr::regex(
        pattern = "annex\\s\\d+\\.?\\d?\\d?", ignore_case = TRUE))

      # remove NA's
      it <- it[!is.na(it)]

      # extract the number
      it <- as.numeric(stringr::str_extract(
        string = it, pattern = "\\b\\d+\\.?\\d?\\d?\\d?\\b"))
    }
  }

  ############## Method 3
  # Set NA

  if (!exists("it")) {
    it <- NA
  }

  return(it)
}



# get vote type -----------------------------------------------------------
# called from parse_vote_descriptions()
# It finds the vote type given the contents of the html

get_vote_type <- function(y, z) {

  # vote title
  title <- z %>%
    rvest::html_element(css = "title") %>%
    rvest::html_text2()

  ############## Method 1
  # match on the row that contains the phrase "secret ballot"
  if (any(stringr::str_detect(
    string = y, pattern = stringr::regex(
      pattern = "secret\\s+ballot", ignore_case = TRUE)))) {

    # extract row with phrase "secret ballot"
    vt <- stringr::str_extract(
      string = y, pattern = stringr::regex(
        pattern = "secret\\s+ballot", ignore_case = TRUE))
    # remove NAs
    vt <- vt[!is.na(vt)]
    # lower case matches
    vt <- stringr::str_to_lower(string = vt, locale = "en")
    # remove multiple matches
    vt <- unique(vt)
  }

  ############## Method 2
  # match on the row that contains the phrase "single vote"
  if (any(stringr::str_detect(
    string = y, pattern = stringr::regex(
      pattern = "single\\s+vote", ignore_case = TRUE)))) {

    # extract row with phrase "single vote"
    vt <- stringr::str_extract(
      string = y, pattern = stringr::regex(
        pattern = "single\\s+vote", ignore_case = TRUE))
    # remove NAs
    vt <- vt[!is.na(vt)]
    # lower case matches
    vt <- stringr::str_to_lower(string = vt, locale = "en")
    # remove multiple matches
    vt <- unique(vt)
  }

  ############## Method 3
  # find the phrase vote in the title surrounded by brackets (only if vt does
  # not exist)
  if (!exists("vt")) {
    if (stringr::str_detect(
      string = title, pattern = stringr::regex(
        pattern = "\\(vote\\)", ignore_case = TRUE))) {

      # set vt to the phrase "vote"
      vt <- "vote"
    }
    # set vt to the phrase "vote"
    vt <- "vote"
  }

  return(vt)
}



# get the voting rule -----------------------------------------------------
# called from parse_vote_descriptions()
# It finds the voting rule given the contents of the html

get_voting_rule <- function(y) {

  # vote title
  vt <- y %>%
    rvest::html_element(css = "title") %>%
    rvest::html_text2()

  ############## Method 1
  # match on the phrase "Rule" followed by a number
  if (stringr::str_detect(
    string = vt,
    pattern = stringr::regex(
      pattern = "rule\\s+\\d+\\w?", ignore_case = TRUE))) {

    # extract the voting rule
    vr <- stringr::str_extract(
      string = vt,
      pattern = stringr::regex(
        pattern = "rule\\s+\\d+\\w?", ignore_case = TRUE))
  }

  ############## Method 2
  # find the phrase vote in the title surrounded by brackets (only if vr does
  # not exist)
  if (!exists("vr")) {
    if (stringr::str_detect(
      string = vt, pattern = stringr::regex(
        pattern = "\\(vote\\)", ignore_case = TRUE))) {

      # set vt to the phrase "vote"
      vr <- NA_character_
    }
  }

  # set the voting rule to NA if nothing is matched
  if (!exists("vr")) vr <- NA_character_

  return(vr)
}


# get vote result ---------------------------------------------------------
# called from parse_vote_descriptions()
# It finds the voting result. Either adopted or rejected

get_vote_result <- function(y, ta_id) {

  ########## Method 1
  # match on the phrase "Adopted" if it was successful
  if (any(stringr::str_detect(
    string = y,
    pattern = stringr::regex(pattern = "Adopted", ignore_case = TRUE))) &&
      all(!is.na(ta_id))) {

    # extract the line with the phrase "Adopted"
    result_line <- stringr::str_detect(
      string = y, pattern = stringr::regex(
        pattern = "Adopted", ignore_case = TRUE))

    # subset
    result_element <- y[result_line][1]

    # detect the texts adopted id in that line
    if (any(stringr::str_detect(
      string = result_element,
      pattern = stringr::fixed(pattern = ta_id)))) {
      result <- "adopted"
    }
  }

  ########## Method 2
  # match on the phrase "Rejected" if it was unsuccessful
  if (any(stringr::str_detect(
    string = y, pattern = stringr::regex(
      pattern = "Rejected", ignore_case = TRUE)))) {

    # extract the phrase
    result <- stringr::str_extract(
      string = y, pattern = stringr::regex(
        pattern = "Rejected", ignore_case = TRUE))

    # remove NAs
    result <- result[!is.na(result)]

    # lower case
    result <- stringr::str_to_lower(string = result, locale = "en")

    # remove duplicates
    result <- unique(result)
  }

  # set the voting result to NA if none of the methods found a match
  if (!exists("result")) result <- NA_character_

  return(result)
}



# MEP functions -----------------------------------------------------------
# extracts MEP functions such as memberships, chairs, and substitutes functions
extract_mep_function <- function(page, what, mep_status_text, term, mepid) {

  # tag position
  idx <- page %>%
    rvest::html_elements(css = 'div[class="erpl_meps-status"]') %>%
    rvest::html_elements(css = 'h4[class="erpl_title-h4"]') %>%
    rvest::html_text2() %>%
    stringr::str_which(pattern = paste0("^", what))

  # if tag was found
  if (length(idx) != 0) {

    # extract tag
    tag <- page %>%
      rvest::html_elements(css = 'div[class="erpl_meps-status"]') %>%
      .[idx] %>%
      rvest::html_elements(css = "li") %>%
      rvest::html_text2()

    # loop over the elements of the tag
    out <- lapply(X = seq_along(tag), FUN = function(y) {

      # current tag
      c_tag <- tag[[y]]

      ## fetch the date
      # everything before the colon
      element_date <- stringr::str_extract_all(
        string = c_tag, pattern = "(.*)(?=:)") %>%
        unlist %>%
        .[. != ""]

      # remove element date from groups_tag
      c_tag <- stringr::str_remove(
        string = c_tag,
        pattern = stringr::fixed(pattern = element_date)) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_remove(patter = "^:") %>%
        stringr::str_trim(side = "both")

      # start date
      start_date <- stringr::str_extract(
        string = element_date, pattern = "\\d{2}-\\d{2}-\\d{4}")

      # stop date is the remainder
      end_date <- stringr::str_remove(
        string = element_date,
        pattern = stringr::fixed(pattern = start_date)) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_remove(patter = "/") %>%
        stringr::str_trim(side = "both")

      # output data
      out <- tibble::tibble(
        ep = as.integer(term),
        mepid = mepid,
        start_date = start_date,
        end_date = end_date,
        member_function = c_tag,
        member_position = what
      )
      return(out)
    })

    # combine list to tibble
    out <- dplyr::bind_rows(out)

  } else {

    # empty output data
    out <- tibble::tibble(
      ep = NA,
      mepid = NA,
      start_date = NA_character_,
      end_date = NA_character_,
      member_function = NA_character_,
      member_position = NA_character_
    )
  }

  # function output
  return(out)
}


# MEP political group -----------------------------------------------------
# extracts the political group membership of the MEP
extract_epg <- function(page, mep_status_text, term, mepid) {

  # define the what text
  what <- "Political\\s+groups"

  # tag position
  idx <- page %>%
    rvest::html_elements(css = 'div[class="erpl_meps-status"]') %>%
    rvest::html_elements(css = 'h4[class="erpl_title-h4"]') %>%
    rvest::html_text2() %>%
    stringr::str_which(pattern = stringr::regex(
      pattern = what,
      ignore_case = TRUE))

  # if tag was found
  if (length(idx) != 0) {

    # extract tag
    tag <- page %>%
      rvest::html_elements(css = 'div[class="erpl_meps-status"]') %>%
      .[idx] %>%
      rvest::html_elements(css = "li") %>%
      rvest::html_text2()

    # loop over the elements of the tag
    out <- lapply(X = seq_along(tag), FUN = function(y) {

      # current tag
      c_tag <- tag[[y]]

      ## fetch the date
      # everything before the colon
      element_date <- stringr::str_extract_all(
        string = c_tag, pattern = "(.*)(?=:)") %>%
        unlist %>%
        .[. != ""]

      # remove element date from groups_tag
      c_tag <- stringr::str_remove(
        string = c_tag,
        pattern = stringr::fixed(pattern = element_date)) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_remove(patter = "^:") %>%
        stringr::str_trim(side = "both")

      # start date
      start_date <- stringr::str_extract(
        string = element_date, pattern = "\\d{2}-\\d{2}-\\d{4}")

      # stop date is the remainder
      end_date <- stringr::str_remove(
        string = element_date,
        pattern = stringr::fixed(pattern = start_date)) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_remove(patter = "/") %>%
        stringr::str_trim(side = "both")

      # check whether "Co-Chair" is in the string and replace with "Co Chair"
      c_tag <- stringr::str_replace(
        string = c_tag, pattern = "Co-Chair",
        replacement = "Co Chair")

      # check whether "Vice-Chair" is in the string and replace with
      # "Vice Chair"
      c_tag <- stringr::str_replace(
        string = c_tag, pattern = "Vice-Chair",
        replacement = "Vice Chair")

      # check whether "Vice-President" is in the string and replace with
      # "Vice President"
      c_tag <- stringr::str_replace(
        string = c_tag, pattern = "Vice-President",
        replacement = "Vice President")

      # check whether "Non-attached" is in the string and replace with
      # "Non attached"
      c_tag <- stringr::str_replace(
        string = c_tag, pattern = "Non-attached",
        replacement = "Non attached")

      # check whether "Co-treasurer" is in the string and replace with
      # "Co treasurer"
      c_tag <- stringr::str_replace(
        string = c_tag, pattern = "Co-treasurer",
        replacement = "Co treasurer")

      # check whether "Co-President" is in the string and replace with
      # "Co President"
      c_tag <- stringr::str_replace(
        string = c_tag, pattern = "Co-President",
        replacement = "Co President")


      ## fetch the party group name
      # everything before the hyphen (if there is a hyphen in the string)
      if (stringr::str_detect(string = c_tag, pattern = "-")) {
        group_long <- stringr::str_extract_all(
          string = c_tag, pattern = ".*(?=-)") %>%
          unlist %>%
          .[. != ""] %>%
          stringr::str_trim(side = "both")
      } else {
        group_long <- c_tag
      }

      # add the hyphen to "Non attached"
      group_long <- stringr::str_replace(
        string = group_long,
        pattern = "Non attached", replacement = "Non-attached")

      # remove the group from the c_tag
      c_tag <- stringr::str_replace(
        string = c_tag,
        pattern = stringr::fixed(pattern = group_long), replacement = "")

      ## fetch the group function
      # everything after the hyphen
      group_function <- stringr::str_extract(
        string = c_tag, pattern = "(?<=-).*") %>%
        stringr::str_trim(side = "both")

      # add the hyphen to "Co-Chair"
      group_function <- stringr::str_replace(
        string = group_function,
        pattern = "Co Chair", replacement = "Co-Chair")

      # add the hyphen to "Vice-President"
      group_function <- stringr::str_replace(
        string = group_function,
        pattern = "Vice President", replacement = "Vice-President")

      # add the hyphen to "Vice-Chair"
      group_function <- stringr::str_replace(
        string = group_function,
        pattern = "Vice Chair", replacement = "Vice-Chair")

      # add the hyphen to "Co-treasurer"
      group_function <- stringr::str_replace(
        string = group_function,
        pattern = "Co treasurer", replacement = "Co-treasurer")

      # add the hyphen to "Co-President"
      group_function <- stringr::str_replace(
        string = group_function,
        pattern = "Co President", replacement = "Co-President")

      # output data
      out <- tibble::tibble(
        ep = as.integer(term),
        mepid = mepid,
        start_date = start_date,
        end_date = end_date,
        group_long = group_long,
        group_function = group_function)
      return(out)
    })

    # combine list to tibble
    out <- dplyr::bind_rows(out)

  } else {

    # empty output data
    out <- tibble::tibble(
      ep = NA,
      mepid = NA,
      start_date = NA_character_,
      end_date = NA_character_,
      group_long = NA_character_,
      group_function = NA_character_
    )
  }

  # function output
  return(out)
}



# MEP national party ------------------------------------------------------
# extracts the national party or parties memberships of the MEP in the given
# legislative term
extract_party <- function(page, mep_status_text, term, mepid) {

  # define the what text
  what <- "National\\s+parties"

  # tag position
  idx <- page %>%
    rvest::html_elements(css = 'div[class="erpl_meps-status"]') %>%
    rvest::html_elements(css = 'h4[class="erpl_title-h4"]') %>%
    rvest::html_text2() %>%
    stringr::str_which(pattern = stringr::regex(
      pattern = what,
      ignore_case = TRUE))

  # if tag was found
  if (length(idx) != 0) {

    # extract tag
    tag <- page %>%
      rvest::html_elements(css = 'div[class="erpl_meps-status"]') %>%
      .[idx] %>%
      rvest::html_elements(css = "li") %>%
      rvest::html_text2()

    # loop over the elements of the tag
    out <- lapply(X = seq_along(tag), FUN = function(y) {

      # current tag
      c_tag <- tag[[y]]

      ## fetch the date
      # everything before the colon
      element_date <- stringr::str_extract_all(
        string = c_tag, pattern = "(.*)(?=:)") %>%
        unlist %>%
        .[. != ""]

      # remove element date from groups_tag
      c_tag <- stringr::str_remove(
        string = c_tag,
        pattern = stringr::fixed(pattern = element_date)) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_remove(patter = "^:") %>%
        stringr::str_trim(side = "both")

      # start date
      start_date <- stringr::str_extract(
        string = element_date, pattern = "\\d{2}-\\d{2}-\\d{4}")

      # stop date is the remainder
      end_date <- stringr::str_remove(
        string = element_date,
        pattern = stringr::fixed(pattern = start_date)) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_remove(patter = "/") %>%
        stringr::str_trim(side = "both")

      ## fetch the party group name
      # everything before the hyphen
      party <- stringr::str_extract_all(
        string = c_tag, pattern = ".*(?=\\()") %>%
        unlist %>%
        .[. != ""] %>%
        stringr::str_trim(side = "both")

      # output data
      out <- tibble::tibble(
        ep = as.integer(term),
        mepid = mepid,
        start_date = start_date,
        end_date = end_date,
        party = party)
      return(out)
    })

    # combine list to tibble
    out <- dplyr::bind_rows(out)

  } else {

    # empty output data
    out <- tibble::tibble(
      ep = NA,
      mepid = NA,
      start_date = NA_character_,
      end_date = NA_character_,
      party = NA_character_
    )
  }

  # function output
  return(out)
}



# EPG group code ----------------------------------------------------------
# this will only work if the political group is still in the current EP
# (otherwise it returns NA)
get_group_code <- function(page, group_long) {

  # long names of the political groups (only those currently in the EP)
  group_long_names <- page %>%
    rvest::html_elements(css = 'nav[aria-label="Political groups"]') %>%
    rvest::html_elements(css = "span") %>%
    rvest::html_text2()

  # find the corresponding group
  group_idx <- lapply(X = group_long, FUN = function(y) {
    idx <- which(group_long_names == y)
    if (length(idx) == 0) idx <- NA
    return(idx)
  }) %>%
    unlist

  if (length(group_idx) != 0) {

    group_code <- lapply(X = group_idx, FUN = function(y) {
      if (!is.na(y)) {
        # extract the corresponding group code
        out <- page %>%
          rvest::html_elements(css = 'nav[aria-label="Political groups"]') %>%
          rvest::html_elements(css = "a") %>%
          rvest::html_attr(name = "href") %>%
          .[y] %>%
          stringr::str_extract(pattern = "\\d+$") %>%
          as.integer()
      } else {
        out <- NA
      }
      return(out)
    }) %>%
      unlist

  } else {
    group_code <- NA
  }

  return(group_code)

}


# use libre office command line api to convert .doc to .docx --------------
doc_to_docx <- function(libre_office_path, docx.dir, doc.files) {

  # construct the command line call to soffice
  cmd <- sprintf(
    fmt = paste0('"%s" --convert-to docx:"MS Word 2007 XML"',
      ' --headless "%s%s" --outdir "%s"'),
    libre_office_path, docx.dir, doc.files, docx.dir)

  if (Sys.info()["sysname"] == "Windows") {
    # call to the command line
    system(cmd, show.output.on.console = FALSE)
  } else {
    message("On Mac: 1) backup current wd path; 2) go to doxc dir; 3)",
      " convert; 4) return to backed up path")
    # on MAC: 1) go to the doxc dir; 2) convert; 3) return
    # current working directory
    org_dir <- getwd()
    # change working directory to docx dir
    setwd(docx.dir)
    # MAC specific CMD call
    cmd <- paste0("/Applications/LibreOffice.app/Contents/MacOS/soffice",
                  " --headless --convert-to docx *.doc")
    # call to the command line
    system(cmd)
    # change back to original working directory
    setwd(org_dir)
  }
}


# canonical data_dir ------------------------------------------------------
# always add the slash to the end of the data_dir directory

canonical_data_dir <- function(data_dir) {

  if (substr(data_dir, start = nchar(data_dir),
    stop = nchar(data_dir)) != "/") {
    data_dir <- paste0(data_dir, "/")
  }
  return(data_dir)
}


# find duplicate votes ----------------------------------------------------

duplicate_votes <- function(votes) {

  message("Find and reomve duplicate votes")

  # initial binding for globals
  minutes_link <- vote_date <- grp_idx <- n_grps <- vote_id <- NULL
  vote_item_id <- text_tabled_id <- is_the_same <- NULL

  # group votes by minutes link (i.e. session)
  votes <- votes %>%
    dplyr::group_by(minutes_link)

  # group indices
  grp_idx <- dplyr::group_indices(.data = votes)

  # number of groups
  n_grps <- dplyr::n_groups(x = votes)

  # ungroup the data
  votes <- dplyr::ungroup(votes)

  # create progress bar
  if (n_grps > 1) {
    pb <- utils::txtProgressBar(min = 1, max = n_grps, style = 3)
  }

  # loop over each seession and then loop over all other sessions to
  # find duplicates
  match_out <- lapply(X = seq_len(n_grps), function(y) {

    # current session
    y_dat <- dplyr::slice(.data = votes, which(grp_idx == y)) # nolint

    # loop over all other sessions
    match_out <- lapply(seq_len(n_grps)[- y], function(z) {

      # session to compare to the current session (y_session)
      z_dat <- dplyr::slice(.data = votes, which(grp_idx == z))

      if (!"tbl_df" %in% class(y_dat) || !"tbl_df" %in% class(z_dat)) {
        stop("y_dat or z_dat is not a tibble")
      }

      # compare table dimensions
      if (nrow(y_dat) == nrow(z_dat)) {

        # remove identifier columns from both sessions (those are the
        # ones that vary)
        y_dat <- dplyr::select(.data = y_dat, -vote_id, vote_item_id,
          -minutes_link, -vote_date, -text_tabled_id)
        z_dat <- dplyr::select(.data = z_dat, -vote_id, vote_item_id,
          -minutes_link, -vote_date, -text_tabled_id)

        # check whether the tables are identical
        is_the_same <- all(y_dat == z_dat, na.rm = TRUE)
      } else {
        is_the_same <- FALSE
      }
      match_out <- tibble::tibble(
        x = y, y = z, is_the_same = is_the_same)
      return(match_out)
    })
    match_out <- dplyr::bind_rows(.data = match_out)

    # update progress bar
    if (n_grps > 1) {
      utils::setTxtProgressBar(pb = pb, value = y)
    }

    return(match_out)
  })

  # close progress bar
  if (n_grps > 1) {
    close(pb)
  }

  # bind all matches
  match_out <- dplyr::bind_rows(.data = match_out) %>%
    dplyr::filter(is_the_same == TRUE)
  if (nrow(match_out) == 0) {
    message("No duplicate votes found")
  } else {
    stop("Duplicate votes found")
  }
  return(votes)
}


# find exact matches for titles -------------------------------------------
find_title_matches <- function(titles = titles, t_pos, x) {

  # data container
  out <- tibble::tibble(
    t_pos = rep(NA, times = length(titles)),
    desc = rep(NA, times = length(titles)),
    timestamp = rep(as.POSIXct(
      x = "",
      tz = "CET", format = "%d/%m/%Y %H:%M:%S"),
      times = length(titles))
  )

  # loop over titles
  for (a in seq_along(titles)) {

    # reset aux vars
    time_stamp_idx <- NA
    time_stamps <- NA
    date_stamps <- NA

    # find matches
    t_match <- stringr::str_which(
      string = t_pos,
      pattern = stringr::fixed(pattern = titles[a]))

    # do matched strings start with topic number followed by a full stop
    if (length(t_match) > 1) {

      # check whether the topic contains a time stamp
      if (any(stringr::str_detect(string = t_pos[t_match],
        pattern = "\\d+:\\d+:\\d+"))) {

        # position of the time stamp match(es)
        time_stamp_idx <- stringr::str_which(
          string = t_pos[t_match], pattern = "\\d+:\\d+:\\d+")

        # extract time stamps
        time_stamps <- stringr::str_extract_all(
          string = t_pos[t_match], pattern = "\\d+:\\d+:\\d+") %>%
          unlist %>%
          unique

        # extract the date
        # date_stamps <- stringr::str_extract_all(
        #   string = t_pos[t_match], pattern = "\\d{1,2}/\\d{1,2}/\\d{4}") %>%
        #   unlist
        date_stamps <- rep(x = x$date, times = length(time_stamps))

        # convert to date-time
        time_stamps <- as.POSIXct(
          x = paste(date_stamps, time_stamps),
          tz = "CET", format = "%Y-%m-%d %H:%M:%S")

        # error message if no time stamp matched
        if (length(time_stamp_idx) == 0) {
          stop("Time stamp missing at ", titles[a])
        }

        # one time stamp matched
        if (length(time_stamp_idx) == 1) {

          # match position
          out$t_pos[a] <- which(t_pos == t_pos[t_match][time_stamp_idx])

          # match title
          out$desc[a] <- unlist(t_pos[t_match][time_stamp_idx])

          # time stamp
          out$timestamp[a] <- time_stamps

        } # end of condition: 1 time stamp matched

        # multiple time stamps matched
        if (length(time_stamp_idx) > 1) {

          # Not the first topic
          if (a > 1) {

            # subset to times that are larger
            time_stamp_idx_try <- time_stamp_idx[
              time_stamps > out$timestamp[(a - 1)]]
            if (length(time_stamp_idx_try) == 0) {
              time_stamp_idx <- time_stamp_idx[
                time_stamps == out$timestamp[(a - 1)]]
            } else {
              time_stamp_idx <- time_stamp_idx_try
            }

            if (length(time_stamps) > 1) {
              time_stamps <- time_stamps[time_stamps > out$timestamp[(a - 1)]]
            }

            # select the time stamp that minimizes the difference to the last
            # time stamp
            time_stamp_idx <- time_stamp_idx[which.min(difftime(
              time1 = time_stamps, time2 = out$timestamp[(a - 1)]))]

            time_stamps <- time_stamps[which.min(difftime(
              time1 = time_stamps, time2 = out$timestamp[(a - 1)]))]

          } else {
            time_stamp_idx <- time_stamp_idx[time_stamps == min(time_stamps)]
            time_stamps <- time_stamps[time_stamps == min(time_stamps)]
          }

          # match position
          # if multiple matches; don't use the ones matched previously
          if (length(which(t_pos == t_pos[t_match][time_stamp_idx])) > 1) {

            # candidate matches
            candidates <- which(t_pos == t_pos[t_match][time_stamp_idx])

            # filter out matches that were already matched previously
            out$t_pos[a] <- which(
              t_pos == t_pos[t_match][time_stamp_idx])[
                !candidates %in% out$t_pos]

          } else {
            out$t_pos[a] <- which(t_pos == t_pos[t_match][time_stamp_idx])
          }

          # match title
          out$desc[a] <- unlist(t_pos[t_match][time_stamp_idx])

          # time stamp
          out$timestamp[a] <- time_stamps
        } # end of condition multiple time stamps matched
        # Below: End of condition: time stamp matched
      } else {
        stop("No time stamp at topic ", a)
      }
      # Below: End of condition: multiple strings matched
    } else {
      stop("Unique string matched")
    }
  } # end of loop over titles

  return(out)
}


# check vote requests -----------------------------------------------------
# vote requests are not always stored in tables, at least not in EP 6 in .doc
# documents. Check whether the requests are listed below the tables
check_requests <- function(requests, t_pos, page, vote_date) {

  # initial binding for globals
  `:=` <- rlang::`:=`

  # minimum length of the requests vector should be 1
  if (length(requests[[1]]) > 0) {

    out_box <- vector(mode = "list", length = length(requests[[1]]))

    # loop over requests
    for (idx in seq_along(requests[[1]])) {

      # current request
      c_request <- unlist(requests[[1]][[idx]])

      # check whether the length of c_request is greater than 1
      if (length(c_request) > 1) {

        # check whether the request is a list
        if ("list" %in% class(c_request)) {
          c_request <- ""
          out_box[[idx]] <- c_request
        } else {
          # replace NA with empty strings
          c_request <- lapply(X = c_request, FUN = function(y) {
            y <- ifelse(test = is.na(y), yes = "", no = y)
          }) %>% unlist
          # does c_request contain the phrase "Request for"?
          if (!any(stringr::str_detect(
            string = c_request, pattern = "[Rr]equest\\s+for"))) {
            c_request <- NA
          } else {
            if (any(stringr::str_detect(
              string = c_request,
              pattern = "[Rr]equest\\s+for\\s+[Cc]onsultation"))) {
              if (!any(stringr::str_detect(
                string = c_request,
                pattern = "[Rr]equest\\s+for\\s+[Ss]eparate") &
                stringr::str_detect(
                  string = c_request,
                  pattern = "[Rr]equest\\s+for\\s+[Ss]plit") &
                stringr::str_detect(
                  string = c_request, pattern = "[Rr]equest\\s+for\\s+[Rr]oll")
              )) {
                c_request <- NA
              }
            }
          }
        }
      } # end of condition: length(c_request) > 1

      # check the request only if it is NA
      if (all(is.na(c_request))) {

        # extract the text following the title or between titles
        if (length(t_pos) == 1) {
          # extract all text following the title
          all_text <- page %>%
            xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
            .[t_pos:length(.)] %>%
            rvest::html_text2()
        } else {
          # extract the text between titles (except for the last title)
          if (idx != length(t_pos)) {
            all_text <- page %>%
              xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
              .[t_pos[idx]: t_pos[(idx + 1)]] %>%
              rvest::html_text2()
          } else {
            # extract all text following the title
            all_text <- page %>%
              xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
              .[t_pos[idx]:length(.)] %>%
              rvest::html_text2()
          }
        }

        # match the phrase "requests for"
        requests_pos <- lapply(X = all_text, FUN = function(y) {
          (stringr::str_detect(
            string = y, pattern = "[Rr]equest[s]?\\s?for\\s?[Rr]oll") |
             stringr::str_detect(
               string = y, pattern = "[Rr]equest[s]?\\s?for\\s?[Ss]plit") |
             stringr::str_detect(
               string = y, pattern = "[Rr]equest[s]?\\s?for\\s?[Ss]eparate")) &
            stringr::str_detect(
              string = y, pattern = "were\\s?withdrawn", negate = TRUE)
        }) %>%
          unlist

        # check whether "requests for" was a match
        if (any(requests_pos)) {

          # bug fixing counter
          err_idx <- 0

          # loop over request types
          out_box[[idx]] <- lapply(X = 1:sum(requests_pos), FUN = function(z) {

            err_idx <<- err_idx + 1L

            # type of request (for example: request for roll-call votes)
            request_type <- page %>%
              xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
              .[(t_pos[idx] + which(requests_pos)[z] - 1)] %>%
              rvest::html_text2()

            # all rows following request type and until the next request type or
            # the next title

            # just one request
            if (sum(requests_pos) == 1) {

              # extract up until minus 3 rows from the next heading
              all_rows <- try(
                expr = {
                  suppressWarnings(
                    page %>%
                      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
                      .[(t_pos[idx] + which(requests_pos)[z] + 0):
                          (t_pos[(idx + 1)] - 3)] %>%
                      xml2::xml_find_all(xpath = ".//w:r") %>%
                      .[stringr::str_detect(
                        string = as.character(.), pattern = "w:t")])
                }, silent = TRUE
              )

              # if minus 3 rows was too much, do minus 2
              if (!"try-error" %in% class(all_rows)) {
                if (length(all_rows) == 0) {
                  # extract up until minus 2 rows from the next heading
                  all_rows <- try(
                    expr = {
                      suppressWarnings(
                        page %>%
                          xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
                          .[(t_pos[idx] + which(requests_pos)[z] + 0):
                              (t_pos[(idx + 1)] - 2)] %>%
                          xml2::xml_find_all(xpath = ".//w:r") %>%
                          .[stringr::str_detect(
                            string = as.character(.), pattern = "w:t")])
                    }, silent = TRUE
                  )
                }
              }

              # redo if (t_pos[idx] + which(requests_pos) +1) is outside
              # doc range
              if ("try-error" %in% class(all_rows)) {
                all_rows <- suppressWarnings(
                  page %>%
                    xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
                    .[(t_pos[idx] + which(requests_pos)[z] + 0):
                        length(.)] %>%
                    xml2::xml_find_all(xpath = ".//w:r") %>%
                    .[stringr::str_detect(
                      string = as.character(.), pattern = "w:t")])
              }
            } else {

              # multiple requests, try going up until the next request
              all_rows <- try(
                expr = {
                  suppressWarnings(
                    page %>%
                      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
                      .[(t_pos[idx] + which(requests_pos)[z] + 0):
                          (t_pos[idx] + which(requests_pos)[(z + 1)] - 2)] %>%
                      xml2::xml_find_all(xpath = ".//w:r") %>%
                      .[stringr::str_detect(
                        string = as.character(.), pattern = "w:t")])
                }, silent = TRUE
              )

              # if this is the last request, try going up to the next title
              if ("try-error" %in% class(all_rows)) {
                all_rows <- try(
                  expr = {
                    suppressWarnings(
                      page %>%
                        xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
                        .[(t_pos[idx] + which(requests_pos)[z] + 0):
                            (t_pos[(idx + 1)] - 3)] %>%
                        xml2::xml_find_all(xpath = ".//w:r") %>%
                        .[stringr::str_detect(
                          string = as.character(.), pattern = "w:t")])
                  }, silent = TRUE
                )
              }

              # if this is the last request and the last title, go until the end
              # of the document
              if ("try-error" %in% class(all_rows)) {
                all_rows <- suppressWarnings(
                  page %>%
                    xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
                    .[(t_pos[idx] + which(requests_pos)[z] + 0):
                        length(.)] %>%
                    xml2::xml_find_all(xpath = ".//w:r") %>%
                    .[stringr::str_detect(
                      string = as.character(.), pattern = "w:t")])
              }
            }

            # loop over rows
            out <- lapply(X = all_rows, FUN = function(y) {

              # extract tabs from the row
              cols <- y %>%
                xml2::xml_find_all(xpath = ".//w:t") %>%
                rvest::html_text2() %>%
                unlist

              # output dataset if a request was matched
              out <- tibble::tibble(
                vote_date = vote_date,
                date_tally = which(t_pos == t_pos[idx]),
                rlang::`!!`(request_type) := cols[1],
                request = paste(cols[-1], collapse = ""))

              return(out)
            }) # end of loop over rows
            out <- dplyr::bind_rows(out)

            # remove empty rows
            out <- out %>%
              dplyr::filter(!is.na(!!rlang::sym(x = request_type)))

            return(out)
          })
          # end of condition: any(requests_pos)
        } else {
          # output dataset if a request was matched
          out_box[[idx]] <- NA
        }
      } # end of condition: is.na(c_request)
    } # end of loop over requests
  } else {
    # end of condition: length(requests) > 0
    out_box[[idx]] <- NA
  }

  return(out_box)
} # end of function


# find exact matches for the titles ---------------------------------------
#' @export
t_pos_fun <- function(titles, t_pos, tbl_idxs) {

  # container for titles
  t_container <- rep(NA, length(titles))

  # loop over the titles
  for (idx in seq_along(t_container)) {

    # keep only letters, numbers, and spaces in current title string
    clear_string <- stringr::str_remove_all(
      string = titles[idx], pattern = "[^\\w\\s]")

    # keep only letters, numbers, and spaces in t_pos (title candidates)
    clear_t_pos <- lapply(t_pos, function(z) {
      stringr::str_remove_all(string = z, pattern = "[^\\w\\s]")
    }) %>% unlist

    # find matches
    t_match <- stringr::str_which(
      string = clear_t_pos,
      pattern = paste0("^", clear_string))

    # exactly 1 match
    if (length(t_match) == 1) {
      # assign the match to the return vector
      t_container[idx] <- t_match
    }

    # multiple matches & not the first iteration
    if (length(t_match) > 1) {

      # check which table number the matches correspond to
      corresponding_tables <- lapply(X = t_match, FUN = function(a) {
        # which table does the match refer to?
        which.min(abs(tbl_idxs - a))
      }) %>% unlist

      # if only 1 table is matched assign that table position
      if (length(unique(corresponding_tables)) == 1) {
        t_match <- tbl_idxs[unique(corresponding_tables)]
      }

      # if multiple tables are matched and it is not the first iteration
      if (length(unique(corresponding_tables)) > 1 && idx != 1) {

        # check which table number the previous matches correspond to
        previous_table <- which.min(abs(tbl_idxs - t_container[idx - 1]))

        # keep only the tables that are bigger than the previous table
        corresponding_tables <- corresponding_tables[
          corresponding_tables > previous_table]

        # which of the corresponding tables is closer to the previous table
        closest_match <- which.min(abs(corresponding_tables - previous_table))

        # assign the table position of the closest match
        t_match <- tbl_idxs[corresponding_tables[closest_match]]

      } else {
        # assign the first corresponding table
        t_match <- tbl_idxs[min(corresponding_tables)]
      }

      # assign the match
      if (length(t_match) > 1) stop("Too many matches")
      t_container[idx] <- t_match

    } # end of condition: length(t_match) > 1 & idx != 1
  } # end of loop over titles
  return(t_container)
}



# hard coded votes --------------------------------------------------------
hard_coded_votes <- function(x, idx) {

  vote_tables <- vector(mode = "list", length = 1L)

  # EP6
  if (x$filename == "PV-6-2005-12-01-TOC_EN.html") {
    # output data
    votes <- tibble::tibble(
      vote_date = x$date,
      date_tally = 1,
      type = "",
      subject = "Referred back to committee (Rule 168)",
      am_no = "",
      author = "",
      rcv_etc = "",
      budget_line = "",
      block_rcv_ev_sep_split = "",
      vote = "",
      rcv_ev_votes_remarks = ""
    )
    requests <- NA
  } else if (x$`name_raw/votes` == "PV-6-2007-11-15-VOT_EN.doc" && idx == 1) {
    votes <- tibble::tibble(
      vote_date = x$date,
      date_tally = 1,
      type = "",
      subject = "The vote was adjourned to the next part-session.",
      am_no = "",
      author = "",
      rcv_etc = "",
      budget_line = "",
      block_rcv_ev_sep_split = "",
      vote = "",
      rcv_ev_votes_remarks = ""
    )
    requests <- NA
  } else if (x$`name_raw/votes` == "PV-6-2005-04-28-VOT_EN.doc" && idx == 1) {
    votes <- tibble::tibble(
      vote_date = x$date,
      date_tally = 1,
      type = "",
      subject = "single vote",
      am_no = "",
      author = "",
      rcv_etc = "",
      budget_line = "",
      block_rcv_ev_sep_split = "",
      vote = "+",
      rcv_ev_votes_remarks = ""
    )
    requests <- NA
  } else if (x$`name_raw/votes` == "PV-6-2005-04-28-VOT_EN.doc" && idx == 2) {
    votes <- tibble::tibble(
      vote_date = x$date,
      date_tally = 2,
      type = "",
      subject = "single vote",
      am_no = "",
      author = "",
      rcv_etc = "",
      budget_line = "",
      block_rcv_ev_sep_split = "",
      vote = "+",
      rcv_ev_votes_remarks = ""
    )
    requests <- NA
  } else if (x$`name_raw/votes` == "PV-6-2006-03-15-VOT_EN.doc" && idx == 4) {
    votes <- tibble::tibble(
      vote_date = x$date,
      date_tally = 4,
      type = "",
      subject = "Motion for a resolution B6-0148/2006",
      am_no = "",
      author = "DEVE Committee",
      rcv_etc = "",
      budget_line = "",
      block_rcv_ev_sep_split = "",
      vote = "",
      rcv_ev_votes_remarks = "Deemed adopted (Rule 90(4))"
    )
    requests <- NA
  } else if (x$`name_raw/votes` == "PV-6-2008-02-21-VOT_EN.doc" && idx == 8) {
    votes <- tibble::tibble(
      vote_date = c(x$date, x$date),
      date_tally = c(8, 8),
      type = "",
      subject = c("§ 8", "Vote: resolution (as a whole)"),
      am_no = c("1", ""),
      author = c("Verts/ALE, ALDE, GUE/NGL",
        "PPE-DE, PSE, ALDE, UEN, GUE/NGL, Verts/ALE"),
      rcv_etc = "",
      budget_line = "",
      block_rcv_ev_sep_split = "",
      vote = c("+", "+"),
      rcv_ev_votes_remarks = ""
    )
    requests <- NA
  } else {
    votes <- NA
    requests <- NA
  }

  # output
  vote_tables[[1]]$votes <- votes
  vote_tables[[1]]$requests <- requests
  return(vote_tables)
}



# hard coded titles -------------------------------------------------------
hard_coded_titles <- function(x) {

  # votes or roll-call votes
  if ("name_raw/votes" %in% names(x)) {
    # PV-6-2007-02-14-VOT_EN.doc
    if (x$`name_raw/votes` == "PV-6-2007-02-14-VOT_EN.doc") {
      titles <- c(
        "Transport and illegal detention of prisoners",
        "Macro-financial aid for Moldova",
        "Conservation of stocks of highly migratory species",
        "Drift nets",
        "Voluntary modulation of direct payments under the CAP",
        paste0("The Court of Auditors’ Special Report No 6/2005 on the",
          " Trans-European Network for Transport"),
        "Climate change",
        "PNR-SWIFT",
        "Lisbon Strategy (European Council of 8 and 9 March 2007)",
        "Radio Spectrum"
      )
    } else if (x$`name_raw/votes` == "PV-6-2008-05-08-VOT_EN.doc") {
      titles <- c(
        paste0("EC/Seychelles Agreement - fishing opportunities and",
          " financial contributions"),
        "Excise duty on beer produced locally in Madeira",
        "Exercise of implementing powers conferred on the Commission",
        "Implementing measures (amendment of Rule 81)",
        "Support schemes for farmers (support for cotton)",
        "Transatlantic Economic Council",
        "Human Rights in the World 2007 and the EU’s policy on the matter",
        "EU Election Observation Missions",
        paste0("Trade and Economic Relations with the countries of",
          " South East Asia (ASEAN)"),
        "Management of deep-sea fish stocks",
        "Framework for the activities of lobbyists in the, EU institutions",
        "White Paper on Sport"
      )
    } else if (x$`name_raw/votes` == "PV-7-2009-10-22-VOT_EN.doc") {
      titles <- c(
        "Draft general budget of the European Union - 2010 financial year",
        "Draft general budget 2010 (section III)",
        paste0("Draft general budget 2010 (sections I, II, IV, V, VI,",
          " VII, VIII and IX)"),
        paste0("Adjustment of basic salaries and allowances applicable",
          " to Europol staff*"),
        paste0("Common organisation of agricultural markets and specific",
          " provisions for certain agricultural products (single",
          " CMO Regulation)"),
        "Progress of SIS II and VIS",
        "Democracy building in external relations",
        paste0("The institutional aspects of setting up the European external",
          " action service"),
        paste0("Preparation of the TEC meeting and the EU-USA Summit",
          " (2 and 3 November 2009)"),
        "Guinea",
        "Iran",
        "Sri Lanka")
    } else if (x$`name_raw/votes` == "PV-7-2012-05-10-VOT_EN.doc") {
      titles <- c(
        paste0("Protection against dumped imports from countries not",
          " members of the European Community ***I"),
        "Restrictions on imports of certain steel products from Russia ***I",
        paste0("Electronic publication of the Official Journal of the",
          " European Union ***"),
        paste0("Request for the defence of Corneliu Vadim Tudor's",
          " parliamentary immunity"),
        "Regional airports and air services",
        "2010 discharge: EU general budget, Section III, Commission",
        "ECA special reports in the context of the 2010 Commission discharge",
        "Discharge 2010: EU general budget, European Parliament",
        "Discharge 2010: 8th, 9th and 10th European Development Funds",
        "Discharge 2010: EU general budget, Council",
        "Discharge 2010: EU general budget, Court of Justice",
        "Discharge 2010: EU general budget, Court of Auditors",
        "Discharge 2010: EU general budget, Economic and Social Committee",
        "Discharge 2010: EU general budget, Committee of the Regions",
        "Discharge 2010: EU general budget, European Ombudsman",
        paste0("Discharge 2010: EU general budget, European Data",
          " Protection Supervisor"),
        paste0("Discharge 2010: performance, financial management",
          " and control of EU agencies"),
        paste0("Discharge 2010: Translation Centre for the Bodies",
          " of the European Union"),
        paste0("Discharge 2010: European Centre for the Development",
          " of Vocational Training (CEDEFOP)"),
        "Discharge 2010: European Police College (CEPOL)",
        "Discharge 2010: Community Fisheries Control Agency",
        "Discharge 2010: European Aviation Safety Agency",
        paste0("Discharge 2010: European Centre for Disease Prevention",
          " and Control (ECDC)"),
        "Discharge 2010: European Chemicals Agency",
        "Discharge 2010: European Environment Agency",
        "Discharge 2010: European Food Safety Agency",
        "Discharge 2010: European Institute for Gender Equality",
        "Discharge 2010: European Medicines Agency",
        paste0("Discharge 2010: European Monitoring Centre for",
          " Drugs and Drug Addiction"),
        "Discharge 2010: European Maritime Safety Agency",
        "Discharge 2010: European Network and Information Security Agency",
        "Discharge 2010: European Railway Agency",
        "Discharge 2010: European Training Foundation",
        "Discharge 2010: European Agency for Health and Safety at Work",
        "Discharge 2010: Euratom Supply Agency",
        paste0("Discharge 2010: European Foundation for the Improvement",
          " of Living and Working Conditions"),
        "Discharge 2010: Eurojust",
        "Discharge 2010: Europol",
        "Discharge 2010: European Union Agency for Fundamental Rights",
        paste0("Discharge 2010: European Agency for the Management of",
          " Operational Cooperation at the External Borders of the Member",
          " States of the European Union (FRONTEX)"),
        "Discharge 2010: European GNSS Agency",
        "Discharge 2010: ARTEMIS – Embedded Computing Systems",
        "Discharge 2010: Clean Sky – Aeronautics and Environment",
        "Discharge 2010: ENIAC Joint Undertaking",
        "Discharge 2010: Fuel Cells and Hydrogen Joint Undertaking (FCH)",
        "Discharge 2010: Initiative on Innovative Medicines (IMI)",
        paste0("Discharge 2010: Joint Undertaking for ITER and the",
          " development of fusion energy"),
        "Discharge 2010: SESAR Joint Undertaking",
        paste0("Protection of the EU financial interests – fight",
          " against fraud – Annual report 2010"),
        paste0("Roaming on public mobile communications networks",
          " within the Union ***I"),
        "Export and import of dangerous chemicals ***I",
        "Amendment of Rules 87a and 88",
        "Law applicable to non-contractual obligations (Rome II)",
        paste0("Trade and investment strategy for the Southern",
          " Mediterranean following the Arab Spring revolutions"),
          "Patenting of essential biological processes",
        "Maritime piracy"
      )
    }  else {
      titles <- NULL
    }
  } else {
    if (x$`name_raw/roll_calls` == "PV-9-2021-06-09-RCV_EN.docx") {
      titles <- c(
        paste0("75th and 76th sessions of the United Nations General",
          " Assembly - A9-0173/2021 - María Soraya Rodríguez ",
          "Ramos - Recommendation"),
        paste0("Neighbourhood, Development and International Cooperation",
          " Instrument 2021-2027 - A9-0198/2021 - Michael Gahler, ",
          "Charles Goerens, Maria Arena, Rasa Juknevičienė - Am 1S"),
        paste0("The conflict of interest of the Prime Minister of ",
          "the Czech Republic - B9-0303/2021 - Am 1"),
        "B9-0303/2021 - Am 2",
        "B9-0303/2021 - Am 3/1",
        "B9-0303/2021 - Am 3/2",
        "B9-0303/2021 - § 35",
        "B9-0303/2021 - § 36/1",
        "B9-0303/2021 - § 36/2",
        "B9-0303/2021 - § 37",
        paste0("Meeting the Global COVID-19 challenge: effects of ",
          "the waiver of the WTO TRIPS Agreement on COVID-19 vaccines, ",
          "treatment, equipment and increasing production and ",
          "manufacturing capacity in developing countries- ",
          "RC-B9-0306/2021 - Am 33"),
        "RC-B9-0306/2021 - Am 9",
        "RC-B9-0306/2021 - Am 10",
        "RC-B9-0306/2021 - Am 14",
        "RC-B9-0306/2021 - Am 15",
        "RC-B9-0306/2021 - Am 16/1",
        "RC-B9-0306/2021 - Am 20",
        "RC-B9-0306/2021 - Am 34",
        "RC-B9-0306/2021 - Am 11= 21=",
        "RC-B9-0306/2021 - Am 2/1",
        "RC-B9-0306/2021 - Am 2/2",
        "RC-B9-0306/2021 - Am 12",
        "RC-B9-0306/2021 - Am 5/1",
        "RC-B9-0306/2021 - Am 5/2",
        "RC-B9-0306/2021 - Am 13",
        "RC-B9-0306/2021 - Am 6/1",
        "RC-B9-0306/2021 - Am 6/2",
        "RC-B9-0306/2021 - Am 6/3",
        "RC-B9-0306/2021 - Am 6/4",
        "RC-B9-0306/2021 - Am 22",
        "RC-B9-0306/2021 - Am 23",
        "RC-B9-0306/2021 - Am 3",
        "RC-B9-0306/2021 - Am 7",
        "RC-B9-0306/2021 - Am 35",
        "RC-B9-0306/2021 - Am 24",
        "RC-B9-0306/2021 - Am 17",
        "RC-B9-0306/2021 - Am 8",
        "RC-B9-0306/2021 - Am 25",
        "RC-B9-0306/2021 - Am 26",
        "RC-B9-0306/2021 - Am 27",
        "RC-B9-0306/2021 - Am 28",
        "RC-B9-0306/2021 - Am 18",
        "RC-B9-0306/2021 - Am 29",
        "RC-B9-0306/2021 - Am 30",
        "RC-B9-0306/2021 - Am 31",
        "RC-B9-0306/2021 - Am 32",
        paste0("The EU's Cybersecurity Strategy for the Digital Decade",
          " - B9-0305/2021 - Am 1"),
        paste0("European Parliament’s Scrutiny on the ongoing assessment",
          " by the Commission and the Council of the national recovery ",
          "and resilience plans - RC-B9-0331/2021 - Am 1"),
        "RC-B9-0331/2021 - Am 9",
        "RC-B9-0331/2021 - Am 2",
        "RC-B9-0331/2021 - Am 3S",
        "RC-B9-0331/2021 - Am 4",
        "RC-B9-0331/2021 - Am 5S",
        "RC-B9-0331/2021 - Am 6S",
        "RC-B9-0331/2021 - Am 7",
        "RC-B9-0331/2021 - Am 8",
        "RC-B9-0331/2021 - Am 10",
        paste0("Regulations and general conditions governing the ",
          "performance of the Ombudsman’s duties - A9-0174/2021 - ",
          "Paulo Rangel - Annex (Draft regulation)"),
        "A9-0174/2021 - Paulo Rangel - Postponement of the vote (Rule 46(3))",
        paste0("Transitional provisions in order to address the impact of ",
          "the COVID-19 crisis (amendment of Regulation (EU) 2016/1628) - ",
          "C9-0185/2021 - Commission proposal"),
        paste0("The conflict of interest of the Prime Minister of the ",
          "Czech Republic - B9-0303/2021 - Motion for a resolution"),
        paste0("Meeting the Global COVID-19 challenge: effects of the ",
          "waiver of the WTO TRIPS Agreement on COVID-19 vaccines, ",
          "treatment, equipment and increasing production and manufacturing ",
          "capacity in developing countries - RC-B9-0306/2021 - Motion ",
          "for a resolution"),
        paste0("Objection pursuant to Rule 112(2) and (3): Maximum residue ",
          "limit for imidacloprid - B9-0313/2021 - Motion for a resolution"),
        paste0("Objection pursuant to Rule 112(2) and (3): Active substances,",
          " including flumioxazine - B9-0312/2021 - Motion for a resolution")
      )
    } else {
      titles <- NULL
    }
  }

  return(titles)
}



# hard coded vote descriptions --------------------------------------------
hard_coded_descriptions <- function(vote.descriptions, x) {

  if (x$`name_raw/votes` == "PV-6-2008-05-08-VOT_EN.doc") {
    vote.descriptions[1] <- "Report: Josu ORTUONDO LARREA (A6-0085/2008)"
    vote.descriptions[2] <- "Report: Sérgio MARQUES (A6-0146/2008)"
    vote.descriptions[4] <- paste0(
      "Report: Monica FRASSONI (A6-0108/2008) qualified majority")
    vote.descriptions[5] <- "Report: Ioannis GKLAVAKIS (A6-0166/2008)"
  }

  return(vote.descriptions)
}


# hard coded table column names -------------------------------------------
hard_coded_table_column_names <- function(x, y, z) {

  if (x$`name_raw/votes` == "PV-6-2007-10-25-VOT_EN.doc" && y == 3) {
    # table names to first row
    l1 <- names(z)
    l1[-1] <- rep("", length(l1[-1]))
    z <- rbind(l1, z)
    # new table column names
    names(z) <- c("Budget line", "Am No", "Block", "RCV, EV, split.",
                  "Vote", "RCV/EV - remarks")

  }

  if (x$`name_raw/votes` == "PV-7-2009-12-16-VOT_EN.doc" && y == 4) {
    # table names to first row
    l1 <- names(z)
    l1[-1] <- rep("", length(l1[-1]))
    z <- rbind(l1, z)
    # new table column names
    names(z) <- c("Subject", "Am No", "Author", "RCV etc.",
                  "Vote", "RCV/EV - remarks")
  }

  # return corrected table
  return(z)

}

# hard coded content ------------------------------------------------------
hard_coded_content <- function(content, date) {

  if (date == "2014-07-17_6") {

    content <- tibble::tibble(
      requests_for_separate_votes =
        c("S&D:", "§§ 24, 27",
          "Verts/ALE:", "§ 27",
          "GUE/NGL:", "§§ 4, 27")
    )
  }
  return(content)
}


# hard coded elements -----------------------------------------------------
hard_coded_elements <- function(subject_jj, date, item_nbr, jj) {

  if(date == paste0("https://www.europarl.europa.eu/doceo/",
    "document/PV-6-2007-02-13-TOC_EN.html") &&
     item_nbr == 6 && jj == 39) {

    subject_jj <- "amendment 183"

  }
  return(subject_jj)
}



# hard_coded_names --------------------------------------------------------
hard_coded_names <- function(rcv_name, term, rcv_date, meps, mepid_pos, c_dat) {

  if (is.na(rcv_name)) {
    mepid_pos <- NA
  } else {
    if (rcv_name == "FRANCESC GAMBÚS" && term == "8") {
      mepid_pos <- which(meps$mepid == "124993")
    } else if (rcv_name == "TOMAŠEVSKI" && term %in% c("7", "8", "9")) {
      mepid_pos <- which(meps$mepid == "96697")
    } else if (
      rcv_name == "VALDEMAR TOMAŠEVSKI" && term %in% c("7", "8", "9")
    ) {
      mepid_pos <- which(meps$mepid == "96697")
    } else if (
      rcv_name == "TOMAŠEVSKI VALDEMAR" && term %in% c("7", "8", "9")
    ) {
      mepid_pos <- which(meps$mepid == "96697")
    } else if (rcv_name == "CEBALLOS" && term == "8") {
      mepid_pos <- which(meps$mepid == "125006")
    } else if (rcv_name == "GILL CBE" && term == "8") {
      mepid_pos <- which(meps$mepid == "4533")
    } else if (rcv_name == "KONSTANTINOS PAPADAKIS" && term == "8" ||
               rcv_name == "PAPADAKIS KONSTANTINOS" && term == "8") {
      mepid_pos <- which(meps$mepid == "125093")
    } else if (rcv_name == "LÓPEZ PALOMA" && term == "8") {
      mepid_pos <- which(meps$mepid == "125047")
    } else if (rcv_name == "NART PEÑALVER" && term ==  "8") {
      mepid_pos <- which(meps$mepid == "125005")
    } else if (rcv_name == "NOSHEENA MOBARIK" && term == "8") {
      mepid_pos <- which(meps$mepid == "189391")
    } else if (rcv_name == "PAUNOVA" && term == "8") {
      mepid_pos <- which(meps$mepid == "98341")
    } else if (rcv_name == "PUNSET BANNEL" && term == "8") {
      mepid_pos <- which(meps$mepid == "90110")
    } else if (rcv_name == "SOLÉ I FERRANDO" && term == "8") {
      mepid_pos <- which(meps$mepid == "185974")
    } else if (rcv_name == "VALENCIANO MARTÍNEZ-OROZCO" && term == "8") {
      mepid_pos <- which(meps$mepid == "4334")
    } else if (
      stringi::stri_trans_general(
        str = rcv_name, "Latin-ASCII"
      ) == "JEDRZEJEWSKA" && term == "7"
    ) {
      mepid_pos <- which(meps$mepid == "96782")
    } else if (rcv_name == "LE PEN JEAN- MARIE" && term == "7") {
      mepid_pos <- which(meps$mepid == "1023")
    } else if (rcv_name == "MANNER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96685")
    } else if (rcv_name == "NEDELCHEVA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96848")
    } else if (rcv_name == "MIHAYLOVA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96861")
    } else if (rcv_name == "MIRSKIS" && term == "7") {
      mepid_pos <- which(meps$mepid == "96999")
    } else if (rcv_name == "ANDRES PERELLO RODRIGUEZ" && term == "7") {
      mepid_pos <- which(meps$mepid == "96989")
    } else if (rcv_name == "CATHERINE GREZE" && term == "7") {
      mepid_pos <- which(meps$mepid == "96745")
    } else if (rcv_name == "CORINA CREŢU" && term == "7") {
      mepid_pos <- which(meps$mepid == "33997")
    } else if (rcv_name == "GABRIEL MATO ADROVER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96936")
    } else if (rcv_name == "GEORGE SABIN CUTAŞ" && term == "7") {
      mepid_pos <- which(meps$mepid == "96856")
    } else if (rcv_name == "GERALD HAEFNER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96758")
    } else if (rcv_name == "HAEFNER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96758")
    } else if (rcv_name == "GIOMMARIA UGGIASS" && term == "7") {
      mepid_pos <- which(meps$mepid == "97131")
    } else if (rcv_name == "MARIELLE GALLO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96896")
    } else if (rcv_name == "MICHAŁ TOMASZ KAMIŃSKIJACEK" && term == "7") {
      mepid_pos <- which(meps$mepid == "23792")
    } else if (rcv_name == "MMHANS-PETER MARTIN" && term == "7") {
      mepid_pos <- which(meps$mepid == "4238")
    } else if (rcv_name == "NADEZHDA MIHAYLOVA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96861")
    } else if (rcv_name == "RIIKKA MANNER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96685")
    } else if (rcv_name == "S-D:ARSENIS" && term == "7") {
      mepid_pos <- which(meps$mepid == "97009")
    } else if (rcv_name == "SIDONIA ELŻBIETA JĘDRZEJEWSKA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96782")
    } else if (rcv_name == "SILVIA-ADRIANA ŢICĂU" && term == "7") {
      mepid_pos <- which(meps$mepid == "36281")
    } else if (rcv_name == "MYLLER" && term == "7") {
      mepid_pos <- which(meps$mepid == "2062")
    } else if (rcv_name == "CUTAŞ GEORGE SABIN" && term == "7") {
      mepid_pos <- which(meps$mepid == "96856")
    } else if (rcv_name == "ANDERSSON" && term == "7") {
      mepid_pos <- which(meps$mepid == "113959")
    } else if (rcv_name == "GREZE CATHERINE" && term == "7") {
      mepid_pos <- which(meps$mepid == "96745")
    } else if (rcv_name == "HIGGINS" && term == "7") {
      mepid_pos <- which(meps$mepid == "28117")
    } else if (rcv_name == "HUSMENOVA" && term == "7") {
      mepid_pos <- which(meps$mepid == "34249")
    } else if (rcv_name == "JĘDRZEJEWSKA SIDONIA ELŻBIETA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96782")
    } else if (rcv_name == "MANNER RIIKKA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96685")
    } else if (rcv_name == "MIHAYLOVA NADEZHDA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96861")
    } else if (rcv_name == "MIRSKIS ALEKSANDRS" && term == "7") {
      mepid_pos <- which(meps$mepid == "96999")
    } else if (rcv_name == "NEDELCHEVA MARIYA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96848")
    } else if (rcv_name == "PERELLO RODRIGUEZ ANDRES" && term == "7") {
      mepid_pos <- which(meps$mepid == "96989")
    } else if (rcv_name == "SCHULZ" && term == "7") {
      mepid_pos <- which(meps$mepid == "1911")
    } else if (rcv_name == "SCOTTA\" GIANCARLO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96996")
    } else if (rcv_name == "STOYANOV" && term == "7") {
      mepid_pos <- which(meps$mepid == "34254")
    } else if (rcv_name == "SVENSSON" && term == "7") {
      mepid_pos <- which(meps$mepid == "28134")
    } else if (rcv_name == "GRÄFIN VON" && term == "7") {
      mepid_pos <- which(meps$mepid == "96776")
    } else if (rcv_name == "(THE EARL OF) DARTMOUTH WILLIAM" && term == "7") {
      mepid_pos <- which(meps$mepid == "96958")
    } else if (rcv_name == "AGGOURAKIS" && term == "7") {
      mepid_pos <- which(meps$mepid == "99419")
    } else if (rcv_name == "GALLO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96896")
    } else if (rcv_name == "TAYLOR" && term == "7") {
      mepid_pos <- which(meps$mepid == "102931")
    } else if (rcv_name == "KELLY" && term == "7") {
      mepid_pos <- which(meps$mepid == "96668")
    } else if (rcv_name == "FERREIRA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96706")
    } else if (rcv_name == "VAN DALEN PETER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96809")
    } else if (rcv_name == "JAHR PETER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96772")
    } else if (rcv_name == "SIMON PETER" && term == "7") {
      mepid_pos <- which(meps$mepid == "96836")
    } else if (rcv_name == "VAN  BAALEN JOHANNES CORNELIS" && term == "7") {
      mepid_pos <- which(meps$mepid == "96937")
    } else if (rcv_name == "DE JONG CORNELIS" && term == "7") {
      mepid_pos <- which(meps$mepid == "96748")
    } else if (rcv_name == "LUDVIGSSON OLLE" && term == "7") {
      mepid_pos <- which(meps$mepid == "96673")
    } else if (rcv_name == "OLLE SCHMIDT" && term == "7") {
      mepid_pos <- which(meps$mepid == "4274")
    } else if (rcv_name == "COZZOLINO ANDREA" && term == "7") {
      mepid_pos <- which(meps$mepid == "96880")
    } else if (rcv_name == "NUTTALL PAUL") {
      mepid_pos <- which(meps$mepid == "96805")
    } else if (rcv_name == "MATO ADROVER GABRIEL" && term == "7") {
      mepid_pos <- which(meps$mepid == "96936")
    } else if (rcv_name == "BORYS PIOTR" && term == "7") {
      mepid_pos <- which(meps$mepid == "96799")
    } else if (rcv_name == "PORĘBA TOMASZ PIOTR" && term == "7") {
      mepid_pos <- which(meps$mepid == "96801")
    } else if (rcv_name == "SWINBURNE KAY" && term == "7") {
      mepid_pos <- which(meps$mepid == "96920")
    } else if (rcv_name == "SOINI TIMO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96666")
    } else if (rcv_name == "CANCIAN ANTONIO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96733")
    } else if (rcv_name == "THOMAS MANN" && term == "7") {
      mepid_pos <- which(meps$mepid == "1922")
    } else if (rcv_name == "HÄNDEL THOMAS" && term == "7") {
      mepid_pos <- which(meps$mepid == "96851")
    } else if (rcv_name == "SCURRIA MARCO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96843")
    } else if (rcv_name == "ZVER MILAN" && term == "7") {
      mepid_pos <- which(meps$mepid == "96933")
    } else if (rcv_name == "DAVID MARTIN" && term == "7") {
      mepid_pos <- which(meps$mepid == "1403")
    } else if (rcv_name == "CAMPBELL BANNERMAN DAVID" && term == "7") {
      mepid_pos <- which(meps$mepid == "96912")
    } else if (rcv_name == "STRASSER ERNST" && term == "7") {
      mepid_pos <- which(meps$mepid == "96766")
    } else if (rcv_name == "KADENBACH KARIN" && term == "7") {
      mepid_pos <- which(meps$mepid == "97014")
    } else if (rcv_name == "MARINE LE PEN" && term == "7") {
      mepid_pos <- which(meps$mepid == "28210")
    } else if (rcv_name == "CANCIAN ANTONIO" && term == "7") {
      mepid_pos <- which(meps$mepid == "96733")
    } else if (rcv_name == "LEGUTKO RYSZARD ANTONI" && term == "7") {
      mepid_pos <- which(meps$mepid == "96796")
    } else if (is.na(rcv_name) && term == "7") {
      mepid_pos <- NA
    } else if (rcv_name == "BADÍA I CUTXET" && term == "6") {
      mepid_pos <- which(meps$mepid == "28342")
    } else if (rcv_name == "LE PEN MARINE" && term %in% c("6", "7", "8")) {
      mepid_pos <- which(meps$mepid == "28210")
    } else if (rcv_name == "FERREIRA ANNE" && term == "6") {
      mepid_pos <- which(meps$mepid == "5735")
    } else if (rcv_name == "MARTIN DAVID" && term %in%
               c("2", "3", "4", "5", "6", "7", "8")) {
      mepid_pos <- which(meps$mepid == "1403")
    } else if (rcv_name == "DOUAIS" && term == "6") {
      mepid_pos <- which(meps$mepid == "28157")
    } else if (rcv_name == "JILLIAN EVANS" && term == "6") {
      mepid_pos <- which(meps$mepid == "4550")
    } else if (rcv_name == "ROBERT J.E. EVANS" && term == "6") {
      mepid_pos <- which(meps$mepid == "2099")
    } else if (rcv_name == "RAYNAUD" && term == "6") {
      mepid_pos <- which(meps$mepid == "28168")
    } else if (rcv_name == "WOJCIECHOWSKI" && term == "6") {
      mepid_pos <- which(meps$mepid == "23784")
    } else if (rcv_name == "BERMINGUER" && term == "6") {
      mepid_pos <- which(meps$mepid == "28369")
    } else if (rcv_name == "SCHMIDT" && term == "6" &&
               all(c_dat$rcv_date < "2009-07-14")) {
      mepid_pos <- which(meps$mepid == "28237")
    } else if (rcv_name == "CARLOS ITURGÁIZ ANGULO" && term == "6") {
      mepid_pos <- which(meps$mepid == "28398")
    } else if (rcv_name == "CARLSHAMRE" && term == "6") {
      mepid_pos <- which(meps$mepid == "28128")
    } else if (rcv_name == "MARIA CARLSHAMRE" && term == "6") {
      mepid_pos <- which(meps$mepid == "28128")
    } else if (rcv_name == "CREŢU CORINA" && term == "6") {
      mepid_pos <- which(meps$mepid == "33997")
    } else if (rcv_name == "CORINA CREŢU" && term == "6") {
      mepid_pos <- which(meps$mepid == "33997")
    } else if (rcv_name == "CREŢU GABRIELA" && term == "6") {
      mepid_pos <- which(meps$mepid == "33977")
    } else if (rcv_name == "GABRIELA CREŢU" && term == "6") {
      mepid_pos <- which(meps$mepid == "33977")
    } else if (rcv_name == "DAVID HAMMERSTEIN MINTZ" && term == "6") {
      mepid_pos <- which(meps$mepid == "28345")
    } else if (rcv_name == "DIMITROV PHILIP DIMITROV" && term == "6") {
      mepid_pos <- which(meps$mepid == "37526")
    } else if (rcv_name == "DUMITRU GHEORGHE MIRCEA COŞEA" && term == "6") {
      mepid_pos <- which(meps$mepid == "33976")
    } else if (rcv_name == "ERE" && term == "6") {
      mepid_pos <- NA
    } else if (rcv_name == "EVANS JILLIAN" && term == "6") {
      mepid_pos <- which(meps$mepid == "4550")
    } else if (rcv_name == "EVANS ROBERT J.E." && term == "6") {
      mepid_pos <- which(meps$mepid == "2099")
    } else if (rcv_name == "GANŢ" && term == "6") {
      mepid_pos <- which(meps$mepid == "33979")
    } else if (rcv_name == "GROUPE ITS" && term == "6") {
      mepid_pos <- NA
    } else if (rcv_name == "GROUPE VERTS/ALE" && term == "6") {
      mepid_pos <- NA
    } else if (rcv_name == "HANS-GERT POETTERING" && term == "6") {
      mepid_pos <- which(meps$mepid == "1253")
    } else if (rcv_name == "HUSMENOVA" && term == "6") {
      mepid_pos <- which(meps$mepid == "34249")
    } else if (rcv_name == "JAVIER POMES" && term == "6") {
      mepid_pos <- which(meps$mepid == "1882")
    } else if (rcv_name == "JOHN ATTARD-MONTALTORICHARD SEEBER" &&
      term == "6") {
      mepid_pos <- "JOHN ATTARD-MONTALTORICHARD SEEBER"
    } else if (rcv_name == "GANŢ" && term == "6") {
      mepid_pos <- which(meps$mepid == "33979")
    } else if (rcv_name == "JUKNEVIČIENĖ" && term == "6") {
      mepid_pos <- which(meps$mepid == "28273")
    } else if (rcv_name == "KAZAK" && term == "6") {
      mepid_pos <- which(meps$mepid == "38613")
    } else if (rcv_name == "LUIS YAÑEZ-BARNUEVO GARCÍA" && term == "6") {
      mepid_pos <- which(meps$mepid == "28278")
    } else if (rcv_name == "MARIANNE THYSSENG" && term == "6") {
      mepid_pos <- which(meps$mepid == "1832")
    } else if (rcv_name == "POETTERING" && term == "6") {
      mepid_pos <- which(meps$mepid == "1253")
    } else if (rcv_name == "ROSELYNE LEFRANCOIS" && term == "6") {
      mepid_pos <- which(meps$mepid == "38846")
    } else if (rcv_name == "SCHMITT" && term == "6" &&
               all(c_dat$rcv_date > "2005-10-17")) {
      mepid_pos <- which(meps$mepid == "28133")
    } else if (rcv_name == "PÁL SCHMITT" && term == "6") {
      mepid_pos <- which(meps$mepid == "28133")
    } else if (rcv_name == "SCHMITT" && term == "6" &&
               all(c_dat$rcv_date == "2005-09-07")) {
      mepid_pos <- which(meps$mepid == "28133")
    } else if (rcv_name == "SILVIA-ADRIANA ŢICĂU" && term == "6") {
      mepid_pos <- which(meps$mepid == "36281")
    }  else if (rcv_name == "COSTA" && term == "6" &&
                all(c_dat$rcv_date > "2005-03-11")) {
      mepid_pos <- which(meps$mepid == "4747")
    } else if (rcv_name == "SSAMU" && term == "6") {
      mepid_pos <- NA
    } else if (rcv_name == "STAVROS LAMBRINIDIS´" && term == "6") {
      mepid_pos <- which(meps$mepid == "28576")
    } else if (rcv_name == "STEFANO ZAPPALÀ" && term == "6") {
      mepid_pos <- which(meps$mepid == "4433")
    } else if (rcv_name == "TITUS CORLĂŢEAN" && term == "6") {
      mepid_pos <- which(meps$mepid == "33968")
    } else if (rcv_name == "WEHENRI WEBER" && term == "6") {
      mepid_pos <- which(meps$mepid == "2319")
    } else if (rcv_name == "WHENRI WEBER" && term == "6") {
      mepid_pos <- which(meps$mepid == "2319")
    } else if (rcv_name == "LEFRANCOIS ROSELYNE" && term == "6") {
      mepid_pos <- which(meps$mepid == "38846")
    } else if (rcv_name == "NICULEŞCU RAREŞ-LUCIAN" && term == "6") {
      mepid_pos <- which(meps$mepid == "39714")
    } else if (rcv_name == "POPA" && term == "6") {
      mepid_pos <- which(meps$mepid == "34003")
    } else if (rcv_name == "CHIRIŢĂ CĂLIN CĂTĂLIN" && term == "6") {
      mepid_pos <- which(meps$mepid == "95018")
    } else if (rcv_name == "ANTOCHI ALIN LUCIAN EMANUEL" && term == "6") {
      mepid_pos <- which(meps$mepid == "95280")
    } else if (rcv_name == "HĂMBĂŞAN IOAN LUCIAN" && term == "6") {
      mepid_pos <- which(meps$mepid == "95704")
    } else if (rcv_name == "PÁLFI" && term == "6") {
      mepid_pos <- which(meps$mepid == "28147")
    } else if (rcv_name == "PÁL SCHMITT" && term == "6") {
      mepid_pos <- which(meps$mepid == "28133")
    } else if (rcv_name == "MAREK ALEKSANDER CZARNECKI" && term == "6") {
      mepid_pos <- which(meps$mepid == "28371")
    } else if (rcv_name == "ANNE FERREIRA" && term == "6") {
      mepid_pos <- which(meps$mepid == "5735")
    } else if (rcv_name == "SCHMIDT" && term == "6") {
      mepid_pos <- which(meps$mepid == "28237")
    } else if (rcv_name == "BERNARD PIOTR WOJCIECHOWSKI" && term == "6") {
      mepid_pos <- which(meps$mepid == "34720")
    } else if (rcv_name == "DE BLASIO ANTONIO" && term == "6") {
      mepid_pos <- which(meps$mepid == "36748")
    } else if (rcv_name == "HOLM JENS" && term == "6") {
      mepid_pos <- which(meps$mepid == "37008")
    } else if (rcv_name == "OLLE SCHMIDT" && term == "6") {
      mepid_pos <- which(meps$mepid == "4274")
    } else if (rcv_name == "KOPPA MARIA ELENI" && term == "6") {
      mepid_pos <- which(meps$mepid == "39319")
    } else if (rcv_name == "MARINE LE PEN" && term == "6") {
      mepid_pos <- which(meps$mepid == "28210")
    } else if (rcv_name == "CZARNECKI MAREK ALEKSANDER" && term == "6") {
      mepid_pos <- which(meps$mepid == "28371")
    } else if (rcv_name == "WOJCIECHOWSKI BERNARD PIOTR" && term == "6") {
      mepid_pos <- which(meps$mepid == "34720")
    } else if (rcv_name == "DE SOUSA CORREIA" && term == "6") {
      mepid_pos <- which(meps$mepid == "28309")
    } else if (rcv_name == "SCHMIDT OLLE" && term %in% c("5", "6", "7")) {
      mepid_pos <- which(meps$mepid == "4274")
    } else if (rcv_name == "JULIA REDA" && term == "8") {
      mepid_pos <- which(meps$mepid == "124816")
    } else if (rcv_name == "SCHMITT PÁL" && term %in% c("6", "7")) {
      mepid_pos <- which(meps$mepid == "28133")
    } else if (rcv_name == "KOFOD" && term == "9") {
      mepid_pos <- which(meps$mepid == "197570")
    } else if (rcv_name == "MARIA MANUEL LEITÃO MARQUES" && term == "9") {
      mepid_pos <- which(meps$mepid == "197635")
    } else if (rcv_name == "KHAN" && term == "8") {
      mepid_pos <- which(meps$mepid == "124962")
    } else if (rcv_name == "MAYER" && term == "8") {
      mepid_pos <- which(meps$mepid == "38511")
    } else if (rcv_name == "SION SIMON" && term == "8") {
      mepid_pos <- which(meps$mepid == "124948")
    } else if ((rcv_name == "MAURIZIO ENZO LUPI" || rcv_name == "POTHAIN")
               && term == "8") {
      mepid_pos <- 9e4
    } else if (rcv_name == "CORINA CREŢU" && term == "8") {
      mepid_pos <- which(meps$mepid == "33997")
    } else if (rcv_name == "PAȘCU" && term == "8") {
      mepid_pos <- which(meps$mepid == "33984")
    } else if (rcv_name == "FERREIRA" && term == "8" && all(
        c_dat$rcv_date < "2019-07-05")) {
      mepid_pos <- which(meps$mepid == "28308")
    } else if (rcv_name == "GILL" && term == "8") {
      mepid_pos <- which(meps$mepid == "4533")
    } else if (rcv_name == "DODDS" && term == "8") {
      mepid_pos <- which(meps$mepid == "96951")
    } else if (rcv_name == "LE PEN" && term  %in% c("7", "8")) {
      mepid_pos <- which(meps$mepid == "1023")
    } else if (rcv_name == "WINKLER" && term %in% c("7", "8")) {
      mepid_pos <- which(meps$mepid == "39725")
    } else if (rcv_name == "EVA PAUNOVA" && term == "8") {
      mepid_pos <- which(meps$mepid == "98341")
    } else if (rcv_name == "SIEGFRIED MUREȘAN" && term == "8") {
      mepid_pos <- which(meps$mepid == "124802")
    } else if (rcv_name == "ANDERSON" && term == "9") {
      mepid_pos <- which(meps$mepid == "197475")
    } else if (rcv_name == "PAPANDREOU" && term == "9") {
      mepid_pos <- which(meps$mepid == "244571")
    } else if (rcv_name == "ANDREAS GLÜCK" && term == "9") {
      mepid_pos <- which(meps$mepid == "197443")
    } else if (rcv_name == "GLÜCK" && term == "9") {
      mepid_pos <- which(meps$mepid == "197443")
    } else if (rcv_name == "AGNIESZKA KOZŁOWSKA-RAJEWICZ" && term == "8") {
      mepid_pos <- which(meps$mepid == "124889")
    } else if (rcv_name == "BODIL CEBALLOS" && term == "8") {
      mepid_pos <- which(meps$mepid == "124993")
    } else if (rcv_name == "CLAUDIA TAPARDEL" && term == "8") {
      mepid_pos <- which(meps$mepid == "124793")
    } else if (rcv_name == "JORDI SEBASTIÀ" && term == "8") {
      mepid_pos <- which(meps$mepid == "125053")
    } else if (rcv_name == "VICTOR BOȘTINARU" && term == "8") {
      mepid_pos <- which(meps$mepid == "39711")
    } else if (rcv_name == "MUIGG" && term == "9") {
      mepid_pos <- which(meps$mepid == "238674")
    } else if (rcv_name == "VAIDERE" && term == "9") {
      mepid_pos <- which(meps$mepid == "28617")
    } else if (rcv_name == "ACHOFF" && term == "9") {
      mepid_pos <- which(meps$mepid == "197435")
    } else if (rcv_name == "GLUECK" && term == "9") {
      mepid_pos <- which(meps$mepid == "197443")
    } else if (rcv_name == "MALDEIKIENĖ" && term == "9") {
      mepid_pos <- which(meps$mepid == "197835")
    } else if (rcv_name == "SOPHIA IN 'T VELD" && term == "9") {
      mepid_pos <- which(meps$mepid == "28266")
    }
  }
  return(mepid_pos)
}


# register cores for parallel processing ----------------------------------
#' Register cores for multicore computing
#'
#' \code{multicore()} registers cores for parallel processing.
#'
#' @param cores Number of cores to be used. An integer. Default is \code{1}.
#' @param type Whether to start or end parallel processing. A character string.
#'   The possible values are \code{open}, \code{close}.
#' @param cl The registered cluster. Default is \code{NULL}
#' @return No return value, called to register or un-register clusters for
#'   parallel processing.

#' @importFrom foreach %dopar%
#' @importFrom doRNG %dorng%

multicore <- function(cores = 1, type, cl = NULL) {

  # Start parallel processing
  if (type == "open") {
    # register clusters for windows
    if (Sys.info()["sysname"] == "Windows") {
      cl <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
    } else {
      cl <- parallel::makeForkCluster(cores)
      doParallel::registerDoParallel(cl)
    }
    return(cl)
  }

  # Stop parallel processing
  if (type == "close") {
    parallel::stopCluster(cl)
  }
}