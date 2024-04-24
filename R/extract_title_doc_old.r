# Extract title from doc.old ----------------------------------------------
#' Extracts titles from EP 7 and older documents
extract_title_doc_old <- function(page) {

  # titles organised via headings (the style is called Heading1)
  titles <- page %>%
    xml2::xml_find_all(xpath = "//w:pStyle[@w:val='Heading1']") %>%
    xml2::xml_parent() %>%
    xml2::xml_parent() %>%
    rvest::html_text(trim = TRUE) %>%
    .[. != ""]

  # titles organised via headings (the style is called StyleHeading1Bold)
  if (length(titles) == 0) {
    titles <- page %>%
      xml2::xml_find_all(xpath = "//w:pStyle[@w:val='StyleHeading1Bold']") %>%
      xml2::xml_parent() %>%
      xml2::xml_parent() %>%
      rvest::html_text(trim = TRUE)
  }

  # titles organised via headings (the style is called StyleHeading112pt)
  if (length(titles) == 0) {
    titles <- page %>%
      xml2::xml_find_all(xpath = "//w:pStyle[@w:val='StyleHeading112pt']") %>%
      xml2::xml_parent() %>%
      xml2::xml_parent() %>%
      rvest::html_text(trim = TRUE)
  }

  # titles organised via headings (the style is called VOTETITLE)
  if (length(titles) == 0) {
    titles <- page %>%
      xml2::xml_find_all(xpath = "//w:pStyle[@w:val='VOTETITLE']") %>%
      xml2::xml_parent() %>%
      xml2::xml_parent() %>%
      rvest::html_text(trim = TRUE)
  }

  # Try finding titles via numbered lists
  if (length(titles) == 0) {
    # titles
    titles <- page %>%
      xml2::xml_find_all(xpath = "//w:numPr") %>%

      xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
      rvest::html_text(trim = TRUE)

    # if titles is length 0, check whether the titles are in
    # .//w:numId[@w:val='1']
    if (length(titles) == 0) {
      titles <- page %>%
        xml2::xml_find_all(xpath = "//w:numPr") %>%
        xml2::xml_find_all(xpath = ".//w:numId[@w:val='1']") %>%
        xml2::xml_parent() %>%
        xml2::xml_parent() %>%
        xml2::xml_parent() %>%
        xml2::xml_parent() %>%
        xml2::xml_parent() %>%
        rvest::html_text(trim = TRUE)
    } else {

      # retry with parent node if titles are empty
      if (all(titles == "")) {

        # titles
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:numPr") %>%
          xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
          xml2::xml_parent() %>%
          rvest::html_text(trim = TRUE)
      }

      # retry with parent node if titles are empty
      if (all(titles == "")) {

        # titles
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:numPr") %>%
          xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          rvest::html_text(trim = TRUE)
      }

      # retry with parent node if titles are empty
      if (all(titles == "")) {

        # titles
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:numPr") %>%
          xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          rvest::html_text(trim = TRUE)
      }

      # retry with parent node if titles are empty
      if (all(titles == "")) {

        # titles
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:numPr") %>%
          xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%

          rvest::html_text(trim = TRUE)
      }

      # retry with parent node if titles are empty
      if (all(titles == "")) {

        # titles
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:numPr") %>%
          xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          rvest::html_text(trim = TRUE)
      }

      # retry with parent node if titles are empty
      if (all(titles == "")) {

        # titles
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:numPr") %>%
          xml2::xml_find_all(xpath = ".//w:numId[@w:val='2']") %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          xml2::xml_parent() %>%
          rvest::html_text(trim = TRUE)
      }
    }
  }

  # look up from the vote description if title is still not matched
  if (length(titles) == 0) {

    # match the voting description
    vote_desc <- page %>%
      xml2::xml_find_all(xpath = "//w:pStyle[@w:val='VOTEREPORTTITLE']") %>%
      xml2::xml_parent() %>%
      xml2::xml_parent() %>%
      rvest::html_text(trim = TRUE)

    # find the position of the voting description
    d_pos <- page %>%
      xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
      rvest::html_text(trim = TRUE)

    # find exact matches for the titles
    d_pos <- lapply(X = vote_desc, FUN = function(y) {

      # find matches
      d_match <- which(grepl(pattern = y, x = d_pos, fixed = TRUE))

      # evaluate string similarity in case of multiple matches
      if (length(d_match) > 1) {
        similarties <- sapply(d_match, function(z) {
          stringdist::stringsim(
            a = y,
            b = d_pos[z]
          )
        })

        # discard matches that are not exact
        d_match <- d_match[similarties == 1]

        # in case there is only 1 exact match, choose it
        if (length(similarties == 1) == 1) {
          d_match <- d_match[similarties == 1]
        }
      }
      return(d_match)
    })

    # loop over title positions
    titles <- lapply(d_pos, function(x) {

      # loop over the 10 nodes before the title
      for (idx in 1:10) {

        # attempt to extract the description
        titles <- page %>%
          xml2::xml_find_all(xpath = "//w:p|//w:tbl") %>%
          .[x - idx] %>%
          rvest::html_text(trim = TRUE)

        # break from the loop if some string was extracted
        if (titles != "" & titles != vote_desc) {
          break
        }
      }
      return(titles)
    })
  }


  # try to get the title from the size of the heading
  if (length(titles) == 0) {

    titles <- page %>%
      xml2::xml_find_all(xpath = "//w:sz[@w:val='22']") %>%
      xml2::xml_parent() %>%
      xml2::xml_parent() %>%
      xml2::xml_text() %>%
      .[. != ""] %>%
      stringr::str_trim(side = "both")
  }

  # try to get the title form the title numbering
  if (length(titles) == 0) {
    title_nodes <- page %>%
      xml2::xml_find_all(xpath = "//w:p") %>%
      xml2::xml_find_all(xpath = paste0(
        "//w:t[starts-with(translate(., '0123456789', '1111111111'),",
        " '1') and substring(., string-length(.), 1) = '.']"
      ))

    # extract title text
    titles <- title_nodes %>%
      xml2::xml_find_all(xpath = ".//../..") %>%
      xml2::xml_text()

    # remove text that starts with a number and ends with a dot
    titles <- titles[!grepl(pattern = "^[0-9].*\\.$", x = titles)]

  }

  return(titles)
}