# rcv_type variable -------------------------------------------------------
# adds the variable rcv_type to rcv_meta
add_rcv_type_var <- function(desc_text) {

  # add rcv_type
  rcv_type <- lapply(X = desc_text, FUN = function(y) {

    #########################
    # Misc votes
    #########################

    # budget votes
    if (stringr::str_detect(string = y, pattern = "Draft\\s+budget")) {
      type <- "budget"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Draft\\s+general\\s+budget", ignore_case = TRUE
      )
    )) {
      # Draft general budget
      type <- "budget"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Draft\\s+amending\\s+budget", ignore_case = TRUE
      )
    )) {
      # budget votes
      type <- "budget"
    } else if (stringr::str_detect(
      string = y, pattern = "Projet\\s+de\\s+budget"
    )) {
      # budget votes French
      type <- "budget"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "budget", ignore_case = TRUE
      )
    )) {
      # budget votes
      type <- "budget"
    } else if (stringr::str_detect(
      string = y, pattern = "[Ss]ecret\\s+vote"
    )) {
      # secret
      type <- "secret vote"
    } else if (stringr::str_detect(
      string = y, pattern = "[Ss]ecret\\s+vote"
    )) {
      # VOTE SECRET
      type <- "secret vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "vote\\s+secret", ignore_case = TRUE
      )
    )) {
      # secret vote French
      type <- "secret vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Ee]lection\\s+of\\s+the\\s+[Ee]uropean\\s+[Oo]mbudsman"
    )) {
      # European Ombudsman
      type <- "election of european ombudsman"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "agenda", ignore_case = TRUE
      )
    )) {
      # Agenda
      type <- "agenda"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Cc]alendar"
    )) {
      # calendar
      type <- "agenda"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Oo]rder\\s+of\\s+business"
    )) {
      # Order of business
      type <- "agenda"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Ordre\\s+du\\s+jour", ignore_case = TRUE
      )
    )) {
      # Agenda
      type <- "agenda"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[aA]ttendance\\s+check"
    )) {
      # Attendance check
      type <- "attendance check"
    } else if (stringr::str_detect(
      string = y,
      pattern = "inadmissibility\\s+motion"
    )) {
      # inadmissibility motion (Rule 167){
      type <- "inadmissibility motion"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[rR]ules\\s+of\\s+[Pp]rocedure"
    )) {
      # interpretation of the Rules of Procedure
      type <- "interpretation of rules of procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Rr]equest\\s+to\\s+hold\\s+over\\s+the\\s+vote"
    )) {
      # Request to hold over the vote
      type <- "request to hold over the vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Motion\\s+to\\s+postpone\\s+the\\s+vote",
        ignore_case = TRUE
      )
    )) {
      # Motion to postpone the vote
      type <- "request to postpone vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "[Pp]ostponement\\s+of\\s+the\\s+vote",
        ignore_case = TRUE
      )
    )) {
      # Postponement of the vote
      type <- "request to postpone vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "[Pp]ostponement\\s+of\\s+vote", ignore_case = TRUE
      )
    )) {
      # Postponement of vote
      type <- "request to postpone vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Rr]equest\\s+for\\s+consultation"
    )) {
      # Request for consultation
      type <- "request for consultation"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[eE]lection\\s+of\\s+the\\s+[Cc]ommisss?ion"
    )) {
      # Election of the Commission
      type <- "election of the commission"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = paste0("Appointment\\s+of\\s+\\w+\\s+Members",
          "\\s+of\\s+the\\s+European\\s+Commission"
        ),
        ignore_case = TRUE
      )
    )) {
      # Appointment of four Members of the European Commission
      type <- "election of the commission"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Décision\\s+de\\s+la\\s+commission",
        ignore_case = TRUE
      )
    )) {
      # Décision de la commission
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposition\\s+de\\s+la\\s+Commision",
        ignore_case = TRUE
      )
    )) {
      # Proposition de la Commision
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposition\\s+de\\s+la\\s+Commisson",
        ignore_case = TRUE
      )
    )) {
      # Proposition de la Commisson
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Commiss?s?i?on\\s+proposal", ignore_case = TRUE
      )
    )) {
      # Commission proposal
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = "proposition Commisss?ion"
    )) {
      # proposition Commission
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposition\\s+de\\s+la\\s+Commisss?ion",
        ignore_case = TRUE
      )
    )) {
      # Proposition de la Commisssion
      # Commission proposal
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposal\\s+for\\s+a\\s+regulation",
        ignore_case = TRUE
      )
    )) {
      # Proposal for a regulation
      type <- "commission proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Rr]equest\\s+from"
    ) & stringr::str_detect(
      string = y,
      pattern = "[Gg]roup"
    )) {
      # request by a group
      type <- "group request"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Pp]roposal\\s+by"
    ) & stringr::str_detect(
      string = y,
      pattern = "[Gg]roup"
    )) {
      # proposal by group
      type <- "group proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Council\\s+implementing\\s+decision", ignore_case = TRUE
      )
    )) {
      # Council implementing decision
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Decision", ignore_case = TRUE
      )
    )) {
      # Decision
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Décision\\s+\\(ensemble\\s+du\\s+texte\\)",
        ignore_case = TRUE
      )
    )) {
      # Decision
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "\\bdécision$",
        ignore_case = TRUE
      )
    )) {
      # décision
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Décision",
        ignore_case = TRUE
      )
    )) {
      # Décision
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "projet\\s+du\\s+Conseil",
        ignore_case = TRUE
      )
    )) {
      # projet du Conseil
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Projet\\s+de\\s+décision\\s+du\\s+Conseil",
        ignore_case = TRUE
      )
    )) {
      # Projet de décision du Conseil
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposition\\s+de\\s+décision\\s+du\\s+Conseil ",
        ignore_case = TRUE
      )
    )) {
      # Proposition de décision du Conseil
      type <- "council implementing decision"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "approval procedure", ignore_case = TRUE
      )
    )) {
      # approval procedure
      type <- "approval procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "approval", ignore_case = TRUE
      )
    )) {
      # approval procedure
      type <- "approval procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Request\\s+for\\s+urgent\\s+procedure",
        ignore_case = TRUE
      )
    )) {
      # Request for urgent procedure
      type <- "request for urgent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Request\\s+for\\s+urgency\\s+procedure",
        ignore_case = TRUE
      )
    )) {
      # Request for urgency procedure
      type <- "request for urgent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "urgent\\s+procedure", ignore_case = TRUE
      )
    )) {
      # Request to apply the urgent procedure
      type <- "request for urgent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = paste0("Demande\\s+d'application\\s+de\\s+la",
          "\\s+procédure\\s+d'urgence"
        ),
        ignore_case = TRUE
      )
    )) {
      # Demande d'application de la procédure d'urgence
      type <- "request for urgent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = paste0("mesures\\s+immédiates\\s+dans\\s+le",
          "\\s+contexte\\s+de\\s+la\\s+pandémie"
        ),
        ignore_case = TRUE
      )
    )) {
      # mesures immédiates dans le contexte de la pandémie
      type <- "request for urgent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "procédure\\s+d’urgence",
        ignore_case = TRUE
      )
    )) {
      # procédure d’urgence
      type <- "request for urgent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Visa($|\\s)", ignore_case = TRUE
      )
    )) {
      # Visa
      type <- "visa"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "consent", ignore_case = TRUE
      )
    )) {
      # consent procedure
      type <- "consent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "assent", ignore_case = TRUE
      )
    )) {
      # Assent
      type <- "assent procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Approbation", ignore_case = TRUE
      )
    )) {
      # Approbation
      type <- "approbation"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Request\\s+for\\s+vote", ignore_case = TRUE
      )
    )) {
      # Request for vote
      type <- "request for vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Agreement on the withdrawal", ignore_case = TRUE
      )
    )) {
      # Agreement on the withdrawal
      type <- "withdrawal agreement"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Accord\\s+sur\\s+le\\s+retrait\\s+du\\s+Royaume-Uni",
        ignore_case = TRUE
      )
    )) {
      # Accord sur le retrait du Royaume-Uni
      type <- "withdrawal agreement"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "[Aa]mended\\s+proprosal", ignore_case = TRUE
      )
    )) {
      # Amended proprosal
      type <- "amended proprosal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Citation", ignore_case = TRUE
      )
    )) {
      # Citation
      type <- "citation"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Referral\\s+\\(Rule\\s+59\\(4", ignore_case = TRUE
      )
    )) {
      # Referral (Rule 59(4))
      type <- "refer back to committee"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "\\(Rule\\s+59\\(4", ignore_case = TRUE
      )
    )) {
      # Referral (Rule 59(4))
      type <- "refer back to committee"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "[Rr]eferrr?al\\s+back\\s+to\\s+committee",
        ignore_case = TRUE
      )
    )) {
      # Referral (Rule 59(4))
      type <- "refer back to committee"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Referral\\s+to\\s+committee", ignore_case = TRUE
      )
    )) {
      # Referral to committee
      type <- "referral to committee"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Council\\s+draft", ignore_case = TRUE
      )
    )) {
      # Council draft
      type <- "council draft"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Draft\\s+Council\\s+directive", ignore_case = TRUE
      )
    )) {
      # Council draft
      type <- "council draft"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = paste0("Motion\\s+calling\\s+for\\s+a\\s+matter",
          "\\s+to\\s+be\\s+declared\\s+inadmissible"
        ),
        ignore_case = TRUE
      )
    )) {
      # Motion calling for a matter to be declared inadmissible
      type <- "council draft"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "recomm[ae]ndation", ignore_case = TRUE
      )
    )) {
      # Recommendation
      type <- "recommendation"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Joint\\s+proposal", ignore_case = TRUE
      )
    )) {
      # Joint proposal
      type <- "joint proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposal\\s+for\\s+rejection", ignore_case = TRUE
      )
    )) {
      # Proposal for rejection
      type <- "proposal for rejection"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "block?\\s+[A-Z]", ignore_case = TRUE
      )
    )) {
      # Block vote
      type <- "block vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "block?\\s+\\d+", ignore_case = TRUE
      )
    )) {
      # Block vote
      type <- "block vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "vote\\s+en\\s+bloc", ignore_case = TRUE
      )
    )) {
      # vote en bloc
      type <- "block vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "COLLECTIVE\\s+VOTE", ignore_case = TRUE
      )
    )) {
      # COLLECTIVE VOTE
      type <- "block vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "block$", ignore_case = TRUE
      )
    )) {
      # Block vote
      type <- "block vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "annex", ignore_case = TRUE
      )
    )) {
      # Annex
      type <- "annex"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "subheading", ignore_case = TRUE
      )
    )) {
      # subheading
      type <- "subheading"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "request\\s+for\\s+the\\s+vote\\s+to\\s+be\\s+adjourned",
        ignore_case = TRUE
      )
    )) {
      # request for the vote to be adjourned
      type <- "request for vote to be adjourned"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = paste0("Request\\s+for\\s+amendments\\s+to",
          "\\s+be\\s+put\\s+to\\s+the\\s+vote"
        ),
        ignore_case = TRUE
      )
    )) {
      # Request for amendments to be put to the vote
      type <- "Request for amendments to be put to vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "rule 59\\(3",
        ignore_case = TRUE
      )
    )) {
      # request for 59(3)
      type <- "rule 59(3)"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Application\\s+of\\s+Rule",
        ignore_case = TRUE
      )
    )) {
      # Application of Rule
      type <- "procedural vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Procedural\\s+vote",
        ignore_case = TRUE
      )
    )) {
      # Procedural vote
      type <- "procedural vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Vote\\s+on\\s+procedure",
        ignore_case = TRUE
      )
    )) {
      # Vote on procedure
      type <- "vote on procedure"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = paste0("Décision\\s+d’engager\\s+des\\",
          "s+négociations\\s+interinstitutionnelles"
        ),
        ignore_case = TRUE
      )
    )) {
      # Décision d’engager des négociations interinstitutionnelles
      type <- "on entering inter-institutional negotiations"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Amended\\s+proposal",
        ignore_case = TRUE
      )
    )) {
      #########################
      # Amended proposal
      #########################
      type <- "amended proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Proposal\\s+as\\s+amended",
        ignore_case = TRUE
      )
    )) {
      # Proposal as amended
      type <- "amended proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "[Pp]roposition\\s+modifiée",
        ignore_case = TRUE
      )
    )) {
      # Proposition modifiée
      type <- "amended proposal"
    } else if (stringr::str_detect(
      string = y,
      pattern = "Article"
    ) & stringr::str_detect(
      string = y,
      pattern = "point"
    )) {
      #########################
      # Article and point
      #########################
      type <- "article"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "\\bPoint\\b", ignore_case = TRUE
      )
    )) {
      #########################
      # Point
      #########################
      type <- "point"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Article", ignore_case = TRUE
      )
    )) {
      #########################
      # Article
      #########################
      type <- "article"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Aa][Mm]\\.\\s+\\d+"
    )) {
      #########################
      # Amendments
      #########################
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Aa][Mm]\\.\\s?\\d+"
    )) {
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(pattern = "Am\\s?\\d+", ignore_case = TRUE)
    )) {
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(pattern = "Am\\s+\\d+", ignore_case = TRUE)
    )) {
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(pattern = "Ams\\s+\\d+", ignore_case = TRUE)
    )) {
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "amendments?\\s+\\d", ignore_case = TRUE
      )
    )) {
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "\\d+/rev", ignore_case = TRUE
      )
    )) {
      # "235/rev" # nolint
      type <- "amendment"
    } else if (stringr::str_detect(
      string = y,
      pattern = "par\\.\\s+\\d+"
    )) {
      #########################
      # Paragraphs
      #########################
      type <- "paragraph"
    } else if (stringr::str_detect(
      string = y,
      pattern = "par\\.\\s?\\d+"
    )) {
      type <- "paragraph"
    } else if (stringr::str_detect(
      string = y,
      pattern = "§"
    )) {
      type <- "paragraph"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Pp][Aa][Rr][Aa][Gg][Rr][Aa][Pp][Hh]"
    )) {
      type <- "paragraph"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "\\bPAR\\b\\s+\\d+", ignore_case = TRUE
      )
    )) {
      type <- "paragraph"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "\\bPARA\\b\\s+\\d+", ignore_case = TRUE
      )
    )) {
      type <- "paragraph"
    } else if (stringr::str_detect(
      string = y,
      pattern = "[Rr][Ee][Cc][Ii][Tt][Aa][Ll]"
    )) {
      #########################
      # Recital
      #########################
      type <- "recital"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "CONSIDÉRANT\\s+\\w+", ignore_case = TRUE
      )
    )) {
      type <- "recital"
    } else if (stringr::str_detect(
      string = y,
      pattern = "CONS\\s+\\w+"
    )) {
      type <- "recital"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "single\\s+vote", ignore_case = TRUE
      )
    )) {
      #########################
      # Single vote
      #########################
      type <- "single vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Vote\\s+unique", ignore_case = TRUE
      )
    )) {
      type <- "single vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Vote\\s+unqiue", ignore_case = TRUE
      )
    )) {
      type <- "single vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(pattern = "res?olution", ignore_case = TRUE)
    )) {
      #########################
      # Final vote
      #########################
      # resolution
      type <- "final vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(pattern = "résolution", ignore_case = TRUE)
    )) {
      # résolution
      type <- "final vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "final\\s+vote", ignore_case = TRUE
      )
    )) {
      # final vote
      type <- "final vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "text\\s+as\\s+a\\s+whole", ignore_case = TRUE
      )
    )) {
      # text as a whole
      type <- "final vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "Vote\\s+final", ignore_case = TRUE
      )
    )) {
      # Vote final
      type <- "final vote"
    } else if (stringr::str_detect(
      string = y,
      pattern = stringr::regex(
        pattern = "joint\\s+text", ignore_case = TRUE
      )
    )) {
      #########################
      # joint text
      #########################
      # text from conciliation - this should also be a final vote
      # text as a whole
      type <- "joint text"
    } else {
      #########################
      # missing
      #########################
      type <- NA_character_
    }
    return(type)
  })

  # unlist rcv_type
  rcv_type <- unlist(rcv_type)

  return(rcv_type)
}