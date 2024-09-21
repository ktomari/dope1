# These functions are aimed at helping scrape and clean program information from the bard-isus website. At the time of this writing, someone else performed the inital scrape of data, so these functions are missing.

#' @title Replace partial string matches of institutions in bard-isus scrape.
#'
#' @description
#' This function helps clean the bard-isus list of approved proposals webscrape, where author information is mixed in with institutional affiliation. The core problem is replacing mismatches at scale (~1750 approved proposals). Furthermore, authors-affiliation information is not consistently presented; there are many cases where author information is partial, or the order in which institutions appear with respect to author is incomplete.
#' @param vect character. Each element of this vector may include multiple author-affiliation information, eg. "J. Jones UC Davis; S. Smith U Hebrew;".
#' @param insts_ tibble. The table of data created from DATASET.R as "inst_strings". This table has a non-regex string match for most if not all institutions that might appear in `vect`.
#' @return character vector of same length as `vect`.
#' @export
replace_inst <- function(
    vect,
    insts_
    ){
  # Input validation
  stopifnot(inherits(vect, "character"))

  # Sort institution table by longest string match.
  insts_ <- insts_ %>%
    dplyr::arrange(
      dplyr::desc(
        stringr::str_length(`string_match`)))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # primary algo
  out <- purrr::map_chr(vect, function(x){
    if(inherits(x, "NULL")){
      return(NULL)
    }

    if(is.na(x)){
      return(as.character(NA))
    }

    # split groups by semicolon
    splt_ <- stringr::str_split_1(x, "\\;") %>%
      stringr::str_squish()

    # remove empty strings
    splt_ <- splt_[splt_ != ""]

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Search each author-inst pair for institutional string_match
    splt_ <- purrr::map_chr(splt_, function(y){
      # At this stage, `y` should represent One Author and One Institution.
      # First remove excess ws
      y <- stringr::str_squish(y)

      # Move iteratively through all string matches
      # to see if `y` has a match in `string_match`
      for(i in 1:nrow(insts_)){
        # See if string_match is in there.
        match_found_ <- stringr::str_detect(
          string = y,
          pattern = insts_$`string_match`[i]
          )

        # If string_match is in there,
        # replace it.
        if(match_found_){
          y <- stringr::str_replace(
            string = y,
            pattern = insts_$`string_match`[i],
            replacement = paste0(
              "[",
              insts_$conversion[i],
              "]"
            )
          )
          break
        }
      }  # END for loop

      # return
      y
    })

    # return
    paste0(splt_, collapse = "; ")
  }, .progress = "replace_inst")

  # return
  out
}

#' @title BARD-ISUS Author-Institution cleaner.
#'
#' @description Scraping the approved proposal/project information from BARD-ISUS results in a very dirty dataset with respect to the "author" and "institution" information. The typical format appears as: "J. Jones U Alabama", but this is hardly a standard easily remedied. This algorithm first takes a table of strings of institutions to match and replace. Then it assumes what is left over is the author name.
#' @param dat is the bard-isus data.frame
#' @return data.frame
#' @export
clean_author_insts <- function(
    dat
){
  # input validation
  stopifnot(inherits(dat, "data.frame"))

  stopifnot("Authors" %in% names(dat))

  # Add ID column to help keep track of things
  # Give it an ID
  dat <- dat %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::select(id, dplyr::everything())

  # Get table of string-matches
  inst_strings <- get("inst_strings",
                      envir = asNamespace("dope1"))

  # Run replace_inst ----
  # Identify all institutions.
  # This step takes several seconds.
  # This works as of 2024-09-20.
  dat <- dat %>%
    dplyr::mutate(
      Authors2 = dope1::replace_inst(`Authors`,
                                     inst_strings))

  # Create new author and institution columns
  dat <- dat %>%
    # Create institutions_ ----
    dplyr::mutate(institutions_ = purrr::map_chr(Authors2, function(x){

      if(is.na(x)){
        return(as.character(NA))
      }

      split_ <- stringr::str_split_1(x, "\\;") %>%
        stringr::str_squish()

      split_ <- split_[split_ != ""]

      split_ <- purrr::map_chr(split_, function(y){
        stringr::str_extract(y, "\\[[^\\]]+\\]") %>%
          stringr::str_remove_all("\\[|\\]")
      })

      paste0(split_, collapse = "; ")
    })) %>%
    # Create authors_ ----
    dplyr::mutate(authors_ = purrr::map_chr(Authors2, function(x){
      if(is.na(x)){
        return(as.character(NA))
      }

      split_ <- stringr::str_split_1(x, "\\;") %>%
        stringr::str_squish()

      split_ <- split_[split_ != ""]

      split_ <- purrr::map_chr(split_, function(y){
        stringr::str_remove(y, "\\[[^\\]]+\\]") %>%
          stringr::str_squish()
      })

      paste0(split_, collapse = "; ")
    }))

  # 4 Categories ----
  # 1) NA
  # 2) normal
  # 3) first initial missing
  # 4) last-name appears first

  dat$authors_ <- purrr::map_chr(dat$authors_, function(auths){
    if(is.na(auths)){
      return(as.character(NA))
    }

    # split "A. Sagi; R. Dunham" into "A. Sagi"   "R. Dunham"
    split_ <- dope1::split_str(auths)

    # now examine each string to see if manipulations are required.
    out <- purrr::map_chr(split_, function(au){
      # First check to see if it has a standard format.
      standard_ <- stringr::str_detect(au, "^[[:alpha:]]\\.")

      # It has a standard start? return() early.
      if(standard_){
        return(au)
      }

      # Does it begin with a period? Remove it.
      dot_ <- stringr::str_detect(au, "^\\.")

      if(dot_){
        au <- stringr::str_remove(au, "^\\.") %>%
          stringr::str_squish()
      }

      # Does it begin with a long name (ie. not initials)?
      last_first <- stringr::str_detect(au, "^[^,.]{2,}\\,")

      if(last_first){
        last_ <- stringr::str_extract(au, "^[^,]+")
        first_ <- stringr::str_remove(au, "^[^,]+\\,") %>%
          stringr::str_squish()

        if(stringr::str_detect(first_, "\\.$", negate = T)){
          first_ <- paste0(first_, ".")
        }

        au <- paste0(first_, " ", last_) %>%
          stringr::str_squish()
      }

      # return
      au
    })

    paste0(out, collapse = "; ")
  })

  dat <- dat %>%
    dplyr::arrange(id) %>%
    dplyr::select(-Authors2, -id)

  # return
  dat
}
