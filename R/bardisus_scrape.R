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
        stringr::str_length(string_match)))

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
        match_found_ <- str_detect(
          string = y,
          pattern = insts_$string_match[i]
          )

        # If string_match is in there,
        # replace it.
        if(match_found_){
          y <- stringr::str_replace(
            string = y,
            pattern = insts_$string_match[i],
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
