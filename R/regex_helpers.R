# This script includes functions to help sort through copious amounts of text to find key information, like the BARD funding information, using regular expressions.

#' Helper function to detect string in a vector.
#' @description
#' This largely serves as a wraparound for str_detect, but designed for character vectors of length > 1.
#' @param vect character.
#' @param rgx character. Specifically a regular expression.
vect_detect <- function(
    vect,
    rgx
){
  # input validation
  stopifnot(inherits(vect, "character"))
  stopifnot(inherits(rgx, "character"))
  # string detect
  purrr::map_lgl(
    .x = vect,
    .f = ~stringr::str_detect(
      string = .x,
      pattern = stringr::regex(
        rgx,
        ignore_case = TRUE
      )
    )
  )
}

#' Find and score text for "BARD".
#'
#' @description
#' Using regular expressions, searches for phrases in the `dopepal::neg_funds` or `dopepal::pos_funds`, which is internal data created in data-raw/DATASET.R
#'
#' @param vect character vector. The text to be searched.
#' @param pos logical. Do we search for signs that the string contains references to US-Isr BARD? Or do we search for "negative" signs that the string refers to some other Bard?
#' @param brk character vector. A regex to break up text by. For example, there may be two entries in a single string like: "CSIR; India". We may or may not care. Most often, you would use semicolon with an optional whitespace.
#' @return a numeric vector with the same length of `vect`.
#' @export
score_BARD <- function(
    vect,
    pos = TRUE,
    brk = NULL
){
  stopifnot(is.vector(vect))
  stopifnot(inherits(pos, "logical"))

  if(pos){
    # assign name of package data.frame for regex search.
    # (This is internal data to the package. See data-raw/DATASET.R)

    regex_data <- dopepal::pos_funds
  } else {
    regex_data <- dopepal::neg_funds
  }

  # Go through each undivided string.
  out <- purrr::map_dbl(
    .x = vect,
    .f = function(chr_){
      # Note, chr_ at this stage is one string
      # (perhaps with breaks like a semicolon).

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Now go through each wrong BARD and assign points.
      pts <- purrr::map(
        .x = 1:nrow(regex_data),
        .f = function(i){
          # Retrieve one row of the negative funds tibble.
          nf <- regex_data[i,]

          # Break up text if required.
          if(!inherits(brk, "NULL")){
            chr_ <- stringr::str_split_1(chr_, pattern = brk)
          }

          # Determine if regex appears in string(s)
          lgl_ <- vect_detect(
            vect = chr_,
            rgx = nf$regex
          )

          # At this point, now we have a logical vector of the same length as chr_.
          # We can now assign points.
          if(TRUE %in% lgl_){
            # Take the amount of points this regex is worth,
            # Then multiply it by how many TRUE string detections we have.
            nf$points * sum(lgl_)
          } else {
            # return
            0
          }
        })

      # return (points)
      pts |>
        unlist() |>
        sum() |>
        # This helps with floating point precision
        round(digits = 2)
  })
  # return
  out
}
