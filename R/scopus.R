
#' List to tibbles.
#'
#' Convert nested list items into a single list with multiple tables. This is useful when we run `rscopus::abstract_retrieval()` and we want to flatten the `content` in the returned list.
#' @param l a list containing only nested character vectors.
#' @return a list composed of tibbles.
#' @importFrom magrittr %>%
list_elements_to_tibble <- function(l){
  # Begin by using tibble::enframe to
  # A. Convert list into tibble with lists in cells.
  # B. See if any items can be converted to other R object types, eg. character vectors.
  # Yields a tibble with two columns: name, value.
  tb <- tibble::enframe(l)
  # Determine the class for each cell in `value`
  # This is used to determine what to do with each row.
  cls <- purrr::map_vec(tb$value, class)

  # define an empty output list
  out <- list()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Logic: are `list` objects present?
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if("list" %in% cls){
    # List: Yes ----

    # Determine row numbers with and without lists.
    # (To clarify, the column `value` is a list forced into the tibble `tb`, but its elements can be any kind of R object. This is what we're testing.)
    is_list <- which(cls == "list")
    non_list <- which(cls != "list")

    # ~~~~~~~~~~~~~~~~~~~~~~~~
    # Items that are not lists
    if(length(non_list) > 0){
      # Non-List Elements ----
      # Subset non-lists
      tb2 <- tb[non_list,]
      # Determine class type
      # (They should all be characters.)
      cls2 <- purrr::map_vec(tb2$value, class)

      # Some items may be null;
      # remove these.
      if("NULL" %in% cls2){
        tb2 <- tb2[cls2 != "NULL",]
        cls2 <- cls2[cls2 != "NULL"]
      }

      if(length(cls2) > 0){
        # If they're all characters,
        # convert `value` from a list to a normal character column in `tb`.
        if(all(cls2 == "character")){
          tb2 <- tb2 %>%
            dplyr::mutate(value = unlist(value))
        } else {
          warning("W1. Not all non-list elements are character vectors.")
        }

        # Save to output list.
        out <- append(out,
                      list(
                        x = tb2
                      )
        )
      }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~
    # Items that are lists
    if(length(is_list) > 0){
      # List Elements ----
      # Recursively apply this current function.
      l2 <- purrr::map(.x = tb$value[is_list],
                       .f = list_elements_to_tibble)

      # Apply names back to list.
      names(l2) <- tb$name[is_list]

      # Save list to output list.
      out <- append(out,
                    l2
      )
    }
  } else {
    # List: No ----
    # This could mean 1 of two things, either we have
    # a normal tibble,
    # or we have a tibble with value as a list column,
    # and all items in this list are characters.
    if("value" %in% names(tb)){
      if(inherits(tb$value, "list")){
        # Convert `value` from list to normal vector column
        # (This should be character.)
        tb <- tb %>%
          dplyr::mutate(value = unlist(value))
      }
    }

    # Save tibble to output
    out <- tb
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Now 'out' is a either a tibble or a list
  # If its a list, lets pull out sub-elements
  if(inherits(out, "list")){
    # Are there are elements that are lists?
    # Effectively a structure like this:
    # out <- list(sub_list = list(...))
    cls3 <- purrr::map_vec(out,
                           function(x) "list" %in% class(x))

    # If so, we want to `unlist` the list.
    # In the example above, we want to remove the outer list
    # to be left with the list element, `sub_list`.
    if(TRUE %in% cls3){

      # Logic: There are two cases to be considered here:
      # A list with two kinds of sub elements: list and tibbles
      # Or
      # Simply a list of lists.
      if(FALSE %in% cls3){
        # Lists + Tibbles (probably).
        o1 <- unlist(out[cls3], recursive = F)
        out <- append(o1,
                      out[!cls3]
        )
      } else {
        # Lists only.
        out <- unlist(out, recursive = F)
      }
    }
  }

  # return
  out
}
