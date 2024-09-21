# Repeatedly used functions

#' @title Add sheets to Excel Workbook
#'
#' @param wb is an openxlsx workbook.
#' @param sheetnm is the name of the sheet you want to write.
#' @param dat is the tibble or data.frame
#' @return an openxlsx workbook
#' @export
custom_xl <- function(wb, sheetnm, dat){
  openxlsx::addWorksheet(wb,
                         sheetName = sheetnm)

  openxlsx::freezePane(wb,
                       sheet = sheetnm,
                       firstRow = TRUE,
                       firstCol = FALSE)

  openxlsx::writeDataTable(wb,
                           sheet = sheetnm,
                           x = dat,
                           colNames = TRUE,
                           rowNames = FALSE)

  # return
  wb
}

#' @title Split single string
#' @param string is a character vector, length 1.
#' @return character vector of variable length.
#' @export
split_str <- function(string){
  stopifnot(inherits(string, "character"))

  if(is.na(string)){
    return(as.character(NA))
  }

  split_ <- stringr::str_split_1(string, "\\;") %>%
    stringr::str_squish()

  split_ <- split_[split_ != ""]

  # return
  split_
}
