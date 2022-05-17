#' convert_time
#'
#' @param time 
#'
#' @description A utils function to check if the give time is in 12 or 24h
#'     format and in case convert it to 24h
#'
#' @return Return the given time converted in 24 hour format
#'
#' @noRd

convert_time <- function(time){
  toConvert <- as.character(time)
  if (grepl(pattern = 'am|pm', toConvert, ignore.case = TRUE) == TRUE){
    #convert to 24 hour format
    if (grepl(pattern = 'am', toConvert, ignore.case = TRUE) == TRUE){
      if (nchar(toConvert) < 5){
        toConvert <- as.numeric(stringr::str_sub(toConvert, end = -3))
        cleanHour <- paste((toConvert), ":00", sep = "")
      }else{
        cleanHour <- as.numeric(stringr::str_sub(stringr::str_trim(toConvert, side = "right") , end = -3))
      }
    } else if (grepl(pattern = 'pm', toConvert, ignore.case = TRUE) == TRUE){
      if (nchar(toConvert) < 5){
        toConvert <- as.numeric(stringr::str_sub(toConvert, end = -3))
        cleanHour <- paste((toConvert + 12), ":00", sep = "")
      }else{
        cleanHour <- as.numeric(stringr::str_sub(stringr::str_trim(toConvert, side = "right") , end = -3))
      }
    }
  }else{
    cleanHour <- toConvert
    #check format to be %H%M and assign to attribute
  }
  cleanHour
}