myenv <- new.env()
myenv$test_lat_cir <- function(df){
  #Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  df <- enc2utf8(df)
  for(j in 1:nrow(df_cir_lat)){
    df <- stringr::str_replace_all(df, df_cir_lat$Lat[j], df_cir_lat$Cir[j])
  }
  df
}
# Function to replace Serbian Cyrillic characters to Serbian Latin characters
myenv$test_cir_lat <- function(df){
  #Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")
  df <- enc2utf8(df)
  for(j in 1:nrow(df_cir_lat)){
    df <- stringr::str_replace_all(df, df_cir_lat$Cir[j], df_cir_lat$Lat[j])
  }
  df
}

#' Serbian Latin to Serbian Ciryllic transliterator
#'
#' Converting Serbian Latin characters to Serbian Cyrillic characters.
#' The function, changes locale settings of R session to Serbian Cyrillic
#' for easier manipulation with Serbian Cyrillic character sets.
#' When this function converts data frame or tibble function does not
#' change the characters of column names. In the future it is planed to
#' add this option.
#'
#' @param df can be a data frame, tibble or character string
#' @return data frame, tibble or character string converted to Serbian Cyrillic characters.
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' lcl(loc = "L")
#' lat_cyr(c("Č", "ž", "đ", "lj", "ć", "dž"))
#'
#' lcl(loc = "L")
#' df_text <- data.frame(num = 1:6, text = c("Č", "ž", "đ", "lj", "ć", "dž"),
#' stringsAsFactors = FALSE)
#' lat_cyr(df_text)
#' }
#' @rdname lat_cyr
#' @export lat_cyr
lat_cyr <- function(df) {
  Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  df_cir_lat <- data.frame(df_cir_lat)
  if(typeof(df) == "double" | typeof(df) == "logical"){
    stop("Argument not coerciable to character vector! Please set argument to character or data frame!")
  }
  if(typeof(df) == "character" & is.null(ncol(df))){
    df <- myenv$test_lat_cir(df)
  } else {
    num_col <- ncol(df)
    for (i in 1:ncol(df)){
      if(typeof(df[[i]]) == "character"){
        df[[i]] <- myenv$test_lat_cir(df[[i]])
        #df[[i]] <- sapply(df[[i]], test_lat_cir)
        }
      }
    }
  Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")
  warning("Locale settings changed!")
  print(Sys.getlocale())
  df
}

#' Serbian Cyrillic to Serbian Latin transliterator
#'
#' Converting Serbian Cyrillic characters to Serbian Latin characters.
#' The function, changes locale settings of R session to Serbian Latin
#' for easier manipulation with Serbian Latin character sets.
#' When this function converts data frame or tibble function does not
#' change the characters of column names. In the future it is planed to
#' add this option.
#'
#' @param df can be a data frame, tibble or character string
#' @return data frame, tibble or character string converted to Serbian Latin characters
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' lcl(loc = "C")
#' cyr_lat(c("Ч", "љ", "ђ", "ж", "ћ", "њ"))
#'
#' lcl(loc = "C")
#' df_text <- data.frame(num = 1:6, text = c("Ч", "љ", "ђ", "ж", "ћ", "њ"),
#' stringsAsFactors = FALSE)
#' cyr_lat(df_text)
#' }
#' @rdname cyr_lat
#' @export cyr_lat

cyr_lat <- function(df) {
  Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")
  df_cir_lat <- data.frame(df_cir_lat)
  if(typeof(df) == "double" | typeof(df) == "logical"){
    stop("Argument not coerciable to character vector! Please set argument to character or data frame!")
  }

  if(typeof(df) == "character" & is.null(ncol(df))){
    df <- myenv$test_cir_lat(df)
  } else {
    num_col <- ncol(df)
    for (i in 1:ncol(df)){
      if(typeof(df[[i]]) == "character"){
        df[[i]] <- myenv$test_cir_lat(df[[i]])
        #df[[i]] <- sapply(df[[i]], test_cir_lat)
      }
    }
  }
  Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  warning("Locale settings changed!")
  print(Sys.getlocale())
  df
}
