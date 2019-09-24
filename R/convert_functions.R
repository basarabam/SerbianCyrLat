# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Function for replacing latin to cirilic characters from string
#' Function for replacing latin to cirilic characters from string
Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
test_lat_cir <- function(df){
  Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  df <- enc2utf8(df)
  for(j in 1:nrow(df_cir_lat)){
    df <- str_replace_all(df, df_cir_lat$Lat[j], df_cir_lat$Cir[j])
  }
  df
}

#' Converts Serbian Latin strings to Serbian Cyrillic strings
#'
#' @param df can be a data frame, tibble or character string
#' @return data frame, tibble or character string converted to Serbian Cyrillic characters
#' @examples
#' text <- "Čačkanje Ljuljaškice Ćuteći Žestoko Đuradj"
#' lat_cir(text)
#'
#' df_text <- data.frame(num = 1:5, text = c("Čačkanje", "Ljuljaškice", "Ćuteći", "Žestoko", "Đuradj"),
#' stringsAsFactors = FALSE)
#' lat_cir(df_text)
lat_cir <- function(df) {
  Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  df_cir_lat <- data.frame(df_cir_lat)
  if(typeof(df) == "double" | typeof(df) == "logical"){
    stop("Argument not coerciable to character vector! Please set argument to character or data frame!")
  }

  if(typeof(df) == "character" & is.null(ncol(df))){
    df <- test_lat_cir(df)
  } else {
    num_col <- ncol(df)
    for (i in 1:ncol(df)){
      if(typeof(df[[i]]) == "character"){
        df[[i]] <- test_lat_cir(df[[i]])
        #df[[i]] <- sapply(df[[i]], test_lat_cir)
        }
      }
    }
  Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")
  df
  }



