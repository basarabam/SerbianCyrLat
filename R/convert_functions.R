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

test_lat_cir <- function(df){
  Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  df <- enc2utf8(df)
  for(j in 1:nrow(df_cir_lat)){
    df <- str_replace_all(df, df_cir_lat$Lat[j], df_cir_lat$Cir[j])
  }
  df
}

#' Function to convert input object (character or df with character columns)
#'
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
    df
  }



