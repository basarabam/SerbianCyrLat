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
