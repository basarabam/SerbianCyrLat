#' Set Aspects of the Locale for Transliteration

locale <- function(loc){
  if(loc == "L"){
    Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  } else if(loc == "C"){
    Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")
  } else if(loc == "D"){
    Sys.setlocale("LC_ALL", "")
  } else {
    stop()
  }
}
