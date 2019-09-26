#' Change Aspects of the Locale for the Transliteration
#'
#' @param loc string, can take one of three values:
#'
#' "L" - Change aspect of the locale to Serbian Latin.
#'
#' "C" - Change aspect of the locale to Serbian Cyrillic.
#'
#' "D" - Change aspect of the locale to system default.
#'
#' It is important to change the aspect of the locale to character
#' before transliteration of the characters.
#' So if trasliteration is done from the Serbian Latin to Serbian
#' Cyrillic it is important to set locale to Serbian Latin,
#' locale(loc = "L"). If not, strange behaviour of characters, may occur.
#' If transliteration is done from the Serbian Cyrillic to Serbian
#' Latin it is important to set loacale to Serbian Cyrillic,
#' locale(loc="C").
#' If locale not set, before entering, transliteration may not be
#' done properly, and strange behaviour of characters may occur.
#' @return A character string of length one describing the locale in use
#' @examples
#' \dontrun{
#' locale(loc = "L") #Important!
#' lat_cyr(c("Č", "ž", "đ", "lj", "ć", "dž"))
#'
#' locale(loc = "L") #Important!
#' df_text <- data.frame(num = 1:6, c("Č", "ž", "đ", "lj", "ć", "dž"),
#' stringsAsFactors = FALSE)
#' lat_cyr(df_text)
#' }
#' @rdname locale
#' @export locale
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
