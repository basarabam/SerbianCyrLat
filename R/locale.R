myenv$def_loc <- Sys.getlocale()


#' Setting locale
#' @param loc string argument for sys.setlocale(), L-Serbian Latin locale,
#' C-Serbian Cyrillic locale settings and D-for system default settings.
#' @export
locale <- function(loc){
  if(loc == "L"){
    Sys.setlocale("LC_ALL", "Serbian (Latin)_Serbia")
  } else if(loc == "C"){
    Sys.setlocale("LC_ALL", "Serbian (Cyrillic)_Serbia")
  } else if(loc == "D"){
    def_loc_arg <<- stringr::str_extract(myenv$def_loc, "(?<=LC_COLLATE=)(.*)(?=;LC_CTYPE)")
    Sys.setlocale("LC_ALL", def_loc_arg)
  } else {
    stop()
  }
}
