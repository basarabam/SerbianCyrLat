% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locale.R
\name{lcl}
\alias{lcl}
\title{Change Aspects of the Locale for the Transliteration}
\usage{
lcl(loc)
}
\arguments{
\item{loc}{string, can take one of three values:

"L" - Change aspect of the locale to Serbian Latin.

"C" - Change aspect of the locale to Serbian Cyrillic.

"D" - Change aspect of the locale to system default.

It is important to change the aspect of the locale to character
before transliteration of the characters.
So if trasliteration is done from the Serbian Latin to Serbian
Cyrillic it is important to set locale to Serbian Latin,
locale(loc = "L"). If not, strange behaviour of characters, may occur.
If transliteration is done from the Serbian Cyrillic to Serbian
Latin it is important to set loacale to Serbian Cyrillic,
lcl(loc="C").
If locale not set, before entering, transliteration may not be
done properly, and strange behaviour of characters may occur.}
}
\value{
A character string of length one describing the locale in use
}
\description{
Change Aspects of the Locale for the Transliteration
}
