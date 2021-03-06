% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/toCurrency_en_US.R
\name{toCurrency_en_US}
\alias{toCurrency_en_US}
\title{toCurrency_en_US Converts a currency value to its word representation (with monetary units) in American English language}
\usage{
toCurrency_en_US(decimal, fraction = FALSE, int_curr = "USD",
  convert_fraction = TRUE)
}
\arguments{
\item{integer}{decimal, the currency total amount without fraction part (e.g. amount of dollars)}

\item{logical}{fraction, indicates if the fractional part of the currency amount (e.g. amount of cents) is desired. Optional. Defaults to FALSE.}

\item{integer}{int_curr, an international currency symbol as defined by the ISO 4217 standard (three characters)}

\item{logical}{convert_fraction indicates fraction to words conversion required (left as numeric if set to FALSE). Optional. Defaults to TRUE.}
}
\value{
string The corresponding word representation for the currency
}
\description{
toCurrency_en_US Converts a currency value to its word representation (with monetary units) in American English language
}
\examples{
toCurrency_en_US('25','34') results in string "twenty-five dollars thirty-four cents"
toCurrency_en_US('25','34',,FALSE) results in string "twenty-five dollars 34 cents"
toCurrency_en_US('-460','12','EUR') results in string "minus four hundred sixty euros twelve euro-cents"
toCurrency_en_US('+123','47','EUR') results in string "plus one hundred twenty-three euros forty-seven euro-cents"
}

