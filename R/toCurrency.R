#' toCurrency Converts a currency to its word representation in a designated language locale
#' @param character string x, representing a decimal between -infinity and infinity to be converted in words
#' @param logical convert_fraction, indicates fraction to words (left as numeric if set to FALSE). Optional. Defaults to TRUE.
#' @param character string locale, representing the locale language, by default, set to 'us_EN' 
#' @return x as named num is the specified locale
#' @examples
#' toCurrency('123.56') returns the num 123.56 named "one hundred twenty-three dollars fifty-six cents" 
#' toCurrency('123.56',FALSE) returns the num 123.56 named "one hundred twenty-three dollars" 
#' toCurrency('-2349.03',,'fr') returns the num -2349.03 named "moins deux mille trois cent quarante-neuf euros trois euro-centimes" 
#' toCurrency('-2349.03',FALSE,'fr') returns the num -2349.03 named "moins deux mille trois cent quarante-neuf euros" 
toCurrency <- compiler::cmpfun (function (x, convert_fraction = TRUE, locale='en_US') {
        stopifnot(is.character(x))
        toCurrency_fr <- skipgram::toCurrency_fr
        toCurrency_en_US <- skipgram::toCurrency_en_US
        locale.abb <- c('en_US','fr')
        locale.currency <- c('USD','EUR')
        localized <- data.frame(locale=locale.abb,currency=locale.currency,stringsAsFactors=FALSE)
        stopifnot(locale %in% localized$locale)
        fraction <- FALSE
        separator <- ' '
        ret <- '' # initialize return value to empty string
        decimal <- gsub('(.\\d+)(\\.)(\\d+)','\\1',x,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
        if ( convert_fraction != FALSE ) { fraction <- gsub('(.\\d+)(\\.)(\\d+)','\\3',x,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) } 
        switch (locale,
                'fr'    = { if (nchar( decimal) > 0 )  { ret <- paste0( toCurrency_fr(decimal, fraction, 'EUR', convert_fraction ) ) } },
                'en_US' = { if (nchar( decimal) > 0 )  { ret <- paste0( toCurrency_en_US(decimal, fraction, 'USD', convert_fraction ) ) } })
        x <- setNames(as.numeric(x), ret)
        return(x)
} , options = list( optimize = 3) )
