#' toWords Converts a number to its word representation in a designated language locale
#' @param character string x, representing a decimal between -infinity and infinity to be converted in words
#' @param logical convert_fraction, indicates fraction to words (left as numeric if set to FALSE). Optional. Defaults to TRUE.
#' @param character string locale, representing the locale language, by default, set to 'us_EN' 
#' @return x as named num is the specified locale
#' @examples
#' toWords('123.45') returns the num 123.45 named "one hundred twenty-three" 
#' toWords('123.45',TRUE) returns the num 123.45 named "one hundred twenty-three and forty-five decimal parts" 
#' toWords('123.45',,'fr') returns the num 123.45 named "cent vingt-trois"
#' toWords('-24123.45',,'fr') returns the num -24123.45 named "moins vingt-quatre mille cent vingt-trois" 
#' toWords('-24123.45',TRUE,'fr') returns the num -24123.45 named "moins vingt-quatre mille cent vingt-trois et quarante-cinq parties decimales" 
toWords <- compiler::cmpfun (function (x, convert_fraction = FALSE, locale='en_US') {
        stopifnot(is.character(x))
        toWords_fr <- skipgram::toWords_fr
        toWords_en_US <- skipgram::toWords_en_US
        locale.abb <- c('en_US','fr')
        stopifnot(locale %in% locale.abb)
        separator <- ' '
        ret <- '' # initialize return value to empty string
        decimal <- gsub('(.\\d+)(\\.)(\\d+)','\\1',x,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
        fraction <- gsub('(.\\d+)(\\.)(\\d+)','\\3',x,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
        switch (locale,
                'fr'    = { if (nchar( decimal) > 0 )  { ret <- paste0( toWords_fr(decimal)) }
                            if (nchar( fraction) > 0 ) { if (convert_fraction != FALSE) { ret <- paste0( ret, ' et ', toWords_fr(fraction), ' parties decimales' ) } } },
                'en_US' = { if (nchar( decimal) > 0 )  { ret <- paste0( toWords_en_US(decimal)) }
                            if (nchar( fraction) > 0 ) { if (convert_fraction != FALSE) { ret <- paste0( ret, ' and ', toWords_en_US(fraction),' decimal parts' ) } } } )
        x <- setNames(as.numeric(x),ret)
        return(x)
}, options = list( optimize = 3) )
