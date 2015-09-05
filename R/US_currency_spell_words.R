#' US_currency_spell_words 
#' @param x numeric
#' @param fraction logical, by default TRUE, to convert also the fractional part 
#' @param local logical, by default TRUE, to compute locally. if FALSE use http service
#' @return x as named num (name is the US_currency_spell_word_local) 
#' @examples 
#' US_currency_spell_words(101.45) returns the num 101.45 named "one hundred one dollars dollars and 45 cents"
#' US_currency_spell_words(-2001) returns the num -2001, named "minus two thousand one dollars"
#' US_currency_spell_words(2001.45,,TRUE) returns the num 2001.45, named "two thousand one dollars"
#' US_currency_spell_words(2001.45,TRUE,TRUE) returns the num 2001.45, named "two thousand one dollars forty-five cents"
#' US_currency_spell_words(2001.45,,FALSE) returns the num 2001.45, named "two thousand one dollars forty-five cents"
#' US_currency_spell_words(-128001000.78,TRUE,TRUE) returns the num -128001001, named "minus one hundred twenty-eight million one thousand dollars seventy-eight cents"
#' US_currency_spell_words(-128001000.78,TRUE,FALSE) returns the num -128001001, named "minus one hundred twenty-eight million one thousand dollars seventy-eight cents"
US_currency_spell_words <- compiler::cmpfun ( function( x, fraction = FALSE, local = TRUE) {
        stopifnot( is.numeric( x ), is.finite( x ))
        if (local != FALSE) {   int_curr<-'USD'
                                decimal <- as.character(trunc(x))
                                if ( fraction != FALSE ) { fraction <- gsub('(.\\d+)(\\.)(\\d+)','\\3',x,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) } 
                                result <- toCurrency_en_US ( decimal, fraction, int_curr, convert_fraction = TRUE) } 
        else { httr::content( GET( paste0( "http://www.tools4noobs.com/online_tools/number-spell_words/?action=ajax_number_spell_words",
                                    "&number=", x, "&type=1", "&locale=en_US" ) ), as = "text" ) -> result
               result <- gsub('(<div class="well">)((\\S*\\s*)*)(<\\/div>)','\\2',result ,ignore.case = TRUE, perl = TRUE, useBytes = TRUE ) }
        setNames( x, result )
}, options = list( optimize = 3) )
