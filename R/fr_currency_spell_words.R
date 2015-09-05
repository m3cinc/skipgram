#' fr_currency_spell_words 
#' @param x numeric
#' @param fraction logical if TRUE, convert also the fractional part 
#' @param local logical if TRUE, compute locally, if FALSE use http service
#' @return x as named num (name is the US_currency_spell_word_local) 
#' @examples 
#' fr_currency_spell_words(-101.45) returns the num -101.45 named "moins cent un euros"
#' fr_currency_spell_words(-101.45,TRUE) returns the num -101.45 named "moins cent un euros quarante-cinq euro-centimes"
#' fr_currency_spell_words(2001) returns the num 2001, named "deux mille un euros"
#' fr_currency_spell_words(2001.45,,TRUE) returns the num 2001.45, named "deux mille un euros"
#' fr_currency_spell_words(2001.45,TRUE,TRUE) returns the num 2001.45, named "deux mille un euros quarante-cinq euro-centimes"
#' fr_currency_spell_words(2001,,FALSE) returns the num 2001, named "The method is not implemented in the requested language."
#' fr_currency_spell_words(-128001000.78,TRUE,FALSE) returns the num -128001001, named "The method is not implemented in the requested language."
#' fr_currency_spell_words(-128001000.78,TRUE,TRUE) returns the num -128001001, named "moins cent vingt-huit million un mille euros six-dix-huit euro-centimes" 
fr_currency_spell_words <- compiler::cmpfun ( function( x, fraction = FALSE, local = TRUE) {
        stopifnot( is.numeric( x ), is.finite( x ))
        if (local != FALSE) {   int_curr<-'EUR'
                                decimal <- as.character(trunc(x))
                                if ( fraction != FALSE ) { fraction <- gsub('(.\\d+)(\\.)(\\d+)','\\3',x,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) } 
                                result <- toCurrency_fr ( decimal, fraction, int_curr, convert_fraction = TRUE) } 
        else { httr::content( GET( paste0( "http://www.tools4noobs.com/online_tools/number-spell_words/?action=ajax_number_spell_words",
                                    "&number=", x, "&type=1", "&locale=fr" ) ), as = "text" ) -> result
               result <- gsub('(.+>)(.+)(<.+)','\\2',result ,ignore.case = TRUE, perl = TRUE, useBytes = TRUE ) }
        setNames( x, result )
}, options = list( optimize = 3) )
