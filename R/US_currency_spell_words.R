#' US_currency_spell_words 
#' @param x numeric 
#' @return x as named num (name is the US_currency_spell_word) 
#' @examples 
#' US_currency_spell_words(101.45) returns a named num 101.45 named "one hundred one dollars dollars and 45 cents"
#' US_currency_spell_words(-2001) returns a named num -2001, named "minus two thousand one dollars"
US_currency_spell_words <- compiler::cmpfun ( function( x ) {
        stopifnot( is.numeric( x ), is.finite( x ))         
        httr::content( GET( paste0( "http://www.tools4noobs.com/online_tools/number-spell_words/?action=ajax_number_spell_words",
                                    "&number=", x,
                                    "&type=1",
                                    "&locale=en_US" ) ), as = "text" ) -> result
        result %<>% gsub('(<div class="well">)((\\S*\\s*)*)(<\\/div>)','\\2',. ,ignore.case = TRUE, perl = TRUE, useBytes = TRUE )
        setNames( x, result )
}, options = list( optimize = 3) )
