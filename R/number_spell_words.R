#' number_spell_words 
#' @param x integer number
#' @return x as named integer (name is the number_spell_word) 
#' @examples 
#' number_spell_words(101) returns a named integer 101, name="one hundred one"
#' number_spell_words(-2001) returns a named integer -2001, name="minus two thousand one"
number_spell_words <- compiler::cmpfun ( function( n, l = "en_US" ) {
        require ( httr, magrittr )
        known_locale <- c("bg","cs","de","dk","en_GB","en_US","es","es_AR","et","fr","fr_BE","he","hu_HU","id","it_IT","lt","nl","pl","pt_BR","ru","sv")
        stopifnot( is.integer( n ), is.finite( n ), l %in% known_locale )         
        httr::content( GET( paste0( "http://www.tools4noobs.com/online_tools/number-spell_words/?action=ajax_number_spell_words",
                                    "&number=", n,
                                    "&locale=", l,
                                    "&type=0") ), as = "text" ) -> result
        result %<>% gsub('(<div class="well">)((\\S*\\s*)*)(<\\/div>)','\\2', . , ignore.case = TRUE, perl = TRUE, useBytes = TRUE )
        setNames( n, result )
}, options = list( optimize = 3) )

