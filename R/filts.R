#' filts.R
#' @param x character
#' @param filters a char vector of tokens to exclude
#' @examples 
#' filts(as.character(citation),filters) removes from citation regex matching items contained in vector filters
#' filts(string,badwords) removes from string regex matching items contained in vector badwords
filts <- compiler::cmpfun( function ( x, f) { 
        stopifnot ( is.character(x), is.character(f) )
        for (i in 1:length(f)) {
                regex <- paste0( '^(', f[i] , ' )|( +' , f[i] , '\\b)' )
                x %<>% gsub(regex,'',.,ignore.case=TRUE,perl=TRUE,useBytes=TRUE) }
        return(x)
} , options = list( optimize = 3) )         
