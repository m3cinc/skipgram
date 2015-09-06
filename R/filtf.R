#' filtf.R
#' @param x character vector
#' @param f a char vector of tokens to exclude
#' @examples 
#' filtf(string,badwords) removes from string vector regex matching items contained in vector badwords
#' filtf(x,myfilter) removes from character vector x regex matching items contained in vector myfilter
filtf <- compiler::cmpfun( function ( x, f) {
        stopifnot ( is.character(x), is.character(f) )
        filts <- skipgram::filts
        x <- pbsapply(1:length(x), function(i){x[i] %<>% filts(f)})
        return(x)
} , options = list( optimize = 3) )         
