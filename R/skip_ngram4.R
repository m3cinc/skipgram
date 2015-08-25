#' skip_ngram4C 
#' @param x character list, possibly compressed
#' @param n integer, the gram type 
#' @param skip integer, the skip number
#' @param cflag integer, 1L if compressed string x is input
#' @param filter is an optional filter disctionary containing tokens to exclude 
#' @return n-gram in x 
#' @examples 
#' skip_ngram4C(as.character(citation())) produces a unigram of citation in list x
#' skip_ngram4C(string,3L) produces trigram of string in list x
#' skip_ngram4C(string,2L,3L,1L,badwords) produces skip3-bigram in list x corresponding to [word X X X word] from a compressed list, excludes tokens in badwords list
skip_ngram4C <- compiler::cmpfun (skip_ngram4 <- function( x,  n = 1L, skip = 0L, cflag = 0L, filter = NULL) {    
        stopifnot( is.integer( n ), is.finite( n ), n > 0L, is.integer( skip ), is.finite( skip ), skip >-1L, is.integer( cflag ), is.finite( cflag ), cflag > -1L)
        if ( cflag > 0L ) { x %>% memDecompress( "g", asChar = TRUE ) %>% strsplit( "\n" ) %>% unlist -> x }      # decompress
        if ( n == 1L && skip > 0L ) { skip <- 0L } # ignore the skip
        stri_split_boundaries <- stringi::stri_split_boundaries 
        stri_join <- stringi::stri_join  
        options <- stringi::stri_opts_brkiter( type = "word", skip_word_none = TRUE, skip_word_number = FALSE )
        tokens <- unlist( stri_split_boundaries( x, opts_brkiter = options ) ) # Split into word tokens
        if (length ( filter > 0L ) ) { tokens <- tokens[ !tokens %in% filter ] }  # filter designated vocabulary
        # if we didn't detect any words or number of tokens is less than n return empty vector 
        if ( all( is.na( tokens )) || length( tokens ) < n + skip ) { character(0) } else { 
        if ( skip == 0) { sapply(1:( length( tokens ) - n - 1L),       function(i) stri_join( tokens[i:(i + n - 1L)], collapse = " "))} else {
                          sapply(1:( length( tokens ) - n - skip -1L ), function(i) stri_join( c( tokens[i], tokens[(i + skip + 1L):(i + skip + n - 1L)]), collapse = " ")) } }
} , options = list( optimize = 3) )

#  test <- c( "This scope of this report is to perform exploratory data analysis on the Coursera Swiftkey data ",
#  "set. The Swiftkey data set contains English, German, Russian and Finnish language data sets for",
#  " exploration. This milestone report focuses on exploring English language datasets within the Swiftkey ",
#  "data set. This data set contains en_US.news.txt, en_US.blogs.txt, and en_US.twitter.txt files." )
