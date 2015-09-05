#' filter_tokens
#' @param x character
#' @param filters a char vector of tokens to exclude
#' @examples 
#' filter_words(as.character(citation()),filters) removes tokens contained in filters
#' filter_words(string,badwords) removes from string tokens contained in badwords
filter_tokens <- compiler::cmpfun( function ( x, filters) { 
        stopifnot( is.character(x), is.character(filters) )
        #' To avoid :: calls 
        stri_split_boundaries <- stringi::stri_split_boundaries 
        stri_join <- stringi::stri_join  
        stri_flatten <- stringi::stri_flatten
        options <- stringi::stri_opts_brkiter(type = "word", skip_word_none = TRUE, skip_word_number = FALSE)
        tokens <- unlist(stri_split_boundaries(x, opts_brkiter = options ))       # Split into word tokens
        tokens[!tokens %in% filters] %>% stri_flatten( collapse = " " ) %>%
        gsub("^\\s*|\\s*$", "",., perl = TRUE, useBytes = TRUE ) %>%        # strip left and right one (or more) spaces
        gsub("\\s+"," ",.,perl = TRUE, useBytes = TRUE ) -> x
} , options = list( optimize = 3) )  
#
# A test case follow to demonstrate
#
# test2 <- c ("alligatorbait This scope of this report is to perform explatory data analysis on #101 pages of the Coursera Swiftkey data ",
#          "set. The Swiftkey'data set contains English, German, Russian and Finnish language data sets for",
#          " exploration. This milestone report # 213 focuses on exploring English language datasets within the Swiftkey ",
#          "data set. This data set contains en_US.news.txt, en_US.blogs.txt, and en_US.twitter.txt files.")

# filter_tokens(test2,badwords)
# > [1] "This scope of this report (...) en_US.twitter.txt files"

