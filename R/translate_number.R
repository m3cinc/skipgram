#' translate_number
#' @param x character
#' @examples 
#' translate_number( as.character( citation() ) ) substitutes number with spelled value
translate_number <- compiler::cmpfun(function ( x ) { 
        stopifnot( is.character( x ) )
        stri_split_boundaries <- stringi::stri_split_boundaries 
        stri_join <- stringi::stri_join  
        stri_flatten <- stringi::stri_flatten
        toWords <- skipgram::toWords
        options <- stringi::stri_opts_brkiter( type = "word", skip_word_none = TRUE, skip_word_number = FALSE )
        if ( length( grep( "([#0-9\\.])",x , perl = TRUE, fixed = FALSE, value = FALSE ) ) == 0L ) { return( x ) } # no number or # character
        # so we have possibly # or digits with possible .
        x %<>% gsub("(#)(\\d+.*)","\\1 \\2",. ,perl = TRUE)                # a combination like "#12" translates into "# 12"
        x %<>% gsub("# ","number ",. ,perl = TRUE )                        # a combination like "#" translates into "number"
        x %<>% gsub("(\\d+)(\\.)(\\d+.*)","\\1 point \\3",. ,perl = TRUE)  # a combination like "555.12" translated to "555 point 12"
        x %<>% gsub("(\\d+1)(st)","\\1",. ,perl = TRUE )                   # ending in fir(st) -> .*1
        x %<>% gsub("(\\d+2)(nd)","\\1",. ,perl = TRUE )                   # ending in seco(nd) -> .*2
        x %<>% gsub("(\\d+3)(rd)","\\1",. ,perl = TRUE )                   # ending in thi(rd) -> .*3
        x %<>% gsub("(\\d+)(th)","\\1",. ,perl = TRUE )                    # ending in (th) -> .*
        x %<>% gsub("(2)([a-zA-Z]+.*)","to \\2",. ,perl = TRUE )           # a combination like "2go" translated to "to go"
        x %<>% gsub("(4)([a-zA-Z]+.*)","for \\2",. ,perl = TRUE )          # a combination like "4me" translated to "for me"
        x %<>% gsub("(\\d+)(\\D+.*)","\\1 \\2",. ,perl = TRUE )            # a combination like "33lives" translated to "33 lives" 
        x %<>% gsub("(\\D+)(\\d+.*)","\\1 \\2",. ,perl = TRUE )            # a combination like "km23" translated to "km 23"
        x %<>% gsub("(#\\S+)","hashtag",. ,perl = TRUE )                   # a hashtag converted to "hashtag"
        if ( length( grep( "([0-9])",x ,perl = TRUE, fixed = FALSE, value = FALSE ) ) == 0L ) { x %<>% stri_flatten( collapse = " " ) ; return( x ) } 
        tokens <- unlist( stri_split_boundaries(x, opts_brkiter=options ) )             # Split into word tokens
        if ( length( tokens ) == 1L) { x <- tokens[length(tokens)] ; return( x ) }      # Empty vector since no word or number of tokens < n  
        change_tokens <- grep( "([0-9]){1,8}", tokens, perl = TRUE, value = FALSE )
        if ( length( change_tokens ) > 0L ) { tokens[change_tokens] <- sapply(change_tokens, function(i) {
                        tokens[i] %<>% gsub( "([0-9]){1,}" , names(toWords(.)),., perl = TRUE) } )
                                              stri_flatten( tokens, collapse = " ") -> x }
} , options = list( optimize = 3) )
  
#
# A number of test cases follow to demonstrate
#
# test <- c("This scope of this report is to perform exploratory data analysis on #101 pages of the Coursera Swiftkey data ",
#          "set. The Swiftkey's data set contains English, German, Russian and Finnish language data sets 2go for",
#          " exploration with many #hash-tags. This milestone report # 213 focuses on exploring English language datasets within the Swiftkey ",
#          "data set. This data set has 555.12 MB contains en_US.news.txt, en_US.blogs.txt, and en_US.twitter.txt files.")
