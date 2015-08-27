#' build_ngram 
#' @param x character list, optionaly compressed if cflag is TRUE
#' @param gramname name of the gram group
#' @param gramdir directory to save grams
#' @param elimdir directory to save discards
#' @param maxn integer, the maximum gram type (1L for unigram, 2L for bigram, etc..) to generate
#' @param sflag logical, set to TRUE to generate skipgrams; note skip is ignored, set to FALSE, on unigram and on nmax-gram!
#' @param cflag logical, set to TRUE if compressed string x is input
#' @param filter is an optional character vector dictionary containing tokens to exclude, omit if no filter is supplied 
#' @return nothing besides the grams files 
#' @examples 
#' build_ngram(as.character(citation())) produces a unigram of citation in list x and saves it in the current directory
#' build_ngram(string,,,3L) builds up to trigram of string in list x no skipgrams generated
#' build_ngram(string,,,3L,1L,1L,badwords) builds up to trigram of string in list x corresponding to [word X X word] from a compressed list, excludes tokens in badwords character vector
build_ngram <- compiler::cmpfun (function (x, gramname, gramdir, elimdir , maxn = 1L, sflag = FALSE, cflag = FALSE, filter = NULL) {
        stopifnot( is.integer( maxn ), is.finite( maxn ), maxn > 0L, is.logical( cflag ), is.logical( sflag ) )
        if ( cflag == TRUE ) { x %>% memDecompress( "g", asChar = TRUE ) %>% strsplit( "\n" ) %>% unlist -> x }      # decompress
        for (n in 1L:maxn) {
                skip <- 0L  # force zero skip to build first full gram(s)
                x %>% skip_ngram( n, skip, cflag, filter) %>% unlist %>% table %>% sort(decreasing=TRUE) -> gram 
                filename <- paste0( gramdir,"/",gramname,"Gram", n, skip, ".RDS") ; saveRDS ( gram, file = filename)
                gram %>% extract(.<2) %>% names -> discard ; filename <- paste0( elimdir, "/", gramname, "Discard", n, skip, ".RDS") ; saveRDS( discard, file = filename) ; rm( discard )
                gram %>% extract(.>1) -> Vocabulary 
                filename <- paste0( gramdir,"/",gramname,"Gram", n, skip, ".RDS") ; saveRDS( Vocabulary, file = filename ) ; rm( gram, Vocabulary ) 
        } # trimmed grams
        if (sflag == TRUE && maxn > 2L) { for (n in 2L:(maxn - 1L)) 
        { for (skip in 1L:(maxn - n)) {
                x %>% skip_ngram( n, skip, cflag, filter) %>% unlist %>% table %>% sort(decreasing=TRUE) -> gram 
                filename <- paste0( gramdir, "/", gramname, "Gram", n, skip, ".RDS") ; saveRDS ( gram, file = filename)
                gram %>% extract(.<2) %>% names -> discard ; filename <- paste0( elimdir, "/",gramname,"Discard", n, skip, ".RDS") ; saveRDS( discard, file = filename) ; rm( discard )
                gram %>% extract(.>1) -> Vocabulary 
                filename <- paste0( gramdir, "/", gramname, "Gram", n, skip, ".RDS") ; saveRDS( Vocabulary, file = filename ) ; rm( gram, Vocabulary )
        } } } # trimmed skip_ngrams
}, options = list( optimize = 3) )
