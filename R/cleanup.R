#' cleanup.R
#' @param x character
#' @examples 
#' cleanup(as.character(citation()) cleans and substitute citation()
#' cleanup(string) cleans and substitute string

cleanup <- compiler::cmpfun (function (x) {
        stopifnot( is.character(x) )
        stri_split_boundaries <- stringi::stri_split_boundaries 
        stri_split_lines <- stringi::stri_split_lines
        stri_trim_both <- stringi::stri_trim_both
        pbsapply <- pbapply::pbsapply
        translate_number <- skipgram::translate_number
        x %>%
        gsub("[^A-Za-z\\' 0-9$+=@#%^&_<>*.-]","",.) %>%
        gsub("[\\w\\-][\\w\\-\\.]+@[\\w\\-][\\w\\-\\.]+[a-zA-Z]{1,4}","email-address",., perl = TRUE ) %>%
        gsub("(?:(?:https?|ftp|file):\\/\\/|www\\.|ftp\\.)(?:\\([-A-Z0-9+&@#\\\\/%=~_|$?!:;,.]*\\)|[-A-Z0-9+&@#\\\\/%=~_|$?!:;,.])*(?:\\([-A-Z0-9+&@#\\\\/%=~_|$?!:;,.]*\\)|[A-Z0-9+&@#\\\\/%=~_|$](?x))","webaddress",.,perl = TRUE, ignore.case = TRUE ) %>%
        gsub("( +([Uu] +[Rr] *|\\.))"," you are ",.,perl=TRUE) %>% 
        gsub("( +([Uu][Rr] |\\.))"," your ",.,perl=TRUE) %>%
        gsub("( +([Rr][Uu] |\\.))"," are you ",.,perl=TRUE) %>%
        gsub("( +([Ll][Uu] |\\.))"," love you ",.,perl=TRUE) %>%
        gsub("( +([Rr][Tt] |\\.))"," return ",.,perl=TRUE) %>% 
        gsub("( +([Rr] |\\.))"," are ",.,perl=TRUE) %>%
        gsub("( +([Uu] |\\.))"," you ",.,perl=TRUE) %>%
        gsub("( +([bb] |\\.))"," be ",.,perl=TRUE) %>%
        gsub("( +([Yy] |\\.))"," why ",.,perl=TRUE) %>%
        gsub("( *p\\.)(\\d+)"," page \\2",.,perl=TRUE) %>%                    # p. in page
        gsub("(\\d+)(-)(\\d+)"," \\1 thru \\3",.,perl=TRUE) %>%               # - between numbers in thru
        gsub("(\\D+)(\\d+)","\\1 \\2",.,perl=TRUE) %>%                        # dublin8 to dublin 8
        gsub("(\\d+)(\\D+)","\\1 \\2",.,perl=TRUE) %>%                        # 8dublin  to 8 dublin
        gsub("((\\$)(\\d+((\\.|\\,)\\d+))+)","\\3 dollars",.,perl=TRUE) %>%   # $ in dollars
        gsub("(((\\$\\.)(\\d+))+)","\\4 cents",.,perl=TRUE) %>%               # fractional $ in cents
        gsub("((\\d+)(\\% )+)","\\2 percent",.,perl=TRUE) %>%                 # percent
        gsub("(\\&|\\+)( *[A-Za-z]+)","and \\2",.,perl=TRUE) %>%              # (&|+)[[:alpha:]] in and [[:alpha:]]
        gsub("(\\&|\\+)( *[A-Za-z]+)"," plus \\2",.,perl=TRUE) %>%            # (&|+)[[:digits:]] in plus [[:digits:]]
        gsub("(\\-)( *[0-9]+)"," minus \\2 ",.,perl=TRUE) %>%                 # -[[:digits]] in minus [[:digits:]]
        gsub("(\\=)( *[A-Za-z]+)"," equal ",.,perl=TRUE) %>%                  # = in equal
        gsub("(\\<)( *[A-Za-z]+)"," less than ",.,perl=TRUE) %>%              # < in less than
        gsub("(\\>)( *[A-Za-z]+)"," greater than ",.,perl=TRUE) %>%           # > in greater than
        stri_split_boundaries( n = -1L, type = "sentence" ) %>%  unlist %>%
        stri_split_lines( omit_empty = TRUE) %>%
        stri_trim_both( pattern = "\\P{Wspace}" ) %>%
        gsub("\\s+"," ",.,perl = TRUE ) %>% unlist -> x
        x[x != "" & !is.na(x)] -> x
        x %>% grep("( +[#0-9]){1,}",.,perl = TRUE, fixed = FALSE, value = FALSE ) -> subline
        if ( length( subline ) > 0 ) { x[subline] <- pbsapply( subline, function(i) { translate_number(x[i]) } ) }
        x %>% gsub("[^A-Za-z \\'\\-]|[0-9]","",.,perl = TRUE ) %>%
        stri_trim_both( pattern = "\\P{Wspace}" ) %>%
        gsub("\\s+"," ",.,perl = TRUE ) %>% unlist %>% tolower %>%
        gsub("( +([^ai])( +|\\.))|(^[^ai])( +|\\.)|( +[^ai]$)( *|\\.)"," ",.,perl = TRUE ) -> x
        x[x != "" & !is.na(x)] -> x
}, options = list( optimize = 3) )
