#' toWords_en_US Converts a number to its word representation in American English language
#' @param character string number, representing an integer between -infinity and infinity to be converted in words
#' @param integer p, the power of ten for the rest of the number to the right. Used internally. Optional, defaults to 0.
#' @param character string powsuffix, the power name to be added to the end of the return string. Used internally. Optional, defaults to ''.
#' @return string The corresponding word representation
#' @examples
#' toWords_en_US('1234') returns the string "one thousand two hundred and thirty-four"
#' toWords_en_US('-678') returns the string "minus six hundred and seventy-eight"
#' toWords_en_US('-123456789012345678901234567890') returns the string "minus one hundred twenty-three octillion...hundred ninety"
#' toWords_en_US('+3','3','thousand') returns the string "plus three thousand"
toWords_en_US <- compiler::cmpfun (function (number, p = 0, powsuffix = '') {
        stopifnot(is.character(number))
        stri_sub <- stringi::stri_sub
        signs.symbol <- c( '-', '+' )
        signs.name <- c( 'minus', 'plus' )
        signs <- setNames(signs.symbol, signs.name)
        exponent.int <- c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,44,48,51,54,57,60,63,66,69,72,
                        78,81,84,87,90,93,99,102,105,108,111,114,117,120,123,126,129,132,
                        135,138,141,144,147,150,153,156,159,162,165,168,171,174,177,180,183,186,
                        189,192,195,198,201,204,207,210,213,216,219,222,225,228,231,234,237,240,
                        243,246,249,252,255,258,261,264,267,270,273,276,279,282,285,288,291,294,300,
                        303,306,309,312)
        exponent.name <- c('','thousand','million','billion','trillion','quadrillion','quintillion',
                           'sextillion','septillion','octillion','nonillion','decillion','undecillion',
                           'duodecillion','tredecillion','quattuordecillion','quindecillion','sexdecillion',
                           'septendecillion','octodecillion','novemdecillion','vigintillion','unvigintillion',
                           'duovigintillion','trevigintillion','quattuorvigintillion','quinvigintillion',
                           'sexvigintillion','septenvigintillion','octovigintillion','novemvigintillion',
                           'trigintillion','untrigintillion','duotrigintillion','trestrigintillion',
                           'quattuortrigintillion','quintrigintillion','sextrigintillion','septentrigintillion',
                           'octotrigintillion','novemtrigintillion','quadragintillion','unquadragintillion',
                           'duoquadragintillion','trequadragintillion','quattuorquadragintillion',
                           'quinquadragintillion','sexquadragintillion','septenquadragintillion','octoquadragintillion',
                           'novemquadragintillion','quinquagintillion','unquinquagintillion','duoquinquagintillion',
                           'trequinquagintillion','quattuorquinquagintillion','quinquinquagintillion','sexquinquagintillion',
                           'septenquinquagintillion','octoquinquagintillion','novemquinquagintillion','sexagintillion',
                           'unsexagintillion','duosexagintillion','tresexagintillion','quattuorsexagintillion',
                           'quinsexagintillion','sexsexagintillion','septensexagintillion','octosexagintillion',
                           'novemsexagintillion','septuagintillion','unseptuagintillion','duoseptuagintillion',
                           'treseptuagintillion','quattuorseptuagintillion','quinseptuagintillion','sexseptuagintillion',
                           'septenseptuagintillion','octoseptuagintillion','novemseptuagintillion','octogintillion',
                           'unoctogintillion','duooctogintillion','treoctogintillion','quattuoroctogintillion',
                           'quinoctogintillion','sexoctogintillion','septoctogintillion','octooctogintillion',
                           'novemoctogintillion','nonagintillion','unnonagintillion','duononagintillion','trenonagintillion',
                           'quattuornonagintillion','quinnonagintillion','sexnonagintillion','septennonagintillion',
                           'octononagintillion','novemnonagintillion','centillion')
        exponent <- setNames(exponent.int, exponent.name)
        digits.int <- c(0:9)
        digits.name <-c ('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine')
        digits <- setNames(digits.int, digits.name)
        infinity <- 'infinity'
        and_ <- 'and'
        dash <- '-'
        plural <- 's'
        separator <- ' '
        ret <- '' # initialize return value to empty string
        if ( stri_sub( number, 1, 1) %in% signs ) { # check for a plus or minus sign
                ret <- paste0(names( signs[signs == stri_sub( number, 1, 1) ] ),separator)
                number <- stri_sub( number, 2) }
        number <- trimws( number )         # strip excessive zero signs and spaces
        number <- gsub('^0+', '', number,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
        # if the absolute value is greater than 9.99*10^302, return infinity
        if (nchar( number ) > max( exponent ) ) { ret <- paste0( ret, infinity) ; return (ret) }
        if (nchar(number) > 3) {
                maxp <- nchar(number)-1
                curp <- maxp
                for (p in (maxp:1)) {
                        if (p %in% exponent) { # send substr from curp to p
                                snumber <- stri_sub( number, maxp - curp + 1, ,curp - p + 1 ) 
                                snumber <- gsub('^0+', '', snumber,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
                                if (nchar(snumber) > 0) { 
                                        cursuffix <- names( exponent[exponent == p] )
                                        if ( nchar( powsuffix ) > 0 ) { cursuffix <- paste0( separator, powsuffix) }
                                        snumber %>% toWords_en_US( p, cursuffix ) %>% paste0(ret,.,separator)-> ret 
                                }
                                curp <- p - 1
                        }
                 }
                number <- stri_sub( number, maxp - curp + 1, ,curp - p + 2 )
                if (as.numeric(number) == 0) { return (ret) }
        } else if ( (as.numeric(number) == 0 ) || ( number == '') ) { ret <- paste0( separator, digits[1] ) ; return (ret) } 
        h <- t <-d <- 0 
        switch (as.character(nchar(number)), 
                "3" = { h <- as.integer(stri_sub(number, -3, ,1)) ; t <- as.integer(stri_sub(number, -2, ,1)) ; d <- as.integer(stri_sub(number, -1, ,1)) },
                "2" = { t <- as.integer(stri_sub(number, -2, ,1)) ; d <- as.integer(stri_sub(number, -1, ,1)) },
                "1" = { d <- as.integer(stri_sub(number, -1, ,1)) },
                "0" = { return } ) 
        if (h > 0) { ret <- paste0(ret, names( digits[h+1] ), separator, 'hundred' ) } 
        # ten, twenty etc.
        switch ( as.character(t), 
                 "9" = { ret <- paste0(ret, separator, names( digits[t+1] ), 'ty') }, 
                 "8" = { ret <- paste0(ret, separator, 'eighty') }, 
                 "7" = { ret <- paste0(ret, separator, names( digits[t+1] ), 'ty') }, 
                 "6" = { ret <- paste0(ret, separator, names( digits[t+1] ), 'ty') }, 
                 "5" = { ret <- paste0(ret, separator, 'fifty') }, 
                 "4" = { ret <- paste0(ret, separator, 'forty') }, 
                 "3" = { ret <- paste0(ret, separator, 'thirty') }, 
                 "2" = { ret <- paste0(ret, separator, 'twenty') }, 
                 "1" = { switch ( as.character(d), 
                                  "0" = { ret <- paste0(ret, separator, 'ten') }, 
                                  "1" = { ret <- paste0(ret, separator, 'eleven') }, 
                                  "2" = { ret <- paste0(ret, separator, 'twelve') }, 
                                  "3" = { ret <- paste0(ret, separator, 'thirteen') }, 
                                  "4" = { ret <- paste0(ret, separator, names( digits[d+1] ), 'teen') }, 
                                  "5" = { ret <- paste0(ret, separator, 'fifteen') }, 
                                  "6" = { ret <- paste0(ret, separator, names( digits[d+1] ), 'teen') }, 
                                  "7" = { ret <- paste0(ret, separator, names( digits[d+1] ), 'teen') }, 
                                  "8" = { ret <- paste0(ret, separator,'eighteen') },
                                  "9" = { ret <- paste0(ret, separator, names( digits[d+1] ), 'teen') } 
                 ) } )    
        # add digits only in <0>,<1,9> and <21,inf> and add dash between [2-9] and digit
        if ((t != 1) && (d > 0)) {if (t > 1) { ret <- paste0(ret, dash, names( digits[d+1] ) ) } else { ret <- paste0(ret, separator, names( digits[d+1] ) ) } }   
        if (nchar(powsuffix) > 0) { ret <- paste0(ret, separator, powsuffix, separator) } 
        ret %<>% gsub('( {2,})',' ',.,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
        ret <- trimws(ret,'b')
        return ( ret ) 
} , options = list( optimize = 3) )
