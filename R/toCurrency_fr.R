#' toCurrency_fr Converts a currency value to its word representation (with monetary units) in French language
#' @param integer decimal, the currency total amount without fraction part (e.g. amount of dollars)
#' @param logical fraction, indicates if the fractional part of the currencyy amount (e.g. amount of cents) is desired. Optional. Defaults to FALSE.
#' @param integer int_curr, an international currency symbol as defined by the ISO 4217 standard (three characters)
#' @param logical convert_fraction indicates fraction to words conversion required (left as numeric if set to FALSE). Optional. Defaults to TRUE.
#' @return string The corresponding word representation for the currency
#' @examples 
#' toCurrency_fr('25','34') results in string "vingt-cinq euros trente-quatre euro-centimes"   
#' toCurrency_fr('25','34',,FALSE) results in string "vingt-cinq euros 34 euro-centimes"
#' toCurrency_fr('-460','12','USD') results in string "moins quatre cent soixante dollars douze cents"
#' toCurrency_fr('+123','47','EUR') results in string "plus cent vingt-trois euros quarante-sept euro-centimes"  
toCurrency_fr <- compiler::cmpfun (function ( decimal, fraction = FALSE, int_curr='EUR', convert_fraction = TRUE) {
        stri_sub <- stringi::stri_sub
        currency.abb <- c ('ALL','AUD','BAM','BGN','BRL','BYR','CAD','CHF','CYP','CZK','DKK','EEK','EUR','GBP',
                           'HKD','HRK','HUF','ILS','ISK','JPY','LTL','LVL','MKD','MTL','NOK','PLN','ROL','RUB',
                           'SEK','SIT','SKK','TRL','UAH','USD','YUM','ZAR')
        currency.name <-c ('lek','Australian dollar','convertible marka','lev','real','Belarussian rouble',
                           'Canadian dollar','Swiss franc','Cypriot pound','Czech koruna','Danish krone',
                           'kroon','euro','pound','Hong Kong dollar','Croatian kuna','forint','new sheqel',
                           'Icelandic kr na','yen','litas','lat','Macedonian dinar','Maltese lira',
                           'Norwegian krone','zloty','Romanian leu','Russian Federation rouble',
                           'Swedish krona','Tolar','Slovak koruna','lira','hryvna','dollar','dinars','rand')
        currency.fraction <-c ('qindarka','cent','fenig','stotinka','centavos','kopiejka','cent','rapp',
                               'cent','halerz','ore','senti','euro-centime','pence','cent','lipa','filler','agora',
                               'aurar','sen','cent','sentim','deni','centym','oere','grosz','bani','kopiejka',
                               'oere','stotinia','','kuru','cent','cent','para','cent')
        currency.plural <-c('','','','','','','','','','','pence','','','','agorot','','','','','','','','','','','',
                            '','','','','','','','','','')
        currency <- data.frame ( abb = currency.abb,name = currency.name,fraction = currency.fraction,plural = currency.plural,stringsAsFactors = FALSE )
        signs.symbol <- c('-','+')
        signs.name <- c('moins','plus')
        signs <- setNames(signs.symbol,signs.name)
        int_curr <- toupper(int_curr)
        curr <- subset(currency, currency$abb == int_curr)
        dash <- '-'
        plural <- 's'
        separator <- ' '
        ret <- '' # initialize return value to empty string
        if (stri_sub(decimal, 1, 1) %in% signs) { # check for a plus or minus sign
                ret <- paste0(ret, names(signs[signs == stri_sub( decimal, 1, 1 ) ] ), separator )
                decimal <- stri_sub( decimal, 2) }
        decimal <- trimws(decimal) # strip excessive zero signs and spaces
        # if the absolute value is greater than 9.99*10^302, return infinity
        if (length(decimal) > 312 ) {return (ret,infinity)}
        decimal <- gsub('^0+', '', decimal,ignore.case=TRUE,perl=TRUE,useBytes=TRUE)
        decimal <- trimws( toWords_fr( decimal,0,'' ) )
        ret <- paste0( ret, decimal )
        lev <- ifelse(decimal == 1, 0 , 1)
        if (lev > 0) { ret <-paste0(ret, separator, curr$name, 's') } else { ret <- paste0(ret, separator, curr$name) }
        if (fraction != FALSE) {
                if (convert_fraction == TRUE) { ret <-paste0(ret, separator, trimws( toWords_fr( fraction, 0,'' ) ) )}  else { ret <-paste0(ret, separator, fraction) }
                lev <- ifelse(fraction == 1, 0 , 1)
                if (lev > 0) { 
                        if (curr$plural != '') { ret <-paste0(ret, separator, curr$plural) } else { ret <-paste0(ret, separator, curr$fraction, 's') }
                } else { ret <-paste0(ret, separator, curr$fraction) }
        }
        return (ret)
}, options = list( optimize = 3) )
