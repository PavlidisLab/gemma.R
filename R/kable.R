#' Create printable tables out of gemma.R outputs
#' 
#' Creates a \code{\link[knitr]{kable}} where certain columns are automatically
#' shortened to better fit a document.
#' 
#' @param table A data.table or data.frame outputted by a gemma.R function
#' @export
gemma_kable <- function(table){
    
    for(i in seq_len(ncol(table))){
        if(class(table[[i]]) == 'character'){
            table[[i]][nchar(table[[i]])>30] %>% sapply(function(x){
                if(grepl('^http',x)){
                    x <- glue::glue("{stringr::str_extract(x,'^.*?[.].*?[.]')}../{basename(x)}")
                } else{
                    x %>% stringr::str_trunc(30) 
                }
            })
        }
    }
    
    
    table %>% knitr::kable() %>% kableExtra::kable_styling()
}





