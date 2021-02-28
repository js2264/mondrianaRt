#' interval2num
#'
#' @export

interval2num <- function(ints, id = 1) {
    `%>%` <- magrittr::`%>%`
    ints %>% 
        stringr::str_replace(',.*', '') %>% 
        stringr::str_replace('\\(|\\[', '') %>% 
        as.numeric()
}
