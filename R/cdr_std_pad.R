#' Integer sequence with preceding zeroes
#'
#' @param n length of the sequence
#'
#' @return a character vector
#'
#' @examples
#' crudr:::cdr_std_pad(3)
#' crudr:::cdr_std_pad(10)
#'
cdr_std_pad <- function(n){

  stringr::str_pad(1:n, width = nchar(n), pad = '0')

}
