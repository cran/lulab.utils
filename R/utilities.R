#' Check for missing values for character columns
#'
#' This function is used to check the distribution of character variables in the data frame.
#' @title check_cha
#' @rdname check_cha
#' @param col a character variable name
#' @param df a data.frame
#' @param verbose logical, controlling the output
#'
#' @return a distribution table of the character variable in the data frame
#' @author Zhen Lu
#' @importFrom magrittr %>%
#' @import boot
#' @export
#' 
#' @examples
#' \donttest{
#' data("melanoma", package = "boot")
#' melanoma2 <- melanoma
#' check_cha('status', melanoma2)
#' # or
#' mapply(check_cha,'status', MoreArgs= list(melanoma2))
#' }
check_cha= function(col, df, verbose=TRUE){
  raw= df

  na_idi= c((purrr::map(raw[col],is.na) %>% unlist) |
              (purrr::map(raw[col],is.null) %>% unlist) |
              (purrr::map(raw[col],function(y) y %in% c('Missing','')) %>% unlist))
  if(sum(na_idi)>0){
    raw[col][[1]][na_idi]= 'Missing'
  }

  r1= raw[col][[1]] %>% table %>% prop.table() %>% round(2)

  if(sum(na_idi)>0){
    r1= r1[c('Missing', setdiff(names(r1),c('Missing')))]
    if(verbose) print(sprintf('%s has missing values', col))
  }
  return(r1)
}
