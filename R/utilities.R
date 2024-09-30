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


#' Test speed of CRAN mirror 
#'
#' This function is used to test the speed of CRAN mirror.  
#' @title test_mirror   
#' @rdname test_mirror
#' @param region a character string, the region of the CRAN mirror, e.g. 'China'
#' @param verbose logical, controlling the output
#'
#' @return a data.frame with the name, URL, and download time of the fastest CRAN mirror  
#' @author Zhen Lu
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
#' \donttest{
#' test_mirror('China')
#' }  
test_mirror= function(region, verbose=TRUE){
  hosts= utils::getCRANmirrors() %>%
    dplyr::filter(!!dplyr::sym('Country') == region)
  
  # Initialize a vector to store download times
  download_times <- numeric(nrow(hosts))
  
  # Loop through each host and test download speed
  if (verbose) cat('Testing the download speed of CRAN mirrors...\n')
  for (i in 1:nrow(hosts)) {
    url <- paste0(hosts$URL[i], "src/contrib/PACKAGES.gz") # download the package list
    start_time <- Sys.time()
    tryCatch({
      utils::download.file(url, destfile = tempfile(), quiet = TRUE) # test the download speed  
      end_time <- Sys.time()
      download_times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
    }, error = function(e) {
      download_times[i] <- Inf
    }, warning = function(w) {
      download_times[i] <- Inf
    }
    )
  }
  
  # Add download times to the hosts dataframe
  hosts$download_time <- download_times
  
  # Sort hosts by download time
  hosts_sorted= hosts %>% 
    dplyr::arrange(!!dplyr::sym("download_time")) %>% 
    dplyr::select("Name", "URL", "download_time")
  
  # reset the CRAN mirror
  # Set the fastest mirror as the new CRAN repository
  fastest_mirror <- hosts_sorted$URL[1]
  if (verbose) {
    hosts_sorted

    cat("You can use the following command to set the fastest CRAN mirror:\n")
    cat(sprintf("options(repos = c(CRAN = '%s'))\n", fastest_mirror))
  }
  
  return(hosts_sorted)
}


