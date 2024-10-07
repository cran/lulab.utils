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
      download_times[i] <- 999
    }, warning = function(w) {
      download_times[i] <- 999
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



#' Check if wget is installed
#'
#' This function is used to check if wget is installed on the system.
#' @title check_wget
#' @rdname check_wget
#' 
#' @return a logical value indicating whether wget is installed
#' @author Zhen Lu
#' @importFrom  magrittr %>%
#' @export
#'
#' @examples
#' \donttest{
#' check_wget()
#' }
check_wget <- function() {
  # check if os is windows
  if (Sys.info()["sysname"] != "Windows") {
    message("This function is intended to check wget on Windows.")
    message("Please install wget manually on your system.")
    return(invisible(FALSE))
  }

  # Run the wget --version command
  result_1= character(0)
  tryCatch(
    {
      result_1 <- system("wget.exe --version", intern = TRUE, ignore.stderr = TRUE)
    }, error = function(e) {
      result_1 <- character(0)
    }
  )
  if (length(result_1) > 0 && grepl("GNU Wget", result_1[1])) {
    message("wget is already installed on your system.")
    return(invisible(TRUE))
  }

  result= character(0)
  tryCatch(
    {
      result <- system(sprintf("%s/wget.exe --version", rappdirs::user_data_dir(appname = "wget", appauthor="LuLab")), intern = TRUE, ignore.stderr = TRUE)
    }, error = function(e) {
      result <- character(0)
    }
  )
  # Check the exit status
  if (length(result) > 0 && grepl("GNU Wget", result[1])) {
    message("wget is already installed on your system.")
    return(invisible(TRUE))
  } else {
    message("wget is not installed.")

    ask_yes_no <- utils::askYesNo("Do you want to download wget now?\n", prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))), default = TRUE)
    if(!interactive()) ask_yes_no= TRUE
    if (is.na(ask_yes_no)) {
      message("Please install wget manually on your system.")
      return(invisible(FALSE))
    }else if(ask_yes_no) {
      url= "https://eternallybored.org/misc/wget/"
      version= "1.21.4" #access date: 2024-10-02
      arch= Sys.info()[["machine"]] %>% stringr::str_sub(-2,-1) %>% as.numeric()

      url2= sprintf("%s/%s/%s/wget.exe", url, version, arch)
      dir <- rappdirs::user_data_dir(appname = "wget", appauthor="LuLab")
      if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
      destfile= file.path(dir, "wget.exe")

      # download wget
      if(requireNamespace('httr2', quietly = TRUE)) {
        req= httr2::request(url2) %>%
          httr2::req_progress()

        req %>% httr2::req_perform(path = destfile)
      } else {
        utils::download.file(url = url2, destfile = destfile)
      }
      message("wget is downloaded successfully.")
      return(invisible(TRUE))
    } else {
      message("Please install wget manually on your system.")
      return(invisible(FALSE))
    }
  }
}

#' Use wget to download files
#' 
#' This function is used to set the download method.
#' @title use_wget
#' @rdname use_wget
#' @param use a logical value, controlling the download method
#' 
#' @return a logical value indicating whether wget is used
#' @author Zhen Lu
#' @export
#'
#' @examples
#' \donttest{
#' use_wget(use = TRUE)
#' getOption("download.file.method")
#' getOption("download.file.extra") 
#' test_url <- "https://eternallybored.org/misc/wget/1.21.4/64/wget.exe"
#' test_destfile <- tempfile()
#' download.file(test_url, destfile = test_destfile)
#' }
use_wget <- function(use = TRUE) {
  # check if wget is installed
  wget= check_wget()

  if (!use) {
    message("But we will use the default download method.")
    options(download.file.method = NULL)
    options(download.file.extra = NULL)
    return(invisible(FALSE))
  }

  if(!wget){
    return(invisible(FALSE))
  }else{
    message("And we will use wget to download files.")
    PATH= Sys.getenv('PATH')
    if (!grepl("wget", PATH)){
      Sys.setenv(PATH = sprintf("%s;%s", rappdirs::user_data_dir(appname = "wget", appauthor="LuLab"), PATH))
    }

    options(download.file.method = "wget")
    options(download.file.extra = c("-c"))
    return(invisible(TRUE))
  }
}
