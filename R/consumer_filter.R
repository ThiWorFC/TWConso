#' Filter Consumers
#'
#' Define criteria to filter consumers with missing values or not enough variability in their 'Liking' scores
#'
#' @param dataset A table with the consumer data
#' @param range_filter Integer: the minimum range in 'Liking' scores consumer need to surpass to be kept in the dataset.
#'
#' @returns The data table after removind undesired consumers
#' @export
#'
#' @examples NULL
consumer_filter <- function(dataset, range_filter=2){

  nbjuge <- length(unique(dataset$Judge))
  nprod <- length(unique(dataset$Product))

  # Filtering Judges

    ## Remove consumers with too many records
  j_toomuch <- table(dataset$Judge, dataset$Product) %>%
    as.data.frame() %>%
    dplyr::filter(Freq > 1) %>%
    dplyr::select(Var1) %>%
    unique() %>%
    unlist() %>%
    as.character()
  if (length(j_toomuch) == 0){
    j_toomuch = NULL
  }

    ## Remove consumers with missing records
  j_na <- dataset %>%
    dplyr::count(Judge) %>%
    dplyr::filter(n < nprod) %>%
    dplyr::select(Judge) %>%
    unique() %>%
    unlist() %>%
    as.character()
  if (length(j_na) == 0){
    j_na = NULL
  }

    ## Remove consumers with not enough variability
  j_novar <- dataset %>%
    dplyr::group_by(Judge) %>%
    dplyr::summarize(Range = diff(range(Liking))) %>%
    dplyr::filter(Range <= range_filter) %>%
    dplyr::select(Judge) %>%
    unique() %>%
    unlist() %>%
    as.character()
  if (length(j_novar) == 0){
    j_novar = NULL
  }

  # Exporting the Results

    ## Combining the Judge to remove
  j_rm <- unique(c(j_toomuch, j_na, j_novar))

    ## Message
  message <- paste0("The final dataset contains ", nbjuge-length(j_rm), " consumers.")
  if (!is.null(j_toomuch)){
    message <- c(message, paste0("The following consumers have been removed as they had too many records: ", paste(j_toomuch, collapse=", ")))
  }
  if (!is.null(j_na)){
    message <- c(message, paste0("The following consumers have been removed as they did not evaluate all the samples: ", paste(j_na, collapse=", ")))
  }
  if (!is.null(j_novar)){
    message <- c(message, paste0("The following consumers have been removed as they did not discriminate the samples (range < ",range_filter+1,"): ", paste(j_novar, collapse=", ")))
  }

    ## Export
  res <- list(remove=j_rm, message=message)
  return(res)
}
