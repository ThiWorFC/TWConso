#' Internal Preference Map (MDPref)
#'
#' Build the Internal Preference Map (or MDPref) by projecting as well the sensory data if avaialble.
#'
#' @param dataset The data table to be used to build the MDPred
#' @param scale.unit Boolean, TRUE by default that standardize the individual consumer liking scores
#' @param sensory The sensory data (should be the mean table with the product as first column and the attributes as subsequent columns)
#'
#' @returns The results of the PrefMap (FactoMineR output)
#' @export
#'
#' @examples NULL
MDPref <- function(dataset, scale.unit=TRUE, sensory=NULL){

  nbjuge <- length(unique(dataset$Judge))
  nbprod <- length(unique(dataset$Product))
  message = NULL

  # Preparing the Data

    ## Liking Data
  data_mdpref <- dataset %>%
    dplyr::select(Judge, Product, Liking) %>%
    tidyr::pivot_wider(names_from=Judge, values_from=Liking)

  ## Sensory Data
  if (!is.null(sensory)){
    sensory <- sensory %>%
      dplyr::rename("Product" = 1) %>%
      dplyr::mutate(Product = as.character(Product))
    prod_common <- intersect(unique(as.character(data_mdpref$Product)), unique(as.character(sensory$Product)))
    if (length(prod_common) == nbprod){
      data_mdpref <- data_mdpref %>%
        dplyr::inner_join(sensory, by="Product")
    } else {
      sensory = NULL
      message = "The sensory couldn't be used: the samples do not exactly match between the two dataset"
    }
  }

  # MDPref Data
  data_mdpref <- data_mdpref %>%
    as.data.frame() %>%
    tibble::column_to_rownames(var="Product") %>%
    dplyr::select(where(is.numeric))

  if (!is.null(sensory)){
    mdpref_pca <- FactoMineR::PCA(data_mdpref, quanti.sup=(nbjuge+1):ncol(data_mdpref), scale.unit=scale.unit, graph=FALSE)
  } else {
    mdpref_pca <- FactoMineR::PCA(data_mdpref, scale.unit=scale.unit, graph=FALSE)
  }

  # Exporting the Results

  res <- list(MDPref = mdpref_pca, message = message)
  return(res)
}
