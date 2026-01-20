#' Module to Extend Validation to HCPC
#'
#' Extend the bootstrap approach to check the validity to HCPC
#'
#' @param dataset The data to analyze
#' @param k Integer: the number of clusters
#' @param consol Boolean: should the cluster solution be consolidated using k-means
#' @param scale.unit Boolean: should the PCA be standardized or not
#'
#' @returns The results of the bootstrap approach
#' @export
#'
#' @examples NULL
HCPC_CBI <- function(dataset, k, consol=FALSE, scale.unit=FALSE){

  # Running the Analysis
  res_pca <- FactoMineR::PCA(dataset, scale.unit=scale.unit, ncp=Inf, graph=FALSE)
  res_hcpc <- FactoMineR::HCPC(res_pca, consol=consol, nb.clust=k, graph=FALSE)

  # Preparing some outputs
  partition <- as.character(res_hcpc$data.clust$clust)
  names(partition) <- rownames(res_hcpc$data.clust)

  clusterlist <- vector("list",k)
  for (cl in 1:k){
    clusterlist[[cl]] <- as.logical(partition == as.character(cl))
  }

  # Exporting the Results
  res <- list(result=res_hcpc, noise=FALSE, nc=k, clusterlist=clusterlist, partition=partition, clustermethod="HCPC")
  return(res)
}
