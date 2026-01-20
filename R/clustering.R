#' Perform HAC
#'
#' Performs Hierarchical Ascending Clustering by adding many interesting results
#'
#' @param dataset The data to analyse
#' @param center Boolean: should the consumer scores be centered (TRUE by default)
#' @param scale Boolean: should the consumer scores be standardized (TRUE by default)
#' @param scale.unit Boolean: should the PCA be standardized (FALSE by default)
#' @param consolidation Boolean: should the cluster solution obtained from HAC be consolidated using k-means (FALSE by default)
#' @param nbcl Integer: number of clusters to consider
#' @param graph Boolean: visualization of the dendrogram (TRUE by default)
#' @param comparison Boolean: compare the results before and after consolidation (FALSE by default)
#' @param stability Boolean: runs simulation to check for stability (FALSE by default)
#' @param NSim Integer: number of simulations to consider for the stability
#'
#' @returns The different results of the clustering analysis
#' @export
#'
#' @examples NULL
clustering <- function(dataset, center=TRUE, scale=TRUE, scale.unit=FALSE,
                       consolidation=FALSE, nbcl, graph=TRUE, comparison=FALSE,
                       stability=FALSE, NSim=100){

  if (!consolidation){
    comparison = FALSE
  }

  # Preparing the data
  data_clust <- dataset %>%
    dplyr::select(Judge, Product, Liking) %>%
    tidyr::pivot_wider(names_from=Judge, values_from=Liking) %>%
    as.data.frame() %>%
    tibble::column_to_rownames(var="Product") %>%
    scale(center=center, scale=scale) %>%
    t()

  # Clustering

    ## Run the PCA
  res_pca <- FactoMineR::PCA(data_clust, scale.unit=scale.unit, ncp=Inf, graph=FALSE)

  ## Perform the Cluster Analysis
  res_hcpc <- FactoMineR::HCPC(res_pca, consol=consolidation, nb.clust=nbcl, graph=FALSE)

  inertia <- tibble::tibble(inertia=res_hcpc$call$t$inert.gain) %>%
    dplyr::mutate(N1=row_number()) %>%
    dplyr::mutate(N2=N1+1) %>%
    dplyr::mutate(choice = ifelse(N2 <= nbcl, "yes", "no")) %>%
    dplyr::filter(N1 < 15) %>%
    tidyr::unite(N, N1, N2, sep="-")

  in_order <- unique(inertia$N)

  inertia <- inertia %>%
    dplyr::mutate(N = factor(N, levels=in_order)) %>%
    ggplot2::ggplot(aes(x=N, y=inertia, fill=choice))+
    ggplot2::geom_bar(stat="identity")+
    ggplot2::labs(x = "Level of Cutting", y = "Inertia")+
    ggplot2::scale_fill_manual(values = c("yes"="black", "no"="grey70"))+
    ggplot2::guides(fill="none")+
    ggplot2::ggtitle("Inertia")+
    ggplot2::theme_bw()

  # Exporting the Results
  clusters <- res_hcpc$data.clust %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var="Judge") %>%
    dplyr::mutate(Judge = as.character(Judge)) %>%
    dplyr::select(Judge, Cluster=clust)

  ## Creating the Graph
  if (graph){

    ### Order the Consumer in the same way as the Dendrogram
    oo <- res_hcpc$call$t$tree$order
    labels <- res_hcpc$call$t$tree$labels[oo]

    ### Find the Color Orientation for the Dendrogram
    clust_col <- clusters %>%
      dplyr::mutate(Judge = factor(Judge, levels=labels)) %>%
      dplyr::mutate(Cluster = as.numeric(as.character(Cluster))) %>%
      dplyr::arrange(Judge) %>%
      dplyr::mutate(RowNb = row_number()) %>%
      dplyr::group_by(Cluster) %>%
      dplyr::summarize(Med = median(RowNb)) %>%
      dplyr::arrange(Med) %>%
      dplyr::ungroup() %>%
      dplyr::select(Cluster) %>%
      unlist()

    ### Create the Dendrogram
    dendrogram <- factoextra::fviz_dend(res_hcpc, k_colors=clust_col, cex=0.5)
  } else {
    dendrogram <- NULL
  }

  # Comparison with/without consolidation
  if (comparison){

    ## Extracting the results with consolidation
    C_solution <- clusters

    ## Running HCPC without consolidation
    res_noconsol <- FactoMineR::HCPC(res_pca, consol=FALSE, nb.clust=nbcl, graph=FALSE)
    NC_solution <- res_noconsol$data.clust %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var="Judge") %>%
      dplyr::select(Judge, Cluster=clust)

    ## Ensuring that the Cluster names are identical
    comp_solution <- C_solution %>%
      dplyr::inner_join(NC_solution, by="Judge") %>%
      dplyr::select(Judge, Before=Cluster.y, After=Cluster.x)

    temp <- table(comp_solution$Before, comp_solution$After)
    oo1 <- order(apply(temp, 1, max), decreasing=TRUE)
    oo2 <- order(apply(temp, 2, max), decreasing=TRUE)
    temp <- temp[oo1,oo2]

    comp_solution <- comp_solution %>%
      dplyr::mutate(Before = factor(Before, levels=rownames(temp), labels=colnames(temp))) %>%
      dplyr::mutate(Before = as.character(Before), After = as.character(After)) %>%
      dplyr::mutate(Compare = ifelse(Before == After, "same", "different"))

    if (graph){

      ### Extract the order of the labels on the dendrogram, and reorder the labels accordingly
      oo <- res_hcpc$call$t$tree$order
      labels <- res_hcpc$call$t$tree$labels[oo]

      ### Apply the label ordering to the results of the clustering
      comp_solution <- comp_solution %>%
        dplyr::mutate(Judge = factor(Judge, levels=labels)) %>%
        dplyr::arrange(Judge) %>%
        dplyr::mutate(Before = as.numeric(Before), After = as.numeric(After))

      ### Build the dendrogram with the right colour code (after consolidation)
      dendrogram <- factoextra::fviz_dend(res_hcpc, label_cols = unlist(comp_solution$After),
                                          k_colors = unique(unlist(comp_solution$Before)), cex = 0.5)

    } else {
      dendrogram <- NULL
    }
  } else {
    comp_solution <- NULL
  }

  # Stability
  if (stability){

    data_cl <- dataset %>%
      dplyr::select(Judge, Product, Liking) %>%
      pivot_wider(names_from=Product, values_from=Liking) %>%
      as.data.frame() %>%
      tibble::column_to_rownames(var="Judge")
    data_cl <- t(scale(t(data_cl), center=TRUE, scale=TRUE))

    res_stability <- fpc::clusterboot(data_cl, clustermethod=HCPC_CBI, k=nbcl, consol=consolidation, scale.unit=TRUE, B=NSim)
    res_stab <- tibble::tibble(`Stability (%)` = round(100*res_stability$bootmean,1)) %>%
      dplyr::mutate(Cluster = as.character(row_number()))
  }

  # Export the Results

    ## Add the Cluster info to the Data
  dataset <- dataset %>%
    dplyr::mutate(Judge = as.character(Judge)) %>%
    dplyr::inner_join(clusters, by="Judge")

  N <- dataset %>%
    dplyr::select(Judge, Cluster) %>%
    unique() %>%
    dplyr::group_by(Cluster) %>%
    dplyr::count(name = "N") %>%
    dplyr::mutate(Cluster = as.character(Cluster)) %>%
    dplyr::ungroup()

  if (stability){
    N <- N %>%
      dplyr::inner_join(res_stab, by="Cluster")
  }

  ## Export
  res <- list(dataset=dataset, dendrogram=dendrogram, inertia=inertia, N=N, comparison=comp_solution)
  return(res)
}
