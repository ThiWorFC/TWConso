#' Cluster Trimming
#'
#' The cluster results can be trimmed by removing consumers who are not correlating well with their cluster means.
#'
#' @param dataset The data to be trimmed
#' @param min_cor Value: the minimum correlation to consider to keep the consumer in the analysis (use -1 to keep all consumers)
#'
#' @returns The data after trimming
#' @export
#'
#' @examples NULL
trimming <- function(dataset, min_cor=0.2){

  nbcl <- dataset %>%
    dplyr::pull(Cluster) %>%
    unique() %>%
    length()

  # Computing the Mean per Cluster
  clust_mean <- dataset %>%
    dplyr::select(Product, Liking, Cluster) %>%
    dplyr::mutate(Text = "Cluster") %>%
    tidyr::unite(Cluster, Text, Cluster, sep=" ") %>%
    dplyr::arrange(Cluster) %>%
    tidyr::pivot_wider(names_from=Cluster, values_from=Liking, values_fn=mean)

  # Performing the Trimming
  res_trim <- dataset %>%
    dplyr::mutate(Cluster = as.character(Cluster)) %>%
    split(.$Cluster) %>%
    purrr::map(function(data){

      cl <- data %>%
        dplyr::pull(Cluster) %>%
        unique()

      data_cl <- data %>%
        dplyr::select(Judge, Product, Liking) %>%
        tidyr::pivot_wider(names_from=Judge, values_from=Liking) %>%
        as.data.frame() %>%
        tibble::column_to_rownames(var="Product")

      mean_cl <- clust_mean %>%
        dplyr::select(Product, tidyselect::all_of(paste("Cluster", cl))) %>%
        as.data.frame() %>%
        tibble::column_to_rownames(var="Product")

      res <- cor(data_cl, mean_cl[rownames(data_cl),]) %>%
        tibble::as_tibble(rownames = "Judge") %>%
        dplyr::mutate(Cluster = as.character(cl)) %>%
        dplyr::rename(Correlation = V1)

      return(res)
    }) %>%
    purrr::reduce(bind_rows)

  # Cleaning the Data
  newdata <- dataset %>%
    dplyr::inner_join(res_trim, by=c("Judge","Cluster")) %>%
    dplyr::mutate(Trimming = ifelse(Correlation >= min_cor, Cluster, "0"))

  clust_trim_sum <- newdata %>%
    dplyr::select(Judge, Cluster, Trimming) %>%
    unique() %>%
    dplyr::group_by(Trimming) %>%
    dplyr::count(Cluster) %>%
    tidyr::pivot_wider(names_from=Trimming, values_from=n, values_fill=0)

  newdata <- newdata %>%
    dplyr::select(Judge, Product, Sequence, Liking, Cluster=Trimming)

  # Distribution of the Correlation
  density_plot <- ggplot2::ggplot(res_trim, ggplot2::aes(x=Correlation, color=Cluster))+
    ggplot2::geom_density(lwd=1.2, show.legend=FALSE)+
    ggplot2::stat_density(geom="line", position="identity", lwd=1.2)+
    geom_vline(xintercept=min_cor, lty=2, color="firebrick2")+
    ggplot2::ggtitle("Distribution of the Correlation between Consumers and their Cluster Means",
            paste0("(Consumers with a correlation lower than ", min_cor, " are being trimmed)"))+
    ggplot2::scale_x_continuous(name="Correlation", limit=c(-1,1), breaks=seq(-1,1,0.1))+
    ggplot2::labs(y = "")+
    ggplot2::theme_bw()

  if (nbcl == 2){
    density_plot = density_plot+
      ggplot2::scale_color_manual(values=c("1"=palette()[1], "2"=palette()[2]))
  } else if (nbcl == 3){
    density_plot = density_plot+
      ggplot2::scale_color_manual(values=c("1"=palette()[1], "2"=palette()[2], "3"=palette()[3]))
  } else if (nbcl == 4){
    density_plot = density_plot+
      ggplot2::scale_color_manual(values=c("1"=palette()[1], "2"=palette()[2], "3"=palette()[3], "4"=palette()[4]))
  } else if (nbcl == 5){
    density_plot = density_plot+
      ggplot2::scale_color_manual(values=c("1"=palette()[1], "2"=palette()[2], "3"=palette()[3], "4"=palette()[4], "5"=palette()[5]))
  }

  res_trim <- res_trim %>%
    dplyr::mutate(Action = ifelse(Correlation >= min_cor, "Keep", "Trim"))

  # Export of the Results
  res <- list(dataset = newdata, trimming = res_trim, density = density_plot, summary = clust_trim_sum)
  return(res)
}
