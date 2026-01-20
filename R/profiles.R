#' Cluster Profile Visualization
#'
#' Generates the cluster profiles as a line-charts
#'
#' @param dataset The data to be represented
#'
#' @returns The graphic visualizing the cluster profiles (in terms of preference) across samples
#' @export
#'
#' @examples NULL
profiles <- function(dataset){

  n_data <- dataset %>%
    dplyr::select(Judge, Cluster) %>%
    unique() %>%
    dplyr::group_by(Cluster) %>%
    dplyr::count() %>%
    dplyr::rename(N = n)

  mean_data <- dataset %>%
    dplyr::filter(Cluster != "0") %>%
    dplyr::group_by(Product, Cluster) %>%
    dplyr::summarize(Mean = mean(Liking)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(n_data, by="Cluster")

  mean_table <- mean_data %>%
    dplyr::mutate(Mean = round(Mean, 2)) %>%
    tidyr::pivot_wider(names_from=Product, values_from=Mean)

  mean_data <- mean_data %>%
    dplyr::mutate(N = paste0("(",N,")")) %>%
    tidyr::unite(Cluster, Cluster, N, sep=" ")

  nbcl <- nlevels(factor(mean_data$Cluster))

  mean_plot <- ggplot2::ggplot(mean_data, ggplot2::aes(x=Product, y=Mean, color=Cluster, group=Cluster)) +
    ggplot2::geom_line(linewidth=1.2)+
    ggplot2::scale_x_discrete(name="", guide=ggplot2::guide_axis(n.dodge=2))+
    ggplot2::scale_y_continuous("Average Liking Score", breaks=seq(1,9,1), limits=c(1,9))+
    ggplot2::scale_color_manual(values=1:nbcl)+
    ggplot2::ggtitle("Clusters' Liking Profiles")+
    ggplot2::theme_bw()

  res <- list(table=mean_table, plot=mean_plot)
  return(res)
}
