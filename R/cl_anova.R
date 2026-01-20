#' ANOVA per cluster
#'
#' Run the ANOVA with post'hoc analysis for each cluster separately
#'
#' @param dataset The data to be analyzed
#' @param test The post'hoc test to be used ('LSD' or 'HSD')
#'
#' @returns The ANOVA and post'hoc test results
#' @export
#'
#' @examples NULL
cl_ANOVA <- function(dataset, test="HSD"){

  dataset <- dataset %>%
    dplyr::filter(Cluster != "0")

  # Overall ANOVA
  aov_overall <- broom::tidy(anova(aov(Liking ~ Product*Cluster, data=dataset))) %>%
    dplyr::mutate(dplyr::across(c("sumsq","meansq","statistic"), \(x) round(x, 2))) %>%
    dplyr::mutate(p.value = round(p.value,3))

  # Results per Cluster
  ANOVAperCl <- function(data, test="HSD"){
    res_aov <- aov(Liking ~ Product + Judge, data=data)
    aov_clust <- broom::tidy(anova(res_aov)) %>%
      dplyr::mutate(dplyr::across(c("sumsq","meansq","statistic"), \(x) round(x, 2))) %>%
      dplyr::mutate(p.value = round(p.value,3))

    if (test == "HSD"){

      aov_posthoc = agricolae::HSD.test(res_aov, "Product")$groups %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var="Product") %>%
        dplyr::mutate(Liking = round(Liking,2))

    } else if (test == "LSD"){

      aov_posthoc = agricolae::LSD.test(res_aov, "Product")$groups %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var="Product") %>%
        dplyr::mutate(Liking = round(Liking,2))

    }

    res <- list(ANOVA=aov_clust, PostHoc=aov_posthoc)
  }

  aov_cluster <- dataset %>%
    split(.$Cluster) %>%
    purrr::map(ANOVAperCl, test=test)

  res <- list(overall=aov_overall, cluster=aov_cluster)
  return(res)

}
