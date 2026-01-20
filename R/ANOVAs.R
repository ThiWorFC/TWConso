#' Perform ANOVAs
#'
#' Run 2-way ANOVA and post'hoc test
#'
#' @param dataset The data to analyze
#' @param var Set of variables to analyze
#' @param test Post'hoc test: 'HSD' or 'LSD'
#'
#' @returns The ANOVA tables and mean tables.
#' @export
#'
#' @examples NULL
ANOVAs <- function(dataset, var, test){

  # ANOVA
  if (median(table(dataset$Judge)) > 1){
    formula = "Scores ~ Product + Judge"
  } else {
    formula = "Scores ~ Product"
  }

  res <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(var)) %>%
    tidyr::pivot_longer(-c(Judge, Product), names_to="Variables", values_to="Scores") %>%
    dplyr::mutate(Variables = fct_inorder(Variables), Scores = as.numeric(Scores)) %>%
    dplyr::nest_by(Variables) %>%
    dplyr::mutate(mod = list(aov(as.formula(formula), data=data)))

  # ANOVA Results
  res_aov <- res %>%
    dplyr::reframe(broom::tidy(mod)) %>%
    dplyr::select(Variables, Effect=term, df, Fvalue=statistic, Pvalue=p.value) %>%
    dplyr::ungroup() %>%
    split(.$Variables)

  # Mean and Post'hoc Test
  if (test == "HSD"){
    res_means <- res %>%
      dplyr::reframe(HSD.test(mod, "Product")$groups %>%
                tibble::as_tibble(rownames="Product") %>%
                dplyr::mutate(Scores = format(Scores, digits=2, nsmall=1))) %>%
      dplyr::ungroup()
  } else {
    res_means <- res %>%
      dplyr::reframe(LSD.test(mod, "Product")$groups %>%
                tibble::as_tibble(rownames="Product") %>%
                dplyr::mutate(Scores = format(Scores, digits=2, nsmall=1))) %>%
      dplyr::ungroup()

    if (test == "No"){
      res_means <- res_means %>%
        dplyr::select(-groups)
    }
  }

  # Summary
  res_sum <- res_aov %>%
    tibble::enframe(name="Vars", value="res") %>%
    tidyr::unnest(res) %>%
    dplyr::select(-Vars) %>%
    dplyr::filter(Effect == "Product") %>%
    dplyr::select(-df)

  res_sum_plot <- res_sum %>%
    dplyr::mutate(Signif = ifelse(Pvalue <=0.05, 1, 0.3)) %>%
    ggplot2::ggplot(aes(x = reorder(Variables, Fvalue), y = Fvalue, alpha = Signif))+
    ggplot2::geom_col()+
    ggplot2::scale_alpha_continuous(limits=c(0,1))+
    ggplot2::guides(alpha="none")+
    ggplot2::labs(x = "", y = "F-value")+
    ggplot2::ggtitle("F-value (Product) from the ANOVA","(Transparent bars are not significant at 5%)")+
    ggplot2::theme_bw()+
    ggplot2::coord_flip()

  # Export
  res <- list(ANOVA=res_aov, Mean=res_means, Summary=res_sum, Plot=res_sum_plot)

  return(res)
}
