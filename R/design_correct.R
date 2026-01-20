#' Correct for the Experimental Design
#'
#' Correct the liking scores based on their position in the design
#'
#' @param dataset The data table to analyse
#' @param pval A p-value threshold to consider for significance (0.05 by default)
#' @param correct Boolean: FALSE by default, controls whether the liking score should be corrected.
#' @param seq_correct Vector: define which positions should be corrected (if correct = TRUE)
#'
#' @returns The data after correction (if asked)
#' @export
#'
#' @examples NULL
design_correct <- function(dataset, pval=0.05, correct=FALSE, seq_correct=NULL){

  max_seq <- max(dataset$Sequence)
  dataset <- dataset %>%
    dplyr::mutate(Sequence = factor(Sequence, levels=1:max_seq))

  # Checking the Need of Correcting the Design

    ## Compute the mean per presentation order
  mean_design <- dataset %>%
    dplyr::group_by(Sequence) %>%
    dplyr::summarize(Mean = mean(Liking))

  ## Test for Significance
  test_design <- FactoMineR::AovSum(Liking ~ Sequence, data=dataset)$Ttest %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var="Effect") %>%
    dplyr::filter(Effect != "(Intercept)") %>%
    tidyr::separate(Effect, c("Effect", "Sequence"), sep=" - ") %>%
    dplyr::select(Sequence, Estimate, Pvalue = `Pr(>|t|)`) %>%
    dplyr::mutate(Signif = dplyr::case_when(
      Pvalue > pval ~ "NS",
      Estimate > 0 ~ "Signif. more",
      .default = "Signif. less"))

  ## Combining the Results
  data_design <- dplyr::full_join(mean_design, test_design, by="Sequence") %>%
    dplyr::mutate(Sequence = factor(Sequence, levels=1:max_seq)) %>%
    dplyr::arrange(Sequence)

  ## Create the Plot
  plot_design <- ggplot2::ggplot(data_design, ggplot2::aes(x=Sequence, y=Mean, fill=Signif))+
    ggplot2::geom_bar(stat="identity")+
    ggplot2::labs(x = "")+
    ggplot2::scale_fill_manual(values = c("NS"="grey80", "Signif. more"="firebrick2", "Signif. less"="steelblue2"))+
    ggplot2::ggtitle("Average Liking Score per Presentation Order")+
    ggplot2::theme_bw()

  # Correcting the Design
  if (correct){
    val_correct <- data_design %>%
      dplyr::mutate(Multiplier = ifelse(Sequence %in% seq_correct, 1, 0)) %>%
      dplyr::mutate(Difference = Estimate*Multiplier) %>%
      dplyr::select(Sequence, Difference)

    dataset <- dataset %>%
      dplyr::inner_join(val_correct, by="Sequence") %>%
      dplyr::mutate(Liking = Liking - Difference) %>%
      dplyr::select(-Difference)
  }

  # Exporting the Results
  dataset <- dataset %>%
    dplyr::mutate(Sequence = as.numeric(as.character(Sequence)))

  test_design <- test_design %>%
    dplyr::mutate(Estimate = round(Estimate, 2)) %>%
    dplyr::mutate(Pvalue = round(Pvalue, 4))

  res <- list(dataset = dataset, design = plot_design, AOV = test_design)
  return(res)
}
