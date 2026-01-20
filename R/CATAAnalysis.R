#' CATA Analysis for Terms
#'
#' @param dataset The Data to analyze
#' @param var The CATA variables
#' @param liking The Liking variable
#' @param res_by 'All' or per 'Product'
#' @param prop Proportion set to 20
#'
#' @returns The results of the CATA Analysis
#' @export
#'
#' @examples NULL
CATAAnalysis <- function(dataset, var, liking, res_by="All", prop=20){

  ## CATA Penalty Plot
  .CATAPenaltyPlot <- function(data, threshold=0.2, title){

    p <- ggplot2::ggplot(data, aes(x=Proportion, y=Penalty, label=Variable, alpha=Signif, colour=Prop, fill=Prop))+
      ggplot2::geom_point(pch=20, cex=5)+
      ggrepel::geom_text_repel()+
      ggplot2::geom_hline(yintercept=0, lty=2, col="grey70")+
      ggplot2::geom_vline(xintercept=threshold, lty=2, col="grey70")+
      ggplot2::labs(x = "", y = "Penalty Score")+
      ggplot2::scale_x_continuous(breaks=seq(0,1,0.2), label=paste0(seq(0,100,20),"%"), limits=c(0,1))+
      ggplot2::scale_alpha_manual(values=c("Signif"=1, "NS"=0.3))+
      ggplot2::scale_colour_manual(values=c("High"="darkorange2", "Low"="black"))+
      ggplot2::scale_fill_manual(values=c("High"="darkorange2", "Low"="black"))+
      ggplot2::guides(alpha="none", colour="none", fill="none")+
      ggplot2::ggtitle(str_c("CATA Penalty Analysis for ", title),"(Terms greyed out have no significant penalty at 5%)")+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line.x = ggplot2::element_line(), axis.line.y = ggplot2::element_line())

    return(p)
  }

  res <- list()

  # Correspondence Analysis
  data_ca <- dataset %>%
    dplyr::select(Product, tidyselect::all_of(var)) %>%
    tidyr::pivot_longer(-Product, names_to=c("Type","CATA"), values_to="Responses", names_sep="_") %>%
    dplyr::select(-Type) %>%
    dplyr::mutate(Responses = as.numeric(Responses)) %>%
    dplyr::group_by(Product, CATA) %>%
    dplyr::summarize(Sum = sum(Responses)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from=CATA, values_from=Sum)

  if (nrow(data_ca) >= 4){
    res_ca <- data_ca %>%
      as.data.frame() %>%
      tibble::column_to_rownames(var="Product") %>%
      FactoMineR::CA(., graph=FALSE)

    p <- factoextra::fviz_ca(res_ca)

  } else {
    p <- NULL
  }

  res$CA <- p

  # CATA Penalty
  if (res_by == "All"){

    res_penalty <- dataset %>%
      dplyr::select(Judge, Product, Liking = tidyselect::all_of(liking), tidyselect::all_of(var)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var), names_to=c("Type","Variable"), values_to="Ticks", names_sep="_") %>%
      dplyr::select(-Type) %>%
      dplyr::mutate(Liking = as.numeric(Liking), Ticks = as.character(Ticks), Variable = forcats::fct_inorder(Variable)) %>%
      split(.$Variable) %>%
      purrr::map(function(data){

        prop <- data %>%
          dplyr::summarize(Proportion = mean(as.numeric(Ticks))) %>%
          dplyr::mutate(term = "Ticks1")

        if (length(unique(data$Ticks)) > 1){
          res <- broom::tidy(summary(lm(Liking ~ Ticks, data=data)))
        } else {
          res <- tibble::tibble(term="(Intercept)", estimate=mean(data$Liking))
        }

        res <- left_join(res, prop, by="term")

        return(res)
      }) %>%
      tibble::enframe(name="Variable", value="res") %>%
      tidyr::unnest(res) %>%
      dplyr::filter(term == "Ticks1") %>%
      dplyr::select(Variable, Penalty = estimate, Proportion, Pvalue=p.value) %>%
      dplyr::mutate(Signif = ifelse(Pvalue <= 0.05, "Signif.", "NS")) %>%
      dplyr::mutate(Prop = ifelse(Proportion >= prop/100, "High","Low"))

    p <- .CATAPenaltyPlot(res_penalty, threshold=prop/100, title="all Samples")

    res$Penalty <- list(All=res_penalty)
    res$PenaltyPlot <- list(All=p)

  } else if (res_by == "Product"){

    res_penalty <- dataset %>%
      dplyr::select(Judge, Product, Liking=tidyselect::all_of(liking), tidyselect::all_of(var)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var), names_to=c("Type","Variable"), values_to="Ticks", names_sep="_") %>%
      dplyr::select(-Type) %>%
      dplyr::mutate(Liking = as.numeric(Liking), Ticks = as.character(Ticks), Variable = forcats::fct_inorder(Variable)) %>%
      split(.$Product) %>%
      purrr::map(function(data){
        data %>%
          split(.$Variable) %>%
          purrr::map(function(data){
            prop <- data %>%
              dplyr::summarize(Proportion = mean(as.numeric(Ticks))) %>%
              dplyr::mutate(term = "Ticks1")

            if (length(unique(data$Ticks)) > 1){
              res <- broom::tidy(summary(lm(Liking ~ Ticks, data=data)))
            } else {
              res <- tibble(term="(Intercept)", estimate=mean(data$Liking))
            }

            res <- left_join(res, prop, by="term")

            return(res)
          }) %>%
          tibble::enframe(name="Variable", value="res") %>%
          tidyr::unnest(res) %>%
          dplyr::filter(term == "Ticks1") %>%
          dplyr::select(Variable, Penalty=estimate, Proportion, Pvalue=p.value) %>%
          dplyr::mutate(Signif = ifelse(Pvalue <= 0.05, "Signif.", "NS")) %>%
          dplyr::mutate(Prop = ifelse(Proportion >= prop/100, "High","Low"))

      })

    p <- purrr::map2(.x=res_penalty, .y=names(res_penalty), .f=.CATAPenaltyPlot, threshold=prop/100)

    res$Penalty = res_penalty
    res$PenaltyPlot = p
  }

  return(res)
}
