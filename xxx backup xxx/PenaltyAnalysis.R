#' Analysis of JAR Data
#'
#' Analysis of JAR Data including Frequency and Penalty Analysis
#'
#' @param dataset The Data to analyze
#' @param var The JAR variables
#' @param liking The Liking variable
#' @param prop The proportion for the JAR variables
#' @param comp Comparison by 'Attribute' or 'Product'
#' @param size Control the size of the text on x-axis label
#' @param split Split the results by 'Type' ('Yes'/'No')
#'
#' @returns The results of the penalty analysis
#' @export
#'
#' @examples NULL
PenaltyAnalysis <- function(dataset, var, liking, prop=0.2, comp="Product", size=8, split="Yes"){

  ## JAR Frequency Plot
  .JARFreqPlot <- function(data, title, comp="Product", size=8){

    data <- data %>%
      dplyr::mutate(Face = ifelse(JAR == "JAR", "bold", "plain"))

    if (comp == "Product"){
      p <- ggplot2::ggplot(data, aes(x=Variables, y=Proportion, fill=JAR, label=Label, colour=Face, fontface=Face))
    } else if (comp == "Attribute"){
      p <- ggplot2::ggplot(data, aes(x=fct_rev(Product), y=Proportion, fill=JAR, label=Label, colour=Face, fontface=Face))
    }

    p = p+
      ggplot2::geom_col(position = ggplot2::position_fill(reverse=TRUE), colour="grey90")+
      ggplot2::geom_text(size=3, position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE))+
      ggplot2::labs(x = "", y = "")+
      ggplot2::scale_y_continuous(breaks=seq(0,1,0.2), label=paste0(seq(0,100,20),"%"), limits=c(0,1))+
      ggplot2::scale_fill_manual(values = c("Too Little"="#619CFF", "JAR"="#00BA38", "Too Much"="#F8766D"))+
      ggplot2::scale_colour_manual(values = c("plain"="black", "bold"="white"))+
      ggplot2::ggtitle(str_c("JAR Frequency for ", title))+
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1), colour = "none")+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(), axis.text = ggplot2::element_text(size = size),
                     legend.position = "bottom", legend.box = "horizontal", legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size=8))+
      coord_flip()

    return(p)
  }

  ## JAR Penalty Plot
  .PenaltyPlot <- function(data, prop, title){

    p <- ggplot2::ggplot(data %>% dplyr::filter(JAR != "JAR"), aes(x=Proportion, y=-1*Penalty, label=Variables, colour=JAR, alpha=Signif))+
      ggplot2::geom_point(aes(size=-1*`Weighted Penalty`), pch=20)+
      ggrepel::geom_text_repel()+
      ggplot2::labs(x = "", y = "Penalty Score")+
      ggplot2::scale_x_continuous(name="", breaks=seq(0,1,0.2), labels=paste0(seq(0,100,20),"%"), limits=c(0,1))+
      ggplot2::geom_hline(yintercept=0, lty=2, col="grey70")+
      ggplot2::geom_vline(xintercept=prop, lty=2, col="grey70")+
      ggplot2::scale_alpha_manual(values = c("Signif."=1, "NS"=0.5))+
      ggplot2::ggtitle(str_c("Penalty Analysis for ", title),"(Terms greyed out are not significant at 5%; Bubbles' sizes based on Weighted Penalty)")+
      ggplot2::guides(colour="none", alpha="none", size="none")+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line())

    return(p)
  }


  # Preparing the Data
  if (split == "Yes"){

    JAR_data <- dataset %>%
      dplyr::select(Judge, Product, Liking = tidyselect::all_of(liking), tidyselect::all_of(var)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var), names_to=c("Type", "Variables"), values_to="Scores", names_sep="_") %>%
      dplyr::select(-Type) %>%
      dplyr::mutate(Variables = forcats::fct_inorder(Variables), dplyr::across(c("Liking","Scores"), as.numeric)) %>%
      dplyr::mutate(JAR = dplyr::case_when(
        Scores == 3 ~ "JAR",
        Scores < 3 ~ "Too Little",
        .default = "Too Much"))

  } else {

    JAR_data <- dataset %>%
      dplyr::select(Judge, Product, Liking = tidyselect::all_of(liking), tidyselect::all_of(var)) %>%
      tidyr::pivot_longer(tidyselect::all_of(var), names_to="Variables", values_to="Scores") %>%
      dplyr::mutate(Variables = forcats::fct_inorder(Variables), dplyr::across(c("Liking","Scores"), as.numeric)) %>%
      dplyr::mutate(JAR = dplyr::case_when(
        Scores == 3 ~ "JAR",
        Scores < 3 ~ "Too Little",
        .default = "Too Much"))
  }

  # Frequency
  JAR_freq <- JAR_data %>%
    dplyr::mutate(JAR = factor(JAR, levels=c("Too Little", "JAR", "Too Much"))) %>%
    dplyr::select(-c(Liking)) %>%
    dplyr::group_by(Product, Variables) %>%
    dplyr::count(JAR, .drop=FALSE) %>%
    dplyr::mutate(Proportion = prop.table(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Label = scales::percent(Proportion, accuracy=1)) %>%
    dplyr::mutate(Label = ifelse(Label == "0%", "", Label)) %>%
    dplyr::select(-n)

  if (comp == "Product"){

    p1 <- JAR_freq %>%
      split(.$Product) %>%
      purrr::map2(.x=., .y=names(.), .f=.JARFreqPlot, size=size, comp="Product")

  } else if (comp == "Attribute"){

    p1 <- JAR_freq %>%
      split(.$Variables) %>%
      purrr::map2(.x=., .y=names(.), .f=.JARFreqPlot, size=size, comp="Attribute")

  }

  # Testing the Difference between JAR levels (dplyr::across products)
  if (JAR_data %>% pull(Product) %>% unique %>% length > 1){

    JAR_test_signif <- JAR_data %>%
      dplyr::mutate(NewScore = ifelse(Scores == 3, 5, 1)) %>%
      split(.$Variables) %>%
      purrr::map(function(data){
        Frequencies(data, var="NewScore", labels=NULL, test="Yes", topn="Top1")$Top[[1]] %>%
          dplyr::filter(Responses == "Top") %>%
          dplyr::select(-Variables, -Max, -Responses, -N)
      }) %>%
      tibble::enframe(name="Variables", value="res") %>%
      tidyr::unnest(res)

  } else {
    JAR_test_signif <- NULL
  }

  # Clean the Data
  var2rmv <- JAR_freq %>%
    dplyr::filter(Proportion == 1 | c(JAR == "JAR" & Proportion == 0)) %>%
    dplyr::select(Product, Variables)

  if (nrow(var2rmv) >= 1){
    JAR_data <- JAR_data %>%
      dplyr::anti_join(var2rmv, by=c("Product", "Variables"))
  }

  # Penalty Analysis
  prod_order <- levels(JAR_data$Product)

  JAR_penalty <- JAR_data %>%
    dplyr::mutate(JAR = factor(JAR, levels=c("JAR", "Too Little", "Too Much"))) %>%
    split(.$Product) %>%
    purrr::map(function(data){
      data %>%
        dplyr::nest_by(Variables) %>%
        dplyr::mutate(mod = list(lm(Liking ~ JAR, data=data))) %>%
        dplyr::reframe(broom::tidy(summary(mod))) %>%
        dplyr::ungroup() %>%
        dplyr::select(Variables, JAR=term, Penalty=estimate, Pvalue=p.value) %>%
        dplyr::mutate(JAR = stringr::str_remove(JAR, "JAR")) %>%
        dplyr::mutate(JAR = stringr::str_replace(JAR, "(\\(Intercept\\))", "JAR"))
    }) %>%
    tibble::enframe(name = "Product", value = "res") %>%
    tidyr::unnest(res) %>%
    dplyr::mutate(Product = factor(Product, levels=prod_order)) %>%
    dplyr::full_join(JAR_freq, by=c("Product", "Variables", "JAR")) %>%
    dplyr::relocate(Proportion, .after=JAR) %>%
    dplyr::mutate(Signif = ifelse(Pvalue <= 0.05, "Signif.", "NS"),
           `Weighted Penalty` = Proportion * Penalty) %>%
    split(.$Product)

  p <- purrr::map2(.x=JAR_penalty, .y=names(JAR_penalty), .f=.PenaltyPlot, prop=prop)

  res <- list(Penalty = JAR_penalty, PenaltyPlot = p, FreqPlot = p1, ProdComp = JAR_test_signif)

  return(res)
}
