#' Analysis of CATA Data
#'
#' Perform analysis on CATA Data by also comparing different sets of CATA list (e.g. product vs. concept)
#'
#' @param dataset The data to analyze
#' @param var1 First set of variables
#' @param var2 Second set of variables
#' @param comp What should be compared: 'Type' or 'Product'
#' @param paircomp Include pair comparison ('Yes' or 'No')
#'
#' @returns The results of the CATA comparison analysis
#' @export
#'
#' @examples NULL
CATAComp <- function(dataset, var1, var2, comp, paircomp="Yes"){

  ## Plot for Type
  .AssocType_plot <- function(data, title){

    colour_sig <- ifelse(data$Pvalue <= 0.05, "red", "black")

    p <- ggplot2::ggplot(data, aes(x = forcats::fct_rev(CATA), y = Sum, fill = Type, label = groups))+
      ggplot2::geom_col(position = "dodge")+
      ggplot2::geom_text(aes(x=CATA, y=Sum+0.8), position = ggplot2::position_dodge(width=1), cex=3)+
      ggplot2::labs(x = "", y = "")+
      ggplot2::scale_y_continuous(breaks = seq(0,100,20), labels = paste0(seq(0,100,20),"%"), limits = c(0,100))+
      ggplot2::ggtitle(stringr::str_c("Concept vs. Product for ", title))+
      ggplot2::guides(fill = ggplot2::guide_legend(nrow=1, reverse=TRUE))+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(), axis.text.y = ggplot2::element_text(colour=rev(colour_sig)),
            legend.position = "bottom", legend.box = "horizontal", legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size=8))+
      ggplot2::coord_flip()

    return(p)

  }

  ## Plot for Product
  .AssocProd_plot <- function(data, title){

    colour_sig <- ifelse(data$Pvalue <= 0.05, "red", "black")

    p <- ggplot2::ggplot(data, aes(x = forcats::fct_rev(CATA), y = Sum, fill = Product, label = groups))+
      ggplot2::geom_col(position = "dodge")+
      ggplot2::geom_text(aes(x=CATA, y=Sum+0.8), position = ggplot2::position_dodge(width=1), cex=3)+
      ggplot2::labs(x = "", y = "")+
      ggplot2::scale_y_continuous(breaks=seq(0,100,20), labels=paste0(seq(0,100,20),"%"), limits=c(0,100))+
      ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse=TRUE))+
      ggplot2::ggtitle(stringr::str_c("Product Comparison for ", title))+
      ggplot2::guides(fill = ggplot2::guide_legend(nrow=1, reverse=TRUE))+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(), axis.text.y = ggplot2::element_text(colour=rev(colour_sig)),
            legend.position = "bottom", legend.box = "horizontal", legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size=8))+
      ggplot2::coord_flip()

    return(p)
  }

  ## CATA test
  .CATAtest <- function(data, effect="Product", paircomp="Yes"){

    data <- data %>%
      dplyr::rename(Effect = tidyselect::all_of(effect))

    nbeffect_j <- data %>%
      dplyr::select(Judge, Effect) %>%
      unique() %>%
      dplyr::group_by(Judge) %>%
      dplyr::summarize(N = n()) %>%
      dplyr::ungroup() %>%
      dplyr::pull(N) %>%
      min()

    nbprod <- data %>%
      dplyr::pull(Effect) %>%
      unique() %>%
      length(.)

    # Run the ANOVA
    if (nbeffect_j > 1){
      formul="Responses ~ Effect + Judge"
    } else {
      formul="Responses ~ Effect"
    }

    if (!is.na(sd(data$Responses, na.rm=TRUE)) && sd(data$Responses, na.rm=TRUE) > 0 && nbprod > 1){
      res_aov <- aov(as.formula(formul), data=data)

      if (paircomp == "Yes"){

        res_hsd <- agricolae::HSD.test(res_aov, "Effect", group=TRUE)$groups %>%
          tibble::as_tibble(rownames=effect) %>%
          dplyr::rename(Mean = Responses)

      } else {

        res_hsd <- data %>%
          dplyr::group_by(Effect) %>%
          dplyr::summarize(Mean = mean(Responses)) %>%
          dplyr::mutate(groups = "")

        colnames(res_hsd)[1] <- effect
      }

      res_aov <- broom::tidy(res_aov) %>%
        dplyr:: filter(term == "Effect") %>%
        dplyr::select(Effect=term, df, Fvalue=statistic, Pvalue=p.value) %>%
        dplyr::mutate(Effect = stringr::str_replace(Effect, "Effect", effect))

    } else {

      res_aov <- tibble::tibble(Effect="Product", df=NA, Fvalue=NA, Pvalue=NA)
      res_hsd <- data %>%
        dplyr::rename(Product = Effect) %>%
        dplyr::group_by(Product) %>%
        dplyr::summarize(Mean = mean(Responses)) %>%
        dplyr::mutate(groups = "")
    }

    res <- list(ANOVA = res_aov, Mean = res_hsd)

    return(res)
  }

  prod_order <- levels(dataset$Product)

  # Compute the counts
  counts <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(var1), tidyselect::all_of(var2)) %>%
    tidyr::pivot_longer(-c(Judge, Product), names_to=c("Type","CATA"), values_to="Responses", names_sep="_") %>%
    dplyr::mutate(CATA = forcats::fct_inorder(CATA), Responses = as.numeric(Responses)) %>%
    dplyr::group_by(Product, Type, CATA) %>%
    dplyr::summarize(Sum = 100 * mean(Responses), Total = n()) %>%
    dplyr::mutate(Label = scales::percent(Sum/100, accuracy=1)) %>%
    dplyr::ungroup()

  # Running the ANOVA
  res <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(var1), tidyselect::all_of(var2)) %>%
    tidyr::pivot_longer(-c(Judge, Product), names_to=c("Type","CATA"), values_to="Responses", names_sep="_") %>%
    dplyr::mutate(CATA = forcats::fct_inorder(CATA), Responses = as.numeric(Responses))

  if (comp == "Type"){

    res <- res %>%
      split(.$Product) %>%
      purrr::map(function(data){
        res <- data %>%
          split(.$CATA) %>%
          purrr::map(.CATAtest, effect=comp, paircomp=paircomp) %>%
          tibble::enframe(name="CATA", value="res") %>%
          tidyr::unnest(res) %>%
          tidyr::unnest(res)
      })

  } else if (comp == "Product"){

    res <- res %>%
      split(.$Type) %>%
      purrr::map(function(data){
        res <- data %>%
          split(.$CATA) %>%
          purrr::map(.CATAtest, effect=comp, paircomp=paircomp) %>%
          tibble::enframe(name="CATA", value="res") %>%
          tidyr::unnest(res) %>%
          tidyr::unnest(res)
      })
  }

  res_aov <- res %>%
    purrr::map(function(data){
      data %>%
        dplyr::select(CATA, Effect, df, Fvalue, Pvalue) %>%
        dplyr::filter(!is.na(Effect))
    })

  res_mean <- res %>%
    purrr::map(function(data){
      data %>%
        dplyr::select(CATA, tidyselect::all_of(comp), Mean, groups) %>%
        dplyr::filter(!is.na(Mean))
    })

  if (comp == "Type"){

    res_mean <- res_mean %>%
      tibble::enframe(name = "Product", value = "res") %>%
      tidyr::unnest(res)

    res_aov2 <- res_aov %>%
      tibble::enframe(name = "Product", value = "res") %>%
      tidyr::unnest(res) %>%
      dplyr::select(Product, CATA, Pvalue)

  } else if (comp == "Product"){

    res_mean <- res_mean %>%
      tibble::enframe(name = "Type", value = "res") %>%
      tidyr::unnest(res)

    res_aov2 <- res_aov %>%
      tibble::enframe(name = "Type", value = "res") %>%
      tidyr::unnest(res) %>%
      dplyr::select(Type, CATA, Pvalue)

  }
  res_aov2 <- res_aov2 %>%
    dplyr::mutate(Pvalue = ifelse(is.na(Pvalue), 1, Pvalue))

  # Finalise the Counts
  if (comp == "Type"){

    res_count <- counts %>%
      dplyr::full_join(res_mean, by=c("Product","Type", "CATA")) %>%
      dplyr::rename(PProduct = Product) %>%
      dplyr::mutate(Label = paste0(Label, "<sup>",groups,"</sup>")) %>%
      dplyr::select(-Sum, -Total, -Mean, -groups) %>%
      tidyr::pivot_wider(names_from=Type, values_from=Label) %>%
      split(.$PProduct) %>%
      purrr::map(function(data){
        data %>%
          dplyr::select(-PProduct)
      })

    data_plot <- counts %>%
      dplyr::full_join(res_mean, by=c("Product","Type", "CATA")) %>%
      dplyr::full_join(res_aov2, by=c("Product","CATA")) %>%
      dplyr::mutate(CATA = forcats::fct_inorder(CATA)) %>%
      split(.$Product)

    res_plot <- purrr::map2(data_plot, names(data_plot), .f=.AssocType_plot)

  } else if (comp == "Product"){

    res_count <- counts %>%
      dplyr::full_join(res_mean, by=c("Product","Type", "CATA")) %>%
      dplyr::mutate(Label = paste0(Label, "<sup>",groups,"</sup>")) %>%
      dplyr::select(-Sum, -Total, -Mean, -groups) %>%
      tidyr::pivot_wider(names_from=Product, values_from=Label) %>%
      split(.$Type) %>%
      purrr::map(function(data){
        data %>%
          dplyr::select(-Type)
      })

    data_plot <- counts %>%
      dplyr::full_join(res_mean, by=c("Product","Type", "CATA")) %>%
      dplyr::full_join(res_aov2, by=c("Type","CATA")) %>%
      dplyr::mutate(Product = factor(Product, levels=prod_order), CATA = forcats::fct_inorder(CATA)) %>%
      dplyr::mutate(Product = forcats::fct_rev(Product)) %>%
      split(.$Type)

    res_plot <- purrr::map2(data_plot, names(data_plot), .f=.AssocProd_plot)
  }

  res <- list(Count=res_count, Plot=res_plot, ANOVA=res_aov)

  return(res)
}
