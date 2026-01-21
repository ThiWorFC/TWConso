#' Frequency Tables
#'
#' Compute the frequency tables with topboxes for a set of variables
#'
#' @param dataset The data to analyze
#' @param var The variable to analyze
#' @param labels The labels attached to the levels for that variable
#' @param test 'yes' or 'no' for significance test
#' @param topn Compute topbox (e.g. 'Top3')
#'
#' @returns The results for the frequency analysis
#' @export
#'
#' @examples NULL
Frequencies <- function(dataset, var, labels=NULL, test="No", topn="Top3"){

  if (is.null(labels)){

    labels <- dataset %>%
      dplyr::select(Product, tidyselect::all_of(var)) %>%
      tidyr::pivot_longer(-Product, names_to="Variables", values_to="Scores") %>%
      dplyr::mutate(Scores = as.numeric(Scores)) %>%
      dplyr::group_by(Variables) %>%
      dplyr::summarize(Max = max(Scores)) %>%
      dplyr::mutate(Max = ifelse(Max <= 5, 5, 9)) %>%
      dplyr::ungroup() %>%
      split(.$Variables) %>%
      purrr::map(function(data){
        tibble::tibble(Display = rep(as.character(data$Variables, data$Max)),
                       Scale = 1:data$Max, Label = as.character(1:data$Max))
      }) %>%
      purrr::reduce(dplyr::bind_rows)
  }

  maxval <- labels %>%
    dplyr::group_by(Display) %>%
    dplyr::summarize(Max = max(Scale)) %>%
    dplyr::ungroup()

  if (topn == "Top3"){
    topval1 = 3
    topval2 = 2
  } else if (topn == "Top2"){
    topval1 = 2
    topval2 = 2
  } else if (topn == "Top1"){
    topval1 = 1
    topval2 = 1
  }

  res_ind <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(var)) %>%
    tidyr::pivot_longer(tidyselect::all_of(var), names_to="Variables", values_to="Responses") %>%
    dplyr::mutate(Responses = as.numeric(Responses)) %>%
    dplyr::full_join(maxval, by=c("Variables"="Display")) %>%
    dplyr::mutate(Variables = factor(Variables, levels=var)) %>%
    dplyr::mutate(Top = ifelse((Max == 9 & Responses >= (Max + 1 - topval1)) | (Max == 5 & Responses >= (Max + 1 - topval2)), 1, 0),
           Bottom = ifelse((Max == 9 & Responses <= topval1) | (Max == 5 & Responses <= topval2), 1, 0)) %>%
    dplyr::mutate(Middle = ifelse(Top + Bottom == 0, 1, 0))

  res_freq <- res_ind %>%
    split(.$Variables) %>%
    purrr::map(function(data){

      var <- data %>%
        dplyr::pull(Variables) %>%
        unique()

      res <- data %>%
        dplyr::mutate(Responses = factor(Responses,
                                  levels=labels %>% dplyr::filter(Display == var) %>% dplyr::pull(Scale),
                                  labels=labels %>% dplyr::filter(Display == var) %>% dplyr::pull(Label))) %>%
        dplyr::group_by(Product) %>%
        dplyr::count(Responses, name="N", .drop=FALSE) %>%
        dplyr::mutate(Proportion = N/sum(N)) %>%
        dplyr::mutate(Proportion = scales::percent(Proportion, accuracy=1)) %>%
        dplyr::arrange(desc(Responses)) %>%
        dplyr::ungroup()
    })

  res_top <- res_ind %>%
    dplyr::group_by(Product, Variables, Max) %>%
    dplyr::summarize(dplyr::across(c("Top","Middle","Bottom"), mean)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(Top:Bottom, names_to="Responses", values_to="N") %>%
    dplyr::mutate(Proportion = scales::percent(N, accuracy=1), N = 100*N) %>%
    split(.$Variables)

  if (test == "Yes"){
    res_signif <- res_ind %>%
      dplyr::group_by(Product, Variables) %>%
      dplyr::summarize(dplyr::across(c("Top","Middle","Bottom"), sum)) %>%
      dplyr::mutate(Total = Top + Middle + Bottom, Prop = Top/Total) %>%
      dplyr::ungroup() %>%
      split(.$Variables) %>%
      purrr::map(function(data){

        # Preparing the Analysis
        prod <- data %>%
          dplyr::pull(Product) %>%
          unique()
        paires <- combn(prod, 2)

        # Computing the p-values
        res_pval <- matrix(NA, length(prod), length(prod), dimnames=list(prod, prod))
        diag(res_pval) <- 1

        for (p in 1:ncol(paires)){
          data_proptest <- data %>%
            dplyr::filter(Product %in% paires[,p])
          res_pval[paires[1,p], paires[2,p]] <- res_pval[paires[2,p], paires[1,p]] <- prop.test(data_proptest$Top, data_proptest$Total, correct = FALSE)$p.value
        }

        # Creating the Groups
        data <- data %>%
          as.data.frame() %>%
          tibble::column_to_rownames(var = "Product") %>%
          dplyr::select(Prop) %>%
          dplyr::arrange(desc(Prop))

        groups <- agricolae::orderPvalue(rownames(data), means=data$Prop, alpha=0.05,
                                         pvalue=res_pval[rownames(data),rownames(data)], console=FALSE) %>%
          tibble::as_tibble(rownames="Product")

        return(groups)

      }) %>%
      tibble::enframe(name="Variables", value="res") %>%
      tidyr::unnest(res) %>%
      dplyr::mutate(Responses = "Top")

    res_top <- res_top %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::left_join(res_signif, by=c("Product","Variables","Responses")) %>%
      dplyr::mutate(Variables = factor(Variables, levels=var),
             Product = factor(Product, levels=levels(res_freq[[1]]$Product))) %>%
      dplyr::mutate(Proportion = ifelse(!is.na(groups), str_c(Proportion, "<sup>", groups, "</sup>"), Proportion)) %>%
      dplyr::select(-means, -groups) %>%
      split(.$Variables)
  }

  res <- list(Freq = res_freq, Top = res_top)
  return(res)
}
