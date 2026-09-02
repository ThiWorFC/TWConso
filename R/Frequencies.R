#' Frequency Tables
#'
#' Compute the frequency tables with topboxes for a set of variables
#'
#' @param dataset The data to analyze
#' @param var The variable to analyze
#' @param labels The labels attached to the levels for that variable
#' @param test 'yes' or 'no' for significance test
#' @param topn Compute topbox (e.g. 'Top3')
#' @param extra_col Optional. Name of a single variable (must be one of `var`) that carries
#'   one extra scale point beyond the usual uneven scale (e.g. a 9pt scale with a 10th point,
#'   or a 5pt scale with a 6th point). Frequencies are reported on the full scale (including
#'   the extra point); Top/Middle/Bottom and the significance tests are computed only on the
#'   uneven scale (1-9 or 1-5); respondents on the extra point are reported separately as
#'   their own "Extra" category and excluded from Top/Middle/Bottom/Total.
#' @param extra_scale Optional. The full scale maximum for `extra_col` (6 or 10). Required
#'   whenever `extra_col` is provided.
#' @param extra_label Optional. Text label to use for the extra point of `extra_col`, used
#'   as a fallback only when no value label can be found attached to the raw data column
#'   (e.g. from an EQ/haven-style import). Falls back to `as.character(extra_scale)` if
#'   neither is available.
#'
#' @returns The results for the frequency analysis
#' @export
#'
#' @examples NULL
Frequencies <- function(dataset, var, labels=NULL, test="No", topn="Top3",
                         extra_col=NULL, extra_scale=NULL, extra_label=NULL){

  ## --- Validate / resolve the "extra point" setup -------------------------
  if (!is.null(extra_col)){

    if (!extra_col %in% var){
      stop("`extra_col` must be one of the variables listed in `var`.")
    }
    if (is.null(extra_scale)){
      stop("`extra_scale` must be provided when `extra_col` is used.")
    }
    if (!extra_scale %in% c(6, 10)){
      warning("`extra_scale` is expected to be 6 or 10 (one point above a 5pt or 9pt scale).")
    }

    # Resolve the label to display for the extra point. Priority order:
    #   1) the Label already present in a user-supplied `labels` table (e.g. built
    #      from EQ metadata upstream) for Display==extra_col & Scale==extra_scale
    #   2) an embedded value label attached to the raw data column itself
    #   3) the `extra_label` argument
    #   4) a plain numeric fallback (as.character(extra_scale))
    extra_label_final <- NULL

    if (!is.null(labels)){
      from_labels <- labels %>%
        dplyr::filter(Display == extra_col, Scale == extra_scale) %>%
        dplyr::pull(Label)
      if (length(from_labels) >= 1 && !is.na(from_labels[1])) extra_label_final <- from_labels[1]
    }

    if (is.null(extra_label_final)){
      raw_labels <- attr(dataset[[extra_col]], "labels")
      if (!is.null(raw_labels)){
        match_name <- names(raw_labels)[raw_labels == extra_scale]
        if (length(match_name) >= 1) extra_label_final <- match_name[1]
      }
    }

    if (is.null(extra_label_final)){
      extra_label_final <- if (!is.null(extra_label)) extra_label else as.character(extra_scale)
    }
  }

  if (is.null(labels)){

    labels <- dataset %>%
      dplyr::select(Product, tidyselect::all_of(var)) %>%
      tidyr::pivot_longer(-Product, names_to="Variables", values_to="Scores") %>%
      dplyr::mutate(Scores = as.numeric(Scores)) %>%
      dplyr::group_by(Variables) %>%
      dplyr::summarize(Max = max(Scores, na.rm=TRUE)) %>%
      dplyr::mutate(Max = ifelse(Max <= 5, 5, 9)) %>%
      { if (!is.null(extra_col))
          dplyr::mutate(., Max = ifelse(Variables == extra_col, extra_scale, Max))
        else . } %>%
      dplyr::ungroup() %>%
      split(.$Variables) %>%
      purrr::map(function(data){

        is_extra <- !is.null(extra_col) && unique(data$Variables) == extra_col

        this_label <- if (is_extra){
          c(as.character(1:(data$Max - 1)), extra_label_final)
        } else {
          as.character(1:data$Max)
        }

        tibble::tibble(Display = rep(as.character(data$Variables), data$Max),
                       Scale = 1:data$Max, Label = this_label)
      }) %>%
      purrr::reduce(dplyr::bind_rows)
  }

  maxval <- labels %>%
    dplyr::group_by(Display) %>%
    dplyr::summarize(Max = max(Scale)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TopMax = Max) %>%
    { if (!is.null(extra_col))
        dplyr::mutate(., TopMax = ifelse(Display == extra_col, extra_scale - 1, TopMax))
      else . }

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
    dplyr::mutate(Extra = ifelse(Responses > TopMax, 1, 0),
           Top = ifelse(Extra == 0 & ((TopMax == 9 & Responses >= (TopMax + 1 - topval1)) | (TopMax == 5 & Responses >= (TopMax + 1 - topval2))), 1, 0),
           Bottom = ifelse(Extra == 0 & ((TopMax == 9 & Responses <= topval1) | (TopMax == 5 & Responses <= topval2)), 1, 0)) %>%
    dplyr::mutate(Middle = ifelse(Top + Bottom + Extra == 0, 1, 0))

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
        dplyr::ungroup() %>%
        dplyr::mutate(TopMax = data$TopMax[1])
    })

  res_top <- res_ind %>%
    dplyr::group_by(Product, Variables, TopMax) %>%
    dplyr::summarize(dplyr::across(c("Top","Middle","Bottom","Extra"), mean)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Max = TopMax) %>%
    tidyr::pivot_longer(cols = c(Top, Middle, Bottom, Extra), names_to="Responses", values_to="N")

  # Keep the "Extra" category only for the variable it actually belongs to -
  # for every other variable it is structurally 0% and would just clutter the legend.
  if (!is.null(extra_col)){
    res_top <- res_top %>%
      dplyr::filter(!(Responses == "Extra" & Variables != extra_col)) %>%
      dplyr::mutate(Label = ifelse(Responses == "Extra", extra_label_final, NA_character_))
  } else {
    res_top <- res_top %>%
      dplyr::filter(Responses != "Extra") %>%
      dplyr::mutate(Label = NA_character_)
  }

  res_top <- res_top %>%
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
