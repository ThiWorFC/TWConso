#' Drivers of Liking
#'
#' Run simple and quadratic regression for each attribute to evaluates the drivers of liking vs. Liking
#'
#' @param data The data set to be analyzed
#' @param attribute Vector: contains the list of attributes to consider as drivers of liking
#' @param pval_mod Value: threshold for the p-value of the model
#' @param pval_quad Value: threshold to the p-value to keep quadratic effect
#'
#' @returns The type of drivers of liking associated to each attribute
#' @export
#'
#' @examples NULL
dol <- function(data, attribute, pval_mod=0.10, pval_quad=0.10){

  ## Linear Model
  res_lin <- data %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Linear") %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod = list(lm(Liking ~ Linear, data=data)))

  res_lin_coeff <- res_lin %>%
    dplyr::reframe(broom::tidy(mod)) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::select(Attribute, term, estimate, p.value) %>%
    dplyr::ungroup()

  res_lin_param <- res_lin %>%
    dplyr::reframe(broom::glance(mod)) %>%
    dplyr::mutate(Model = "Linear") %>%
    dplyr::select(Attribute, Model, r.squared, adj.r.squared, p.value)

  ## Quadratic Model
  res_quad <- data %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Linear") %>%
    dplyr::mutate(Quadratic = Linear^2) %>%
    dplyr::nest_by(Attribute) %>%
    dplyr::mutate(mod = list(lm(Liking ~ Linear + Quadratic, data=data)))

  res_quad_coeff <- res_quad %>%
    dplyr::reframe(broom::tidy(mod)) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::select(Attribute, term, estimate, p.value)

  res_quad_param <- res_quad %>%
    dplyr::reframe(broom::glance(mod)) %>%
    dplyr::mutate(Model = "Quadratic") %>%
    dplyr::select(Attribute, Model, r.squared, adj.r.squared, p.value)

  ## Extracting the Attributes with Quadratic Effect
  attr_quad <- res_quad_coeff %>%
    dplyr::filter(term == "Quadratic", p.value <= pval_quad) %>%
    dplyr::pull(Attribute)

  ## Combining Results
  res_param <- dplyr::bind_rows(dplyr::filter(res_lin_param, !(Attribute %in% attr_quad)),
                                dplyr::filter(res_quad_param, Attribute %in% attr_quad)) %>%
    dplyr::select(Attribute, R2=r.squared, Adj.R2=adj.r.squared, Pvalue=p.value)

  res_coef <- dplyr::bind_rows(dplyr::filter(res_lin_coeff, !(Attribute %in% attr_quad)),
                               dplyr::filter(res_quad_coeff, Attribute %in% attr_quad)) %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Attribute) %>%
    dplyr::select(-p.value) %>%
    tidyr::pivot_wider(names_from=term, values_from=estimate, values_fill=NA)

  if (!"Quadratic" %in% colnames(res_coef)){
    res_coef <- res_coef %>%
      dplyr::mutate(Quadratic = NA)
  }

  res_final <- res_coef %>%
    dplyr::mutate(Model = dplyr::case_when(
      (is.na(Quadratic) & Linear >= 0) ~ "Linear +",
      (is.na(Quadratic) & Linear < 0) ~ "Linear ",
      (!is.na(Quadratic) & Quadratic <=0) ~ "Optimum",
      .default = "Anti-Ideal")) %>%
    dplyr::full_join(res_param, by="Attribute") %>%
    dplyr::mutate(Attribute = factor(Attribute, levels=attribute)) %>%
    dplyr::arrange(Attribute)

  ## Extracting the R2
  res_R2_pval <- dplyr::bind_rows(dplyr::filter(res_lin_param, !(Attribute %in% attr_quad)),
                                  dplyr::filter(res_quad_param, Attribute %in% attr_quad)) %>%
    dplyr::select(Attribute, Pvalue=p.value)

  res_R2 <- dplyr::bind_rows(res_lin_param, dplyr::filter(res_quad_param, Attribute %in% attr_quad)) %>%
    dplyr::select(Attribute, Model, R2=r.squared) %>%
    dplyr::mutate(Model = stringr::str_c("R2_",Model)) %>%
    tidyr::pivot_wider(names_from=Model, values_from=R2)

  if (!"R2_Quadratic" %in% colnames(res_R2)){
    res_R2 <- res_R2 %>%
      dplyr::mutate(R2_Quadratic = NA)
  }

  res_R2 <- res_R2 %>%
    dplyr::full_join(res_lin_coeff, by="Attribute") %>%
    dplyr::full_join(res_R2_pval, by="Attribute") %>%
    dplyr::mutate(Significance = ifelse(Pvalue <= pval_mod, 1, 0.3)) %>%
    dplyr::mutate(`R2 (Linear)` = dplyr::case_when(
      is.na(R2_Quadratic) ~ sign(estimate) * R2_Linear,
      .default = R2_Linear),
           `R2 (Quadratic)` = dplyr::case_when(
             !is.na(R2_Quadratic) ~ R2_Quadratic - R2_Linear,
             .default = 0)) %>%
    dplyr::mutate(R2 = `R2 (Linear)` + `R2 (Quadratic)`) %>%
    dplyr::arrange(R2) %>%
    dplyr::select(Attribute, `R2 (Linear)`, `R2 (Quadratic)`, Significance) %>%
    dplyr::mutate(Attribute = forcats::fct_inorder(Attribute)) %>%
    tidyr::pivot_longer(-c(Attribute, Significance), names_to="Model", values_to="R2") %>%
    dplyr::mutate(`Driver of Liking` = dplyr::case_when(
      Model == "R2 (Quadratic)" ~ "Quadratic",
      R2 >= 0 ~ "Positive",
      .default = "Negative"))

  ## Extracting the Data
  data_export <- data %>%
    tidyr::pivot_longer(tidyselect::all_of(attribute), names_to="Attribute", values_to="Score") %>%
    dplyr::inner_join(res_final, by="Attribute")

  res <- list(DoL=res_final, R2=res_R2, Data=data_export)

  return(res)
}
