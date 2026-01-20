#' Regression for PrefMap
#'
#' The modeling tool for the Preference Mapping
#'
#' @param data The data to be analyzed
#' @param model The model to consider - chose from 'Linear', 'Circular', 'Elliptic', 'Quadratic' and 'Best'
#' @param param The parameter to chose for selecting the best model - chose from 'AIC', 'BIC' and Adj. R2
#' @param threshold The p-value to consider for model selection
#' @param grid The values used for prediction for the PrefMap
#' @param indsup Information regarding supplementary individuals for extra calculation
#'
#' @returns Prediction values and other results
#' @export
#'
#' @examples NULL
PrefMap_reg <- function(data, model, param, threshold, grid, indsup=NULL){

  # Running the Model
  if (model %in% c("Linear","Best")){
    mod1 <- lm(Liking ~ Dim1 + Dim2, data=data, na.action=na.omit)
    mod1_AIC <- broom::glance(mod1)$AIC
    mod1_BIC <- broom::glance(mod1)$BIC
    mod1_AdjR2 <- broom::glance(mod1)$adj.r.squared
  } else {
    mod1_AIC <- mod1_BIC <- Inf
    mod1_AdjR2 <- -Inf
  }

  if (model %in% c("Circular","Best")){
    mod2 <- lm(Liking ~ Dim1 + Dim2 + `Dim1_sq + Dim2_sq`, data=data, na.action=na.omit)
    mod2_AIC <- broom::glance(mod2)$AIC
    mod2_BIC <- broom::glance(mod2)$BIC
    mod2_AdjR2 <- broom::glance(mod2)$adj.r.squared
  } else {
    mod2_AIC <- mod2_BIC <- Inf
    mod2_AdjR2 <- -Inf
  }

  if (model %in% c("Elliptic","Best")){
    mod3 <- lm(Liking ~ Dim1 + Dim2 + Dim1_sq + Dim2_sq, data=data, na.action=na.omit)
    mod3_AIC <- broom::glance(mod3)$AIC
    mod3_BIC <- broom::glance(mod3)$BIC
    mod3_AdjR2 <- broom::glance(mod3)$adj.r.squared
  } else {
    mod3_AIC <- mod3_BIC <- Inf
    mod3_AdjR2 <- -Inf
  }

  if (model %in% c("Quadratic","Best")){
    mod4 <- mod4 <- lm(Liking ~ Dim1 + Dim2 + Dim1_sq + Dim2_sq + Dim1Dim2, data=data, na.action=na.omit)
    mod4_AIC <- broom::glance(mod4)$AIC
    mod4_BIC <- broom::glance(mod4)$BIC
    mod4_AdjR2 <- broom::glance(mod4)$adj.r.squared
  } else {
    mod4_AIC <- mod4_BIC <- Inf
    mod4_AdjR2 <- -Inf
  }

  # Selecting the Best Model
  if (model == "Best"){
    mod_best <- tibble::tibble(Model = c("Linear","Circular","Elliptic","Quadratic"),
                       AIC = c(mod1_AIC, mod2_AIC, mod3_AIC, mod4_AIC),
                       BIC = c(mod1_BIC, mod2_BIC, mod3_BIC, mod4_BIC),
                       AdjR2 = c(mod1_AdjR2, mod2_AdjR2, mod3_AdjR2, mod4_AdjR2))

    if (param == "AIC"){
      mod_best <- mod_best %>%
        dplyr::filter(AIC == min(AIC)) %>%
        dplyr::pull(Model)
    } else if (param == "BIC"){
      mod_best <- mod_best %>%
        dplyr::filter(BIC == min(BIC)) %>%
        dplyr::pull(Model)
    } else if (param == "AdjR2"){
      mod_best <- mod_best %>%
        dplyr::filter(AdjR2 == max(AdjR2)) %>%
        dplyr::pull(Model)
    }
  } else {
    mod_best = model
  }

  if (mod_best == "Linear"){
    mod = mod1
  } else if (mod_best == "Circular"){
    mod = mod2
  } else if (mod_best == "Elliptic"){
    mod = mod3
  } else if (mod_best == "Quadratic"){
    mod = mod4
  }

  # Extracting the Info
  mod_info <- broom::glance(mod) %>%
    dplyr::select(R2 = r.squared, AdjR2 = adj.r.squared, AIC, BIC) %>%
    dplyr::mutate(Model = mod_best)

  # Prediction
  mod_pred <- grid %>%
    modelr::add_predictions(mod) %>%
    dplyr::mutate(pred = ifelse(pred < 0, 0, ifelse(pred > 9, 9, pred))) %>%
    dplyr::mutate(accept = ifelse(pred >= threshold, 1, 0)) %>%
    dplyr::select(Dim1, Dim2, Prediction=pred, Accept=accept)

  # Second Prediction
  if (!is.null(indsup)){
    pred_sup <- indsup %>%
      dplyr::rename(Dim1=3, Dim2=4) %>%
      dplyr::mutate(Dim1_sq = Dim1^2, Dim2_sq = Dim2^2, Dim1Dim2 = Dim1*Dim2) %>%
      dplyr::mutate(`Dim1_sq + Dim2_sq` = Dim1_sq + Dim2_sq) %>%
      modelr::add_predictions(mod) %>%
      dplyr::select(Type, Product, Prediction=pred) %>%
      dplyr::mutate(Threshold = threshold)
  } else {
    pred_sup = NULL
  }

  # Results
  res <- tibble::tibble(info = list(mod_info), predict = list(mod_pred), ind_sup = list(pred_sup))
  return(res)
}
