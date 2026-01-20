#' Reverse Regression
#'
#' Reverse regression is used to predict the optimal sample from the PrefMap space by providing the coordinates of the optimal sample to predict.
#'
#' @param coord The coordinates of the products on the PrefMap space
#' @param senso The sensory profiles of the samples
#' @param coord.ideal The coordinates of the sample profile to predict
#'
#' @returns The predicted sensory profile of the optimal sample
#' @export
#'
#' @examples NULL
reversereg <- function(coord, senso, coord.ideal){

  res <- coord %>%
    dplyr::rename(Dim1=2, Dim2=3) %>%
    dplyr::inner_join(senso, by="Product") %>%
    tidyr::pivot_longer(-c(Product, Dim1, Dim2), names_to="Sensory", values_to="Scores") %>%
    split(.$Sensory) %>%
    purrr::map(function(data){

      res_lm <- lm(Scores ~ Dim1 + Dim2, data=data)

      coord.ideal %>%
        modelr::add_predictions(res_lm) %>%
        dplyr::mutate(broom::glance(res_lm)) %>%
        dplyr::select(Product, Dim1, Dim2, Prediction=pred, R2=r.squared, AdjR2=adj.r.squared, Pval=p.value)

    }) %>%
    tibble::enframe(name="Attribute", value="res") %>%
    tidyr::unnest(res)

  return(res)
}
