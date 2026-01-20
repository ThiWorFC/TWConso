#' Significance Test for Action Standards
#'
#' Compute the significance test for Action Standards
#'
#' @param dataset The Data to be analyzed
#' @param liking The Liking variable
#' @param expected The Expected Liking variable
#' @param concept The Meeting Expectation variable
#'
#' @returns The results of the significance test
#' @export
#'
#' @examples NULL
ActStd_signif <- function(dataset, liking, expected, concept){

  .test_prop <- function(data){

    product <- unique(data$Product)
    nbprod <- length(product)
    npaires <- combn(nbprod, 2)

    res <- matrix(NA, nbprod, nbprod, dimnames=list(product, product))
    diag(res) = 1

    for (p in 1:ncol(npaires)){
      data_bis <- data %>%
        dplyr::filter(Product %in% product[npaires[,p]])

      if (!is.na(prop.test(unlist(data_bis$`1`), unlist(data_bis$Total), correct=FALSE)$p.value)){
        res[product[npaires[1,p]],product[npaires[2,p]]] <- res[product[npaires[2,p]],product[npaires[1,p]]] <- prop.test(unlist(data_bis$`1`), unlist(data_bis$Total), correct=FALSE)$p.value
      } else {
        res[product[npaires[1,p]],product[npaires[2,p]]] <- res[product[npaires[2,p]],product[npaires[1,p]]] <- 1
      }
    }

    res <- res %>%
      tibble::as_tibble(rownames="Product")

    return(res)
  }

  nbprod <- dataset %>%
    dplyr::pull(Product) %>%
    unique() %>%
    length(.)

  if (nbprod > 1){

    res <- dataset %>%
      dplyr::select(Product, Liking = tidyselect::all_of(liking), Expected = tidyselect::any_of(expected), `Concept Fit` = tidyselect::any_of(concept)) %>%
      tidyr::pivot_longer(tidyselect::any_of(c("Liking", "Expected", "Concept Fit")), names_to="Variables", values_to="Scores") %>%
      dplyr::mutate(Scores = as.numeric(Scores)) %>%
      dplyr::mutate(Top = ifelse((Scores >= 7 & Variables %in% c("Liking","Expected")) | (Scores <=2 & Variables == "Concept Fit"), 1, 0)) %>%
      dplyr::mutate(Top = factor(Top, levels=c("0","1"))) %>%
      dplyr::group_by(Product, Variables) %>%
      dplyr::count(Top, .drop=FALSE) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from=Top, values_from=n, values_fill=0) %>%
      dplyr::mutate(Total = `0` + `1`) %>%
      dplyr::mutate(Variables = factor(Variables, levels=c("Expected","Liking","Concept Fit"))) %>%
      dplyr::arrange(Variables) %>%
      dplyr::mutate(Variables = droplevels(Variables)) %>%
      split(.$Variables) %>%
      purrr::map(.test_prop)

  } else {
    res <- NULL
  }

  return(res)

}
