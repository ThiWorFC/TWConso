#' Action Standard
#'
#' Computes the Action Standard in different situations
#'
#' @param dataset The Data to be analyzed
#' @param type The type of test: 'npd' or 'sc'
#' @param liking The Liking variable
#' @param expected The Expected Liking variable
#' @param concept The Meeting Expectation variable
#' @param prop_lik Acceptable prorpotion for liking
#' @param prop_exp Acceptable prorpotion for expected liking
#' @param prop_con Acceptable prorpotion for meeting expectation
#' @param anota The A-Not A variable
#' @param prod_cur The current sample
#' @param prop_pd Proportion of Discriminators (for A-Not A)
#'
#' @returns The table of Action Standard
#' @export
#'
#' @examples NULL
ActStd <- function(dataset, type="npd", liking,
                   expected=NULL, concept=NULL, prop_lik=60, prop_exp=60, prop_con=75,
                   anota = NULL, prod_cur = NULL, prop_pd=10){

  if (type == "npd"){
    desc <- tibble::tibble(Variables=c("Expected", "Liking", "Concept Fit"),
                   Description=c(paste0("High expected liking: >=", prop_exp, "% top 3 box before tasting (based on concept only)"),
                                 paste0("High product liking: >=", prop_lik, "% top 3 box liking after tasting"),
                                 paste0("Meeting expectations: >=", prop_con, "% meeting or above expectations")))

    if (expected == "No Expected Liking"){
      expected = NULL
    }
    if (concept == "No Concept Fit"){
      concept = NULL
    }

    res <- dataset %>%
      dplyr::select(Product, Liking = tidyselect::all_of(liking), Expected = tidyselect::any_of(expected), `Concept Fit` = tidyselect::any_of(concept)) %>%
      tidyr::pivot_longer(tidyselect::any_of(c("Liking", "Expected", "Concept Fit")), names_to="Variables", values_to="Scores") %>%
      dplyr::mutate(Scores = as.numeric(Scores)) %>%
      dplyr::mutate(Top = ifelse((Scores >= 7 & Variables %in% c("Liking","Expected")) | (Scores >=3 & Variables == "Concept Fit"), 1, 0)) %>%
      dplyr::group_by(Product, Variables) %>%
      dplyr::summarize(Top = 100 * mean(Top)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Col = dplyr::case_when(
        (Variables == "Concept Fit" & Top >= prop_con) ~ 2,
        (Variables == "Liking" & Top >= prop_lik) ~ 2,
        (Variables == "Expected" & Top >= prop_exp) ~ 2,
        .default = 1)) %>%
      dplyr::inner_join(desc, by="Variables") %>%
      dplyr::relocate(Description, .before="Product") %>%
      dplyr::arrange(Description)

  } else if (type == "sc"){

    desc <- tibble::tibble(Variables=c("Liking", "ANotA"),
                   Description=c(paste0("High product liking: >=", prop_lik, "% top 3 box liking after tasting"),
                                 paste0("A-Not A score: No difference (Pd <", prop_pd, "%) with current product")))

    # Liking
    res <- dataset %>%
      dplyr::select(Product, Liking=tidyselect::all_of(liking)) %>%
      tidyr::pivot_longer(Liking, names_to="Variables", values_to="Scores") %>%
      dplyr::mutate(Scores = as.numeric(Scores)) %>%
      dplyr::mutate(Top = ifelse(Scores >= 7, 1, 0)) %>%
      dplyr::group_by(Product, Variables) %>%
      dplyr::summarize(Top = 100 * mean(Top)) %>%
      dplyr::ungroup()

    # A - Not A
    res_count <- dataset %>%
      dplyr::select(Judge, Product, ANotA = tidyselect::all_of(anota)) %>%
      dplyr::mutate(ANotA = factor(ANotA, levels=c(1,2), labels=c("Yes","No"))) %>%
      dplyr::group_by(Product) %>%
      dplyr::count(ANotA, .drop=FALSE, name="Top") %>%
      dplyr::mutate(Prop = Top/sum(Top)) %>%
      dplyr::ungroup()

    count <- res_count %>%
      dplyr::select(-Prop) %>%
      dplyr::mutate(Product = as.character(Product)) %>%
      tidyr::pivot_wider(names_from=ANotA, values_from=Top) %>%
      dplyr::mutate(Total = Yes + No)

    product <- count %>%
      dplyr::filter(Product != prod_cur) %>%
      dplyr::pull(Product) %>%
      unique()
    nbprod <- length(product)

    for (p in 1:nbprod){

      val1 = count %>%
        dplyr::filter(Product %in% prod_cur) %>%
        dplyr::pull(Yes)

      val2 = count %>%
        dplyr::filter(Product %in% product[p]) %>%
        dplyr::pull(No)

      val3 = count %>%
        dplyr::filter(Product %in% c(prod_cur, product[p])) %>%
        dplyr::pull(Total) %>%
        sum()

      val <- 2 * ((val1 + val2)/val3 - 1/2)

      res_count <- dplyr::bind_rows(res_count,
                             tibble::tibble(Product = product[p], ANotA = "Pd", Top=val, Prop=val))
    }

    # Finalizing Table
    res_count <- res_count %>%
      dplyr::mutate(Prop = ifelse(ANotA == "Pd", scales::percent(Prop, accuracy=0.1), scales::percent(Prop, accuracy=1))) %>%
      dplyr::mutate(Variables = "ANotA")

    res <- res %>%
      dplyr::mutate(Prop = as.character(round(Top, 1))) %>%
      dplyr::bind_rows(res_count) %>%
      dplyr::mutate(Col = dplyr::case_when(
        (Variables == "Liking" & Top >= prop_lik) ~ 2,
        (Variables == "ANotA" & ANotA == "Pd" & Top < prop_pd) ~ 2,
        Variables == "ANotA" ~ 3,
        .default = 1)) %>%
      dplyr::inner_join(desc, by="Variables") %>%
      dplyr::relocate(Description, .before="Product") %>%
      dplyr::relocate(Statistic=ANotA, .after=Product) %>%
      dplyr::select(-Top) %>%
      dplyr::rename(Top = Prop)
  }

  return(res)

}
