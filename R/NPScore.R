#' Net Promoter Score
#'
#' Compute the NPS Score
#'
#' @param dataset The data to analuze
#' @param var The NPS Score variable
#' @param liking The Liking variable
#' @param test Significance test: 'HSD' or 'LSD'
#'
#' @returns The NPS scores
#' @export
#'
#' @examples NULL
NPScore <- function(dataset, var, liking, test="HSD"){

  nps_data <- dataset %>%
    dplyr::select(Judge, Product, NPS = tidyselect::all_of(var), Liking = tidyselect::all_of(liking)) %>%
    dplyr::mutate(Liking = as.numeric(Liking), NPS = as.numeric(NPS)) %>%
    dplyr::mutate(NPS_ = dplyr::case_when(
      NPS >= 9 ~ 1,
      NPS <= 6 ~ -1,
      .default = 0)) %>%
    dplyr::mutate(`NPS (EU)` = dplyr::case_when(
      NPS >= 8 ~ 1,
      NPS <= 5 ~ -1,
      .default = 0)) %>%
    dplyr::mutate(`NPS (Liking)` = dplyr::case_when(
      Liking >= 8 ~ 1,
      Liking <= 5 ~ -1,
      .default = 0)) %>%
    dplyr::select(-NPS, -Liking) %>%
    dplyr::rename(NPS = NPS_) %>%
    tidyr::pivot_longer(-c(Judge, Product), names_to="NPS", values_to="Score")

  # Net Promoter Scores
  nps <- nps_data %>%
    split(.$NPS) %>%
    purrr::map(function(data){
      if (test == "HSD"){

        res_aov <- aov(Score ~ Product, data=data)
        res <- agricolae::HSD.test(res_aov, "Product")$groups %>%
          tibble::as_tibble(rownames="Product") %>%
          dplyr::mutate(Score = round(100*Score))

      } else if (test == "LSD"){

        res_aov <- aov(Score ~ Product, data=data)
        res <- agricolae::LSD.test(res_aov, "Product")$groups %>%
          tibble::as_tibble(rownames="Product") %>%
          dplyr::mutate(Score = round(100*Score))

      } else if (test == "No"){

        res <- data %>%
          dplyr::group_by(Product) %>%
          dplyr::summarize(Score = round(100*mean(Score))) %>%
          dplyr::mutate(groups = "") %>%
          dplyr::ungroup()

      }
    }) %>%
    tibble::enframe(name = "NPS", value = "res") %>%
    tidyr::unnest(res)

  # NPS Details
  nps_detail <- nps_data %>%
    split(.$NPS) %>%
    purrr::map(function(data){
      res <- data %>%
        dplyr::group_by(Product) %>%
        dplyr::count(Score) %>%
        dplyr::mutate(Total = sum(n)) %>%
        dplyr::mutate(Proportion = n/Total) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Score = factor(Score, levels=c(1,0,-1), labels=c("%Promoters", "%Passives", "%Detractors"))) %>%
        split(.$Score) %>%
        purrr::map(function(data){

          data <- data %>%
            dplyr::mutate(Product = as.character(Product)) %>%
            dplyr::arrange(desc(Proportion))

          product <- data %>%
            dplyr::pull(Product) %>%
            unique()

          if (length(product) > 1){

            choix <- combn(product, 2)
            pval <- matrix(NA, length(product), length(product), dimnames=list(product, product))
            diag(pval) = 1

            for (c in 1:ncol(choix)){
              data2 <- data %>%
                dplyr::filter(Product %in% choix[,c])

              pval[choix[1,c],choix[2,c]] = pval[choix[2,c],choix[1,c]] = prop.test(x=data2$n, n=data2$Total)$p.value
            }

            res <- orderPvalue(product, data %>% dplyr::pull(Proportion), alpha=0.05, pvalue=pval) %>%
              tibble::as_tibble(rownames = "Product") %>%
              dplyr::full_join(data, by=c("Product", "means"="Proportion")) %>%
              dplyr::select(Product, N=n, Proportion=means, Group = groups)

          } else {
            res <- data %>%
              dplyr::mutate(Group = "") %>%
              dplyr::select(Product, N=n, Proportion, Group)
          }

          return(res)
        }) %>%
        tibble::enframe(name = "NPSGroup", value = "res") %>%
        tidyr::unnest(res) %>%
        dplyr::mutate(Proportion = scales::percent(Proportion, accuracy=1))
    }) %>%
    tibble::enframe(name = "NPS", value = "res") %>%
    tidyr::unnest(res)

  if (test == "No"){
    nps_detail <- nps_detail %>%
      dplyr::mutate(Group = "")
  }

  nps <- nps %>%
    dplyr::mutate(Score = as.character(Score)) %>%
    dplyr::rename(Proportion = Score, Group = groups) %>%
    dplyr::mutate(NPSGroup = "NPS Score")

  res <- nps_detail %>%
    dplyr::bind_rows(nps)

  return(res)
}
