#' Bar Chart
#'
#' Build the Bart Chart
#'
#' @param dataset The data to be analyzed
#' @param file The file location
#' @param import The type of import data
#' @param exit_q the variables to represent
#' @param per_prod Should the results be split per product
#' @param plot_adj Plot adjustments: 'Raw' or 'Transposed'
#'
#' @returns The generated graphs
#' @export
#'
#' @examples NULL
BarChart <- function(dataset, file, import, exit_q, per_prod=FALSE, plot_adj="Raw"){

  if (!per_prod){
    dataset <- dataset %>%
      dplyr::distinct(Judge, !!!syms(exit_q)) %>%
      dplyr::mutate(Product = "noprod") %>%
      dplyr::relocate(Product, .after=Judge)
  }

  data <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(exit_q)) %>%
    tidyr::pivot_longer(tidyselect::all_of(exit_q), names_to="Variables", values_to="Responses") %>%
    dplyr::mutate(Variables = forcats::fct_inorder(Variables), Responses = as.numeric(Responses))

  if (import == "EQ"){

    data_freq <- readxl::read_xlsx(file, sheet="Attributes") %>%
      dplyr::mutate(Display = stringr::str_replace_all(Display, "[\\/]", " ")) %>%
      dplyr::filter(Display %in% exit_q) %>%
      dplyr::select(Attribute, Display) %>%
      tidyr::separate(Attribute, c("Group", "Num"), sep="_", extra="merge") %>%
      dplyr::select(-Num) %>%
      dplyr::inner_join(data, by=c("Display"="Variables")) %>%
      dplyr::select(Group, Judge, Product, Variables=Display, Responses)

  } else {

    data_freq <- data %>%
      tidyr::separate(Variables, c("Group","Attr"), sep="_", remove=FALSE, extra="merge")

    if (any(is.na(data_freq$Attr))){

      data_freq <- data_freq %>%
        dplyr::mutate(Group = "Q1") %>%
        dplyr::select(Group, Judge, Product, Variables, Responses)

    } else {

      data_freq <- data_freq %>%
        dplyr::mutate(Attr = forcats::fct_inorder(Attr)) %>%
        dplyr::select(Group, Judge, Product, Variables=Attr, Responses)

    }
  }

  if (!per_prod){

    res_freq <- data_freq %>%
      dplyr::select(-Product) %>%
      unique() %>%
      split(.$Group) %>%
      purrr::map(function(data){
        data %>%
          dplyr::group_by(Variables) %>%
          dplyr::summarize(Prop = mean(Responses)) %>%
          dplyr::ungroup()
      })

    plot_freq <- res_freq %>%
      purrr::map(function(data){

        if (plot_adj %in% c("Raw", "Transpose")){
          if (plot_adj == "Raw"){
            p = ggplot2::ggplot(data, aes(x = reorder(Variables, -Prop), y=Prop))
          } else if (plot_adj == "Transpose"){
            p = ggplot2::ggplot(data, aes(x = reorder(Variables, Prop), y=Prop))
          }

          p = p+
            ggplot2::geom_col()+
            ggplot2::labs(x = "", y = "")+
            ggplot2::scale_y_continuous(breaks=seq(0,1,0.25), labels=paste0(seq(0,100,25),"%"), limits=c(0,1))+
            ggplot2::theme_minimal()+
            ggplot2::theme(axis.line = ggplot2::element_line(colour="grey70"))

          if (plot_adj == "Transpose"){
            p = p+
              ggplot2::coord_flip()
          } else {
            p = p+
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))
          }

        } else if (plot_adj == "Center"){

          data_long <- data %>%
            dplyr::mutate(Min = -1*(Prop/2), Max = (Prop/2)) %>%
            tidyr::pivot_longer(c(Min, Max), names_to="Bar", values_to="BarVal")

          p = ggplot2::ggplot(data_long, aes(x = forcats::fct_rev(Variables), y = BarVal)) +
            ggplot2::geom_col(fill = "darkblue") +
            ggplot2::geom_text(data=data, aes(x = forcats::fct_rev(Variables), y = 0, label = scales::percent(Prop, accuracy = 1)), col="white", inherit.aes = FALSE) +
            ggplot2::coord_flip() +
            ggplot2::scale_y_continuous(limits = c(-0.5, 0.5), label = NULL)+
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.title.y = ggplot2::element_blank())

        }

        return(p)

      })

  } else {

    res_freq <- data_freq %>%
      split(.$Group) %>%
      purrr::map(function(data){
        data %>%
          dplyr::group_by(Product, Variables) %>%
          dplyr::summarize(Prop = mean(Responses)) %>%
          dplyr::ungroup()
      })

    plot_freq <- res_freq %>%
      purrr::map(function(data){

        if (plot_adj %in% c("Raw", "Transpose")){

          if (plot_adj == "Raw"){
            p = ggplot2::ggplot(data, aes(x = Variables, y = Prop))
          } else if (plot_adj == "Transpose"){
            p = ggplot2::ggplot(data, aes(x = forcats::fct_rev(Variables), y = Prop))
          }

          p = p+
            ggplot2::geom_col()+
            ggplot2::labs(x = "", y = "")+
            ggplot2::scale_y_continuous(breaks=seq(0,1,0.25), labels=paste0(seq(0,100,25),"%"), limits=c(0,1))+
            ggplot2::theme_minimal()+
            ggplot2::theme(axis.line = ggplot2::element_line(colour="grey70"))

          if (plot_adj == "Transpose"){
            p = p+
              ggplot2::coord_flip()
          } else {
            p = p+
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))
          }

        } else if (plot_adj == "Center"){

          data_long <- data %>%
            dplyr::mutate(Min = -1*(Prop/2), Max = (Prop/2)) %>%
            tidyr::pivot_longer(c(Min, Max), names_to="Bar", values_to="BarVal")

          p = ggplot2::ggplot(data_long, aes(x = forcats::fct_rev(Variables), y = BarVal)) +
            ggplot2::geom_col(fill = "darkblue") +
            ggplot2::geom_text(data = data, aes(x = forcats::fct_rev(Variables), y = 0, label = scales::percent(Prop, accuracy = 1)),
                               col="white", inherit.aes = FALSE) +
            ggplot2::coord_flip() +
            ggplot2::scale_y_continuous(limits = c(-0.5, 0.5), label = NULL)+
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.title.y = ggplot2::element_blank())

        }

        p = p+
          ggplot2::facet_wrap(~Product, scales="free")

        return(p)
      })
  }

  res <- list(Pct = res_freq, Plot = plot_freq)
  return(res)
}
