#' Build Pie Chart
#'
#' Create Pie Chart
#'
#' @param dataset The Data to be analyzed
#' @param file The file to import
#' @param import The type of import ('EQ' or 'Other')
#' @param exit_q The variable to use
#' @param per_prod Are the proportion product related?
#' @param plot_adj Different types of graph: 'Pie', 'Center', 'Raw', 'Transpose'
#'
#' @returns The generated plots
#' @export
#'
#' @examples NULL
PieChart <- function(dataset, file, import, exit_q, per_prod=FALSE, plot_adj="Pie"){

  data_pct <- dataset %>%
    dplyr::select(Judge, Product, tidyselect::all_of(exit_q)) %>%
    tidyr::pivot_longer(tidyselect::all_of(exit_q), names_to="Variables", values_to="Responses") %>%
    unique()

  if (import == "EQ"){

    vars <- readxl::read_xlsx(file, sheet="Attributes") %>%
      dplyr::filter(Display %in% exit_q) %>%
      dplyr::select(Display, tidyselect::starts_with("Label")) %>%
      tidyr::pivot_longer(-Display, names_to="Responses", values_to="Label") %>%
      dplyr::mutate(Responses = stringr::str_remove(Responses, "Label")) %>%
      dplyr::filter(!is.na(Label))

    data_pct <- data_pct %>%
      split(.$Variables) %>%
      purrr:map(function(data){

        var <- data %>%
          dplyr::pull(Variables) %>%
          unique()

        lev <- vars %>%
          dplyr::filter(Display == var)

        res <- data %>%
          dplyr::mutate(Responses = factor(Responses, levels=lev$Responses, labels=lev$Label))

        return(res)
      })

  } else {

    if ("ExitLabel" %in% readxl::excel_sheets(file)){

      labels <- readxl::read_xlsx(file, sheet="ExitLabel")

    } else {

      labels <- data_pct %>%
        dplyr::distinct(Variables, Responses) %>%
        dplyr::mutate(Labels = as.character(Responses)) %>%
        dplyr::rename(Codes = Responses)

    }

    data_pct <- data_pct %>%
      dplyr::mutate(Responses = as.character(Responses)) %>%
      split(.$Variables) %>%
      purrr:map(function(data){

        var <- data %>%
          dplyr::pull(Variables) %>%
          unique()

        labels_v <- labels %>%
          dplyr::filter(Variables == var)

        if (nrow(labels_v) > 0){

          data %>%
            dplyr::mutate(Responses = factor(Responses, levels=labels_v$Codes, labels=labels_v$Labels))

        }

      })
  }

  res_pct <- data_pct %>%
    purrr:map(function(data){

      if (per_prod){

        data %>%
          dplyr::group_by(Product) %>%
          dplyr::count(Responses, .drop=FALSE) %>%
          dplyr::mutate(Prop = n/sum(n)) %>%
          dplyr::mutate(Label = scales::percent(Prop, accuracy=1)) %>%
          dplyr::mutate(ypos = cumsum(Prop)-0.5*Prop) %>%
          ungroup()

      } else {

        data %>%
          dplyr::count(Responses, .drop=FALSE) %>%
          dplyr::mutate(Prop = n/sum(n)) %>%
          dplyr::mutate(Label = scales::percent(Prop, accuracy=1)) %>%
          dplyr::mutate(ypos = cumsum(Prop)-0.5*Prop)
      }
    })

  if (plot_adj == "Pie"){

    plot_pct <- purrr:map2(.x=res_pct, .y=names(res_pct), .f=function(data, name){

      p <- ggplot2::ggplot(data, aes(x="", y=Prop, fill=Responses))+
        ggplot2::geom_col(width=1, colour="white", position = ggplot2::position_stack(reverse=TRUE))+
        ggplot2::geom_text(aes(y=ypos, label=Label), colour="white", size=3, nudge_x=0.15)+
        ggplot2::coord_polar("y", start=0, direction=-1)+
        ggplot2::guides(fill = ggplot2::guide_legend(name))+
        ggplot2::theme_void()

      if (per_prod){

        p <- p+
          ggplot2::facet_wrap(~Product)

      }
      return(p)
    })

  } else {

    if (plot_adj == "Center"){

      plot_pct <- res_pct %>%
        purrr:map(function(data){

          data_long <- data %>%
            dplyr::select(-n, -ypos) %>%
            dplyr::mutate(Min = -1*(Prop/2), Max = (Prop/2)) %>%
            tidyr::pivot_longer(c(Min, Max), names_to="Bar", values_to="BarVal")

          p = ggplot2::ggplot(data_long, aes(x = forcats::fct_rev(Responses), y = BarVal)) +
            ggplot2::geom_col(fill = "darkblue") +
            ggplot2::geom_text(data=data, aes(x = forcats::fct_rev(Responses), y = 0, label = Label), col="white", inherit.aes = FALSE) +
            ggplot2::coord_flip() +
            ggplot2::scale_y_continuous(name = NULL, limits = c(-0.5, 0.5), label = NULL)+
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.title.y = ggplot2::element_blank())

          if (per_prod){
            p = p+
              ggplot2::facet_wrap(~Product)
          }

          return(p)
        })

    } else {

      plot_pct <- res_pct %>%
        purrr:map(function(data){

          if (plot_adj == "Raw"){
            p = ggplot2::ggplot(data, aes(x=Responses, y=Prop))
          } else if (plot_adj == "Transpose"){
            p = ggplot2::ggplot(data, aes(x=forcats::fct_rev(Responses), y=Prop))
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

          if (per_prod){

            p = p+
              ggplot2::facet_wrap(~Product)

          }

          return(p)
        })

    }
  }

  res <- list(Pct = res_pct, Plot = plot_pct)
  return(res)
}
