#' Frequency Plot
#'
#' Plot for frequencies, either based on the raw frequencies or topboxes.
#'
#' @param res_freq The results from the frequencies function
#' @param type The type of plot, raw or topbox
#' @param expectation Expectation to control for colouring
#' @param topn The topN to consider
#' @param size Size of text on axis
#'
#' @returns The plot to generate
#' @export
#'
#' @examples NULL
Frequencies_plot <- function(res_freq, type="Topbox", expectation=NULL, topn="Top3", size=8){

  ## Raw Frequency Plot
  .Frequency_raw <- function(data, title, size=8){

    data <- data %>%
      dplyr::mutate(Value = stringr::str_remove (Proportion, "%")) %>%
      dplyr::mutate(Value = as.numeric(Value)) %>%
      dplyr::mutate(Value = Value/100) %>%
      dplyr::mutate(Proportion = ifelse(Proportion != "0%", Proportion, ""))

    colfun <- grDevices::colorRampPalette(c("red", "gold", "darkgreen"))

    p <- ggplot2::ggplot(data, aes(x = forcats::fct_rev(Product), y = Value, fill = Responses, label = Proportion))+
      ggplot2::geom_col(position = ggplot2::position_fill(reverse=FALSE))+
      ggplot2::geom_text(size=3, position = ggplot2::position_stack(vjust = 0.5, reverse = FALSE))+
      ggplot2::labs(x = "", y = "")+
      ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"))+
      ggplot2::scale_fill_manual(breaks = as.character(levels(data$Responses)), values = colfun(nlevels(data$Responses)))+
      ggplot2::ggtitle(stringr::str_c("Stacked barchart for ", title))+
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, reverse=TRUE))+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(), axis.title = ggplot2::element_text(size=size),
            legend.position = "bottom", legend.box = "horizontal", legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size=8))+
      ggplot2::coord_flip()

    return(p)
  }

  ## Topbox Plot
  .Frequency_top <- function(data, title, expectation=NULL, topn=3, size=8){

    if (is.null(expectation) | title != expectation){
      col_mid = "gold1"
    } else {
      col_mid = "#ADDB7B" #Previous colour: "#5EF263"
    }

    data <- data %>%
      dplyr::mutate(N = N/100) %>%
      dplyr::mutate(Proportion = scales::percent(N, accuracy=1)) %>%
      dplyr::mutate(Proportion = ifelse(Proportion != "0%", Proportion, "")) %>%
      dplyr::mutate(Responses = forcats::fct_inorder(Responses))

    if (unique(data$Max) == 9){
      if (topn == "Top3"){
        vals <- c("1-3","4-6","7-9")
      } else if (topn == "Top2"){
        vals <- c("1-2","3-7","8-9")
      } else if (topn == "Top1"){
        vals <- c("1","2-8","9")
      }
    } else if (unique(data$Max) == 5){
      if (topn %in% c("Top3","Top2")){
        vals <- c("1-2","3","4-5")
      } else if (topn == "Top1"){
        vals <- c("1","2-4","5")
      }
    }

    p <- ggplot2::ggplot(data, aes(x = forcats::fct_rev(Product), y=N, fill=Responses, label=Proportion))+
      ggplot2::geom_col(position = ggplot2::position_fill(reverse=TRUE))+
      ggplot2::geom_text(aes(colour = Responses), size = 3, position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE))+
      labs(x = "", y = "")+
      ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"))+
      ggplot2::scale_colour_manual(values=c("Top"="white", "Middle"="black", "Bottom"="black"))+
      ggplot2::scale_fill_manual(values=c("Top"="darkgreen", "Middle"=col_mid, "Bottom"="red"),
                                 labels=c("Top"=str_c("Top (",vals[3],")"), "Middle"=str_c("Middle (",vals[2],")"), "Bottom"=str_c("Bottom (",vals[1],")")))+
      ggplot2::ggtitle(stringr::str_c("Topbox chart for ", title))+
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1), colour="none")+
      ggplot2::theme_minimal()+
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_line(), axis.title = ggplot2::element_text(size=size),
            legend.position = "bottom", legend.box = "horizontal", legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size=8))+
      ggplot2::coord_flip()

    return(p)
  }

  if (type == "Frequency"){
    p <- purrr::map2(.x = res_freq$Freq, .y = names(res_freq$Freq), .f = .Frequency_raw, size=size)
  } else if (type == "Topbox"){
    p <- purrr::map2(.x = res_freq$Top, .y = names(res_freq$Top), .f = .Frequency_top, expectation=expectation, topn=topn, size=size)
  }
  return(p)
}
