#' Spider Plot
#'
#' Build a spider plot to compare sensory profiles of samples
#'
#' @param data The data to be analyzed
#' @param var The list of attributes in the data
#'
#' @returns The spider plot
#' @export
#'
#' @examples NULL
spiderplot <- function(data, var){

  max_score <- max(data$Score)
  if (max_score < 10){
    limit = c(0,10)
  } else {
    limit = c(0,100)
  }

  p <- ggplot2::ggplot(data, aes(x=Num, y=Score, group=Product, colour=Product))+
    ggplot2::geom_line(aes(linetype=Type), lwd=2)+
    ggplot2::scale_x_continuous(name="", breaks=1:length(var), labels=var, limits=c(0,length(var)))+
    ggplot2::scale_y_continuous(name="", limits=limit)+
    ggplot2::scale_linetype_manual(values=c("Fictive"="dashed", "Real"="solid"))+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour="grey90"), panel.grid.minor = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(), legend.position = "bottom", legend.title = ggplot2::element_blank())+
    ggplot2::coord_polar()+
    ggplot2::guides(linetype = "none")

  return(p)
}
