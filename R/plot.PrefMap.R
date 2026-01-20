#' Surface Response Plot for PrefMap
#'
#' Build the surface response plot for the PrefMap
#'
#' @param x Results obtained from the PrefMap function
#' @param space Sensory space
#' @param map Depends on the acceptance defined in PrefMap(): is it 'acceptance' or 'prediction'
#' @param colour The colouring scheme for the PrefMap (3 colours)
#' @param col_sup The colour for the supplementary individuals
#' @param include_var Boolean: should the variables be projected as well
#' @param var_size Integer: size the of the variable (if represented)
#' @param var_col Colour code to apply for the variables (if represented)
#' @param title Title of the graph
#' @param x_lab Title for the X-axis
#' @param y_lab Title for the Y-axis
#' @param ... Additional arguments passed to plotting functions
#'
#' @returns The surface response plot
#' @export
#'
#' @examples NULL
plot.PrefMap <- function(x, space, map="Proportion", colour=c("#39AECF","white","#DC3545"),
                         col_sup = "grey32", include_var=FALSE, var_size=1, var_col="#F7B924",
                         title=NULL, x_lab=NULL, y_lab=NULL, ...){

  # Preparing the Space
  depasse <- x$Prediction %>%
    dplyr::select(x=Dim1, y=Dim2, z=tidyselect::all_of(map))

  # Product Configuration
  prod_config <- space$Scores %>%
    dplyr::rename(Dim1=2, Dim2=3)

  # Variable Configuration
  if (include_var){
    attr_config <- space$Loadings %>%
      dplyr::rename(Dim1=2, Dim2=3)
  }

  # Graphic

    ## Title
  if (is.null(title)){
    title="External Preference Mapping"
  }

  if (is.null(x_lab)){
    x_lab = colnames(space)[1]
  }
  if (is.null(y_lab)){
    y_lab = colnames(space)[2]
  }


    ## Type of Map
  if (map == "Proportion"){
    map_legend = "Proportion (%)"
    map_limit = c(0,100)
    contour_break = c(seq(0, 70, 10), seq(80,100,5))
    map_break = seq(0, 100, 25)
  } else if (map == "Prediction"){
    map_legend = "Liking Score"
    map_limit = c(1,9)
    contour_break = c(seq(1, 6, 1), seq(7,9,0.5))
    map_break = seq(1, 9, 2)
  }

  ## Generating the Plot
  p <- ggplot2::ggplot(depasse)+
    ggplot2::geom_raster(aes(x=x, y=y, fill=z), interpolate=TRUE)+
    ggplot2::geom_contour(aes(x=x, y=y, z=z), colour="white", breaks=contour_break)+
    # geom_text_contour(aes(x=x, y=y, z=z), colour="white", breaks=contour_break, cex=2.5, skip=0)+
    ggplot2::geom_point(data=prod_config, aes(x=Dim1, y=Dim2, alpha=Transp), pch=20)+
    ggrepel::geom_text_repel(data=prod_config, aes(x=Dim1, y=Dim2, label=Product, alpha=Transp), vjust=-1)+
    ggplot2::guides(alpha = "none")+
    ggplot2::scale_alpha_identity(limits=c(0,1))

  if (include_var){
    p = p+
      ggplot2::geom_text(data=attr_config, aes(x=Dim1, y=Dim2, label=Attributes), vjust=-1, color=var_col, size=var_size)
  }

  p = p+
    ggplot2::geom_hline(yintercept=0, lty=3)+
    ggplot2::geom_vline(xintercept=0, lty=3)+
    ggplot2::labs(x = x_lab, y = y_lab)+
    ggplot2::scale_fill_gradient2(low=colour[1], mid=colour[2], high=colour[3], midpoint=sum(map_limit)/2, breaks=map_break, limits=map_limit)+
    ggplot2::guides(fill=guide_legend(reverse=TRUE, title=map_legend))+
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), axis.line=ggplot2::element_blank())+
    ggplot2::ggtitle(title)+
    ggplot2::coord_fixed()

  if (!is.null(space$ScoresSup)){

    scores_sup <- space$ScoresSup %>%
      dplyr::rename(Dim1=2, Dim2=3)

    p <- p+
      ggplot2::geom_point(data=scores_sup, aes(x=Dim1, y=Dim2, alpha=Transp), pch=18, col=col_sup)+
      ggrepel::geom_text_repel(data=scores_sup, aes(x=Dim1, y=Dim2, label=Product, alpha=Transp), col=col_sup, vjust=-1)

  }

  return(p)
}
