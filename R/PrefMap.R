#' PrefMap
#'
#' Run the PrefMap analysis
#'
#' @param dataset The data to be analyzed
#' @param col.p Integer: corresponds to the position of the 'Product' column
#' @param col.j Integer: corresponds to the position of the 'Consumer' column
#' @param col.lik Integer: corresponds to the position of the 'Liking' column
#' @param sensory The sensory data - a table with the coordinates of the products
#' @param xlimits Vector: contains the min and max on the X-axis
#' @param ylimits Vector: contains the min and max on the Y-axis
#' @param resolution Integer: define the granularity for the prediction
#' @param regmod Type of model to chose from - 'Linear', 'Circular', 'Elliptic', 'Quadratic' and 'Best'
#' @param param The parameter to chose for selecting the best model - chose from 'AIC', 'BIC' and Adj. R2
#' @param acceptance The type of value to consider to define acceptance: 'mean' or 'value'
#' @param level The threshold to consider
#' @param predict Additional points to predict (e.g. supplementary entities)
#' @param incProgress Parameter to use for the progress bar...
#'
#' @returns The numerical results of the PrefMap analysis
#' @export
#'
#' @examples NULL
PrefMap <- function(dataset, col.p=2, col.j=1, col.lik=6,
                    sensory=NULL, xlimits=NULL, ylimits=NULL, resolution=200,
                    regmod="Best", param="AdjR2", acceptance="Mean", level=0,
                    predict=NULL, incProgress=NULL){

  #----- Preparing the Analysis -----
  res <- list()

  if (!is.null(incProgress)) {
    shiny::incProgress(0.1, detail="Preparing the Data...")
    progr = 0.1
  }

  dataset <- dataset %>%
    dplyr::select(Judge = tidyselect::all_of(col.j), Product = tidyselect::all_of(col.p), Liking = tidyselect::all_of(col.lik)) %>%
    dplyr::mutate(Judge = as.character(Judge), Product=as.character(Product))

  # Determining if the model is suitable to the data
  nbprod <- dataset %>%
    dplyr::pull(Product) %>%
    unique() %>%
    length()

  if (nbprod <= 4){

    stop("Not enough products to run the PrefMap")

  } else if (nbprod <= 6){

    if (regmod != "Linear"){
      regmod="Linear"
    }

  } else if (nbprod <= 7){

    if (!regmod %in% c("Linear", "Circular")){
      regmod="Circular"
    }

  } else if (nbprod <= 8){

    if (!regmod %in% c("Linear", "Circular", "Elliptic")){
      regmod="Elliptic"
    }

  }

  #----- Consumer Data -----

  # Cleaning the Data
  j.rm <- dataset %>%
    dplyr::group_by(Judge) %>%
    dplyr::summarize(SD = sd(Liking, na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(near(SD, 0)) %>%
    dplyr::select(Judge)

  data.j <- dataset %>%
    dplyr::anti_join(j.rm, by="Judge") %>%
    tidyr::pivot_wider(names_from=Judge, values_from=Liking) %>%
    as.data.frame() %>%
    tibble::column_to_rownames(var="Product")

  nbconso <- ncol(data.j)

  #----- Running the Regression -----

  if (!is.null(incProgress)) {
    shiny::incProgress(0.1, detail="Preparing the consumer Data...")
    progr = progr + 0.1
  }

  # Combining Data
  MatH <- data.j %>%
    tibble::as_tibble(rownames = "product")

  Mat <- sensory %>%
    tibble::as_tibble(rownames="product") %>%
    dplyr::rename(Dim1=2, Dim2=3) %>%
    dplyr::mutate(Dim1_sq=Dim1^2,
                  Dim2_sq=Dim2^2,
                  Dim1Dim2=Dim1 * Dim2,
                  `Dim1_sq + Dim2_sq`=Dim1_sq + Dim2_sq) %>%
    dplyr::select(product, tidyselect::starts_with("Dim"))

  matrice <- dplyr::inner_join(Mat, MatH, by="product")

  # Range of dimensions for the regression
  if (!is.null(incProgress)) {
    shiny::incProgress(0.1, detail="Defining the space's dimensions...")
    progr = progr + 0.1
  }

  if (is.null(xlimits)){

    xlimits = range(Mat$Dim1)

  } else {

    xlimits = c(min(min(range(Mat$Dim1)), min(xlimits)),
                max(max(range(Mat$Dim1)), max(xlimits)))

  }
  if (is.null(ylimits)){

    ylimits = range(Mat$Dim2)

  } else {

    ylimits = c(min(min(range(Mat$Dim2)), min(ylimits)),
                max(max(range(Mat$Dim2)), max(ylimits)))

  }

  xrange <- diff(range(xlimits))
  yrange <- diff(range(ylimits))
  pas <- max(xrange, yrange)/resolution
  f1 <- seq((min(xlimits) - xrange * 0.05), (max(xlimits) + xrange * 0.05), pas)
  f2 <- seq((min(ylimits) - yrange * 0.05), (max(ylimits) + yrange * 0.05), pas)
  grid <- expand.grid(Dim1=f1, Dim2=f2) %>%
    dplyr::mutate(Dim1_sq=Dim1^2, Dim2_sq=Dim2^2, Dim1Dim2=Dim1*Dim2, `Dim1_sq + Dim2_sq`=Dim1_sq+Dim2_sq) %>%
    tibble::as_tibble()

  # Threshold Values
  if (!is.null(incProgress)) {
    shiny::incProgress(0.1, detail="Defining the Threshold value(s)...")
    progr = progr + 0.1
  }

  if (acceptance == "Mean"){

    thres <- matrice %>%
      tidyr::pivot_longer(8:ncol(.), names_to="Consumer", values_to="Liking") %>%
      dplyr::group_by(Consumer) %>%
      dplyr::summarize(Thres = mean(Liking, na.rm=TRUE) + level*sd(Liking, na.rm=TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from=Consumer, values_from=Thres) %>%
      as.vector()

  } else if (acceptance == "Value"){

    thres <- matrice %>%
      tidyr::pivot_longer(8:ncol(.), names_to="Consumer", values_to="Liking") %>%
      dplyr::select(Consumer) %>%
      unique() %>%
      dplyr::mutate(Thres = level) %>%
      tidyr::pivot_wider(names_from=Consumer, values_from=Thres) %>%
      as.vector()
  }

  # PrefMap - Modified to include progress updates
  if (!is.null(incProgress)) {
    shiny::incProgress(0.05, detail="Building the consumers' models...")
    progr = progr + 0.05
  }

  # Split data by consumer
  data_split <- matrice %>%
    tidyr::pivot_longer(8:ncol(.), names_to="Consumer", values_to="Liking") %>%
    split(.$Consumer)

  # Calculate progress increment per consumer
  total_consumers <- length(data_split)
  progress_per_consumer <- 0.35 / total_consumers  # 0.35 is the remaining progress for this step

  # Process each consumer with progress updates
  res_prefmap_list <- vector("list", total_consumers)
  consumer_names <- names(data_split)

  for (i in seq_along(data_split)) {

    res_prefmap_list[[i]] <- PrefMap_reg(data = data_split[[i]], threshold = thres[[i]], param = param,
                                         grid = grid, model = regmod, indsup = predict)

    # Update progress after each consumer
    if (!is.null(incProgress)) {
      detail_msg <- sprintf("Building model %d/%d...", i, total_consumers)
      shiny::incProgress(progress_per_consumer, detail = detail_msg)
      progr = progr + progress_per_consumer
    }
  }

  # Combine results
  res_prefmap <- tibble::tibble(Conso = consumer_names, res = res_prefmap_list) %>%
    tidyr::unnest(res)

  res_prefmap_info <- res_prefmap %>%
    dplyr::select(-predict, -ind_sup) %>%
    tidyr::unnest(info)

  res_prefmap_pred <- res_prefmap %>%
    dplyr::select(-info, -ind_sup) %>%
    tidyr::unnest(predict) %>%
    dplyr::group_by(Dim1, Dim2) %>%
    dplyr::summarize(Proportion = 100*mean(Accept), Prediction = mean(Prediction)) %>%
    dplyr::ungroup()

  #----- Prediction -----

  if (!is.null(predict)){

    if (!is.null(incProgress)) {
      shiny::incProgress(0.1, detail="Prediction for Suppl. Samples...")
      progr = progr + 0.1
    }

    res_prefmap_sup <- res_prefmap %>%
      dplyr::select(-info, -predict) %>%
      tidyr::unnest(ind_sup)

  } else {

    res_prefmap_sup = NULL

  }

  #----- Returning the Results -----

  if (!is.null(incProgress)) {
    shiny::incProgress((1-progr), detail="Exporting the results...")
  }

  res <- list(Info = res_prefmap_info, Prediction = res_prefmap_pred, ProdPred = res_prefmap_sup)
  class(res) <- c("PrefMap", "list")

  return(res)
}
