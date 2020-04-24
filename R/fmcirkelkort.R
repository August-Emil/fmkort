#' Title
#'
#' @param data Choose the data frame
#' @param lat The name of the column in the data with the latitude coordinates.
#' @param lon The name of the column in the data with the longitude coordinates.
#' @param label Adds a lable to the to the center of the circle. Should be specified as a name of one of the columns of the data.
#' @param radius The radius of the circles on the map (in meters). If a numerical value is chosen, all circles will have the same radius. If a string is chosen, the column in the data with the name of the string will be chosen. The radius of the circles will be equal to the number in the chosen column. The column must contain numeric values. Defalut is 30000 meters.
#' @param color A string equal to a column name in the data. If specified, the circles will have different colors.
#' @param scale The type of the scale for the colors (if color is specified). Can be either 'factor', 'numeric' or 'bin'
#' @param bins The number of bins if 'bin' is specified in scale.
#'
#' @return something
#' @export
#'
#' @import leaflet
#'
#' @importFrom grDevices rgb
#'
#'
#' @examples
#' \dontrun{
#' fmcirkelkort()
#' }
fmcirkelkort <- function(data, lat, lon, label = NULL, radius = 3, color = NULL, scale = "factor", bins = 7){

  # Load the map data
  shapefile <- fmkort::regional

  # Adjust the data
  data$tom <- 0

  add_label <- TRUE
  if(is.null(label)){
    add_label <- FALSE
    label <- 'tom'
  }

  if(is.null(color)){
    scale <- "factor"
    color <- 'tom'
  }

  if(is.numeric(radius)){
    data[,ncol(data)+1] <- radius
    colnames(data)[ncol(data)] <- c("radius")
    radius <- "radius"
  }

  colnames<- c(lat, lon, label, color, radius)
  data <- data[,c(colnames)]
  colnames(data) <- c("lat","lon", "label", "color", "radius")


  # Make the FM color pallet
  farver <- c(rgb(3/255,29/255,92/255), rgb(148/255,0/255,39/255),rgb(176/255,201/255,51/255),
              rgb(116/255,201/255,230/255), rgb(30/255,119/255,150/255), rgb(176/255,148/255,9/255),
              rgb(0/255,84/255,46/255), rgb(230/255,68/255,21/255), rgb(112/255,80/255,185/255),
              rgb(85/255,145/255,205/255), rgb(240/255,0/255,95/255))

  if (scale == "factor"){
    farver <- farver[1:length(unique(data$color))]
    factpal <- colorFactor(farver, data$color, ordered = T)
  }
  else if (scale == "bin"){
    farver <- farver[c(1,bins)]
    factpal <- colorBin(farver, data$color, bins=bins)
  }
  else if (scale == "numeric"){
    farver <- farver[c(1,2)]
    factpal <- colorNumeric(farver, data$color)
  }
}
