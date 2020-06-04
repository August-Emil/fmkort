#' fmcirkelkort
#'
#' @param data Choose the data frame
#' @param lat The name of the column in the data with the latitude coordinates.
#' @param lon The name of the column in the data with the longitude coordinates.
#' @param label Adds a lable to the to the center of the circle. Should be specified as a name of one of the columns of the data.
#' @param radius The radius of the circles on the map (in meters). If a numerical value is chosen, all circles will have the same radius. If a string is chosen, the column in the data with the name of the string will be chosen. The radius of the circles will be equal to the number in the chosen column. The column must contain numeric values. Defalut is 30000 meters.
#' @param color A string equal to a column name in the data. If specified, the circles will have different colors.
#' @param scale The type of the scale for the colors (if color is specified). Can be either 'factor', 'numeric', bin.num' or 'bin.cat'
#' @param bins The number of bins if 'bin' is specified in scale.
#' @param alpha The transparency of the circle. [0,1].
#' @param dot Shold the middle of the circle have a dot (TRUE/FALSE, default = TRUE)
#' @param textsize The textsize of the label. Default is "11px".
#' @param legend Add a legend to the graph
#' @param output The name of the output (a .png file). If not specified, not outcome will b exported.
#' @param background Choose the background color
#' @param filetype Choose the file type of the output file ("png" (= defalut), "pdf" or ".jpeg")
#' @param farver A character vector containing colors. (default = FM colors)
#'
#' @return A map of Denmark with circles on
#' @export
#'
#' @import leaflet
#' @import leaflet.extras
#'
#' @importFrom grDevices rgb
#' @importFrom mapview mapshot
#'
#'
#' @examples
#' \dontrun{
#' fmcirkelkort()
#' }
fmcirkelkort <- function(data, lat, lon, label = NULL, radius = 10, color = NULL, scale = "factor", bins = 7, alpha = 0.3, dot = TRUE, textsize = "11px", legend = FALSE, output = NULL, background = rgb(249/255,248/255,224/255), filetype = ".png", farver = NULL){

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


  # Change the color pallet.
  if (is.null(farver)){
    farver <- c(rgb(3/255,29/255,92/255),rgb(148/255,0/255,39/255), rgb(116/255,201/255,230/255),
                rgb(176/255,201/255,51/255), rgb(30/255,119/255,150/255), rgb(176/255,148/255,9/255),
                rgb(0/255,84/255,46/255), rgb(230/255,68/255,21/255), rgb(112/255,80/255,185/255),
                rgb(85/255,145/255,205/255), rgb(240/255,0/255,95/255))
  }

  if (scale == "factor"){
    farver <- farver[1:length(unique(data$color))]
    factpal <- colorFactor(farver, data$color, ordered = T)
  }
  else if (scale == "bin.num"){
    farver <- farver[c(3,1)]
    factpal <- colorBin(farver, data$color, bins=bins)
  }
  else if (scale == "bin.cat"){
    farver <- farver[1:bins]
    factpal <- colorBin(farver, data$color, bins=bins)
  }
  else if (scale == "numeric"){
    farver <- farver[c(1,2)]
    factpal <- colorNumeric(farver, data$color)
  }


  # Move Bornholm
  shapefile@polygons[[4]]@Polygons[[1]]@coords[,1] <- regional@polygons[[4]]@Polygons[[1]]@coords[,1]-2.7
  shapefile@polygons[[4]]@Polygons[[1]]@coords[,2] <- regional@polygons[[4]]@Polygons[[1]]@coords[,2]+2.2

  data$lat[which(data$lon>13)] <- data$lat[which(data$lon>13)]+2.2
  data$lon[which(data$lon>13)] <- data$lon[which(data$lon>13)]-2.7


  # Create the map of Denmark
  leafletmap <- leaflet(shapefile, options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
    addPolygons(fillOpacity = 1,
                color = rgb(180/255,202/255,213/255),
                stroke = F)


  # Add the circles to the map
  leafletmap <- addCircles(leafletmap,lng = data$lon, lat = data$lat, weight = 1, fillOpacity = alpha, opacity = 0.5, radius = data$radius * 1000, color = factpal(data$color))


  # Add a dot in the middle of the circle
  if (dot == TRUE){
    leafletmap <- addCircleMarkers(leafletmap, lng = data$lon, lat = data$lat, radius=2,
                                   opacity = 1, fillOpacity = 0.75, weight=0, color=factpal(data$color),
                                   label = data$label, labelOptions = labelOptions(noHide = add_label, textOnly=T, textsize = textsize))
  }

  # Create some lines to show that Bornholm has been moved.
  leafletmap <- addPolylines(leafletmap, lng = c(11.8, 11.8, 12.6), lat = c(57.6, 57.1,57.1),
                             color='Black', weight = 1, opacity = 1)


  # Change the backgroup color to the FM color
  leafletmap <- leafletmap %>% setMapWidgetStyle(list(background = background))


  # Moved the map to the center of Denmark
  leafletmap <- leafletmap %>% setView(lng = 10.41765, lat = 56.163221, zoom = 7)


  # Add the legend if the legend is specified.
  if (legend == TRUE){
    leafletmap <- addLegend(leafletmap, pal = factpal, values = data$color, position = "bottomleft", labFormat = labelFormat(big.mark = " "), className = "panel panel-default legend", opacity = 1)
  }


  # Export the graph as a .png file if specified.
  if(!is.null(output)){
    graph_name <- paste(output, filetype, sep="")
    mapview::mapshot(leafletmap, file = graph_name, vwidth = 500, vheight = 600)
  }


  return(leafletmap)

}
