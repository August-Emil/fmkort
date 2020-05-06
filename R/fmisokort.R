#' fmisokort
#'
#' @param data Choose the data frame
#' @param lat The name of the column in the data with the latitude coordinates.
#' @param lon The name of the column in the data with the longitude coordinates.
#' @param quota Set the quota left on the api key
#' @param api_key Set the api key
#' @param profile Choose the profile. This is the same as the profile for openrouteservice::ors_isochrones. Default = 'driving-car'.
#' @param range Choose the range in km for the isochrones. Defalut = 10.
#' @param intervals Choose the number of isochrones for each coorinate. Default = 1
#' @param output The name of the output (a .png file). If not specified, not outcome will b exported.
#' @param map Add the isochrones to a map.
#' @param color The color of the isochrones
#' @param legend Add a legend? default = FALSE
#'
#' @return a map
#'
#' @importFrom openrouteservice ors_isochrones
#' @import leaflet
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fmisokort()
#' }
fmisokort <- function(data = NULL, lat = NULL, lon = NULL, color = NULL, quota = 500, profile = 'driving-car', range = 10, intervals = 1, api_key = NULL, legend = FALSE, output = NULL, map = NULL){

  # Find the maximum number of iterations
  max_reaced <- min(quota, ceiling(nrow(data) / 5))

  # Load the map data
  shapefile <- fmkort::regional

  # Make the FM color pallet
  if (is.null(color)){
    colors <- numeric(nrow(data))
  } else {
  colnames<- c(color)
  colors <- data[,c(colnames)]
  }
  colors <- as.data.frame(colors)
  colnames(colors) <- c("color")

  farver <- c(rgb(3/255,29/255,92/255),rgb(148/255,0/255,39/255),rgb(116/255,201/255,230/255),
              rgb(176/255,201/255,51/255),rgb(30/255,119/255,150/255), rgb(176/255,148/255,9/255),
              rgb(0/255,84/255,46/255), rgb(230/255,68/255,21/255), rgb(112/255,80/255,185/255),
              rgb(85/255,145/255,205/255), rgb(240/255,0/255,95/255))

  farver <- farver[1:length(unique(colors$color))]
  factpal <- colorFactor(farver, colors$color, ordered = T)

  # Move Bornholm
  shapefile@polygons[[4]]@Polygons[[1]]@coords[,1] <- regional@polygons[[4]]@Polygons[[1]]@coords[,1]-2.7
  shapefile@polygons[[4]]@Polygons[[1]]@coords[,2] <- regional@polygons[[4]]@Polygons[[1]]@coords[,2]+2.2

  # Create the map of Denmark
  leafletmap <- map

  if (is.null(map)){
  leafletmap <- leaflet(shapefile, options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
    addPolygons(fillOpacity = 1,
                color = rgb(180/255,202/255,213/255),
                stroke = F)
  }
  # Create and add the isochrones
  for (i in 1:max_reaced) {

    row_start <- 1 + (i-1) * 5
    row_end <- 5 + (i-1) * 5
    if (row_end>nrow(data)){
      row_end <- nrow(data)
    }

    coordinates <- data[row_start:row_end, c(lat, lon)]

    # Create the isochrones
    isodata <- ors_isochrones(coordinates, range = range, interval = range / intervals, range_type = 'distance', area_units = 'km', units = 'km', api_key = api_key, output = "sf")

    #Add to leafletmap
    for (j in 1:nrow(isodata)){
      isodata[[4]][[j]][[1]][,2][which(isodata[[4]][[j]][[1]][,1]>13)] <- isodata[[4]][[j]][[1]][,2][which(isodata[[4]][[j]][[1]][,1]>13)] + 2.2
      isodata[[4]][[j]][[1]][,1][which(isodata[[4]][[j]][[1]][,1]>13)] <- isodata[[4]][[j]][[1]][,1][which(isodata[[4]][[j]][[1]][,1]>13)] - 2.7

      colornum <- row_start + isodata[[1]][j]

      leafletmap <- addPolygons(leafletmap, lng = isodata[[4]][[j]][[1]][,1], lat = isodata[[4]][[j]][[1]][,2],
                                color = factpal(colors[colornum,1]), opacity = 0, fillOpacity = 0.8)
    }

  }

  # Create some lines to show that Bornholm has been moved.
  leafletmap <- addPolylines(leafletmap, lng = c(11.8, 11.8, 12.6), lat = c(57.6, 57.1,57.1),
                             color='Black', weight = 1, opacity = 1)


  # Change the backgroup color to the FM color
  leafletmap <- leafletmap %>% setMapWidgetStyle(list(background = rgb(249/255,248/255,224/255)))


  # Moved the map to the center of Denmark
  leafletmap <- leafletmap %>% setView(lng = 10.41765, lat = 56.163221, zoom = 7)

  # Add the legend if the legend is specified.
  if (legend == TRUE){
    leafletmap <- addLegend(leafletmap, pal = factpal, values = colors[,1], position = "bottomleft", labFormat = labelFormat(big.mark = " "), className = "panel panel-default legend", opacity = 1)
  }

  # Export the graph as a .png file if specified.
  if(!is.null(output)){
    graph_name <- paste(output,".png", sep="")
    mapview::mapshot(leafletmap, file = graph_name,
                     remove_controls = c("zoomControl"), vwidth = 500, vheight = 600)
  }



  return(leafletmap)
}
