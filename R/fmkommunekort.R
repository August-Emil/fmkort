#' fmkommunekort
#'
#' @param data The data frame for the municipality data
#' @param id The name of the municipality
#' @param value The value for the municipality which shall correspond to the color on the map
#' @param scale The type of the scale for the colors. Can be either 'factor', 'numeric' (default), 'bin.num' or 'bin.cat'
#' @param legend Choose whether the map should includ a legend (default = FALSE)
#' @param legendtitle Choose the name above the legendtitle. Default is the same name as the column specified under 'legend'
#' @param output The name of the exported .png file. No map exported if the output = NULL (default = NULL)
#' @param bins The number of bins if 'bin' specified under 'scale'. Default = 5.
#' @param bornholm Should Bornholm be moved north west to be included in the map?
#'
#' @return A map of the Danish municipalities.
#'
#' @import leaflet
#' @import leaflet.extras
#' @importFrom stringr str_to_title
#' @importFrom grDevices rgb
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fmkommunekort(data = data, id = id, value = value)
#' }
fmkommunekort <- function(data = NULL, id = NULL, value = NULL, scale = 'numeric', bins = 5, legend = FALSE,  legendtitle = NULL, bornholm = T, output = NULL){

  # Load the municipality map.
  shapefile <- fmkort::municipal

  # Chage the Danish letters and the formating.
  shapefile <- join_map_data(value = value, id = id, mapdata = data, shapefile)

  if(is.null(legendtitle)) legendtitle <- value

  # Adjust the data
  data$tom <- 0
  colnames<- c(value)
  data <- data[,c(colnames)]
  data <- as.data.frame(data)
  colnames(data) <- c("values")


  # Change the format of the pop-up data (only visible as HTML/in RStudio).
  data_popup <- paste0("<strong>", shapefile$name, "</strong>",
                       "<br>", prettyNum(shapefile$values, big.mark = ".", decimal.mark = ","))


  # Change the color pallet.
  farver <- c(rgb(3/255,29/255,92/255),rgb(148/255,0/255,39/255), rgb(116/255,201/255,230/255),
              rgb(176/255,201/255,51/255), rgb(30/255,119/255,150/255), rgb(176/255,148/255,9/255),
              rgb(0/255,84/255,46/255), rgb(230/255,68/255,21/255), rgb(112/255,80/255,185/255),
              rgb(85/255,145/255,205/255), rgb(240/255,0/255,95/255))

  if (scale == "factor"){
    farver <- farver[1:length(unique(data$values))]
    factpal <- colorFactor(farver, data$values, ordered = T)
  }
  else if (scale == "bin.num"){
    farver <- farver[c(3,1)]
    factpal <- colorBin(farver, data$values, bins=bins)
  }
  else if (scale == "bin.cat"){
    farver <- farver[1:bins]
    factpal <- colorBin(farver, data$values, bins=bins)
  }
  else if (scale == "numeric"){
    farver <- farver[c(3,1)]
    factpal <- colorNumeric(farver, data$values)
  }


  # Move Bornholm
  if(bornholm == T){
  shapefile@polygons[[100]]@Polygons[[1]]@coords[,1] <- shapefile@polygons[[100]]@Polygons[[1]]@coords[,1]-2.7
  shapefile@polygons[[100]]@Polygons[[1]]@coords[,2] <- shapefile@polygons[[100]]@Polygons[[1]]@coords[,2] +2.2
  }

  # Create the map and fill the municipalities with the colors
  leafletmap <- leaflet(shapefile, options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE))%>%
    addPolygons(fillColor = ~factpal(values),
                fillOpacity = 1,
                color = "000000",
                stroke = F,
                popup = data_popup,
                smoothFactor = 0.75)

  # Add a legend if specified
  if (legend == TRUE){
    leafletmap <- addLegend(leafletmap, pal = factpal, values = data$values, position = "bottomleft", labFormat = labelFormat_decimal(digits = 2, big.mark = " ", decimal.mark = ","), className = "panel panel-default legend", opacity = 1, title = stringr::str_to_title(legendtitle))
  }

  # Change the background color
  leafletmap <- leafletmap %>% setMapWidgetStyle(list(background = rgb(249/255,248/255,224/255)))

  # Make lines around Bornholm
  if(bornholm ==T){
  leafletmap <- addPolylines(leafletmap, lng = c(11.8, 11.8, 12.6), lat = c(57.6, 57.1,57.1),
                             color='Black', weight = 1, opacity = 1)
  }

  # Center the map
  leafletmap <- leafletmap %>% setView(lng = 10.41765, lat = 56.163221, zoom = 6.5)

  # Export the graph as a .png file if specified.
  if(!is.null(output)){
    graph_name <- paste(output,".png", sep="")
    mapview::mapshot(leafletmap, file = graph_name, vwidth = 500, vheight = 600)
  }



  return(leafletmap)
}
