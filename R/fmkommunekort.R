#' Title
#'
#' @param data x
#' @param id x
#' @param value x
#' @param num_val x
#' @param legend x
#' @param legendtitle x
#' @param output x
#' @param bins x
#'
#' @return A map
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
#' fmkommunekort()
#' }
fmkommunekort <- function(data = NULL, id = NULL, value = NULL, num_val = 'numeric', bins = 5, legend = FALSE,  legendtitle = NULL, output = NULL){

  # Load the municipality map.
  shapefile <- fmkort::municipal

  # Chage the Danish letters and the formating.
  shapefile <- join_map_data(value = value, id = id, mapdata = data, shapefile)

  if(is.null(legendtitle)) legendtitle <- value

  # Adjust the data
  data$tom <- 0
  colnames<- c(value)
  data <- data[,c(colnames)]
  colnames(data) <- c("values")


  # Change the format of the pop-up data (only visible as HTML/in RStudio).
  data_popup <- paste0("<strong>", shapefile$name, "</strong>",
                       "<br>", prettyNum(shapefile$values, big.mark = ".", decimal.mark = ","))


  # Change the color pallet.
  farver <- c(rgb(3/255,29/255,92/255),rgb(148/255,0/255,39/255), rgb(116/255,201/255,230/255),
              rgb(176/255,201/255,51/255), rgb(30/255,119/255,150/255), rgb(176/255,148/255,9/255),
              rgb(0/255,84/255,46/255), rgb(230/255,68/255,21/255), rgb(112/255,80/255,185/255),
              rgb(85/255,145/255,205/255), rgb(240/255,0/255,95/255))

  if (num_val == "factor"){
    farver <- farver[1:length(unique(data$values))]
    factpal <- colorFactor(farver, data$values, ordered = T)
  }
  else if (num_val == "bin"){
    farver <- farver[1:bins]
    factpal <- colorBin(farver, data$values, bins=bins)
  }
  else if (num_val == "numeric"){
    farver <- farver[c(3,1)]
    factpal <- colorNumeric(farver, data$values)
  }


  # Move Bornholm
  shapefile@polygons[[100]]@Polygons[[1]]@coords[,1] <- shapefile@polygons[[100]]@Polygons[[1]]@coords[,1]-2.7
  shapefile@polygons[[100]]@Polygons[[1]]@coords[,2] <- shapefile@polygons[[100]]@Polygons[[1]]@coords[,2] +2.2


  # Create the map and fill the municipalities with the colors
  leafletmap <- leaflet(shapefile) %>%
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


  return(leafletmap)
}
