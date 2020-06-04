#' fmregionkort
#'
#' @param data The data frame for the region data
#' @param id The name of the region
#' @param value The value for the region which shall correspond to the color on the map
#' @param scale The type of the scale for the colors. Can be either 'factor', 'numeric' (default), 'bin.num' or 'bin.cat'
#' @param bins The number of bins if 'bin' specified under 'scale'. Default = 5.
#' @param legend Choose whether the map should includ a legend (default = FALSE)
#' @param legendtitle Choose the name above the legendtitle. Default is the same name as the column specified under 'legend'
#' @param bornholm Should Bornholm be moved north west to be included in the map?
#' @param output The name of the exported .png file. No map exported if the output = NULL (default = NULL)
#' @param marker Adds marker to the map saying e.g. "Nordjylland: xx" where the value of xx correspond to the value for Nordjylland in the data. Defalut = TRUE
#' @param suffix The suffix of the marker text, e.g. "pct."
#' @param ndigits The number of digits on the marker text
#' @param background Choose the background color
#' @param filetype Choose the file type of the output file ("png" (= defalut), "pdf" or ".jpeg")
#' @param farver A character vector containing colors. (default = FM colors)
#'
#' @return A map of the Danish regions
#' @export
#'
#' @examples
#' \dontrun{
#' fmregionkort(data = data, id = id, value = value)
#' }
fmregionkort <- function(data = NULL, id = NULL, value = NULL, scale = 'numeric', bins = 5, legend = FALSE,  legendtitle = NULL, bornholm = T, marker = T, suffix ="", output = NULL, ndigits = 1, background = rgb(249/255,248/255,224/255), filetype = ".png", farver = NULL){

  # Load the municipality map.
  shapefile <- fmkort::regional

  # Chage the Danish letters and the formating.
  shapefile <- join_map_data(value = value, id = id, mapdata = data, shapefile)

  if(is.null(legendtitle)) legendtitle <- value

  # Adjust the data
  data$tom <- 0
  colnames<- c(id, value)
  data <- data[,c(colnames)]
  data <- as.data.frame(data)
  colnames(data) <- c("id", "values")

  data$id <- fix_names_encoding(data$id)
  data$id <- fix_names_join(data$id)

  # Change the format of the pop-up data (only visible as HTML/in RStudio).
  data_popup <- paste0("<strong>", shapefile$name, "</strong>",
                       "<br>", prettyNum(shapefile$values, big.mark = ".", decimal.mark = ","))


  # Change the color pallet.
  if (is.null(farver)){
    farver <- c(rgb(3/255,29/255,92/255),rgb(148/255,0/255,39/255), rgb(116/255,201/255,230/255),
                rgb(176/255,201/255,51/255), rgb(30/255,119/255,150/255), rgb(176/255,148/255,9/255),
                rgb(0/255,84/255,46/255), rgb(230/255,68/255,21/255), rgb(112/255,80/255,185/255),
                rgb(85/255,145/255,205/255), rgb(240/255,0/255,95/255))
  }

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
    shapefile@polygons[[4]]@Polygons[[1]]@coords[,1] <- regional@polygons[[4]]@Polygons[[1]]@coords[,1]-2.7
    shapefile@polygons[[4]]@Polygons[[1]]@coords[,2] <- regional@polygons[[4]]@Polygons[[1]]@coords[,2]+2.2
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

  # Add markers if specified
  if(marker == T){
    if(is.numeric(data$values)){
      data$values <- format(round(data$values, digits = ndigits), nsmall = ndigits, decimal.mark = ",")
    }

    lon_mark <-  c(9.7,9.5, 9.5, 11.8, 12.5)
    lat_mark <- c(57, 56.2, 55.2, 55.3, 55.8)
    val_mark <- c(paste("Nordjylland: ", data[which(data$id == "regionnordjylland"),]$values , suffix),
                  paste("Midtjylland: ", data[which(data$id == "regionmidtjylland"),]$values ,suffix),
                  paste("Syddanmark: ", data[which(data$id == "regionsyddanmark"),]$values ,suffix),
                  paste("Sj\u00E6lland: ", data[which(data$id == "regionsjaelland"),]$values ,suffix),
                  paste("Hovedstaden: ", data[which(data$id == "regionhovedstaden"),]$values ,suffix))



    leafletmap <- leafletmap  %>% addLabelOnlyMarkers(lng = lon_mark, lat = lat_mark, label =  val_mark,
                                                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                                                  style=list(
                                                                                    'font-family'= 'arial',
                                                                                    'font-size' = '14px',
                                                                                    'font-weight'= 'bold',
                                                                                    'border-color' = 'rgba(0,0,0,0.5)',
                                                                                    'border-top-right-radius' = '0px 0px',
                                                                                    'border-top-left-radius' = '0px 0px',
                                                                                    'border-bottom-right-radius' = '0px 0px',
                                                                                    'border-bottom-left-radius' = '0px 0px',
                                                                                    'background-color' = 'rgba(255,255,255,0.7)'
                                                                                  )))
  }


  # Export the graph as a .png file if specified.
  if(!is.null(output)){
    graph_name <- paste(output,filetype, sep="")
    mapview::mapshot(leafletmap, file = graph_name, vwidth = 500, vheight = 600)
  }

  return(leafletmap)

}
