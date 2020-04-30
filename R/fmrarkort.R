#' Title
#'
#' @param data data
#' @param id id
#' @param value value
#' @param suffix suffix
#' @param ndigits digits
#' @param bornholm bornholm
#' @param marker marker
#'
#' @return a map
#' @export
#'
#' @examples
#' \dontrun{
#' fmrarkort(data = data, id = id, value = value)
#' }
fmrarkort <- function(data = NULL, id = NULL, value = NULL, suffix = "", bornholm = TRUE, marker = TRUE, ndigits = 1){

  # Adjust the data
  data$tom <- 0
  colnames<- c(id, value)
  data <- data[,c(colnames)]
  colnames(data) <- c("id", "value")

  # Fix the encoding
  data$id <- fix_names_encoding(data$id)
  data$id <- fix_names_join(data$id)

  # Create a municipality data frame (the RAR areas are based on the municipalities)
  komdata <- c('Bornholm',
               'Christians\u00F8',
               'Assens',
               'Faaborg-Midtfyn',
               'Kerteminde',
               'Langeland',
               'Nordfyns',
               'Nyborg',
               'Odense',
               'Svendborg',
               '\u00C6r\u00F8',
               'Albertslund',
               'Aller\u00F8d',
               'Ballerup',
               'Br\u00F8ndby',
               'Drag\u00F8r',
               'Egedal',
               'Fredensborg',
               'Frederiksberg',
               'Frederikssund',
               'Fures\u00F8',
               'Gentofte',
               'Gladsaxe',
               'Glostrup',
               'Gribskov',
               'Halsn\u00E6s',
               'Helsing\u00F8r',
               'Herlev',
               'Hiller\u00F8d',
               'Hvidovre',
               'H\u00F8je Taastrup',
               'H\u00F8rsholm',
               'Ish\u00F8j',
               'K\u00F8benhavn',
               'Lyngby-Taarb\u00E6k',
               'Rudersdal',
               'R\u00F8dovre',
               'T\u00E5rnby',
               'Vallensb\u00E6k',
               'Mors\u00F8',
               'Br\u00F8nderslev',
               'Frederikshavn',
               'Hj\u00F8rring',
               'Jammerbugt',
               'L\u00E6s\u00F8',
               'Mariagerfjord',
               'Rebild',
               'Thisted',
               'Vesthimmerlands',
               'Aalborg',
               'Faxe',
               'Greve',
               'Guldborgsund',
               'Holb\u00E6k',
               'Kalundborg',
               'K\u00F8ge',
               'Lejre',
               'Lolland',
               'N\u00E6stved',
               'Odsherred',
               'Ringsted',
               'Roskilde',
               'Slagelse',
               'Solr\u00F8d',
               'Sor\u00F8',
               'Stevns',
               'Vordingborg',
               'Billund',
               'Esbjerg',
               'Fan\u00F8',
               'Fredericia',
               'Haderslev',
               'Kolding',
               'Middelfart',
               'S\u00F8nderborg',
               'T\u00F8nder',
               'Varde',
               'Vejen',
               'Vejle',
               'Aabenraa',
               'Herning',
               'Holstebro',
               'Ikast-Brande',
               'Lemvig',
               'Ringk\u00F8bing-Skjern',
               'Skive',
               'Struer',
               'Favrskov',
               'Hedensted',
               'Horsens',
               'Norddjurs',
               'Odder',
               'Randers',
               'Sams\u00F8',
               'Silkeborg',
               'Skanderborg',
               'Syddjurs',
               'Viborg',
               'Aarhus')

  komdata <- as.data.frame(komdata)
  colnames(komdata) <- c("id")

  # Add the value of the RAR areas to the komdata
  komdata$value <- NA
  komdata[1:2,]$value <- data[which(data$id == "bornholm"),]$value
  komdata[3:11,]$value <- data[which(data$id == "fyn"),]$value
  komdata[12:40,]$value <- data[which(data$id == "hovedstaden"),]$value
  komdata[41:50,]$value <- data[which(data$id == "nordjylland"),]$value
  komdata[51:67,]$value <- data[which(data$id == "sjaelland"),]$value
  komdata[68:80,]$value <- data[which(data$id == "sydjylland"),]$value
  komdata[81:87,]$value <- data[which(data$id == "vestjylland"),]$value
  komdata[88:99,]$value <- data[which(data$id == "oestjylland"),]$value

  leafletmap <- fmkommunekort(data = komdata, id = "id", value = "value")

  # Add markers if specified
  if(marker == T){
    if(is.numeric(data$value)){
    data$value <- format(round(data$value, digits = ndigits), nsmall = ndigits)
    }

    lon_mark <-  c(9.7,8.7,10.4, 9, 10.4, 11.8, 12.5)
    lat_mark <- c(57, 56.2, 56.1, 55.3, 55.15, 55.5, 55.8)
    val_mark <- c(paste("Nordjylland: ", data[which(data$id == "nordjylland"),]$value , suffix),
                  paste("Vestjylland: ", data[which(data$id == "vestjylland"),]$value ,suffix),
                  paste("\u00D8stjylland: ", data[which(data$id == "oestjylland"),]$value ,suffix),
                  paste("Sydjylland: ", data[which(data$id == "sydjylland"),]$value ,suffix),
                  paste("Fyn: ", data[which(data$id == "fyn"),]$value ,suffix),
                  paste("Sj\u00E6lland: ", data[which(data$id == "sjaelland"),]$value ,suffix),
                  paste("Hovedstaden: ", data[which(data$id == "hovedstaden"),]$value ,suffix))

    if(bornholm == T){
      lon_mark[8] <- 11.9
      lat_mark[8] <- 56.9
      val_mark[8] <- paste("Bornholm: ", data[which(data$id == "bornholm"),]$value ,suffix)
    }



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

  return(leafletmap)

}
