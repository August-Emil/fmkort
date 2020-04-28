# Helper function
#
# Disse er taget fra leafletDK" pakken lavet af Mikkel Krogsholm
join_map_data <- function(value = NULL, id = NULL, mapdata = NULL, shapefile){

  # Fix possible encoding issues
  shapefile$name <- fix_names_encoding(shapefile$name)
  shapefile@data$name <- fix_names_encoding(shapefile@data$name)

  shapefile_data <- shapefile@data

  mapdata$joinID <- fix_names_join(fix_names_encoding(mapdata[id][[1]]))

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- dplyr::left_join(shapefile_data, mapdata, by = "joinID")

  shapefile@data <- shapefile_data

  missingnames <- unique(shapefile$name[is.na(shapefile$values)])

  if(length(missingnames) != 0){message(paste0("Missing values for ", sort(missingnames), "\n"))}

  # shapefile <- subset(shapefile, !(is.na(shapefile$values)))

  return(shapefile)
}

# Helper function.
fix_names_encoding <- function(x) {
  x <- gsub("\u00C3\u00A6", "\u00E6", x)
  x <- gsub("\u00C3\u00B8", "\u00F8", x)
  x <- gsub("\u00C3\u00A5", "\u00E5", x)
  x <- gsub("\u00C3\u2020", "\u00C6", x)
  x <- gsub("\u00C3\u02DC", "\u00D8", x)
  x <- gsub("\u00C3\u2026", "\u00C5", x)
  return(x)
}

# Helper function.
fix_names_join <- function(x, to_lower = TRUE) {
  x <- gsub("\u00E6", "ae", x)
  x <- gsub("\u00F8", "oe", x)
  x <- gsub("\u00E5", "aa", x)
  x <- gsub("\u00C6", "Ae", x)
  x <- gsub("\u00D8", "Oe", x)
  x <- gsub("\u00C5", "Aa", x)
  x <- gsub("-", "", x)
  x <- gsub(" ", "", x)

  if(to_lower){
    x <- tolower(x)
  }

  return(x)
}

# Helper
labelFormat_decimal <- function (prefix = "", suffix = "", between = " &ndash; ", digits = 3,
                                 big.mark = ",", transform = identity, decimal.mark = "."){
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark, decimal.mark = decimal.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}
