SPDF2bnd <- function(map, poly.names = NULL)
{
  if(!is(map, "SpatialPolygonsDataFrame"))
    stop("map must be of class 'SpatialPolygonsDataFrame'!")
  require("maptools")
  rval <- list()
  gn <- FALSE
  pn <- NULL
  if(is.null(poly.names)) {
    gn <- TRUE
  } else {
    if(length(poly.names) < 2L) {
      pnd <- as.character(slot(map, "data")[[grep(poly.names, names(slot(map, "data")),
        ignore.case = TRUE)]])
    } else {
      pnd <- poly.names
    }
  }
  k <- 1
  for(i in 1:length(slot(map, "polygons"))) {
    np <- length(slot(slot(map, "polygons")[[i]], "Polygons"))
    for(j in 1:np) {
      rval[[k]] <- slot(slot(slot(map, "polygons")[[i]], "Polygons")[[j]], "coords")
      k <- k + 1
    }
    if(gn)
      pn <- c(pn, rep(slot(slot(map, "polygons")[[i]], "ID"), length.out = np))
    else
      pn <- c(pn, rep(pnd[i], length.out = np))
  }
  names(rval) <- pn
  class(rval) <- c("bnd", "list")
  attr(rval, "asp") <- 1.5 ## as.numeric(mapasp(map)) FIXME
  rval
}
