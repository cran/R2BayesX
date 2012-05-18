plotmap <-
function(map, x = NULL, id = NULL, c.select = NULL, legend = TRUE, 
  swap = FALSE, range = NULL, names = FALSE, values = FALSE, col = NULL, ncol = 100, 
  breaks = NULL, cex.legend = 1, cex.names = 1, cex.values = cex.names, digits = 2L,
  mar.min = 2, add = FALSE, ...)
{
  if(missing(map))
    stop("map object is missing!")
  if(is(map, "SpatialPolygonsDataFrame"))
    map <- SPDF2bnd(map)
  if(!is.list(map))
    stop("argument map must be a list() of matrix polygons!")
  args <- list(...)
  map.limits <- find.limits(map, mar.min, ...)
  if(is.null(args$asp))
    args$asp <- attr(map, "asp")
  n <- length(map)
  if(is.null(x))
    legend <- FALSE
  poly.names.orig <- names(map)
  if(!any(is.na(poly.names <- x2int(names(map))))) {
    poly.names <- sort(poly.names)
    poly.names <- as.character(poly.names)
  } else {
    poly.names <- sort(names(map))
  }
  ## FIXME
  ## if(length(unique(poly.names)) < length(poly.names)) {
  ##   names(map) <- poly.names <- paste(1L:length(map))
  ## }
  map <- map[poly.names]
  poly.names <- names(map)
  surrounding <- attr(map, "surrounding")
  inner.id <- which(sapply(surrounding, length) > 0L)
  if(length(inner.id)) {
    poly.names <- c(poly.names[- inner.id], poly.names[inner.id])
    map <- c(map[- inner.id], map[inner.id])
  }
  if(!is.null(args$ylim))
    map.limits$ylim <- args$ylim
  if(!is.null(args$xlim))
    map.limits$xlim <- args$xlim
  if(is.null(args$symmetric))
    symmetric <- TRUE
  else
    symmetric <- args$symmetric
  if(!is.null(x)) {
    if(is.null(col)) {
      col <- colorspace::diverge_hcl
      # col <- colorspace::diverge_hcl(ncol, h = c(130, 10), c = 250,
      #  l = c(30, 90), power = 1.5, gamma = 2.4, fixup = TRUE)
    }
    x <- compute.x.id(x, id, c.select, range, symmetric)
    colors <- make_pal(col = col, ncol = ncol, data = x$x, 
      range = range, breaks = breaks, swap = swap, 
      symmetric = symmetric)$map(x$x)
  } else {
    if(is.null(col))
      colors <- rep(NA, length.out = n)
    else {
      if(is.function(col))
        colors <- col(ncol)
      else colors <- col
      colors <- rep(colors, length.out = n)
    }
  }
  if(legend && !is.null(args$pos) && args$pos[1L] == "right") {
    par.orig <- par(c("mar", "las", "mfrow"))
    mar.orig <- mar <- par.orig$mar
    mar[4L] <- 0
    on.exit(par(par.orig))
    par(mar = mar)
    layout(matrix(c(1, 2), nrow = 1), widths = c(1, 0.18))
  }
  if(!is.null(map.limits$mar) && is.null(args$asp) && !add)
    par(mar = map.limits$mar)
  args$x <- map.limits$x
  args$y <- map.limits$y
  if(is.null(args$type))
    args$type <- "n"
  if(is.null(args$axes))
    args$axes <- FALSE
  if(is.null(args$xlab))
    args$xlab <- ""
  if(is.null(args$ylab))
    args$ylab <- ""
  if(!add)
    do.call(graphics::plot.default, delete.args(graphics::plot.default, args)) 
  args$ylab <- args$xlab <- args$main <- ""
  args$type <- NULL
  args$axes <- NULL
  lwd.p <- rep(args$lwd, length.out = n)
  if(is.null(lwd.p))
    lwd.p <- rep(1, length.out = n)
  lty.p <- rep(args$lty, length.out = n)
  if(is.null(lty.p))
    lty.p <- rep(1, length.out = n)
  border.p <- rep(args$border, length.out = n)
  if(is.null(border.p))
    border.p <- rep("black", length.out = n)
  density.p <- rep(args$density, length.out = n)
  angle.p <- rep(args$angle, length.out = n)
  if(is.null(angle.p))
    angle.p <- rep(90, length.out = n)
  i <- 1L
  for(poly in poly.names) {
    args$x <- map[[poly]][,1L]
    args$y <- map[[poly]][,2L]
    args$border <- border.p[i]
    args$angle <- angle.p[i]
    args$lwd <- lwd.p[i]
    args$lty <- lty.p[i]
    if(!is.null(density.p))
      args$density <- density.p[i]
    if(!is.null(x)){ 
      if(!is.na(k <- pmatch(poly, x$id))) {
        args$col <- colors[k]
        args$density <- NULL
      } else {
        args$col <- NULL
        if(is.null(args$density))
          args$density <- 20L
      }
    } else args$col <- colors[i]
    do.call(graphics::polygon, 
    delete.args(graphics::polygon, args, 
    c("lwd", "cex", "lty")))
    if(names && !values) {
      args$polygon <- map[[poly]]
      args$poly.name <- poly.names.orig[i]
      args$counter <- i
      args$cex <- cex.names
      do.call(centroidtext, delete.args(centroidtext, args, "font"))
    }
    if(values && !names) {
      args$polygon <- map[[poly]]
      args$poly.name <- as.character(round(x$x[k], digits = digits))
      args$counter <- k
      args$cex <- cex.values
      do.call(centroidtext, delete.args(centroidtext, args, "font"))
    }
    i <- i + 1L
  }
  if(legend) {
    if(is.null(args$pos))
      args$pos <- "topleft"
    if(args$pos == "right") {
      args$full <- TRUE
      args$side.legend <- 2L
      args$side.ticks <- 2L
      mar <- mar.orig
      mar[2L] <- 0
      par(mar = mar, xaxs = "i", yaxs = "i")
      args$plot <- TRUE
      args$add <- FALSE
    } else {
      args$plot <- FALSE
      if(is.null(args$xpd))
        args$xpd <- TRUE
      args$add <- TRUE
    }
    if(is.null(args$width))
      args$width <- 0.6
    if(is.null(args$height))
      args$height <- 0.2
    if(is.null(args$distance.labels))
     args$distance.labels <- 2
    if(is.null(args$length.ticks))
      args$length.ticks <- 2L
    args$xlim <- map.limits$xlim
    args$ylim <- map.limits$ylim
    args$color <- col
    args$ncol <- ncol
    args$x <- x$x
    args$breaks <- breaks
    args$swap <- swap
    args$digits <- digits
    args$cex.labels <- cex.legend
    args$symmetric <- symmetric
    args$range <- range
    if(is.null(args$lrange))
      args$lrange <- x$range
    do.call(colorlegend, delete.args(colorlegend, args, c("font")))
  }
  if(!is.null(args$xlab))
    mtext(args$xlab, side = 1L)
  if(!is.null(args$ylab))
    mtext(args$ylab, side = 2L)

  return(invisible(NULL))
}

