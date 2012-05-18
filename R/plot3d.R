plot3d <-
function(x, residuals = FALSE, col.surface = NULL, 
  ncol = 99L, swap = FALSE, col.residuals = NULL, col.contour = NULL, 
  c.select = NULL, grid = 30L, image = FALSE, contour = FALSE, 
  legend = TRUE, cex.legend = 1, breaks = NULL, range = NULL, 
  digits = 2L, d.persp = 1L, r.persp = sqrt(3), linear = TRUE, extrap = FALSE, 
  outscale = 0, data = NULL, ...)
{
  if(isTRUE(getOption("use.akima"))) {
    stopifnot(require("akima"))
  } else {
    if(require("akima")) {
      cat("NOTE: Package 'akima' has an ACM license that restricts applications to non-commercial usage.\n")
    } else {
      stop(paste("plot3() can only be used if the 'akima' package is installed. ",
        "Note that 'akima' has an ACM license that restricts applications to ",
        "non-commercial usage.", sep = ""))
    }
  }
  
  if(is.null(x))
    return(invisible(NULL))
  if(inherits(x,"formula")) {
    if(is.null(data))
      data <- environment(x)
    else
      if(is.matrix(data))
        data <- as.data.frame(data)
    x <- model.frame(x, data = data)
    if(ncol(x) < 3L)
      stop("formula is specified wrong!")
    if(ncol(x) > 3L)
      x <- x[,c(2L, 3L, 1L, 4L:ncol(x))]
    else
      x <- x[,c(2L, 3L, 1L)]
  }
  if(is.data.frame(x))
    x <- df2m(x)
  if(!is.matrix(x))
    stop("x must be a matrix!")
  if(ncol(x) < 3)
    stop("x must have at least 3 columns!")
  args <- list(...)
  e <- NULL
  if(!is.null(attr(x, "partial.resids")))
    e <- attr(x, "partial.resids")
  if(!is.null(e) && all(is.na(e)))
    residuals <- FALSE
  specs <- attr(x, "specs")
  by <- specs$by
  if(is.null(by))
    by <- "NA"
  else {
    if(!is.null(specs)  && length(specs$term) > 2L)
      by <- specs$term[length(specs$term)]
  }
  nx <- colnames(x)
  x <- x[order(x[,1L]),]
  X <- x[,1L]
  z <- x[,2L]
  xrd <- diff(range(X))
  zrd <- diff(range(z))
  xn <- seq(min(X) - outscale * xrd , max(X) + outscale * xrd, length = grid)
  zn <- seq(min(z) - outscale * zrd, max(z) + outscale * zrd, length = grid)
  fitted <- list(NA)
  if(!is.null(c.select)) {
    take <- NULL
    id <- 1L:length(nx)
    if(length(c.select) < 2L && c.select == 95) 
      c.select <- as.character(c.select)
    if(length(c.select) < 2L && c.select == 80)
      c.select <- as.character(c.select)
    is.se <- FALSE
    if(!is.na(pmatch("95", c.select))) {
      take <- id[nx %in% c("2.5%", "97.5%")]
      is.se <- TRUE
    }
    if(!is.na(pmatch("80", c.select))) {
      take <- id[nx %in% c("10%", "90%")]
      is.se <- TRUE
    }
    if(is.se) {
      take2 <- c("mean", "Mean", "MEAN", "estimate", 
        "Estimate", "ESTIMATE", "mean", "pmode")
      for(k in take2)
        if(any(nx %in% k))
          take <- c(take[1], id[nx %in% k][1], take[2])
    }
    if(is.null(take) && !is.character(c.select)) {
      if(min(c.select) < 3L)
        c.select <- c.select + 2L
      if(max(c.select) > ncol(x) || min(c.select) < 3L)
        stop("argument c.select is specified wrong!")
      take <- unique(c.select)
    }
    if(is.null(take) && is.character(c.select))
      for(k in c.select)
        for(i in 1L:length(nx))
          if(!is.na(pmatch(k, nx[i])))
            take <- c(take, i)
    if(is.null(take))
      stop("argument c.select is specified wrong!")
    for(k in 1:length(take)) {
      fitted[[k]] <- akima::interp(X, z, x[,take[k]], xo = xn, yo = zn, 
        duplicate = "strip", linear = linear, extrap = extrap)$z
    }
  }
  if(length(fitted[[1L]]) == 1L && is.na(fitted[[1L]][1L])) {
    fitted[[1L]] <- akima::interp(X, z, x[,3L], xo = xn, yo = zn, 
      duplicate = "strip", linear = linear, extrap = extrap)$z
  }
  if(!is.null(range))
    for(k in 1L:length(fitted)) {
      if(min(range, na.rm = TRUE) > min(fitted[[k]], na.rm = TRUE))
        fitted[[k]][fitted[[k]] < min(range, na.rm = TRUE)] <- min(range, na.rm = TRUE)  
      if(max(range, na.rm = TRUE) < max(fitted[[k]], na.rm = TRUE))
        fitted[[k]][fitted[[k]] > max(range, na.rm = TRUE)] <- max(range, na.rm = TRUE)  
    }
  names <- colnames(x)[1L:2L]
  if(residuals)
    zlimit <- range(c(unlist(fitted), e[,3L]), na.rm = TRUE)
  else
    zlimit <- range(unlist(fitted), na.rm = TRUE)
  if(is.null(args$xlab))
    args$xlab <- names[1L]
  if(is.null(args$ylab))
    args$ylab <- names[2L]
  if(is.null(args$zlab)) {
    if(!is.null(specs) && is.null(specs$label))
      args$zlab <- "fitted"
    else
      args$zlab <- specs$label
  }
  if(is.null(args$zlab))
    args$zlab <- try(paste("f(", nx[1L], ",", nx[2L], ")", sep = ""))
  args$y <- substitute(zn)
  args$x <- substitute(xn)
  if(is.null(col.surface))
    col.surface <- colorspace::diverge_hcl
  if(!is.null(args$add) && args$add)
    par(new = TRUE)
  if(!image && !contour) {
    myfit <- matrix(fitted[[1L]], grid, grid)
    if(length(fitted) < 2L) {
      args$col <- make_pal(col = col.surface, ncol = ncol, data = myfit, 
        range = range, breaks = breaks, swap = swap, 
        symmetric = args$symmetric)$map(myfit)
    } else args$col <- col.surface
    args$z <- substitute(myfit)
    args$d <- d.persp
    args$r <- r.persp
    if(is.null(args$zlim))
      args$zlim <- zlimit
    if(is.null(args$theta))
      args$theta <- 40
    if(is.null(args$phi))
      args$phi <- 40
    if(!is.null(c.select) && length(fitted) > 1L) {
      nf <- length(fitted)
      if(is.null(args$border))
        args$border <- c("green", "black", "red")
      if(is.function(args$col) || is.null(args$col))
        args$col <- NA
      color <- rep(args$col, length.out = nf)
      bcol <- rep(args$border, length.out = nf)
      args$col <- color[1L]
      args$border <- bcol[1L]
      pmat <- do.call(graphics::persp, 
        delete.args(graphics:::persp.default, args, c("lwd", "lty")))
      for(k in 2L:length(fitted)) {
        par(new = TRUE)
        args$col <- color[k]
        args$border <- bcol[k]
        myfit <- matrix(fitted[[k]], grid, grid)
        args$z <- substitute(myfit)
        pmat <- do.call(graphics::persp, 
          delete.args(graphics:::persp.default, args, c("lwd", "lty")))
      }
    } else {
      pmat <- do.call(graphics::persp, delete.args(graphics:::persp.default, args, c("lwd", "lty")))
    }
    if(residuals && !is.null(e)) {
      t3d <- trans3d(e[,1L], e[,2L], e[,3L], pmat)
      if(is.null(col.residuals))
        col.residuals <- "black"
      points(x = t3d$x, y = t3d$y, cex = args$cex, pch = args$pch, col = col.residuals)
    }
  }
  if(image || contour) {
    myfit <- matrix(fitted[[1L]], grid[1L], grid[1L])
    pal <- make_pal(col = col.surface, ncol = ncol, data = myfit, 
      range = range, breaks = breaks, swap = swap, 
      symmetric = args$symmetric)
    args$col <- pal$colors
    args$breaks <- pal$breaks
    if(is.null(args$xlim))
      args$xlim <- range(xn)
    if(is.null(args$ylim))
      args$ylim <- range(zn)
    add <- FALSE
    args$z <- substitute(myfit)
    args$x <- xn
    args$y <- zn
    args$zlab <- NULL
    if(image) {
      mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
      mar <- mar.orig
      on.exit(par(par.orig))
      if(legend) {
        mar[4L] <- 0
        par(mar = mar)
        layout(matrix(c(1, 2), nrow = 1), widths = c(1, 0.2))
      }
      do.call(graphics::image, 
        delete.args(graphics::image.default, args, 
        c("xlab", "ylab", "main", "axes")))
      if(!is.null(args$image.map)) {
          args2 <- args
          args2$map <- args$image.map
          args2$add <- TRUE
          args2$legend <- FALSE
          args2$x <- NULL
          args2$id <- NULL
          args2$col <- NULL
          do.call(plotmap, delete.args(plotmap, args2))
      }
      if(contour) {
        if(is.null(col.contour)) 
          args$col <- "black"
        else
          args$col <- col.contour
        args$add <- TRUE
        do.call(graphics::contour.default, 
          delete.args(graphics::contour.default, args, 
          c("xlab", "ylab", "main", "axes")))
        contour <- FALSE
      }
      if(legend) {
        mar <- mar.orig
        mar[2L] <- 0.5
        par(mar = mar, xaxs = "i", yaxs = "i")
        args2 <- args
        if(is.null(args$side.legend))
          args2$side.legend <- 2L
        if(is.null(args$distance.labels))
          args2$distance.labels <- 3L
        if(is.null(args$side.ticks))
          args2$side.ticks <- 2L
        args2$color <- col.surface
        args2$ncol <- ncol
        args2$x <- args$z
        args2$xlim <- range(xn)
        args2$ylim <- range(zn)
        args2$breaks <- breaks
        args2$swap <- swap
        args2$plot <- TRUE
        args2$digits <- digits
        args2$cex <- cex.legend
        args2$range <- range
        args2$add <- FALSE
        args2$full <- TRUE
        do.call(colorlegend, delete.args(colorlegend, args2, c("font", "cex")))
      }
    }
    if(contour) {
      if(is.null(col.contour)) 
        args$col <- "black"
      else
        args$col <- col.contour
      do.call(graphics::contour.default, 
        delete.args(graphics::contour.default, args, 
        c("xlab", "ylab", "main", "axes")))
    }
  }

  return(invisible(args))
}

