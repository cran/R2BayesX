plot2d <-
function(x, residuals = FALSE, rug = TRUE, jitter = TRUE, 
  col.residuals = NULL, col.lines = NULL, col.polygons = NULL, 
  col.rug = NULL, c.select = NULL, fill.select = NULL, data = NULL, ...)
{
  if(is.null(x))
    return(invisible(NULL))
  if(inherits(x, "formula")) {
    if(is.null(data))
      data <- environment(x)
    else
      if(is.matrix(data))
        data <- as.data.frame(data)
    if(any(grep("+", as.character(x)[2L]))) {
      xch <- as.character(x)
      x <- model.frame(as.formula(paste("~", xch[2L])), data = data)
      x <- cbind(model.frame(as.formula(paste("~", xch[3L])), data = data), x)
    } else x <- model.frame(x, data = data)
    if(ncol(x) < 2L)
      stop("formula is specified wrong!")
  }
  is.bayesx <- grepl(".bayesx", class(x))[1L]
  if(is.data.frame(x))
    x <- df2m(x)
  if(!is.list(x) && !is.matrix(x))
    stop("x must be a matrix!")
  if(!is.list(x) && ncol(x) < 2L)
    stop("x must have at least 2 columns!")
  args <- list(...)
  nc <- ncol(x)
  if(is.null(c.select)) {
    if(is.bayesx)
      c.select <- c(1L, 2L, 3L, 4L, 6L, 7L) 
    else 
      c.select <- 1L:nc
  }
  if(is.null(c.select))
    c.select <- 1L:nc
  if(length(c.select) > nc)
    c.select <- c.select[1L:nc]
  if(is.null(fill.select))
    if(is.bayesx)
      fill.select <- c(0L, 0L, 1L, 2L, 2L, 1L) 
  if(!is.bayesx && length(fill.select) < nc) {
    fill.select <- NULL
    if(ncol(x) > 2L) {
      if(any(rm <- grepl("pcat", colnames(x)))) {
        x <- x[, !rm]
        c.select <- 1:ncol(x)
      }
    }
  }
  if(is.null(col.polygons))
    args$col.polygons <- rep(c("grey80", "grey70"), round(nc/2))
  else
    args$col.polygons <- col.polygons
  if(residuals && !is.null(pres <- attr(x, "partial.resids")))
    residuals <- TRUE
  else
    residuals <- FALSE
  by <- attr(x, "specs")$by
  if(is.null(by))
    by <- "NA"
  xnam <- attr(x, "specs")$term
  if(is.null(xnam))
    xnam <- colnames(x)[1L]
  if(is.null(xnam))
    xnam <- "x"
  if(by[1L] != "NA"){
    if(any(by == 0))
      x <- x[by != 0,]
    if(length(xnam) > 1L)	
      byname <- xnam[length(xnam)]
    else
      byname <- by
		xnam <- xnam[1L]
  }
  if(length(xnam) > 1L)
    xnam <- xnam[1L]
  if(is.null(args$xlab))
    args$xlab <- xnam
  if(is.null(args$ylab)) {
    if(is.null(attr(x, "specs")$label))
      args$ylab <- paste("Effect of", args$xlab)
    else
      args$ylab <- attr(x, "specs")$label
  }	
  if(is.character(c.select)) 
    c.select <- pmatch(c.select, colnames(x))
  x <- x[,c.select]
  if(residuals)
    attr(x, "partial.resids") <- pres
  if(is.null(args$ylim)) {
    ylim <- NULL
    for(j in 2L:ncol(x))
      if(j <= 7L)
        ylim <- c(ylim, x[,j])
    if(residuals)
      args$ylim <- base::range(c(ylim, pres[,2L]), na.rm = TRUE)
    else
      args$ylim <- base::range(ylim, na.rm = TRUE)
  }
  if(is.null(args$xlim))
    args$xlim <- base::range(x[,1L], na.rm = TRUE)
  if(!is.null(args$add) && args$add)
    par(new = TRUE)
  graphics::plot(args$xlim, args$ylim, type = "n", axes = FALSE, 
    xlab = args$xlab, ylab = args$ylab, main = args$main)
  args <- set.plot2d.specs(ncol(x) - 1L, args, col.lines, is.bayesx)
  args$specs <- args
  args$residuals <- residuals
  args$col.residuals <- col.residuals
  args$col.rug <- col.rug
  args$fill.select <- fill.select
  args$pb <- FALSE
  args$rug <- rug
  args$jitter <- jitter
  args$x <- x
  do.call(plot2d.default, delete.args(plot2d.default, args))
  if(is.null(args$type))
    box()
  else
    if(args$type != "n")
      box()
  if(is.null(args$axes)) {
    axis(2L)
    axis(1L)
  } else {
    if(args$axes) {
      axis(2L)
      axis(1L)
    }
  }

  return(invisible(NULL))
}

