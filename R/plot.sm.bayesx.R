plot.sm.bayesx <-
function(x, diagnostics = FALSE, ...)
{
  if(!is.null(x)) {
    args <- list(...)
    args$x <- x
    if(diagnostics == FALSE) {
      if(attr(x, "specs")$dim == 1L)
        plottype <- "plot2d"
      else
        plottype <- "plot3d"
      do.call(plottype, args)
    } else coeffun(x, args, diagnostics)
  } else warning("there is nothing to plot!")

  return(invisible(NULL))
}

