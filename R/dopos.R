dopos <-
function(pos, limits, width, height, side.legend)
{
  if(side.legend > 1L)
    limits <- rev(limits)
  if(pos == "bottomleft") {
    xlim <- c(min(limits[[1L]], na.rm = TRUE), min(limits[[1L]], na.rm = TRUE) + width)
    ylim <- c(min(limits[[2L]], na.rm = TRUE), min(limits[[2L]], na.rm = TRUE) + height)
  }
  if(pos == "topleft") {
    xlim <- c(min(limits[[1L]], na.rm = TRUE), min(limits[[1L]], na.rm = TRUE) + width)
    ylim <- c(max(limits[[2L]], na.rm = TRUE) - height, max(limits[[2L]], na.rm = TRUE))
  }
  if(pos == "topright") {
    xlim <- c(max(limits[[1L]], na.rm = TRUE) - width, max(limits[[1L]], na.rm = TRUE))
    ylim <- c(max(limits[[2L]], na.rm = TRUE) - height, max(limits[[2L]], na.rm = TRUE))
  }
  if(pos == "bottomright") {
    xlim <- c(max(limits[[1L]], na.rm = TRUE) - width, max(limits[[1L]], na.rm = TRUE))
    ylim <- c(min(limits[[2L]], na.rm = TRUE), min(limits[[2L]], na.rm = TRUE) + height)
  }

  return(list(xlim = xlim, ylim = ylim))
}

