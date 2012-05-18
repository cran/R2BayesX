setmfrow <-
function(np) 
{
  if(np > 12L) {
    par(ask = TRUE)
    par(mfrow = c(4L, 3L))
  } else par(mfrow = n2mfrow(np))

  return(invisible(NULL))
}

