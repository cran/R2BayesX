centroids <-
function(map) 
{
  n <- length(map)
  cp <- matrix(0, n, 2L)
  for(i in 1L:n)
    cp[i,] <- centroidpos(map[[i]])
  rownames(cp) <- names(map)
  colnames(cp) <- c("xco", "yco")

  return(cp)
}

