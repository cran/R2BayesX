cpos <-
function(p, np) 
{
  rval <- .Call("cpos",
    as.numeric(p),
    as.integer(np))

  return(rval)
}

