DIC.bayesx <-
function(object, ...)
{
  obj <- list(...)
  if(length(obj)) 
    for(k in 1L:length(obj))
      if(inherits(obj[[k]], "bayesx"))
        object <- c(object, obj[[k]])
  val <- extract.model.diagnostic(object, 1L:length(object), "DIC", FALSE)
  if(length(obj)) {
    Call <- match.call()
    val <- data.frame(pd = extract.model.diagnostic(object, 1L:length(object), "pd", FALSE), 
      DIC = val)
    row.names(val) <- as.character(Call[-1L])
  } else {
    if(length(val) > 1L) {
      val <- data.frame(pd = extract.model.diagnostic(object, 1L:length(object), "pd", FALSE), 
        DIC = val)
      rownames(val) <- names(object)
    }
  }

  return(val)
}

