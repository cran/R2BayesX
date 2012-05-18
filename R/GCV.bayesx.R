GCV.bayesx <-
function(object, ...)
{
  obj <- list(...)
  if(length(obj)) 
    for(k in 1L:length(obj))
      if(inherits(obj[[k]], "bayesx"))
        object <- c(object, obj[[k]])
  val <- extract.model.diagnostic(object, 1L:length(object), "GCV", FALSE)
  if(length(obj)) {
    Call <- match.call()
    val <- data.frame(df = extract.model.diagnostic(object, 1L:length(object), "df", FALSE), 
      GCV = val)
    row.names(val) <- as.character(Call[-1L])
  } else {
    if(length(val) > 1L) {
      val <- data.frame(df = extract.model.diagnostic(object, 1L:length(object), "df", FALSE), 
        GCV = val)
      rownames(val) <- names(object)
    }
  }

  return(val)
}

