AIC.bayesx <-
function(object, ..., k)
{
  obj <- list(...)
  if(length(obj)) 
    for(k in 1L:length(obj))
      if(inherits(obj[[k]], "bayesx"))
        object <- c(object, obj[[k]])
  val <- extract.model.diagnostic(object, 1L:length(object), "AIC", FALSE)
  if(length(obj)) {
    Call <- match.call()
    Call$k <- NULL
    val <- data.frame(df = extract.model.diagnostic(object, 1L:length(object), "df", FALSE), 
      AIC = val)
    row.names(val) <- as.character(Call[-1L])
  } else {
    if(length(val) > 1L) {
      val <- data.frame(df = extract.model.diagnostic(object, 1L:length(object), "df", FALSE), 
        AIC = val)
      rownames(val) <- names(object)
    }
  }

  return(val)
}

