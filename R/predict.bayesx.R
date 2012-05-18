predict.bayesx <-
function(object, newdata, model = NULL, term = NULL, ...) 
{
  if(!missing(newdata))
    warning("out of sample prediction currently not available, using fitted values instead!")

  return(fitted.bayesx(object, model, term, ...))
}

