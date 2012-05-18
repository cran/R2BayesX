f2int <- function(x, type = 1L) 
{
  warn <- getOption("warn")
  options(warn = -1L)
  if(any(is.na(as.numeric(as.character(x)))))
    if(is.factor(x))
      levels(x) <- 1:nlevels(x)
  x <- as.integer(as.numeric(as.character(x)))
  if(type != 2L)
    x <- x - 1L
  if(min(x) < 0)
    x <- x + min(x)
  options(warn = warn)

  return(x)
}
