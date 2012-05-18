bayesx.construct.te.smooth.spec <- bayesx.construct.pspline2dimrw2.smooth.spec <- 
function(object, dir, prg, data)
{
  object$by <- object$term[1L]
  object$term <- object$term[2L]
  class(object) <- "ps.smooth.spec"
  term <- bayesx.construct(object, dir, prg, data)
  term <- gsub("psplinerw2", "pspline2dimrw2", term)
  term <- gsub("psplinerw1", "pspline2dimrw1", term)

  return(term)
}

