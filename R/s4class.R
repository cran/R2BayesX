s4class <-
function(x)
{
  if(grepl("_random", x))
    cx <- "random.bayesx"
  if(grepl("_pspline", x))
    cx <- "sm.bayesx"
  if(grepl("_season", x))
    cx <- "sm.bayesx"
  if(grepl("_rw", x))
    cx <- "sm.bayesx"
  if(grepl("_spatial", x))
    cx <- "mrf.bayesx"
  if(grepl("_geospline", x))
    cx <- "geo.bayesx"
  if(grepl("_geokriging", x))
    cx <- "geo.bayesx"
  if(grepl("_logbaseline", x))
    cx <- "sm.bayesx"
  if(grepl("_kriging", x))
    cx <- "sm.bayesx"

  return(cx)
}

s4bs <-
function(x)
{
  if(grepl("_random", x))
    bs <- "re"
  if(grepl("_pspline", x))
    bs <- "ps"
  if(grepl("_season", x))
    bs <- "season"
  if(grepl("_rw", x))
    bs <- "rw"
  if(grepl("_spatial", x))
    bs <- "mrf"
  if(grepl("_geospline", x))
    bs <- "gs"
  if(grepl("_geokriging", x))
    bs <- "gk"
  if(grepl("_logbaseline", x))
    bs <- "bl"
  if(grepl("_kriging", x))
    bs <- "kr"

  return(bs)
}
