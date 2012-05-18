samples <-
function(object, model = NULL, term = NULL, acf = FALSE, ...)
{
  if(is.null(object) || length(object) < 2L)
    stop("nothing to do!")
  is.fit <- FALSE
  if(inherits(object, "data.frame")) {
    if(is.null(xn <- attr(object, "specs")$term))
      xn <- paste(xn, collapse = ".", sep = "")
    else xn <- names(object)[1L]
    object <- list(object)
    names(object) <- xn
    object <- list(list(effects = object))
    term <- 1L
    is.fit <- TRUE
  }
  if(inherits(object, "fit.bayesx") && !is.fit) {
    is.fit <- TRUE
    if(any(grepl("bayesx", class(object[[1L]]))))
      object <- list(object)
    k <- length(object)
    tmp <- list()
    for(i in 1L:k)
      tmp[[i]] <- list(effects = object[[i]])
    object <- tmp
    if(is.null(term))
      term <- 1L
  }
  if(is.null(term) && !is.fit)
    term <- "linear-samples"
  if(!is.fit)
    object <- get.model(object, model)
  rval <- list()
  k <- length(object)
  mn <- rep("model", length.out = k)
  for(i in 1L:k) {
    rval[[i]] <- list()
    for(j in 1:length(term)) {
      if(is.na(match(term[j], "linear-samples")) && is.na(match(term[j], "var-samples"))) {
        if(!is.null(attr(object[[i]]$effects[[term[j]]], "specs")) &&
          !is.null(attr(object[[i]]$effects[[term[j]]], "specs")$is.factor) && 
          attr(object[[i]]$effects[[term[j]]], "specs")$is.factor) {
          sattr <- NULL
          for(jj in 1L:length(object[[i]]$effects[[term[j]]]))
            sattr <- cbind(sattr, attr(object[[i]]$effects[[term[j]]][[jj]], "sample"))
          if(!is.null(sattr) && ncol(sattr) > 1L)
            colnames(sattr) <- paste("C", 1L:ncol(sattr), sep = "")
          attr(object[[i]]$effects[[term[j]]], "sample") <- sattr
        }
        tmp <- list()
        if(!is.null(attr(object[[i]]$effects[[term[j]]], "sample")))
          tmp$Coef <- attr(object[[i]]$effects[[term[j]]], "sample")
        if(!is.null(attr(object[[i]]$effects[[term[j]]], "variance.sample")))
          tmp$Var <- attr(object[[i]]$effects[[term[j]]], "variance.sample")
        if(acf) {
          tmp2 <- list()
          if(!is.null(tmp$Coef))           
            tmp2$Coef <- samplesacf(tmp$Coef, ...)
          if(!is.null(tmp$Var))
            tmp2$Var <- samplesacf(tmp$Var, ...)
          tmp <- tmp2
        }
        if(!is.null(xn <- attr(object[[i]]$effects[[term[j]]], "specs")$term))
          xn <- paste(xn, collapse = ".", sep = "")
        else
          xn <- term[j]
        eval(parse(text = paste("rval[[i]]$'", xn, "' <- tmp", sep = "")))
      } 
      if(!is.na(match(term[j], "linear-samples"))) {
        if(acf) {
          if(!is.null(attr(object[[i]]$fixed.effects, "sample"))) {
            tmp <- samplesacf(attr(object[[i]]$fixed.effects, "sample"), ...)
          } else tmp <- NULL
        } else tmp <- attr(object[[i]]$fixed.effects, "sample")
        eval(parse(text = paste("rval[[i]]$'Lin' <- tmp", sep = "")))
      } 
      if(!is.na(match(term[j], "var-samples"))) {
        if(acf) {
          if(!is.null(attr(object[[i]]$variance, "sample"))) {
            tmp <- samplesacf(attr(object[[i]]$variance, "sample"), ...)
          } else tmp <- NULL
        } else tmp <- attr(object[[i]]$variance, "sample")
        eval(parse(text = paste("rval[[i]]$'Var' <- tmp", sep = "")))
      } 
    }
    if(!is.null(object[[i]]$bayesx.setup$model.name) && k > 1L)
      mn[i] <- object[[i]]$bayesx.setup$model.name
  }
  if(k > 1L) {
    mn[duplicated(mn)] <- paste(mn[duplicated(mn)], 1:length(mn[duplicated(mn)]) + 1L, sep = "")
    names(rval) <- mn
  } else rval <- rval[[1L]]
  if(is.null(rval) || length(rval) < 1L)
    rval <- NA
  if(any(is.na(rval)))
    warning("samples are missing in object!")
  rval <- delete.NULLs(rval)
  if(length(rval) < 2L && length(rval[[1L]]) < 2L)
    names(rval[[1L]]) <- names(rval)
  rval <- as.data.frame(rval)

  return(rval)
}


samplesacf <- function(x, ...) 
{
  if(is.matrix(x)) {
    rval <- NULL
    for(j in 1L:ncol(x))
      rval <- cbind(rval, stats::acf(stats::ts(x[,j]), plot = FALSE, ...)$acf)
    rval <- rval[2L:nrow(rval),]
    if(!is.matrix(rval))
      rval <- matrix(rval, ncol = 1L)
    colnames(rval) <- colnames(x)
    rownames(rval) <- paste("lag-", 1L:nrow(rval), sep = "")
  } else {
    rval <- as.vector(stats::acf(stats::ts(x), plot = FALSE, ...)$acf)
    rval <- rval[2L:length(rval)]
    names(rval) <- paste("lag-", 1L:length(rval), sep = "")
  }

  return(rval)
}

