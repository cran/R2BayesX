find.smooth.random <-
function(dir, files, data, response, eta, model.name, minfo, info)
{
  if(file.exists(info)) {
    info <- readLines(info)
    info <- info[1L:(length(info) - 1L)]
  } else info <- NULL
  effects <- list()
  SmoothHyp <- RandomHyp <- fsnames <- NULL
  if(any((i <- grep(".res", files)))) {
    resfiles <- files[i]
    endings <- c("_predict.res", "FixedEffects", "LinearEffects", "scale.res", "_variance_", 
      "_var.res", ".raw", "_param.res", "_interact.res", "_df.res", "_lambda.res", 
      "_knots.raw", "_contour.res", "_theta.res", "_variance_sample.ps", "_random_fixed.res")
    for(res in endings)
      resfiles <- resfiles[!grepl(res, resfiles, fixed = TRUE)]
    resfiles <- resfiles[!grepl("_lasso", resfiles)]
    resfiles <- resfiles[!grepl("_ridge", resfiles)]
    resfiles <- resfiles[!grepl("_nigmix", resfiles)]
    if(length(resfiles) > 0L) {
      neffect <- 1; nameseffects <- NULL
      for(res in resfiles) {
        cxbs <- NULL
        x <- df2m(read.table(paste(dir, "/", res, sep = ""), header = TRUE))
        dimx <- s4dim(x)
        if(sum(x[,(dimx + 1L):ncol(x)], na.rm = TRUE) != 0) {
          colnx <- colnames(x)
          x <- x[order(x[,1L]),]
          colnames(x) <- rep(colnx, length.out = ncol(x))
          cx <- s4class(res)
          cxbs <- s4bs(res)
          dimx2 <- dimx
          if(cx == "geo.bayesx") {
            dimx2 <- dimx + 1L
            xnam <- colnames(x)[1L:dimx2]
            xnam2 <- xnam[1L]
          } else xnam <- xnam2 <- colnames(x)[1L:dimx2]
          # xnam <- unlist(strsplit(xnam, "_c"))
          xnam2 <- xnam
          vx <- vx2 <- vx3 <- NULL
          if(grepl("_effect_of_", res, fixed = TRUE)) {
            res2 <- strsplit(res, "_effect_of_")[[1L]]
            res2 <- gsub(".res", "", res2[length(res2)], fixed = TRUE)
            for(gsx in xnam)
              res2 <- gsub(gsx, "", res2, fixed = TRUE)
            if(length(res2 <- splitme(res2))) {
              vx <- resplit(res2[1L:(length(res2) - 1L)])
            }
          }
          if(grepl("_f_", res, fixed = TRUE)) {
            res2 <- gsub(paste(model.name, "_", sep = ""), "", strsplit(res, "_f_")[[1L]])[1L]
            if(res2 != model.name)
              vx <- res2
            if(length(res3 <- strsplit(res, "_f_")[[1L]]) > 1L) {
              res3 <- strsplit(res3[2L], xnam)[[1L]]
              res3 <- strsplit(res3[1L], "_")[[1L]]
              if(length(res3) >= 1L) {
                  vx2 <- vx3 <- res3[1L]
                if(!is.null(minfo)) {
                  if(!is.null(minfo$YLevels) && !is.null(minfo$nYLevels)) {
                    oL <- eval(parse(text = minfo$YLevels))
                    nL <- eval(parse(text = minfo$nYLevels))
                    vx2 <- oL[nL == res3[1L]]
                  } else vx2 <- vx3 <- NULL
                }
              }
            }
          }
          colnames(x)[1L:dimx2] <- rep(rrmfs(xnam), length.out = length(1L:dimx2))
          rownames(x) <- 1L:nrow(x)
          labelx <- make.label(cx, xnam, dimx, vx)
          if(!is.null(info)) {
            for(k in 1L:length(info)) {
              term <- eval(parse(text = info[k]))
              if(!is.null(term$term)) {
                if(!is.null(term$israndom) && term$israndom) {
                  term2 <- gsub("f(", "r(", term$term, fixed = TRUE)
                  term2 <- gsub("s(", "r(", term2, fixed = TRUE)
                  if(length(grep("sx(", term$term, fixed = TRUE))) {
                    term2 <- gsub("r(", "sx(", term2, fixed = TRUE)
                    labelx <- gsub("r(", "sx(", labelx, fixed = TRUE)
                  }
                } else {
                  term2 <- gsub("f(", "s(", term$term, fixed = TRUE)
                  term2 <- gsub("sx(", "s(", term$term, fixed = TRUE)
                }
                if(term2 == make.label(cx, rrmfs(xnam), dimx, NULL)) {
                  labelx <- term$term
                  if(!is.null(vx))
                    labelx <- paste(labelx, ":", vx, sep = "")
                }
              }
            }
          }
          if(!is.null(vx2))
            labelx <- paste(labelx, ":", vx2, sep = "")
          if(grepl("_spatialtotal.res", res))
            labelx <- paste(labelx, ":total", sep = "")
          ## labelx <- paste(labelx, if(!is.null(cxbs)) cxbs else cx, sep = ":")
          ## labelx <- gsub("total:mrf", "total", labelx)
          if(!is.null(data))
            attr(x, "partial.resids") <- blow.up.resid(data, x, xnam, response, eta, dimx, cx)
          attr(x, "specs") <- list(dim = dimx, term = rrmfs(xnam), 
            by = rrmfs(vx), label = rrmfs(labelx), is.factor = FALSE)
          ## search and set additional attributes
          nx <- length(xnam2)
          if(nx > 1L) {
            if(nx > 2L) {
              af1 <- grep(paste("_", xnam[1L], ".res", sep = ""), files, value = TRUE)
              af2 <- grep(paste("_", xnam2[1L], "_", sep = ""), files, value = TRUE)
            } else {
              af1 <- grep(paste("_", xnam[1L], "_", xnam[2L],".res", sep = ""), files, value = TRUE)
              af2 <- grep(paste("_", xnam2[1L], "_", xnam[2L], sep = ""), files, value = TRUE)
            }
          } else {
            af1 <- grep(paste("_", xnam[1L], ".res", sep = ""), files, value = TRUE)
            af2 <- grep(paste("_", xnam2[1L], "_", sep = ""), files, value = TRUE)
          }
          af <- c(af1, af2)
          if(any(grep("_random", res, fixed = TRUE)))
            af <- grep("_random", af, fixed = TRUE, value = TRUE)
          if(any(grep("_spatial", res, fixed = TRUE)))
            af <- grep("_spatial", af, fixed = TRUE, value = TRUE)
          if(any(grep("_geokriging", res, fixed = TRUE)))
            af <- grep("_geokriging", af, fixed = TRUE, value = TRUE)
          if(!is.null(vx))
            af <- af[grepl(paste(vx, "_", sep = ""), af)]
          if(length(af) > 0L) {
            if(length(varf <- grep("_var", af, value = TRUE))) {
              if(length(vf <- grep("_var.res", varf, value = TRUE))) {
                if(!is.null(vx3))
                  vf <- grep(vx3, vf, value = TRUE)
                if(!is.null(vx))
                  vf <- grep(vx, vf, value = TRUE)
                attr(x, "variance") <- df2m(read.table(paste(dir, "/", vf[1L], sep = ""), 
                  header = TRUE))
                rownames(attr(x, "variance"))[1L] <- labelx
                if(cx == "random.bayesx")
                  RandomHyp <- rbind(RandomHyp, attr(x, "variance"))
                else
                  SmoothHyp <- rbind(SmoothHyp, attr(x, "variance"))
              }
              if(length(vf <- grep("_variance_", varf, value = TRUE))) {
                vf <- unique(vf)
                if(length(vf2 <- vf[!grepl("sample", vf)])) {
                  attr(x, "variance") <- df2m(read.table(paste(dir, "/", vf2[1L], sep = ""), 
                    header = TRUE))
                  rownames(attr(x, "variance"))[1L] <- labelx
                  if(cx == "random.bayesx")
                    RandomHyp <- rbind(RandomHyp, attr(x, "variance"))
                  else
                    SmoothHyp <- rbind(SmoothHyp, attr(x, "variance"))
                }
                if(length(vf2 <- vf[grepl("sample", vf)])) {
                  vf2 <- vf2[!grepl("_sample.ps", vf2)]
                  if(length(vf2)) {
                    for(tf in vf2) {
                      attr(x, "variance.sample") <- df2m(read.table(paste(dir, "/", tf[1L], sep = ""), 
                        header = TRUE))
                      if(is.matrix(attr(x, "variance.sample")))
                        attr(x, "variance.sample") <- attr(x, "variance.sample")[,1L]
                      if(is.matrix(attr(x, "variance.sample"))) {
                        if(ncol(attr(x, "variance.sample")) < 2L)
                          colnames(attr(x, "variance.sample")) <- "Variance"
                        else
                          colnames(attr(x, "variance.sample")) <- paste("Var", 1:ncol(attr(x, "variance.sample")), sep = "")
                      }
                    }
                  }
                }
              }
            }
            if(length(sf <- grep("_sample", af, value = TRUE)))
              if(length(sf <- sf[!grepl("_variance_", sf)])) {
                sf <- sf[!grepl("_sample.ps", sf)]
                if(length(sf)) {
                  for(tf in sf) {
                    attr(x, "sample") <- df2m(read.table(paste(dir, "/", tf[1L], sep = ""), 
                      header = TRUE))
                    colnames(attr(x, "sample")) <- paste("C", 1L:ncol(attr(x, "sample")), sep = "")
                  }
                }
              }
            if(length(pf <- grep("_param", af, value = TRUE)))
              for(tf in pf)
                attr(x, "param") <- df2m(read.table(paste(dir, "/", tf[1L], sep = ""), header = TRUE))
            if(length(kf <- grep("_knots", af, value = TRUE)))
              attr(x, "knots") <-  df2m(read.table(paste(dir, "/", kf[1L], sep = ""), header = TRUE))
            if(length(cf <- grep("_contour", af, value = TRUE))) {
              attr(x, "contourprob") <-  df2m(read.table(paste(dir, "/", cf[1L], sep = ""), 
                header = TRUE))
            }
            if(length(df <- grep("_df.", af, value = TRUE))) {
              attr(x, "df") <-  df2m(read.table(paste(dir, "/", df[1L], sep = ""), 
                header = TRUE))
            }
          }
          class(x) <- c(cx, "matrix")
          effects[[neffect]] <- x
          nameseffects <- c(nameseffects, attr(x, "specs")$label)
          neffect <- neffect + 1
          ## eval(parse(text = paste("effects$\'", attr(x, "specs")$label, "\' <- x", sep = "")))
        }
      }
    if(!is.null(nameseffects))
      names(effects) <- nameseffects
    }
  }

  return(list(effects = effects, smooth.hyp = mum(SmoothHyp), random.hyp = mum(RandomHyp)))
}

