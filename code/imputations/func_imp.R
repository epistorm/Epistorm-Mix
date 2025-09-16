#### Functions for Imputations ####

#### loading functions ####
load.myps = function(x){
  if(!require(x, character.only = T)){
    install.packages(x)
  }
  library(x, character.only = T)
  return(x)
}

#### missing forest ####
missForest2 = function (xmis, maxiter = 10, ntree = 100, variablewise = FALSE, 
                        decreasing = FALSE, verbose = FALSE, mtry = floor(sqrt(ncol(xmis))), 
                        replace = TRUE, classwt = NULL, cutoff = NULL, strata = NULL, 
                        sampsize = NULL, nodesize = NULL, maxnodes = NULL, xtrue = NA, 
                        parallelize = c("no", "variables", "forests")) 
{
  
  n <- nrow(xmis)
  p <- ncol(xmis)
  if (!is.null(classwt)) 
    stopifnot(length(classwt) == p, typeof(classwt) == "list")
  if (!is.null(cutoff)) 
    stopifnot(length(cutoff) == p, typeof(cutoff) == "list")
  if (!is.null(strata)) 
    stopifnot(length(strata) == p, typeof(strata) == "list")
  if (!is.null(nodesize)) 
    stopifnot(length(nodesize) == 2)
  if (any(apply(is.na(xmis), 2, sum) == n)) {
    indCmis <- which(apply(is.na(xmis), 2, sum) == n)
    xmis <- xmis[, -indCmis]
    p <- ncol(xmis)
    cat("  removed variable(s)", indCmis, "due to the missingness of all entries\n")
  }
  # parallelize <- match.arg(parallelize)
  if (parallelize %in% c("variables", "forests")) {
    if (getDoParWorkers() == 1) {
      stop("You must register a 'foreach' parallel backend to run 'missForest' in parallel. Set 'parallelize' to 'no' to compute serially.")
    }
    else if (verbose) {
      if (parallelize == "variables") {
        cat("  parallelizing over the variables of the input data matrix 'xmis'\n")
      }
      else {
        cat("  parallelizing computation of the random forest model objects\n")
      }
    }
    if (getDoParWorkers() > p) {
      stop("The number of parallel cores should not exceed the number of variables (p=", 
           p, ")")
    }
  }
  ximp <- xmis
  varType <- character(p)
  for (t.co in 1:p) {
    if (is.numeric(xmis[[t.co]])) {
      varType[t.co] <- "numeric"
      ximp[is.na(xmis[, t.co]), t.co] <- mean(xmis[, t.co], 
                                              na.rm = TRUE)
      (next)()
    }
    if (is.factor(xmis[[t.co]])) {
      varType[t.co] <- "factor"
      max.level <- max(table(ximp[[t.co]]))
      class.assign <- sample(names(which(max.level == summary(ximp[[t.co]]))), 
                             1)
      if (class.assign != "NA's") {
        ximp[is.na(xmis[[t.co]]), t.co] <- class.assign
      }
      else {
        while (class.assign == "NA's") {
          class.assign <- sample(names(which(max.level == 
                                               summary(ximp[[t.co]]))), 1)
        }
        ximp[is.na(xmis[[t.co]]), t.co] <- class.assign
      }
      (next)()
    }
    stop(sprintf("column %s must be factor or numeric, is %s", 
                 names(xmis)[t.co], class(xmis[[t.co]])))
  }
  NAloc <- is.na(xmis)
  noNAvar <- apply(NAloc, 2, sum)
  sort.j <- order(noNAvar)
  if (decreasing) 
    sort.j <- rev(sort.j)
  sort.noNAvar <- noNAvar[sort.j]
  nzsort.j <- sort.j[sort.noNAvar > 0]
  if (parallelize == "variables") {
    "%cols%" <- get("%dorng%")
    idxList <- as.list(isplitVector(nzsort.j, chunkSize = getDoParWorkers()))
  }
  Ximp <- vector("list", maxiter)
  iter <- 0
  k <- length(unique(varType))
  convNew <- rep(0, k)
  convOld <- rep(Inf, k)
  OOBerror <- numeric(p)
  names(OOBerror) <- varType
  if (k == 1) {
    if (unique(varType) == "numeric") {
      names(convNew) <- c("numeric")
    }
    else {
      names(convNew) <- c("factor")
    }
    convergence <- c()
    OOBerr <- numeric(1)
  }
  else {
    names(convNew) <- c("numeric", "factor")
    convergence <- matrix(NA, ncol = 2)
    OOBerr <- numeric(2)
  }
  stopCriterion <- function(varType, convNew, convOld, iter, 
                            maxiter) {
    k <- length(unique(varType))
    if (k == 1) {
      (convNew < convOld) & (iter < maxiter)
    }
    else {
      ((convNew[1] < convOld[1]) | (convNew[2] < convOld[2])) & 
        (iter < maxiter)
    }
  }
  while (stopCriterion(varType, convNew, convOld, iter, maxiter)) {
    if (iter != 0) {
      convOld <- convNew
      OOBerrOld <- OOBerr
    }
    if (verbose) {
      cat("  missForest iteration", iter + 1, "in progress...")
    }
    t.start <- proc.time()
    ximp.old <- ximp
    if (parallelize == "variables") {
      for (idx in idxList) {
        results <- foreach(varInd = idx, .packages = "randomForest") %cols% 
          {
            obsi <- !NAloc[, varInd]
            misi <- NAloc[, varInd]
            obsY <- ximp[obsi, varInd]
            obsX <- ximp[obsi, seq(1, p)[-varInd]]
            misX <- ximp[misi, seq(1, p)[-varInd]]
            typeY <- varType[varInd]
            if (typeY == "numeric") {
              RF <- randomForest(x = obsX, y = obsY, 
                                 ntree = ntree, mtry = mtry, replace = replace 
                                 # sampsize = if (!is.null(sampsize)) 
                                 #   sampsize[[varInd]]
                                 # else if (replace) 
                                 #   nrow(obsX)
                                 # else ceiling(0.632 * nrow(obsX)), nodesize = if (!is.null(nodesize)) 
                                 #   nodesize[1]
                                 # else 1, maxnodes = if (!is.null(maxnodes)) 
                                 #   maxnodes
                                 # else NULL
              )
              oerr <- RF$mse[ntree]
              misY <- predict(RF, misX)
            }
            else {
              obsY <- factor(obsY)
              summarY <- summary(obsY)
              if (length(summarY) == 1) {
                oerr <- 0
                misY <- factor(rep(names(summarY), length(misi)))
              }
              else {
                RF <- randomForest(x = obsX, y = obsY, 
                                   ntree = ntree, mtry = mtry, replace = replace 
                                   # classwt = if (!is.null(classwt)) 
                                   #   classwt[[varInd]]
                                   # else rep(1, nlevels(obsY)), cutoff = if (!is.null(cutoff)) 
                                   #   cutoff[[varInd]]
                                   # else rep(1/nlevels(obsY), nlevels(obsY)), 
                                   # strata = if (!is.null(strata)) 
                                   #   strata[[varInd]]
                                   # else obsY, sampsize = if (!is.null(sampsize)) 
                                   #   sampsize[[varInd]]
                                   # else if (replace) 
                                   #   nrow(obsX)
                                   # else ceiling(0.632 * nrow(obsX)), nodesize = if (!is.null(nodesize)) 
                                   #   nodesize[2]
                                   # else 5, maxnodes = if (!is.null(maxnodes)) 
                                   #   maxnodes
                                   # else NULL
                )
                oerr <- RF$err.rate[[ntree, 1]]
                misY <- predict(RF, misX)
              }
            }
            list(varInd = varInd, misY = misY, oerr = oerr)
          }
        for (res in results) {
          misi <- NAloc[, res$varInd]
          ximp[misi, res$varInd] <- res$misY
          OOBerror[res$varInd] <- res$oerr
        }
      }
    }
    else {
      for (s in 1:p) {
        varInd <- sort.j[s]
        if (noNAvar[[varInd]] != 0) {
          obsi <- !NAloc[, varInd]
          misi <- NAloc[, varInd]
          obsY <- ximp[obsi, varInd]
          obsX <- ximp[obsi, seq(1, p)[-varInd]]
          misX <- ximp[misi, seq(1, p)[-varInd]]
          typeY <- varType[varInd]
          if (typeY == "numeric") {
            if (parallelize == "forests") {
              xntree <- NULL
              RF <- foreach(xntree = idiv(ntree, chunks = getDoParWorkers()), 
                            .combine = "combine", .multicombine = TRUE, 
                            .packages = "randomForest") %dorng% {
                              randomForest(x = obsX, y = obsY, ntree = xntree, 
                                           mtry = mtry, replace = replace, 
                                           # sampsize = if (!is.null(sampsize)) 
                                           #   sampsize[[varInd]]
                                           # else if (replace) 
                                           #   nrow(obsX)
                                           # else ceiling(0.632 * nrow(obsX)), nodesize = if (!is.null(nodesize)) 
                                           #   nodesize[1]
                                           # else 1, maxnodes = if (!is.null(maxnodes)) 
                                           #   maxnodes
                                           # else NULL
                              )
                            }
              OOBerror[varInd] <- mean((predict(RF) - 
                                          RF$y)^2, na.rm = TRUE)
            }
            else {
              RF <- randomForest(x = obsX, y = obsY, 
                                 ntree = ntree, mtry = mtry, replace = replace 
                                 # sampsize = if (!is.null(sampsize)) 
                                 #   sampsize[[varInd]]
                                 # else if (replace) 
                                 #   nrow(obsX)
                                 # else ceiling(0.632 * nrow(obsX)), nodesize = if (!is.null(nodesize)) 
                                 #   nodesize[1]
                                 # else 1, maxnodes = if (!is.null(maxnodes)) 
                                 #   maxnodes
                                 # else NULL
              )
              OOBerror[varInd] <- RF$mse[ntree]
            }
            misY <- predict(RF, misX)
          }
          else {
            obsY <- factor(obsY)
            summarY <- summary(obsY)
            if (length(summarY) == 1) {
              misY <- factor(rep(names(summarY), sum(misi)))
            }
            else {
              if (parallelize == "forests") {
                
                # classwt1 = if (!is.null(classwt)){classwt[[varInd]]} else{rep(1, nlevels(obsY))}
                # cutoff1 = if (!is.null(cutoff)) {cutoff[[varInd]]} else {rep(1/nlevels(obsY), nlevels(obsY))}
                # strata1 = if (!is.null(strata)) {strata[[varInd]]} else {obsY}
                # sampsize1 = if (!is.null(sampsize)) {sampsize[[varInd]]} else if (replace) {nrow(obsX)} else {ceiling(0.632 * nrow(obsX))}
                # nodesize1 = if (!is.null(nodesize)) {nodesize[2]} else {5}
                # maxnodes1 = if (!is.null(maxnodes)) {maxnodes} else {NULL}
                
                RF <- foreach(xntree = idiv(ntree, chunks = getDoParWorkers()), 
                              .combine = "combine", .multicombine = TRUE, 
                              .packages = "randomForest") %dorng% 
                  {
                    randomForest(x = obsX, y = obsY, 
                                 ntree = xntree, mtry = mtry, 
                                 replace = replace 
                                 # classwt = classwt1, 
                                 # cutoff = cutoff1, 
                                 # strata = strata1, 
                                 # sampsize = sampsize1, 
                                 # nodesize = nodesize1, 
                                 # maxnodes = maxnodes1
                    )
                  }
                ne <- as.integer(predict(RF)) != as.integer(RF$y)
                ne <- ne[!is.na(ne)]
                OOBerror[varInd] <- sum(ne)/length(ne)
              }
              else {
                RF <- randomForest(x = obsX, y = obsY, 
                                   ntree = ntree, mtry = mtry, replace = replace 
                                   # classwt = if (!is.null(classwt)) 
                                   #   classwt[[varInd]]
                                   # else rep(1, nlevels(obsY)), cutoff = if (!is.null(cutoff)) 
                                   #   cutoff[[varInd]]
                                   # else rep(1/nlevels(obsY), nlevels(obsY)), 
                                   # strata = if (!is.null(strata)) 
                                   #   strata[[varInd]]
                                   # else obsY, sampsize = if (!is.null(sampsize)) 
                                   #   sampsize[[varInd]]
                                   # else if (replace) 
                                   #   nrow(obsX)
                                   # else ceiling(0.632 * nrow(obsX)), nodesize = if (!is.null(nodesize)) 
                                   #   nodesize[2]
                                   # else 5, maxnodes = if (!is.null(maxnodes)) 
                                   #   maxnodes
                                   # else NULL
                )
                OOBerror[varInd] <- RF$err.rate[[ntree, 
                                                 1]]
              }
              misY <- predict(RF, misX)
            }
          }
          ximp[misi, varInd] <- misY
        }
      }
    }
    if (verbose) {
      cat("done!\n")
    }
    iter <- iter + 1
    Ximp[[iter]] <- ximp
    t.co2 <- 1
    for (t.type in names(convNew)) {
      t.ind <- which(varType == t.type)
      if (t.type == "numeric") {
        convNew[t.co2] <- sum((ximp[, t.ind] - ximp.old[, 
                                                        t.ind])^2)/sum(ximp[, t.ind]^2)
      }
      else {
        dist <- sum(as.character(as.matrix(ximp[, t.ind])) != 
                      as.character(as.matrix(ximp.old[, t.ind])))
        convNew[t.co2] <- dist/(n * sum(varType == "factor"))
      }
      t.co2 <- t.co2 + 1
    }
    if (!variablewise) {
      NRMSE <- sqrt(mean(OOBerror[varType == "numeric"])/var(as.vector(as.matrix(xmis[, 
                                                                                      varType == "numeric"])), na.rm = TRUE))
      PFC <- mean(OOBerror[varType == "factor"])
      if (k == 1) {
        if (unique(varType) == "numeric") {
          OOBerr <- NRMSE
          names(OOBerr) <- "NRMSE"
        }
        else {
          OOBerr <- PFC
          names(OOBerr) <- "PFC"
        }
      }
      else {
        OOBerr <- c(NRMSE, PFC)
        names(OOBerr) <- c("NRMSE", "PFC")
      }
    }
    else {
      OOBerr <- OOBerror
      names(OOBerr)[varType == "numeric"] <- "MSE"
      names(OOBerr)[varType == "factor"] <- "PFC"
    }
    if (any(!is.na(xtrue))) {
      err <- suppressWarnings(mixError(ximp, xmis, xtrue))
    }
    if (verbose) {
      delta.start <- proc.time() - t.start
      if (any(!is.na(xtrue))) {
        cat("    error(s):", err, "\n")
      }
      cat("    estimated error(s):", OOBerr, "\n")
      cat("    difference(s):", convNew, "\n")
      cat("    time:", delta.start[3], "seconds\n\n")
    }
  }
  if (iter == maxiter) {
    if (any(is.na(xtrue))) {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr)
    }
    else {
      out <- list(ximp = Ximp[[iter]], OOBerror = OOBerr, 
                  error = err)
    }
  }
  else {
    if (any(is.na(xtrue))) {
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld)
    }
    else {
      out <- list(ximp = Ximp[[iter - 1]], OOBerror = OOBerrOld, 
                  error = suppressWarnings(mixError(Ximp[[iter - 
                                                            1]], xmis, xtrue)))
    }
  }
  class(out) <- "missForest"
  return(out)
}

#### Assortativity ####
assortativity.calc <- function(my.matrix) {
  
  my.matrix = as.matrix(my.matrix)
  
  my.matrix  = Reduce(rbind, lapply(c(1:nrow(my.matrix)), function(x)
    my.matrix[,x]/sum(my.matrix[,x], na.rm = T)
  ))
  
  Tr <- sum(diag(my.matrix), na.rm = T)
  Q = (Tr-1)/(nrow(my.matrix)-1)
  
  return(Q)
  
}