

#' Demo of workflow
#' @import glasso
#' @import xgboost
gowi <- function(n=5000, p=20, p_z=.05) {

  library(corrplot)
  library(glasso)
  library(xgboost)

  X <- matrix(rnorm(n*p), n, p)

  for(i in 2:p) X[, i] <- X[, i] + rnorm(1) * X[, i-1]

  R <- cor(X)

  corrplot(R)

  path <- glassopath(R, trace = 0, penalize.diagonal=FALSE)

  path$loglik <- 0

  for(i in seq_along(path$rholist)) {
    path$loglik[i] <- with(path,
                           glasso(R, rholist[i], n, start="warm", w.init=w[,,i], wi.init=wi[,,i], penalize.diagonal=FALSE)
                           )$loglik
    path$density[i] <- mean(path$wi[,,i] != 0)
  }


  corrplot(path$w[,,2])

  corrplot(path$w[,,4])

  corrplot(path$w[,,8])

  plot(path$rholist, path$loglik, main = "LL Trace", type='l')
  plot(path$rholist, path$density, main = 'Density trace', type='l')



  # MCAR

  Z <- sample(length(X), p_z*length(X))
  X[Z] <- NA

  R_z <- cor(X, use='pairwise.complete.obs')

  corrplot(R_z)


  xgb_gain <- 0 * R_z


  require(xgboost)


  for(i in seq(p)) {
    message(i)
    xgb_data <- xgb.DMatrix(X[!is.na(X[,i]), -i], label=X[!is.na(X[,i]),i, drop=FALSE])
    fit <- xgb.train(data=xgb_data, nrounds=100)

    importances <- xgb.importance(seq(p)[-i] |> as.character(), model=fit)
    importances$Feature <- as.integer(importances$Feature)

    xgb_gain[i, importances$Feature] <- importances$Gain
  }

  #Symmetrize
  xgb_gain <- .5*xgb_gain + .5*t(xgb_gain)
  diag(xgb_gain) <- 1

  corrplot(xgb_gain)

  path <- glassopath(xgb_gain, trace = 0, penalize.diagonal=FALSE)

  path$loglik <- 0

  for(i in seq_along(path$rholist)) {
    path$loglik[i] <- with(path,
                           glasso(xgb_gain, rholist[i], n, start="warm", w.init=w[,,i], wi.init=wi[,,i], penalize.diagonal=FALSE)
    )$loglik
    path$density[i] <- mean(path$wi[,,i] != 0)
  }

  corrplot(path$w[,,1])

  corrplot(path$w[,,2])




  NULL
}
