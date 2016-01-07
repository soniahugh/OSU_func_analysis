# function that takes fd object and  returns tidy version with columns: time, rep, value
fortify.fd <- function(fdObj){
  require(reshape2)
  xrange <- fdObj$basis$rangeval
  nx <- max(c(501, 10 * dim(fdObj$coefs)[1] + 1))
  time <- seq(xrange[1], xrange[2], length.out = nx)
  fd_mat <- eval.fd(time, fdObj)
  rownames(fd_mat) <- time
  fd_df <- melt(fd_mat)
  names(fd_df) <- c("time", "rep", "value")
  fd_df
}

# function that takes pca.fd object and  returns tidy version with columns: time, rep, value
# ready for plotting
get_pcs <- function(pcaObj, npcs = NULL){
  stopifnot(inherits(pcaObj, "pca.fd"))  
  if(is.null(npcs)) {
    npcs <- dim(pcaObj$harmonics$coefs)[2]
  }
  pcs <- fortify(pcaObj$harmonics[1:npcs, ])
  
  pc_aux <- data.frame(eigenvalue = pcaObj$values[1:npcs],
    var_explained = pcaObj$varprop[1:npcs],
    rep = pcaObj$harmonics$fdnames[[2]][1:npcs])
  pcs <- left_join(pcs, pc_aux)
  
  mean_f <- fortify(pcaObj$meanfd)
  mean_f <- plyr::rename(mean_f, c("value" = "mean"))
  pcs <- inner_join(pcs, mean_f[, c("time", "mean")])
  pcs
}


predict_fregress <- function(model, newdata){
  p <- length(model$betaestlist)
  stopifnot(length(newdata) == p)
  yhat <- newdata[[1]] * model$betaestlist[[1]]$fd
  for (i in 2:p){
    yhat <- yhat + newdata[[i]] * model$betaestlist[[i]]$fd
  }
  yhat
}


# make sure factors are coded as factors first!
covar_matrix<- function(formula, data){
  facs <- vapply(data, is.factor, logical(1))
  contr_arg <- lapply(data[facs], contrasts, contrast = FALSE)
  m <- model.matrix(formula, data, contrasts.arg = contr_arg)
  attributes(m) <- attributes(m)[c("dim", "dimnames")]
  m
}

covar_list <- function(covar_mat){
  require(plyr)
  l <- alply(covar_mat, 2, identity)
  names(l) <- attr(l,"split_labels")[[1]]
  attributes(l) <- attributes(l)["names"]
  l
}