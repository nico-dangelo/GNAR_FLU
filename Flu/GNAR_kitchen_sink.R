# Fit dummy multivariate normal and multinomial data series to mobility GNAR networks 
library(GNAR)
library(tidyverse)
library(sandwich)
library(estimatr)
# library(MASS)
library(igraph)
# Create mobility network and GNAR for New England without population restriction
mobility_df_list <- readRDS("~/Library/CloudStorage/GoogleDrive-nd672@georgetown.edu/My Drive/Lab Files/GNAR_FLU/Data/Mobility/mobility_df_list.rds")
NEng_counties <- make_fips_vec(c("MA", "RI", "CT", "VT", "NH", "ME"), county_shape = US_county_shape)
mobility_df_list_NEng<- mobility_df_list |> lapply(function(B){B |> select(origin, destination) |> filter(origin %in% NEng_counties, destination %in% NEng_counties)})

mobility_igraph_list_NEng <- mobility_df_list_NEng |> lapply(graph_from_data_frame)


# Create dummy multivariate time series for NENG network
NEng_mean_vec<- rep(0, gorder(mobility_igraph_list_NEng[[11]] |> igraph::simplify()))
NEng_covar <- diag(nrow=gorder(mobility_igraph_list_NEng[[11]] |> igraph::simplify()), ncol=gorder(mobility_igraph_list_NEng[[11]] |> igraph::simplify()))
ts_mvtn <- MASS::mvrnorm(n=100, mu=NEng_mean_vec, Sigma = NEng_covar)
# colnames(ts_mvtn) <- NEng_counties
mvtn_mobility_NEng_GNAR <- mobility_igraph_list_NEng[[11]] |> igraph::simplify() |> igraphtoGNAR()
mvtn_mobility_NEng_GNAR_fit <- GNARfit(vts=ts_mvtn, net=mvtn_mobility_NEng_GNAR)
# summary(mvtn_mobility_NEng_GNAR_fit)
logLik(mvtn_mobility_NEng_GNAR_fit)
BIC(mvtn_mobility_NEng_GNAR_fit)


# Automatic ARIMA models to determine stationarity and give baseli --------
library(forecast)

# Use HC estimators from lm_robust ----------------------------------------

GNARfit_robust <- function(vts = GNAR::fiveVTS, net = GNAR::fiveNet, alphaOrder = 2, 
                           betaOrder = c(1, 1), fact.var = NULL, globalalpha = TRUE, 
                           tvnets = NULL, netsstart = NULL, ErrorIfNoNei = TRUE){
  
  stopifnot(is.GNARnet(net))
  stopifnot(ncol(vts) == length(net$edges))
  stopifnot(alphaOrder > 0)
  stopifnot(floor(alphaOrder) == alphaOrder)
  stopifnot(length(betaOrder) == alphaOrder)
  stopifnot(floor(betaOrder) == betaOrder)
  if (!is.null(fact.var)) {
    stopifnot(length(fact.var) == length(net$edges))
  }
  stopifnot(is.matrix(vts))
  stopifnot(is.logical(globalalpha))
  if (!is.null(tvnets)) {
    cat("Time-varying networks not yet supported")
  }
  stopifnot(is.null(tvnets))
  useNofNei <- 1
  frbic <- list(nnodes = length(net$edges), alphas.in = alphaOrder, 
                betas.in = betaOrder, fact.var = fact.var, globalalpha = globalalpha, 
                xtsp = tsp(vts), time.in = nrow(vts), net.in = net, 
                final.in = vts[(nrow(vts) - alphaOrder + 1):nrow(vts), 
                ])
  dmat <- GNARdesign(vts = vts, net = net, alphaOrder = alphaOrder, 
                     betaOrder = betaOrder, fact.var = fact.var, globalalpha = globalalpha, 
                     tvnets = tvnets, netsstart = netsstart)
  if (ErrorIfNoNei) {
    if (any(apply(dmat == 0, 2, all))) {
      parname <- strsplit(names(which(apply(dmat == 0, 
                                            2, all)))[1], split = NULL)[[1]]
      betastage <- parname[(which(parname == ".") + 1):(length(parname))]
      stop("beta order too large for network, use max neighbour set smaller than ", 
           betastage)
    }
  }
  predt <- nrow(vts) - alphaOrder
  yvec <- NULL
  for (ii in 1:length(net$edges)) {
    yvec <- c(yvec, vts[((alphaOrder + 1):(predt + alphaOrder)), 
                        ii])
  }
  if (sum(is.na(yvec)) > 0) {
    yvec2 <- yvec[!is.na(yvec)]
    dmat2 <- dmat[!is.na(yvec), ]
    modNoIntercept <- estimatr::lm_robust(yvec2 ~ dmat2 + 0)
  }
  else {
    modNoIntercept <- estimatr::lm_robust(yvec ~ dmat + 0)
  }
  out <- list(mod = modNoIntercept, y = yvec, dd = dmat, frbic = frbic)
}
GNAR_robust_fit_NEng<- GNARfit_robust(vts=flu_ts_NEng, net=mobility_GNAR_NEng)
#compute residuals

GNAR_robust_fit_NEng$resid <- GNAR_robust_fit_NEng$y- GNAR_robust_fit_NEng$mod$fitted.values
#Compute covariance matrix as in GNAR

(1/GNAR_robust_fit_NEng$frbic$time.in) * t(tmp.resid) %*% tmp.resid
residToMatrobust <- function(robustobj){
  if (is.null(robustobj$ys)) {
    yvec <- robustobj$y
    dmat <- robustobj$dd
    nnodes <- robustobj$frbic$nnodes
  }
  resid.gaps <- rep(0, length(yvec))
  resid.gaps[is.na(yvec)] <- 1
  resid.gaps[na.row(dmat)] <- 1
  resid.with.gaps <- rep(NA, length = length(yvec))
  resid.with.gaps[!resid.gaps] <- robustobj$resid
  fit.with.gaps <- rep(NA, length = length(yvec))
  fit.with.gaps[!resid.gaps] <- robustobj$mod$fitted.values
  resid.matrix <- matrix(resid.with.gaps, ncol = nnodes, byrow = FALSE)
  fitted.matrix <- matrix(fit.with.gaps, ncol = nnodes, byrow = FALSE)
  return(list(resid = resid.matrix, fit = fitted.matrix))
}
residMatrobust_NEng <- residToMatrobust(GNAR_robust_fit_NEng)
BIC.GNARfit_robust <- function(object,...){
  nnodes.in <- object$frbic$nnodes
  alphas.in <- object$frbic$alphas.in
  betas.in <- object$frbic$betas.in
  fact.var <- object$frbic$fact.var
  tot.time <- object$frbic$time.in
  globalalpha <- object$frbic$globalalpha
  tmp.resid <- residToMatrobust(robustobj = GNARfit_robust())$resid
  tmp.resid[is.na(tmp.resid)] <- 0
  larg <- det((1/tot.time) * t(tmp.resid) %*% tmp.resid)
  stopifnot(larg != 0)
  tmp1 <- log(larg)
  if (globalalpha) {
    tmp2 <- f.in * (alphas.in + sum(betas.in)) * log(tot.time)/tot.time
  }    
  return(tmp1 + tmp2)

}
BIC.GNARfit_robust(GNAR_robust_fit_NEng)
#still singular

#Try HAC from sandwich

GNARfit_sandwich <- function(vts, net, alphaOrder=2, betaOrder=c(1,1), fact.var=NULL, globalalpha=TRUE, tvnets=NULL, netsstart=NULL,ErrorIfNoNei=TRUE){
  stopifnot(is.GNARnet(net))
  stopifnot(ncol(vts) == length(net$edges))
  stopifnot(alphaOrder > 0)
  stopifnot(floor(alphaOrder) == alphaOrder)
  stopifnot(length(betaOrder) == alphaOrder)
  stopifnot(floor(betaOrder) == betaOrder)
  if (!is.null(fact.var)) {
    stopifnot(length(fact.var) == length(net$edges))
  }
  stopifnot(is.matrix(vts))
  stopifnot(is.logical(globalalpha))
  if (!is.null(tvnets)) {
    cat("Time-varying networks not yet supported")
  }
  stopifnot(is.null(tvnets))
  useNofNei <- 1
  frbic <- list(nnodes = length(net$edges), alphas.in = alphaOrder, 
                betas.in = betaOrder, fact.var = fact.var, globalalpha = globalalpha, 
                xtsp = tsp(vts), time.in = nrow(vts), net.in = net, 
                final.in = vts[(nrow(vts) - alphaOrder + 1):nrow(vts), 
                ])
  dmat <- GNARdesign(vts = vts, net = net, alphaOrder = alphaOrder, 
                     betaOrder = betaOrder, fact.var = fact.var, globalalpha = globalalpha, 
                     tvnets = tvnets, netsstart = netsstart)
  if (ErrorIfNoNei) {
    if (any(apply(dmat == 0, 2, all))) {
      parname <- strsplit(names(which(apply(dmat == 0, 
                                            2, all)))[1], split = NULL)[[1]]
      betastage <- parname[(which(parname == ".") + 1):(length(parname))]
      stop("beta order too large for network, use max neighbour set smaller than ", 
           betastage)
    }
  }
  predt <- nrow(vts) - alphaOrder
  yvec <- NULL
  for (ii in 1:length(net$edges)) {
    yvec <- c(yvec, vts[((alphaOrder + 1):(predt + alphaOrder)), 
                        ii])
  }
  if (sum(is.na(yvec)) > 0) {
    yvec2 <- yvec[!is.na(yvec)]
    dmat2 <- dmat[!is.na(yvec), ]
    modNoIntercept <- sandwich::sandwich(lm(yvec2 ~ dmat2 + 0))
  }
  else {
    modNoIntercept <- sandwich::sandwich(lm(yvec ~ dmat + 0))
  }
  out <- list(mod = modNoIntercept, y = yvec, dd = dmat, frbic = frbic)
  # class(out) <- "GNARfit"
  return(out)  
  return(GNAR_sandwich)}

residual_var_covar_sandwich <- function(object,...){
  # stopifnot(is.GNARfit(object))
  nnodes.in <- object$frbic$nnodes
  alphas.in <- object$frbic$alphas.in
  betas.in <- object$frbic$betas.in
  fact.var <- object$frbic$fact.var
  tot.time <- object$frbic$time.in
  globalalpha <- object$frbic$globalalpha
  dotarg <- list(...)
  if (length(dotarg) != 0) {
    if (!is.null(names(dotarg))) {
      warning("... not used here, input(s) ", paste(names(dotarg), 
                                                    collapse = ", "), " ignored")
    }
    else {
      warning("... not used here, input(s) ", paste(dotarg, 
                                                    collapse = ", "), " ignored")
    }
  }
  if (!is.null(fact.var)) {
    f.in <- length(unique(fact.var))
  }
  else {
    f.in <- 1
  }
  stopifnot(is.logical(globalalpha))
  stopifnot(length(nnodes.in) == 1)
  stopifnot(floor(nnodes.in) == nnodes.in)
  stopifnot(tot.time != 0)
  # tmp.resid <- residToMat(GNARobj = object, nnodes = nnodes.in)$resid
  # tmp.resid[is.na(tmp.resid)] <- 0
  larg <- sandwich::vcovHAC(object)
  return(larg)}



# Use glmnet to do ride or Lasso regularization on Alpha, Beta ------------
library(glmnet)
#lasso with cv
GNARfit_lasso<- function(vts = GNAR::fiveVTS, net = GNAR::fiveNet, alphaOrder = 2, 
                         betaOrder = c(1, 1), fact.var = NULL, globalalpha = TRUE, 
                         tvnets = NULL, netsstart = NULL, ErrorIfNoNei = TRUE, alpha_lasso=1, lambda=NULL, cv=TRUE, nfolds=10) {
                           stopifnot(is.GNARnet(net))
                           stopifnot(ncol(vts) == length(net$edges))
                           stopifnot(alphaOrder > 0)
                           stopifnot(floor(alphaOrder) == alphaOrder)
                           stopifnot(length(betaOrder) == alphaOrder)
                           stopifnot(floor(betaOrder) == betaOrder)
                           if (!is.null(fact.var)) {
                             stopifnot(length(fact.var) == length(net$edges))
                           }
                           stopifnot(is.matrix(vts))
                           stopifnot(is.logical(globalalpha))
                           if (!is.null(tvnets)) {
                             cat("Time-varying networks not yet supported")
                           }
                           stopifnot(is.null(tvnets))
                           useNofNei <- 1
                           frbic <- list(nnodes = length(net$edges), alphas.in = alphaOrder, 
                                         betas.in = betaOrder, fact.var = fact.var, globalalpha = globalalpha, 
                                         xtsp = tsp(vts), time.in = nrow(vts), net.in = net, 
                                         final.in = vts[(nrow(vts) - alphaOrder + 1):nrow(vts), 
                                         ])
                           dmat <- GNARdesign(vts = vts, net = net, alphaOrder = alphaOrder, 
                                              betaOrder = betaOrder, fact.var = fact.var, globalalpha = globalalpha, 
                                              tvnets = tvnets, netsstart = netsstart)
                           if (ErrorIfNoNei) {
                             if (any(apply(dmat == 0, 2, all))) {
                               parname <- strsplit(names(which(apply(dmat == 0, 
                                                                     2, all)))[1], split = NULL)[[1]]
                               betastage <- parname[(which(parname == ".") + 1):(length(parname))]
                               stop("beta order too large for network, use max neighbour set smaller than ", 
                                    betastage)
                             }
                           }
                           predt <- nrow(vts) - alphaOrder
                           yvec <- NULL
                           for (ii in 1:length(net$edges)) {
                             yvec <- c(yvec, vts[((alphaOrder + 1):(predt + alphaOrder)), 
                                                 ii])
                           }
                           if (sum(is.na(yvec)) > 0) {
                             yvec <- yvec[!is.na(yvec)]
                             dmat <- dmat[!is.na(yvec), ]
                           }
                           if(cv){
                             nnodes      <- length(net$edges)
                             n_times      <- predt
                             fold_id_time <- cut(seq_len(n_times), nfolds, labels = FALSE)
                             fold_id_full <- rep(fold_id_time, times = nnodes)
                             if (any(is.na(fold_id_full))) fold_id_full <- fold_id_full[!is.na(fold_id_full)]
                             modNoIntercept <- cv.glmnet(
                               x           = dmat,
                               y           = yvec,
                               alpha       = alpha_lasso,
                               foldid      = fold_id_full,
                               standardize = TRUE
                             )
                             lambda_out <- modNoIntercept$lambda.min
                           }
                           else{
                             modNoIntercept <- glmnet(x           = dmat,
                                                      y           = yvec,
                                                      alpha       = alpha_lasso, standardize = TRUE)
                             
                             lambda_out <- if (is.null(lambda)) modNoIntercept$lambda else lambda
                             
                             }
                           out <- list(mod = modNoIntercept, y = yvec, dd = dmat, lambda=lambda, alpha_lasso=alpha_lasso, frbic = frbic, cv=cv)
                           class(out) <- "GNARfit_lasso"
                           return(out)
                         }

coef.GNARlasso <- function(object, lambda = NULL, ...) {
  s   <- if (!is.null(lambda)) lambda else object$lambda
  raw <- coef(object$mod, s = s)
  setNames(as.numeric(raw), rownames(raw))
}
fitted.GNARlasso <- function(object, lambda = NULL, ...) {
  s    <- if (!is.null(lambda)) lambda else object$lambda
  fhat <- as.numeric(predict(object$mod, newx = object$dd, s = s))
  
  if (any(is.na(object$y))) {
    out                  <- rep(NA_real_, length(object$y))
    out[!is.na(object$y)] <- fhat
    return(out)
  }
  fhat
}


#' Residuals from a GNARlasso fit

residuals.GNARlasso <- function(object, lambda = NULL, ...) {
  object$y - fitted.GNARlasso(object, lambda = lambda)
}
residtoMatlasso <- function(object,lambda=NULL,...){
  yvec <- object$y
  dmat <- object$dd
  nnodes <- object$frbic$nnodes
  resid.gaps <- rep(0, length(yvec))
  resid.gaps[is.na(yvec)] <- 1
  resid.gaps[na.row(dmat)] <- 1
  resid.with.gaps <- rep(NA, length = length(yvec))
  resid.with.gaps[!resid.gaps] <- residuals.GNARlasso(object, lambda = lambda)
  fit.with.gaps <- rep(NA, length = length(yvec))
  fit.with.gaps[!resid.gaps] <- fitted.GNARlasso(object, lambda = lambda)
  resid.matrix <- matrix(resid.with.gaps, ncol = nnodes, byrow = FALSE)
  fitted.matrix <- matrix(fit.with.gaps, ncol = nnodes, byrow = FALSE)
  return(list(resid = resid.matrix, fit = fitted.matrix))
}

BIC.GNARlasso <- function(object, lambda = NULL, ...) {
  nnodes.in <- object$frbic$nnodes
  alphas.in <- object$frbic$alphas.in
  betas.in <- object$frbic$betas.in
  fact.var <- object$frbic$fact.var
  tot.time <- object$frbic$time.in
  globalalpha <- object$frbic$globalalpha

  stopifnot(is.logical(globalalpha))
  stopifnot(length(nnodes.in) == 1)
  stopifnot(floor(nnodes.in) == nnodes.in)
  stopifnot(tot.time != 0)
  # compute residual matrix
  tmp.resid <- residtoMatlasso(object, lambda=lambda)$resid
  tmp.resid[is.na(tmp.resid)] <- 0
  larg <- det((1/tot.time) * t(tmp.resid) %*% tmp.resid)
  stopifnot(larg != 0)
  tmp1 <- log(larg)
  if (globalalpha) {
    tmp2 <- f.in * (alphas.in + sum(betas.in)) * log(tot.time)/tot.time
  }
  else {
    tmp2 <- (ncol(tmp.resid) * alphas.in + sum(betas.in)) * 
      log(tot.time)/tot.time
  }
  return(tmp1 + tmp2)
}



GNAR_lasso_fit_NEng <-GNARfit_lasso(vts=flu_ts_NEng, net=mobility_GNAR_NEng, cv=TRUE, alpha_lasso = 1, globalalpha = F)
residuals.GNARlasso(GNAR_lasso_fit_NEng)
BIC.GNARlasso(GNAR_lasso_fit_NEng)

#Try with HHS 5
HHS_5_counties <- make_fips_vec(c("IL", "IN", "MI", "MN", "OH", "WI"), US_county_shape)
HHS_5_counties_10k <- county_pop_2024 |> filter_at(vars(contains("20")), all_vars(.>10000)) |> select(FIPS) |> filter(FIPS %in% HHS_5_counties) |> pull()
mobility_df_list_HHS_5_10k <- mobility_df_list |> lapply(function(B){B|> select(origin, destination) |> filter(origin %in% HHS_5_counties_10k, destination %in% HHS_5_counties_10k)})
mobility_igraph_list_HHS_5_10k <- mobility_df_list_HHS_5_10k |> lapply(graph_from_data_frame)
mobility_igraph_list_HHS_5_10k |> lapply(gorder) |> unlist()
mobility_igraph_list_HHS_5_10k |> lapply(gsize) |> unlist() |> which.max()
#10
flu_ts_df_HHS_5_10k_restricted <- county_flu_ac_season_norm |> filter(county_fips %in% V(mobility_igraph_list_HHS_5_10k[[10]])$name, season=="2018-2019") |> select(county_fips, year_week_dt, conf_flu_norm) |> spread(county_fips, conf_flu_norm) |> column_to_rownames(var="year_week_dt") 
flu_ts_HHS_5_10k_restricted <- as.matrix(flu_ts_df_HHS_5_10k_restricted)
mobility_max_HHS_5_10k_restricted_GNAR <- mobility_igraph_list_HHS_5_10k[[10]] |> igraph::simplify() |> igraphtoGNAR()

GNAR_lasso_fit_HHS_5_10k <- GNARfit_lasso(vts=flu_ts_HHS_5_10k_restricted, net=mobility_max_HHS_5_10k_restricted_GNAR)
BIC.GNARlasso(GNAR_lasso_fit_HHS_5_10k)
