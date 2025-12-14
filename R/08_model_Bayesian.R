#' Train Bayesian model
#'
#' Use Spike and Slab prior
#' @param dat date frame
#' @return fit.JAGS(posterior samples;summary;sims.array;input data;
#' initial values;running info), mcmc_list(posterior samples:iter*para,
#' Gelman-Rubin Rhat, ESS, ACF, densplot), pip, beta_mean
#' @import R2jags
#' @import coda
#' @export
train_Bayesian <- function(dat){
  y <- dat[[ncol(dat)]]
  X <- dat[,-ncol(dat),drop=FALSE]

  X_m <- stats::model.matrix(~.-1,data=X)
  N <- nrow(X_m)
  P <- ncol(X_m)
  pred_name <- colnames(X_m)

  dat.JAGS <- list(
    y = as.numeric(y),
    X = X_m,
    N = N,
    P = P
  )

  model_string <- "
  model {

    # likelihood
    for (i in 1:N) {
      y[i] ~ dnorm(mu[i], tau)
      mu[i] <- alpha + inprod(X[i, ], beta[])
    }

    # prior for beta
    for (j in 1:P) {
      beta_raw[j] ~ dnorm(0, tau_beta)   # slab
      gamma[j]    ~ dbern(pi)           # 0/1
      beta[j]     <- gamma[j] * beta_raw[j]  # hard spike
    }

    # hyperpriors
    alpha ~ dnorm(0.0, 1.0E-4)

    tau ~ dgamma(0.1, 0.1)
    sigma2 <- 1 / tau

    sigma_beta ~ dunif(0, 10)
    tau_beta   <- 1 / (sigma_beta * sigma_beta)

    pi ~ dbeta(1, 1)
  }
  "

  inits.JAGS <- function() {
    list(
      alpha      = rnorm(1, 0, 1),
      beta_raw   = rnorm(P, 0, 0.1),
      gamma      = rbinom(P, size = 1, prob = 0.5),
      tau        = rgamma(1, 1, 1),
      sigma_beta = runif(1, 0.5, 2),
      pi         = runif(1, 0.25, 0.75)
    )
  }

  para.JAGS <- c("alpha", "beta", "gamma", "pi", "sigma2", "sigma_beta")

  set.seed(2025)
  fit.JAGS <- R2jags::jags(
    data      = dat.JAGS,
    inits     = inits.JAGS,
    parameters.to.save = para.JAGS,
    n.chains  = 1,
    n.iter    = 5000,
    n.burnin  = 2000,
    n.thin    = 2,
    model.file = textConnection(model_string)
  )

  mcmc_list <- coda::as.mcmc(fit.JAGS)
  mcmc_all  <- do.call(rbind, mcmc_list)

  #posterior inclusion prob
  gamma_cols <- grep("^gamma\\[", colnames(mcmc_all))
  gamma_mat  <- mcmc_all[, gamma_cols, drop = FALSE]
  pip        <- colMeans(gamma_mat)
  names(pip) <- colnames(X_m)

  # posterior mean of beta
  beta_cols <- grep("^beta\\[", colnames(mcmc_all))
  beta_mat  <- mcmc_all[, beta_cols, drop = FALSE]
  beta_mean <- colMeans(beta_mat)
  names(beta_mean) <- colnames(X_m)

  list(
    fit       = fit.JAGS,    # R2jags class
    mcmc      = mcmc_list,   # coda  mcmc.list
    pip       = pip,
    beta_mean = beta_mean
  )


}
