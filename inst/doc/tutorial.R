## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BayesChange)

## -----------------------------------------------------------------------------
data("eu_inflation")

## -----------------------------------------------------------------------------
out <- detect_cp(data = eu_inflation[1,],
                 n_iterations = 2000, n_burnin = 500, q = 0.5,
                 params = list(prior_var_phi = 0.1, prior_delta_c = 1, prior_delta_d = 1), kernel = "ts")

## -----------------------------------------------------------------------------
print(out)
summary(out)

## -----------------------------------------------------------------------------
cp_est <- posterior_estimate(out, loss = "binder")
cumsum(table(cp_est))[-length(table(cp_est))] + 1

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

## -----------------------------------------------------------------------------
coda::traceplot(out$lkl_MCMC, ylab = "Log-Likelihood")

## -----------------------------------------------------------------------------
params_multi <- list(m_0 = rep(0,3),
                     k_0 = 1,
                     nu_0 = 10,
                     S_0 = diag(0.1,3,3),
                     prior_var_phi = 0.1,
                     prior_delta_c = 1,
                     prior_delta_d = 1)

## -----------------------------------------------------------------------------
out <- detect_cp(data = eu_inflation[1:3,], n_iterations = 2000,
          n_burnin = 500, q = 0.5, params = params_multi, kernel = "ts")

table(posterior_estimate(out, loss = "binder"))

## -----------------------------------------------------------------------------
plot(out, loss = "binder", plot_freq = TRUE)

## -----------------------------------------------------------------------------
data("epi_synthetic")

## -----------------------------------------------------------------------------
params_epi <- list(M = 250, xi = 1/8, a0 = 4, b0 = 10, I0_var = 0.1)

out <- detect_cp(data = epi_synthetic, n_iterations = 2000, n_burnin = 500,
                 q = 0.25, params = params_epi, kernel = "epi")

print(out)

## -----------------------------------------------------------------------------
plot(out)

## -----------------------------------------------------------------------------
data("stock_uni")

## -----------------------------------------------------------------------------
params_uni <- list(a = 1,
                   b = 1,
                   c = 1,
                   phi = 0.1)

out <- clust_cp(data = stock_uni[1:5,], n_iterations = 2000, n_burnin = 500,
                L = 1, q = 0.5, B = 1000, params = params_uni, kernel = "ts")

posterior_estimate(out, loss = "binder")

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

## -----------------------------------------------------------------------------
plot_psm(out, reorder = TRUE)

## -----------------------------------------------------------------------------
data("stock_multi")

## -----------------------------------------------------------------------------
params_multi <- list(m_0 = rep(0,2),
                     k_0 = 1,
                     nu_0 = 10,
                     S_0 = diag(1,2,2),
                     phi = 0.1)

out <- clust_cp(data = stock_multi[,,1:5], n_iterations = 2500, n_burnin = 500,
                L = 1, B = 1000, params = params_multi, kernel = "ts")

posterior_estimate(out, loss = "binder")

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

## ----eval = FALSE-------------------------------------------------------------
# data("epi_synthetic_multi")
# 
# params_epi <- list(M = 100, xi = 1/8,
#                    alpha_SM = 1,
#                    a0 = 4,
#                    b0 = 10,
#                    I0_var = 0.1,
#                    avg_blk = 2)
# 
# out <- clust_cp(epi_synthetic_multi[,10:150], n_iterations = 2000, n_burnin = 500,
#                 L = 1, B = 1000, params = params_epi, kernel = "epi")
# 
# posterior_estimate(out, loss = "binder")
# plot(out, loss = "binder")

