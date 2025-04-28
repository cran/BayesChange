## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BayesChange)

## -----------------------------------------------------------------------------
data_uni <- as.numeric(c(rnorm(50,0,0.1), rnorm(50,1,0.25)))

## -----------------------------------------------------------------------------
out <- detect_cp(data = data_uni,                             
                 n_iterations = 1000, n_burnin = 100,  
                 params = list(q = 0.25, phi = 0.1, a = 1, b = 1, c = 0.1), kernel = "ts")

## -----------------------------------------------------------------------------
print(out)

summary(out)

## -----------------------------------------------------------------------------
table(posterior_estimate(out, loss = "binder"))

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

## -----------------------------------------------------------------------------
data_multi <- matrix(NA, nrow = 3, ncol = 100)

data_multi[1,] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_multi[2,] <- as.numeric(c(rnorm(50,0,0.125), rnorm(50,1,0.225)))
data_multi[3,] <- as.numeric(c(rnorm(50,0,0.175), rnorm(50,1,0.280)))

## -----------------------------------------------------------------------------
out <- detect_cp(data = data_multi, n_iterations = 1000, n_burnin = 100,
                 q = 0.25, params = list(k_0 = 0.25, nu_0 = 4, phi_0 = diag(1,3,3), 
                      m_0 = rep(0,3), par_theta_c = 2, par_theta_d = 0.2, 
                      prior_var_gamma = 0.1), kernel = "ts")

table(posterior_estimate(out, loss = "binder"))

## -----------------------------------------------------------------------------
plot(out, loss = "binder", plot_freq = TRUE)

## -----------------------------------------------------------------------------
data_mat <- matrix(NA, nrow = 1, ncol = 100)

betas <- c(rep(0.45, 25),rep(0.14,75))

## -----------------------------------------------------------------------------
inf_times <- sim_epi_data(10000, 10, 100, betas, 1/8)

inf_times_vec <- rep(0,100)
names(inf_times_vec) <- as.character(1:100)

for(j in 1:100){
  if(as.character(j) %in% names(table(floor(inf_times)))){
    inf_times_vec[j] = table(floor(inf_times))[which(names(table(floor(inf_times))) == j)]
  }
}

data_mat[1,] <- inf_times_vec

## -----------------------------------------------------------------------------
out <- detect_cp(data = data_mat, n_iterations = 200, n_burnin = 50,
                 params = list(xi = 1/8, a0 = 40, b0 = 10, M = 1000), kernel = "epi")

print(out)
table(posterior_estimate(out, loss = "binder"))

## -----------------------------------------------------------------------------
plot(out)

## -----------------------------------------------------------------------------
data_mat <- matrix(NA, nrow = 5, ncol = 100)

data_mat[1,] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_mat[2,] <- as.numeric(c(rnorm(50,0,0.125), rnorm(50,1,0.225)))
data_mat[3,] <- as.numeric(c(rnorm(50,0,0.175), rnorm(50,1,0.280)))
data_mat[4,] <- as.numeric(c(rnorm(25,0,0.135), rnorm(75,1,0.225)))
data_mat[5,] <- as.numeric(c(rnorm(25,0,0.155), rnorm(75,1,0.280)))


## -----------------------------------------------------------------------------
out <- clust_cp(data = data_mat, n_iterations = 1000, n_burnin = 100, 
                kernel = "ts",
                params = list(B = 1000, L = 1, gamma = 0.5))

posterior_estimate(out, loss = "binder")

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

## -----------------------------------------------------------------------------
data_array <- array(data = NA, dim = c(3,100,5))

data_array[1,,1] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_array[2,,1] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_array[3,,1] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))

data_array[1,,2] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_array[2,,2] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_array[3,,2] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))

data_array[1,,3] <- as.numeric(c(rnorm(50,0,0.175), rnorm(50,1,0.280)))
data_array[2,,3] <- as.numeric(c(rnorm(50,0,0.175), rnorm(50,1,0.280)))
data_array[3,,3] <- as.numeric(c(rnorm(50,0,0.175), rnorm(50,1,0.280)))

data_array[1,,4] <- as.numeric(c(rnorm(25,0,0.135), rnorm(75,1,0.225)))
data_array[2,,4] <- as.numeric(c(rnorm(25,0,0.135), rnorm(75,1,0.225)))
data_array[3,,4] <- as.numeric(c(rnorm(25,0,0.135), rnorm(75,1,0.225)))

data_array[1,,5] <- as.numeric(c(rnorm(25,0,0.155), rnorm(75,1,0.280)))
data_array[2,,5] <- as.numeric(c(rnorm(25,0,0.155), rnorm(75,1,0.280)))
data_array[3,,5] <- as.numeric(c(rnorm(25,0,0.155), rnorm(75,1,0.280)))


## -----------------------------------------------------------------------------
out <- clust_cp(data = data_array, n_iterations = 1000, n_burnin = 100, 
                kernel = "ts", params = list(gamma = 0.1, k_0 = 0.25, nu_0 = 5,  phi_0 = diag(0.1,3,3), m_0 = rep(0,3)))

posterior_estimate(out, loss = "binder")

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

## -----------------------------------------------------------------------------
data_mat <- matrix(NA, nrow = 5, ncol = 50)

betas <- list(c(rep(0.45, 25),rep(0.14,25)),
               c(rep(0.55, 25),rep(0.11,25)),
               c(rep(0.50, 25),rep(0.12,25)),
               c(rep(0.52, 10),rep(0.15,40)),
               c(rep(0.53, 10),rep(0.13,40)))

  inf_times <- list()

  for(i in 1:5){

    inf_times[[i]] <- sim_epi_data(S0 = 10000, I0 = 10, max_time = 50, beta_vec = betas[[i]], xi_0 = 1/8)

    vec <- rep(0,50)
    names(vec) <- as.character(1:50)

    for(j in 1:50){
      if(as.character(j) %in% names(table(floor(inf_times[[i]])))){
      vec[j] = table(floor(inf_times[[i]]))[which(names(table(floor(inf_times[[i]]))) == j)]
      }
    }
    data_mat[i,] <- vec
  }



## -----------------------------------------------------------------------------
out <- clust_cp(data = data_mat, n_iterations = 100, n_burnin = 10, 
                kernel = "epi", 
                list(M = 100, B = 1000, L = 1, q = 0.1, gamma = 1/8))

posterior_estimate(out, loss = "binder")

## -----------------------------------------------------------------------------
plot(out, loss = "binder")

