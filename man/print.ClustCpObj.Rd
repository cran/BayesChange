% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClustCpObj.R
\name{print.ClustCpObj}
\alias{print.ClustCpObj}
\title{ClustCpObj print method}
\usage{
\method{print}{ClustCpObj}(x, ...)
}
\arguments{
\item{x}{an object of class \code{ClustCpObj}.}

\item{...}{parameter of the generic method.}
}
\description{
The \code{ClustCpObj} method prints which algorithm was run.
}
\examples{

data_mat <- matrix(NA, nrow = 5, ncol = 100)

data_mat[1,] <- as.numeric(c(rnorm(50,0,0.100), rnorm(50,1,0.250)))
data_mat[2,] <- as.numeric(c(rnorm(50,0,0.125), rnorm(50,1,0.225)))
data_mat[3,] <- as.numeric(c(rnorm(50,0,0.175), rnorm(50,1,0.280)))
data_mat[4,] <- as.numeric(c(rnorm(25,0,0.135), rnorm(75,1,0.225)))
data_mat[5,] <- as.numeric(c(rnorm(25,0,0.155), rnorm(75,1,0.280)))

out <- clust_cp(data = data_mat, n_iterations = 5000, n_burnin = 1000,
                params = list(L = 1, B = 1000, phi = 0.5), kernel = "ts")

print(out)

}
