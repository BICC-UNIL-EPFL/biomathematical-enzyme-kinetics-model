# Libraries
library(tidyverse)

# Constants
n <- 3
t_max <- 5000
precision <- 0.000001

# Functions
delta_ <- function(i, j) {
  return (ifelse(i == j, 1, 0))
}
j_ <- function(l) {
  j_tmp <- l %% n
  return(ifelse(j_tmp == 0, n, j_tmp))
}
i_ <- function(l) {
  return(floor((l - j_(floor(l))) / n) + 1)
}

nxn <- n * n
R_tot <- matrix(rep(0,n),nrow=n,ncol=1)
FF <- matrix(rep(0,nxn),nrow=nxn,ncol=1)
RR <- matrix(rep(0,nxn),nrow=nxn,ncol=1)
Jacobi <- matrix(rep(0,nxn*nxn),nrow=nxn,ncol=nxn)

k_on <- matrix(c(0.2, 0.19, 0.2, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001, 0.00001), nrow=n, ncol=n, byrow=TRUE)
k_off <- matrix(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1), nrow=n, ncol=n, byrow=TRUE)
k_cat <- matrix(c(0.1, 0.08, 0.1, 0, 0, 0, 0, 0, 0), nrow=n, ncol=n, byrow=TRUE)
K_M <- (k_off + k_cat) / k_on

dt <- 0.1
K_ <- 20
q_R1 <- 1
q_R2 <- 1
q_R3 <- 2
d_R1 <- 0.005
d_R2 <- 0.005
d_R3 <- 0.005

for (model in c('2.1', '2.2')) {

  if (model == '2.1') {
    alpha <- 0
    mutation_rate <- 0.01
  }
  if (model == '2.2') {
    alpha <- 0.005
    mutation_rate <- 0
  }

  n_steps <- round(t_max / dt) + 1
  res <- tibble(step = rep(0, n_steps), t = rep(0, n_steps), R1 = rep(0, n_steps), R2 = rep(0, n_steps), R3 = rep(0, n_steps), iter = rep(0, n_steps))
  res[1,] <- list(1, 0, 1, 1, 0, 0)

  t_t <- 0
  t_l <- 1

  while (t_t < t_max) {
    t_l <- t_l + 1
    t_t <- t_t + dt
    print(t_t)
 
    R_R <- matrix(rep(0,n*n),nrow=n,ncol=n)

    for (i in 1:n) {
      R_tot[i, 1] <- res[[t_l - 1, i+2]]
    }
    R_free <- R_tot
  
    PI_ <- (1 - (R_tot[1, 1] / q_R1 + R_tot[2, 1] / q_R2 + R_tot[3, 1] / q_R3) / K_)

    RR_old <- matrix(rep(0,nxn),nrow=nxn,ncol=1)

    err <- 1
    steps <- 0
    while (err > precision) {
      err <- 0
      for (l in 1:nxn) {
	i_l <- i_(l)
	j_l <- j_(l)
	FF[l, 1] <- K_M[i_l, j_l] * R_R[i_l, j_l] - R_free[i_l, 1] * R_free[j_l, 1]
	tmp1 <- FF[l, 1]
	for (m in 1:nxn) {
	  i_m <- i_(m)
	  j_m <- j_(m)
	  d_K_M <- delta_(l, m)
	  d_r_i <- delta_(i_m, i_l) + delta_(j_m, j_l) - delta_(i_m, i_l) * delta_(j_m, i_l)
	  d_r_j <- delta_(i_m, j_l) + delta_(j_m, j_l) - delta_(i_m, j_l) * delta_(j_m, j_l)
	  Jacobi[l, m] <- d_K_M * K_M[i_l, j_l] + d_r_i * R_free[j_l, 1] + d_r_j * R_free[i_l, 1]
	}
      }
      Result <- solve(Jacobi) %*% FF
      for (l in 1:nxn) {
	RR[l, 1] <- RR_old[l, 1] - Result[l, 1]
	err <- err + abs(RR[l, 1] - RR_old[l, 1])
      }
    
      RR_old <- RR
      for (l in 1:nxn) {
	i_l <- i_(l)
	j_l <- j_(l)
	R_R[i_l, j_l] <- RR[l, 1]
      }
      for (i in 1:n) {
       tmp1 <- R_tot[i, 1]
       for (k in 1:n) {
	 tmp1 <- tmp1 - R_R[k, i] - R_R[i, k]
       }
       R_free[i, 1] <- tmp1 + R_R[i, i]
      }
      steps <- steps + 1
    } #End While err > precision
    R_tot[1, 1] <- R_tot[1, 1] + dt * (k_cat[1, 2] * R_R[1, 2] * (1 - mutation_rate) * PI_ - d_R1 * R_tot[1, 1])
    R_tot[2, 1] <- R_tot[2, 1] + dt * (k_cat[1, 1] * R_R[1, 1] * (1 - mutation_rate) * PI_ - d_R2 * R_tot[2, 1])
    R_tot[3, 1] <- R_tot[3, 1] + dt * (alpha + (k_cat[1, 3] * R_R[1, 3] + mutation_rate * (k_cat[1, 2] * R_R[1, 2] + k_cat[1, 1] * R_R[1, 1])) * PI_ - d_R3 * R_tot[3, 1])
    res[t_l,] <- list(t_l, t_t, R_tot[[1, 1]], R_tot[[2, 1]], R_tot[[3, 1]], steps)
    PI_ <- (1 - (R_tot[1, 1] / q_R1 + R_tot[2, 1] / q_R2 + R_tot[3, 1] / q_R3) / K_)
  }
  write_tsv(res, paste0("Model_",model,"_res.tsv.gz"))
  figure <- ggplot(res) + geom_point(aes(t, R1, color = 'R1')) + geom_point(aes(t, R2, color = 'R2')) + geom_point(aes(t, R3, color = 'R3')) + scale_colour_manual(values=c("red", "chartreuse", "blue"))
  pdf(paste0('Model_',model,'.pdf'),paper="a4r",width=0,height=0)
  print(figure)
  dev.off()
}
###
