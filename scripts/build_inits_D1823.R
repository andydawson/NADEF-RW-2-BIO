library(rstan)

#######################################################################################################################################
# build_inits
#######################################################################################################################################

post = readRDS(file = 'output/D1823_output.RDS')
#   dat = readRDS('data/D1823/D1823_input_update.RDS')
# } else {
#   dat = readRDS('data/D1823/D1823_input.RDS')
# }

unlist(post)

beta0 = mean(post$beta0)
beta = colMeans(post$beta)
beta_sd = mean(post$beta_sd)

beta_t = colMeans(post$beta_t)
beta_t_sd = mean(post$beta_t_sd)

sig_x = mean(post$sig_x)
sig_x_obs = mean(post$sig_x_obs)

d_init = colMeans(post$d_init)

x = apply(post$x, c(2,3), mean)

saveRDS(list(list(beta0 = beta0,
             beta = beta,
             beta_sd = beta_sd,
             beta_t = beta_t,
             beta_t_sd = beta_t_sd,
             sig_x = sig_x,
             sig_x_obs = sig_x_obs,
             d_init = d_init,
             x = x)),
        file = 'data/D1823/D1823_inits.RDS')
