library(rstan)

update = TRUE

#######################################################################################################################################
# 
#######################################################################################################################################

if (update){
  dat = readRDS('data/D1823/D1823_input_update.RDS')
} else {
  dat = readRDS('data/D1823/D1823_input.RDS')
}
# 
compiled <- stan_model(file = 'models/tree_model.stan')

fit <- sampling(compiled, 
                data = dat, 
                iter = 500, 
                chains = 1,
                verbose=TRUE)
plot(fit)
rm(compiled)

post=rstan::extract(fit)
rm(fit)

if (update){
  saveRDS(post, file = 'output/D1823_output_update.RDS')
} else {
  saveRDS(post, file = 'output/D1823_output.RDS')
}

# #######################################################################################################################################
# # 
# #######################################################################################################################################
# 
# dat = readRDS('data/D1823/D1823_stacked_input.RDS')
# 
# compiled <- stan_model(file = 'models/tree_model_stacked.stan')
# 
# fit <- sampling(compiled, 
#                 data = dat, 
#                 iter = 2000, 
#                 chains = 1,
#                 verbose=TRUE)
# plot(fit)
# rm(compiled)
# 
# post=rstan::extract(fit)
# rm(fit)
# 
# saveRDS(post, file = 'output/D1823_stacked_output.RDS')
