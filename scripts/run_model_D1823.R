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

init = readRDS('data/D1823/D1823_inits.RDS')

#######################################################################################################################################
# 
#######################################################################################################################################

# 
compiled <- stan_model(file = 'models/tree_model_dev.stan')

fit <- sampling(compiled, 
                data = dat, 
                # init = init,
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

#######################################################################################################################################
#
#######################################################################################################################################

compiled <- stan_model(file = 'models/tree_model_species.stan')

fit <- sampling(compiled, 
                data = dat, 
                # init = init,
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


#######################################################################################################################################
#
#######################################################################################################################################

dat = readRDS('data/D1823/D1823_input_update_interval.RDS')

model = 'species_time'

compiled <- stan_model(file = 'models/tree_model_species_time.stan')

fit <- sampling(compiled, 
                data = dat, 
                # init = init,
                iter = 5000, 
                chains = 1,
                verbose=TRUE)
plot(fit)
rm(compiled)

post=rstan::extract(fit)
rm(fit)

if (update){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
}

#######################################################################################################################################
#
#######################################################################################################################################
N_iter = 5000

dat = readRDS('data/D1823/D1823_input_update_interval.RDS')

model = 'species_time_negd'

compiled <- stan_model(file = paste0('models/tree_model_', model, '.stan'))

fit <- sampling(compiled, 
                data = dat, 
                # init = init,
                iter = N_iter, 
                chains = 1,
                verbose=TRUE)
plot(fit)
rm(compiled)

post=rstan::extract(fit)
rm(fit)


post_sub = post

iter_keep = sample(N_iter/2, 200)

for (i in 1:length(post)) {
  if (length(dim(post[[i]])==1)){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]])==2)) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]])==3)) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_', model, '_subset.RDS'))
}




#######################################################################################################################################
#
#######################################################################################################################################

# dat = readRDS('data/D1823/D1823_input_update_test.RDS')
# 
# 
# model = 'species_time_interval'
# 
# compiled <- stan_model(file = 'models/tree_model_species_time_interval.stan')
# 
# fit <- sampling(compiled, 
#                 data = dat, 
#                 # init = init,
#                 iter = 2000, 
#                 chains = 1,
#                 verbose=TRUE)
# plot(fit)
# rm(compiled)
# 
# post=rstan::extract(fit)
# rm(fit)
# 
# if (update){
#   saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
# } else {
#   saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
# }

#######################################################################################################################################
#
#######################################################################################################################################
# 
# model = 'species_time_interval_sigd'
# 
# compiled <- stan_model(file = 'models/tree_model_species_time_interval_sigd.stan')
# 
# fit <- sampling(compiled, 
#                 data = dat, 
#                 # init = init,
#                 iter = 500, 
#                 chains = 1,
#                 verbose=TRUE)
# plot(fit)
# rm(compiled)
# 
# post=rstan::extract(fit)
# rm(fit)
# 
# if (update){
#   saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
# } else {
#   saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
# }

