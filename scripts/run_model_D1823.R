library(rstan)

update = TRUE
dbh_repeat = TRUE
live_only = TRUE

N_iter = 500
N_keep = 200

#######################################################################################################################################
# 
#######################################################################################################################################

if (update) {
  dat = readRDS('data/D1823/D1823_input_update.RDS')
} else if (!update) {
  dat = readRDS('data/D1823/D1823_input.RDS')
}

init = readRDS('data/D1823/D1823_inits.RDS')

# #######################################################################################################################################
# # 
# #######################################################################################################################################
# 
# # 
# compiled <- stan_model(file = 'models/tree_model_dev.stan')
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
#   saveRDS(post, file = 'output/D1823_output_update.RDS')
# } else {
#   saveRDS(post, file = 'output/D1823_output.RDS')
# }
# 
# #######################################################################################################################################
# #
# #######################################################################################################################################
# 
# compiled <- stan_model(file = 'models/tree_model_species.stan')
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
#   saveRDS(post, file = 'output/D1823_output_update.RDS')
# } else {
#   saveRDS(post, file = 'output/D1823_output.RDS')
# }
# 
# 
# #######################################################################################################################################
# #
# #######################################################################################################################################
# 
# dat = readRDS('data/D1823/D1823_input_update_interval.RDS')
# 
# model = 'species_time'
# 
# compiled <- stan_model(file = 'models/tree_model_species_time.stan')
# 
# fit <- sampling(compiled, 
#                 data = dat, 
#                 # init = init,
#                 iter = 5000, 
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
# N_iter = 500

# dat = readRDS('data/D1823/D1823_input_update_interval.RDS')


if (update & !dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')
  dat$sig_d_obs = 0.02
} else if (update & dbh_repeat & !live_only) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_status.RDS')
  dat$sig_d_obs = 0.02
} else if (update & dbh_repeat & live_only) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_live.RDS')
  dat$sig_d_obs = 0.02
}

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
# rm(fit)


post_sub = post

iter_keep = sample(N_iter/2, 200)

for (i in 1:length(post)) {
  if (length(dim(post[[i]]))==1){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]]))==2) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]]))==3) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update & !dbh_repeat){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else if (update & dbh_repeat & !live_only) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_', model, '_subset.RDS'))
} else if (update & dbh_repeat & live_only) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_live_only_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_live_only_', model, '_subset.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_', model, '_subset.RDS'))
}


#######################################################################################################################################
# PITH
#######################################################################################################################################
# N_iter = 500

# dat = readRDS('data/D1823/D1823_input_update_pith.RDS')

if (update & !dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')
  dat$sig_d_obs = 0.02
} else if (update & dbh_repeat & !live_only) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_status.RDS')
  dat$sig_d_obs = 0.02
} else if (update & dbh_repeat & live_only) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_live.RDS')
  dat$sig_d_obs = 0.02
}

model = 'species_time_negd_pith'

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

iter_keep = sample(N_iter/2, N_keep)

for (i in 1:length(post)) {
  if (length(dim(post[[i]]))==1){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]]))==2) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]]))==3) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update & !dbh_repeat){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else if (update & dbh_repeat & !live_only) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_', model, '_subset.RDS'))
} else if (update & dbh_repeat & live_only) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_live_only_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_live_only_', model, '_subset.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_', model, '_subset.RDS'))
}




#######################################################################################################################################
# PITH
#######################################################################################################################################
N_iter = 500

# dat = readRDS('data/D1823/D1823_input_update_pith.RDS')

if (update & !dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')
  dat$sig_d_obs = 0.02
} else if (update & dbh_repeat & !live_only) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_status.RDS')
  dat$sig_d_obs = 0.02
} else if (update & dbh_repeat & live_only) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_live.RDS')
  dat$sig_d_obs = 0.02
}
model = 'species_time_negd_2pith'

compiled = stan_model(file = paste0('models/tree_model_', model, '.stan'))

fit = sampling(compiled, 
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
  if (length(dim(post[[i]]))==1){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]]))==2) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]]))==3) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update & !dbh_repeat){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else if (update & dbh_repeat) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_', model, '_subset.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_', model, '_subset.RDS'))
}


#######################################################################################################################################
# PITH
#######################################################################################################################################
N_iter = 500

# dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')

# > status_codes
# [1] "vd"   "vp"   "vcas" "vc"  

if (update & !dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')
  dat$sig_d_obs = c(0.02, 0.1, 0.02, 0.5)
} else if (update & dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_status.RDS')
  dat$sig_d_obs = c(0.02, 0.1, 0.02, 0.5)
}

model = 'species_time_negd_2pith_status'

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
  if (length(dim(post[[i]]))==1){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]]))==2) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]]))==3) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update & !dbh_repeat){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else if (update & dbh_repeat) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_', model, '_subset.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_', model, '_subset.RDS'))
}

#######################################################################################################################################
# PITH
#######################################################################################################################################
N_iter = 500

# dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')

# > status_codes
# [1] "vd"   "vp"   "vcas" "vc"  

if (update & !dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')
  dat$sig_d_obs = c(0.02, 0.5, 0.02, 0.8)
} else if (update & dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_status.RDS')
  dat$sig_d_obs = c(0.02, 0.5, 0.02, 0.8)
}

model = 'species_time_negd_status'

compiled <- stan_model(file = paste0('models/tree_model_', model, '.stan'))

fit <- sampling(compiled, 
                data = dat, 
                # init = init,
                iter = N_iter, 
                chains = 1,
                verbose=TRUE)
plot(fit)
rm(compiled)

if (update & !dbh_repeat){
  saveRDS(fit, file = paste0('output/D1823_stan_fit_update_', model, '.RDS'))
} else if (update & dbh_repeat) {
  saveRDS(fit, file = paste0('output/D1823_stan_fit_update_repeat_', model, '.RDS'))
}

post=rstan::extract(fit)
rm(fit)

post_sub = post
iter_keep = sample(N_iter/2, 200)

for (i in 1:length(post)) {
  if (length(dim(post[[i]]))==1){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]]))==2) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]]))==3) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update & !dbh_repeat){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else if (update & dbh_repeat) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_', model, '_subset.RDS'))
} else {
  saveRDS(post, file = paste0('output/D1823_output_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_', model, '_subset.RDS'))
}

#######################################################################################################################################
# PITH
#######################################################################################################################################
N_iter = 500

# dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')

# > status_codes
# [1] "vd"   "vp"   "vcas" "vc"  

if (update & !dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_pith_status.RDS')
  dat$sig_d_obs = c(0.02, 0.5, 0.02, 0.8)
} else if (update & dbh_repeat) {
  dat = readRDS('data/D1823/D1823_input_update_repeat_pith_status.RDS')
  dat$sig_d_obs = c(0.02, 0.5, 0.02, 0.8)
}

model = 'species_time_negd_pith_status'

compiled <- stan_model(file = paste0('models/tree_model_', model, '.stan'))

fit <- sampling(compiled, 
                data = dat, 
                # init = init,
                iter = N_iter, 
                chains = 1,
                verbose=TRUE)
plot(fit)
rm(compiled)

if (update & !dbh_repeat){
  saveRDS(fit, file = paste0('output/D1823_stan_fit_update_', model, '.RDS'))
} else if (update & dbh_repeat) {
  saveRDS(fit, file = paste0('output/D1823_stan_fit_update_repeat_', model, '.RDS'))
}

post=rstan::extract(fit)
rm(fit)

post_sub = post
iter_keep = sample(N_iter/2, 200)

for (i in 1:length(post)) {
  if (length(dim(post[[i]]))==1){
    post_sub[[i]] = post[[i]][iter_keep]
  } else if (length(dim(post[[i]]))==2) {
    post_sub[[i]] = post[[i]][iter_keep,]
  } else if (length(dim(post[[i]]))==3) {
    post_sub[[i]] = post[[i]][iter_keep, ,]
  }
}


if (update & !dbh_repeat){
  saveRDS(post, file = paste0('output/D1823_output_update_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_', model, '_subset.RDS'))
} else if (update & dbh_repeat) {
  saveRDS(post, file = paste0('output/D1823_output_update_repeat_', model, '.RDS'))
  saveRDS(post_sub, paste0('output/D1823_output_update_repeat_', model, '_subset.RDS'))
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

