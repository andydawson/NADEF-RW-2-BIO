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

compiled <- stan_model(file = 'models/tree_model_species_time.stan')

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
