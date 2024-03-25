library(loo)


figure_dir = file.path('figures', model_name)

dataset = readRDS(paste0('data/D1823/D1823_input_update_', data_name, '.RDS'))
post = readRDS(paste0('output/D1823_output_update_', model_name, '.RDS'))

loo(post)
