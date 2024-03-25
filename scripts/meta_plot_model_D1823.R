



# model = 'species_time_negd'
# model = 'species_time_negd_pith'
# model = 'species_time_negd_2pith'
# # model = 'repeat_species_time_negd'
# # model = 'repeat_species_time_negd_pith'
# # model = 'repeat_species_time_negd_2pith'
# # model = 'species_time_negd_2pith_status'


models = c('species_time_negd',
           'species_time_negd_pith',
           'species_time_negd_pith_status',
           'species_time_negd_2pith',
           'species_time_negd_2pith_status')

# data_name = 'pith'
dataset_root = 'pith_status'
# data_name = 'repeat_pith_status'
# 
# 
# datasets = c('pith_status',
#              'repeat_pith_status')


for (model_name in models){
  
  data_name = dataset_root
  
  print(paste0('Processing output for model: ', model_name))
  print(paste0(  'Dataset: ', data_name))
  source('scripts/plot_model_D1823.R')

  model_name = paste0('repeat_', model_name)
  data_name =  paste0('repeat_', dataset_root)
  
  print(paste0('Processing output for model: ', model_name))
  print(paste0(  'Dataset: ', data_name))
  source('scripts/plot_model_D1823.R')
  
  # paste0('End processing')
}

