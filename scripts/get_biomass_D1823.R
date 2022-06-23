library(allodb)
library(ggplot2)
library(reshape2)
library(dplyr)
# library(grid)

coords = read.csv('data/maleki-2021/hectare_plots.csv')
coords_D1823 = as.numeric(coords[which(coords$plot_id == 'H1823'), c('long', 'lat')])

############################################################################################
# 
############################################################################################

dat  = readRDS('data/D1823/D1823_input.RDS')
post = readRDS('output/D1823_output.RDS')

names(post)
dim(post$d_latent)

d_post = post$d_latent
d_post[,1,1]
N_iter = dim(d_post)[1]

d_post_mean = apply(d_post, c(2,3), mean)
dim(d_post_mean)

list2env(dat, envir = globalenv())
list2env(post, envir = globalenv())

species_table = data.frame(species_ids = species_ids,
                           genus = c('Thuja', 'Abies', 'Populus', 'Betula', 'Picea'),
                           species = c('occidentalis', 'balsamea', 'tremuloides', 'papyrifera', 'glauca'))

# logy[logy==(-999)] = NA
# y[y==(-999)] = NA

############################################################################################
# 
############################################################################################

## example
bio_test = get_biomass(dbh = 20,
                       genus = 'Picea',
                       species = 'glauca',
                       coords = coords_D1823)

## example
biomass = array(NA, c(N_trees, N_years))

for (tree in 1:N_trees){
  
  tree_genus   = species_table[core2species[tree],'genus']
  tree_species = species_table[core2species[tree],'genus']
  
  biomass[tree,] = get_biomass(dbh = d_post_mean[tree,],
                               genus = tree_genus,
                               species = tree_species,
                               coords = coords_D1823)
}



# ## example
# biomass = array(NA, c(N_iter, N_trees, N_years))
# 
# for (iter in 1:N_iter){
#   for (tree in 1:N_trees){
#     
#     tree_genus   = species_table[d2species[tree],'genus']
#     tree_species = species_table[d2species[tree],'genus']
#     
#     biomass[iter, tree,] = get_biomass(dbh = d_post[iter, tree,],
#                                        genus = tree_genus,
#                                        species = tree_species,
#                                        coords = coords_D1823)
#   }
# }

biomass

############################################################################################
# 
############################################################################################

bio_df = data.frame(stat_id = core2tree, 
                    species_id = species_ids[core2species], 
                    biomass)

bio_melt = melt(bio_df, id.vars = c('stat_id', 'species_id'))
bio_melt$year = years[as.numeric(substr(bio_melt$variable, 2, 5))]

ggplot() +
  geom_line(data=bio_melt, aes(x=year, y=value, colour=stat_id, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('Biomass (kg)')

ggplot() +
  geom_line(data=bio_melt, aes(x=year, y=value, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('Biomass (kg)') +
  facet_wrap(~species_id)
            
############################################################################################
# calculate biomass increment
############################################################################################

agbi_melt =  bio_melt %>% 
  group_by(stat_id, species_id) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(agbi = value - lag(value))

ggplot() +
  geom_line(data=agbi_melt, aes(x=year, y=agbi, colour=stat_id, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('Biomass increment (kg)')

ggplot() +
  geom_line(data=agbi_melt, aes(x=year, y=agbi, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('Biomass increment (kg)') +
  facet_wrap(~species_id)
