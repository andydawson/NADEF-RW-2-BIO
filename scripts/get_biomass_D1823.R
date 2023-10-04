library(allodb)
library(ggplot2)
library(reshape2)
library(dplyr)
# library(grid)
rm(list = ls())
coords = read.csv('data/maleki-2021/hectare_plots.csv')
coords_D1823 = as.numeric(coords[which(coords$plot_id == 'H1823'), c('long', 'lat')])

############################################################################################
# 
############################################################################################

update = TRUE # I guess we don't need to run the model with the original dataset
interval_cut = FALSE #what is this?

remove_deadtrees = TRUE

model = 'species_time_negd_2pith_status'
# model = 'species_time_interval'
data_name = 'pith_status'

if (update){
  # dat = readRDS('data/D1823/D1823_input_update.RDS')
  # dat = readRDS('data/D1823/D1823_input_update_pith.RDS')
  dat = readRDS(paste0('data/D1823/D1823_input_update_', data_name, '.RDS'))
  # post = readRDS('output/D1823_output_update.RDS')
  post = readRDS(paste0('output/D1823_output_update_', model, '.RDS'))
} else {
  # dat = readRDS('data/D1823/D1823_input.RDS')
  # post = readRDS('output/D1823_output.RDS') 
}

list2env(dat, envir = globalenv())
list2env(post, envir = globalenv())


year_idx = seq(1, N_years)

###What is the interval_cut? I am not sure if this is still needed in the new version.
###Should we delete this part?
if (interval_cut){
  for (i in 1:N_trees){
    year_start_tree = rw_year_start[i]
    year_end_tree = rw_year_end[i]
    
    if (year_start_tree != 1){
      
      # post$x[,i,1:(year_start_tree-1)] = NA
      # post$d_latent[,i,1:(year_start_tree-1)] = NA
      
      x[,i,1:(year_start_tree-1)] = NA
      d_latent[,i,1:(year_start_tree-1)] = NA
      
    }
    
    if (year_end_tree != max(year_idx)){
      
      # post$x[,i,(year_end_tree+1):N_years] = NA
      # post$d_latent[,i,(year_end_tree+1):N_years] = NA 
      
      x[,i,(year_end_tree+1):N_years] = NA
      d_latent[,i,(year_end_tree+1):N_years] = NA 
      
    }
  }
}

names(post)
dim(post$x)

N_iter = dim(post$x)[1]

x_post = post$x
x_post[,1,1]

# x = apply(x_post, c(2,3), mean)

#replace -999 with NA
logy[logy==(-999)] = NA
y[y==(-999)] = NA

year_lo = min(years)
year_hi = max(years)
species_table = data.frame(species_ids = species_ids,
                            genus = c( 'Abies', 'Thuja', 'Populus', 'Betula', 'Picea', 'Picea'),
                            species = c( 'balsamea', 'occidentalis', 'tremuloides', 'papyrifera', 'glauca', 'mariana'))
####
### where is this species table? I cannot find it in my folder
####
species_table = read.csv('data/species_table.csv', stringsAsFactors = FALSE)


# ############################################################################################
# # non-negative dbh values and the random year of death for trees died since the 1994 census
# ############################################################################################
# keep only non-negative dbh values for each tree
d_latent_pos = post$d_latent
d_latent_pos[d_latent_pos <= 0] = NA

if(remove_deadtrees){
  d_latent_pos_living_trees = d_latent_pos
  # a random process to determine the year of death 
  meta_update = read.csv("data/D1823/D1823_meta_update.csv")
  dead_trees = meta_update[grepl("M", meta_update$status_id, fixed = TRUE), ]
  unique(dead_trees$year)
  dead_trees = dead_trees %>% arrange(year)
  dead_trees_first_year = dead_trees %>% group_by(stem_id) %>% slice(1)
  dead_trees_first_year = as.data.frame(t(apply(dead_trees_first_year, 1, function(x){
    yr1 = as.numeric(x[7])
    if(yr1 == 2004){yr0 = 1994}
    if(yr1 == 2011){yr0 = 2004}
    if(yr1 == 2019){yr0 = 2011}
    x_new = c(x[1:6], "year0" = yr0, x[7:11])
    return(x_new)
  })))
  
  #
  for (tree in 1:N_trees){
    print(tree)
    stem_id = core2stemids[tree]
    if(stem_id %in% dead_trees_first_year$stem_id){ #these trees are assumed to die between the start_death and the end_death
      start_death =  1+as.numeric(dead_trees_first_year$year0[dead_trees_first_year$stem_id == stem_id]) 
      end_death =  as.numeric(dead_trees_first_year$year[dead_trees_first_year$stem_id == stem_id]) 
      
      traj_tree = apply(d_latent_pos[ ,tree, ], 1, function(x){
        death_yr_random = sample(seq(start_death, end_death, 1), 1)
        x[which(years >=  death_yr_random)] <- NA
        return(x)
      })
      
      d_latent_pos_living_trees[ ,tree, ] = t(traj_tree)
    }
  }
  saveRDS(d_latent_pos_living_trees, 'data/D1823/D1823_processed_living_dbh_trajectories.RDS')
}

d_latent_pos_living_trees = readRDS('data/D1823/D1823_processed_living_dbh_trajectories.RDS')

d_post = d_latent_pos_living_trees

############################################################################################
# using get_biomass function
############################################################################################

# ## example
# bio_test = get_biomass(dbh = c(20, 24, 27),
#                        genus = 'Picea',
#                        species = 'glauca',
#                        coords = coords_D1823)

## example
biomass_iter = array(NA, c(N_iter, N_trees, N_years))
biomass_iter = array(NA, c(50, N_trees, N_years))

for (tree in 1:N_trees){
  
  print(paste0("tree: ", tree))
  
  for (iter in 1:50){#N_iter){
    
    # print(paste0("tree: ", tree, "; iter: ", iter))
    
    tree_genus   = species_table[core2species[tree],'genus']
    tree_species = species_table[core2species[tree],'species']
    
    biomass_iter[iter, tree,] = get_biomass(dbh = d_post[iter, tree,],
                                 genus = tree_genus,
                                 species = tree_species,
                                 coords = coords_D1823)
    
    foo = get_biomass(dbh = d_post[iter, tree,],
                                            genus = tree_genus,
                                            species = tree_species,
                                            coords = coords_D1823)
    
    # print(all(diff(foo)>0, na.rm = TRUE))
  }
}


agb_melt = melt(biomass_iter)

colnames(agb_melt) = c('iter', 'stat_id', 'year_idx', 'agb')

############################################################################################
#
############################################################################################

agb_melt = data.frame(species_id = species_ids[core2species[agb_melt$stat_id]],
                      agb_melt)

agb_melt$year = years[agb_melt$year_idx]

# ggplot() +
#   geom_line(data=bio_melt, aes(x=year, y=value, colour=stat_id, group=stat_id)) +
#   theme_bw(14) +
#   xlab('year') +
#   ylab('Biomass (kg)')
# 
# ggplot() +
#   geom_line(data=bio_melt, aes(x=year, y=value, group=stat_id)) +
#   theme_bw(14) +
#   xlab('year') +
#   ylab('Biomass (kg)') +
#   facet_wrap(~species_id)

############################################################################################
# calculate biomass increment
############################################################################################

# agbi_melt =  bio_melt %>%
#   group_by(stat_id, species_id) %>%
#   arrange(year, .by_group=TRUE) %>%
#   mutate(agbi = value - lag(value))
# 
# ggplot() +
#   geom_line(data=agbi_melt, aes(x=year, y=agbi, colour=stat_id, group=stat_id)) +
#   theme_bw(14) +
#   xlab('year') +
#   ylab('Biomass increment (kg)')
# 
# ggplot() +
#   geom_line(data=agbi_melt, aes(x=year, y=agbi, group=stat_id)) +
#   theme_bw(14) +
#   xlab('year') +
#   ylab('Biomass increment (kg)') +
#   facet_wrap(~species_id)

# ############################################################################################
# # apply allometric equations for different species
# ############################################################################################
# 
# # verify if the allodb package directly uses the equation parameters returned from the est_params() function
# # Must run these codes as the 'equation_biomass' will be used
# curve_biomass = data.frame(matrix(nrow = 21*6, ncol = 4))
# colnames(curve_biomass) = c('species_id', 'dbh', 'biomass', "equation_est")
# curve_biomass$species_id = rep(unique(species_table$species_ids), each = 21)
# curve_biomass$dbh = rep(seq(0,100,5), 6)
# 
# equation_biomass <- data.frame(matrix(ncol = 7))
# 
# for(i in 1:nrow(species_table)){
#   speices_id = species_table$species_ids[i]
#   genus_i = species_table$genus[i]
#   species_i = species_table$species[i]
#   equation_biomass[i,] <- est_params(
#     genus = genus_i,
#     species = species_i,
#     coords = coords_D1823
#   )
#   
#   for (j in 1:21) {
#     curve_biomass[21*(i-1)+j,3] <- get_biomass(dbh = curve_biomass[21*(i-1)+j,2] ,
#                                               genus = genus_i,
#                                               species = species_i,
#                                               coords = coords_D1823)
#     curve_biomass[21*(i-1)+j,4] <- equation_biomass[i,5]*curve_biomass[21*(i-1)+j,2]^equation_biomass[i,6]
#   }
# }
# 
# #the graph shows that the allodb package directly uses these equation parameters 
# ggplot(curve_biomass)+
#   geom_point(aes(x = dbh, y = biomass, col = species_id))+
#   geom_line(aes(x = dbh, y = equation_est, col = species_id))+
#   theme_bw()
# 
# # apply the equation parameters, rather than the get_biomass() function, to convert dbh to biomass
# N_iter  = dim(d_latent_pos_living_trees)[1]
# biomass_iter = array(NA, c(N_iter, N_trees, N_years))
# 
# for (tree in 1:N_trees){
#   #tree = 1
#   print(tree)
#   species_id = species_ids[core2species[tree]]
#   
#   genus = species_table[species_table$species_ids == species_id, 'genus']
#   species = species_table[species_table$species_ids == species_id, 'species']
#   equation = equation_biomass[equation_biomass$X1 == genus&equation_biomass$X2 == species,]
#   if(remove_deadtrees){
#     biomass_iter[,tree,] = equation[,5]*d_latent_pos_living_trees[,tree,]^equation[,6]
#   }else{
#     biomass_iter[,tree,] = equation[,5]*d_latent_pos[,tree,]^equation[,6]
#   }
# }

############################################################################################
# 
############################################################################################

# bio_df = data.frame(stat_id = core2tree, 
#                     species_id = species_ids[core2species], 
#                     biomass)

# agb_melt = melt(biomass_iter)#, id.vars = c('stat_id', 'species_id'))
# colnames(agb_melt) = c('iter', 'stat_id', 'year', 'agb')
# agb_melt$year = years[agb_melt$year]
# agb_melt$species_id = species_ids[core2species[agb_melt$stat_id]]

# bio_mean = apply(biomass_iter, c(1,2), mean)
# bio_quant = t(apply(biomass_iter, 2, function(x) quantile(x, c(0.025, 0.5, 0.975))))
# 
# bio_tree = data.frame(bio_mean = bio_mean, 
#                       bio_median = bio_quant[,2], 
#                       bio_lo = bio_quant[,1], 
#                       bio_hi = bio_quant[,3], 
#                       year = years)

agb_quants = agb_melt %>% 
  dplyr::group_by(stat_id, year, species_id) %>%
  dplyr::summarize(agb_mean = mean(agb, na.rm=TRUE), 
            agb_median = median(agb, na.rm=TRUE), 
            agb_lo = quantile(agb, c(0.025), na.rm=TRUE),
            agb_hi = quantile(agb, c(0.975), na.rm=TRUE), 
            .groups="keep")


# > unique(agb_melt[which(agb_melt$species_id == 'BPA'), 'stat_id'])
# [1]   65   84   88  100  275  291  390  392  398  399  409  410  447  527  554  629
# [17]  635  651  699  701  755  762  804  848  852  873  876  878  886  949  952  979
# [33]  994 1014 1015 1016 1017 1019 1024 1054 1070 1071 1072 1103 1106 1112 1114 1133
# [49] 1135 1142 1149 1160
bpa_stat_id = unique(agb_melt[which(agb_melt$species_id == 'BPA'), 'stat_id'])
for (i in 1:length(bpa_stat_id)){
  id = bpa_stat_id[i]
  print(id)
  
  agb_sub = agb_quants[which(agb_quants$stat_id == id),]
  
  p = ggplot(data=agb_sub) +
    geom_line(aes(x=year, y=agb_median)) +
    ggtitle(id)
  print(p)
  
}
# agb_quants[which(agb_quants$stat_id %in% bpa_stat_id),]

ggplot() +
  geom_ribbon(data=agb_quants, aes(x=year, ymin=agb_lo, ymax=agb_hi, group=stat_id), fill='lightgrey') +
  geom_line(data=agb_quants, aes(x=year, y=agb_median, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass (kg)')

if (update){
png('figures/agb_vs_year_ind_species_facet_update.png', width=1200, height=700)
} else {
png('figures/agb_vs_year_ind_species_facet.png', width=1200, height=700)
}
# pdf('figures/agb_vs_year_ind_species_facet.pdf', width=10, height=6)
p <- ggplot() +
  geom_ribbon(data=agb_quants, aes(x=year, ymin=agb_lo, ymax=agb_hi, group=stat_id), fill='lightgrey') +
  geom_line(data=agb_quants, aes(x=year, y=agb_median, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass (kg)') +
  facet_wrap(~species_id, scales = 'free_y')
print(p)
dev.off()

agb_species = agb_melt %>% 
  group_by(year, iter, species_id) %>%
  summarize(agb = sum(agb, na.rm = TRUE), .groups="keep")

agb_quants_species = agb_species %>% 
  dplyr::group_by(year, species_id) %>%
  dplyr::summarize(agb_mean = mean(agb, na.rm = TRUE), 
                   agb_median = median(agb, na.rm = TRUE), 
                   agb_lo = quantile(agb, c(0.025), na.rm = TRUE),
                   agb_hi = quantile(agb, c(0.975), na.rm = TRUE), .groups="keep")

if (update){
pdf('figures/agb_vs_year_by_species_update.pdf', width=10, height=6)
} else {
  pdf('figures/agb_vs_year_by_species.pdf', width=10, height=6)
}
p <- ggplot() +
  geom_ribbon(data=agb_quants_species, aes(x=year, ymin=agb_lo, ymax=agb_hi, group=species_id, colour=species_id, fill=species_id), alpha=0.5) +
  geom_line(data=agb_quants_species, aes(x=year, y=agb_median, group=species_id, colour=species_id)) +
  theme_bw(18) +
  xlab('year') +
  ylab('biomass (kg)') 
print(p)
# ggsave('figures/agb_vs_year_by_species.png', width=10, height=10, scale=3)
dev.off()

ggplot() +
  geom_ribbon(data=agb_quants_species, aes(x=year, ymin=agb_lo, ymax=agb_hi), fill='lightgrey') +
  geom_line(data=agb_quants_species, aes(x=year, y=agb_median)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass increment (kg)')  +
  facet_wrap(~species_id)

agb_all = agb_melt %>% 
  group_by(year, iter) %>%
  summarize(agb = sum(agb, na.rm = TRUE), .groups="keep")

agb_quants_all = agb_all %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(bio_mean = mean(agb, na.rm = TRUE), 
                   bio_median = median(agb, na.rm = TRUE), 
                   bio_lo = quantile(agb, c(0.025), na.rm = TRUE),
                   bio_hi = quantile(agb, c(0.975), na.rm = TRUE), .groups="keep")

ggplot() +
  geom_ribbon(data=agb_quants_all, aes(x=year, ymin=bio_lo, ymax=bio_hi), fill='lightgrey') +
  geom_line(data=agb_quants_all, aes(x=year, y=bio_median)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass (kg)') 
############################################################################################
# calculate biomass increment
############################################################################################

agbi_melt =  agb_melt %>% 
  group_by(stat_id, species_id, iter) %>%
  arrange(year, .by_group=TRUE) %>%
  mutate(agbi = agb - lag(agb))

agbi_melt = agbi_melt[which(!is.na(agbi_melt$agbi)),]
agbi_quants = agbi_melt %>% 
  dplyr::group_by(stat_id, year, species_id) %>%
  dplyr::summarize(agbi_mean = mean(agbi, na.rm = TRUE), 
                   agbi_median = median(agbi, na.rm = TRUE), 
                   agbi_lo = quantile(agbi, c(0.025), na.rm = TRUE),
                   agbi_hi = quantile(agbi, c(0.975), na.rm = TRUE), .groups="keep")


ggplot() +
  geom_ribbon(data=agbi_quants, aes(x=year, ymin=agbi_lo, ymax=agbi_hi, group=stat_id), fill='lightgrey') +
  geom_line(data=agbi_quants, aes(x=year, y=agbi_median, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass increment (kg)')

ggplot() +
  geom_ribbon(data=agbi_quants, aes(x=year, ymin=agbi_lo, ymax=agbi_hi, group=stat_id), fill='lightgrey') +
  geom_line(data=agbi_quants, aes(x=year, y=agbi_median, group=stat_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass increment (kg)') +
  facet_wrap(~species_id)

agbi_species = agbi_melt %>% 
  group_by(year, iter, species_id) %>%
  summarize(agbi = sum(agbi, na.rm = TRUE), .groups="keep")

agbi_quants_species = agbi_species %>% 
  dplyr::group_by(year, species_id) %>%
  dplyr::summarize(agbi_mean = mean(agbi, na.rm = TRUE), 
                   agbi_median = median(agbi, na.rm = TRUE), 
                   agbi_lo = quantile(agbi, c(0.025), na.rm = TRUE),
                   agbi_hi = quantile(agbi, c(0.975), na.rm = TRUE), .groups="keep")

if (update){
  pdf('figures/agbi_vs_year_by_species_update.pdf', width=10, height=6)
} else {
  pdf('figures/agbi_vs_year_by_species.pdf', width=10, height=6)
}
ggplot() +
  geom_ribbon(data=agbi_quants_species, aes(x=year, ymin=agbi_lo, ymax=agbi_hi, group=species_id, colour=species_id, fill=species_id), alpha=0.5) +
  geom_line(data=agbi_quants_species, aes(x=year, y=agbi_median, group=species_id, colour=species_id)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass increment (kg)')  +
  xlim(c(year_lo, year_hi)) 
dev.off()

if (update){
  pdf('figures/agbi_vs_year_by_species_update.pdf', width=10, height=6)
} else {
  pdf('figures/agbi_vs_year_by_species.pdf', width=10, height=6)
}
ggplot() +
  geom_ribbon(data=agbi_quants_species, aes(x=year, ymin=agbi_lo, ymax=agbi_hi, group=species_id, colour=species_id, fill=species_id), alpha=0.5) +
  geom_line(data=agbi_quants_species, aes(x=year, y=agbi_median, group=species_id, colour=species_id)) +
  theme_bw(18) +
  xlab('year') +
  ylab('biomass increment (kg)') +
  xlim(c(year_lo, year_hi)) 
dev.off()

if (update){
pdf('figures/agbi_vs_year_facet_species_update.pdf', width=10, height=6)
} else {
pdf('figures/agbi_vs_year_facet_species.pdf', width=10, height=6)
}
  p <- ggplot() +
  geom_ribbon(data=agbi_quants_species, aes(x=year, ymin=agbi_lo, ymax=agbi_hi), fill='lightgrey') +
  geom_line(data=agbi_quants_species, aes(x=year, y=agbi_median)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass increment (kg)')  +
  xlim(c(year_lo, year_hi)) +
  facet_wrap(~species_id) 
print(p)
dev.off()

agbi_all = agbi_melt %>% 
  group_by(year, iter) %>%
  summarize(agbi = sum(agbi, na.rm = TRUE), .groups="keep")

agbi_quants_all = agbi_all %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(agbi_mean = mean(agbi, na.rm = TRUE), 
                   agbi_median = median(agbi, na.rm = TRUE), 
                   agbi_lo = quantile(agbi, c(0.025), na.rm = TRUE),
                   agbi_hi = quantile(agbi, c(0.975), na.rm = TRUE), .groups="keep")

if (update){
pdf('figures/agbi_vs_year_overall_update.pdf', width=10, height=6)
} else {
  pdf('figures/agbi_vs_year_overall.pdf', width=10, height=6)
}
p <- ggplot() +
  geom_ribbon(data=agbi_quants_all, aes(x=year, ymin=agbi_lo, ymax=agbi_hi), fill='lightgrey') +
  geom_line(data=agbi_quants_all, aes(x=year, y=agbi_median)) +
  theme_bw(16) +
  xlab('year') +
  ylab('biomass increment (kg)') +
  xlim(c(year_lo, year_hi)) 
print(p)
dev.off()

# ggplot() +
#   geom_line(data=agbi_melt, aes(x=year, y=agbi, colour=stat_id, group=stat_id)) +
#   theme_bw(14) +
#   xlab('year') +
#   ylab('Biomass increment (kg)')
# 
# ggplot() +
#   geom_line(data=agbi_melt, aes(x=year, y=agbi, group=stat_id)) +
#   theme_bw(14) +
#   xlab('year') +
#   ylab('Biomass increment (kg)') +
#   facet_wrap(~species_id)

############################################################################################
# uncertainty
############################################################################################

agbi_quants_all$width = agbi_quants_all$agbi_hi - agbi_quants_all$agbi_lo

agbi_quants_all_sub = subset(agbi_quants_all, year %in% seq(year_lo, year_hi))

ggplot(data=agbi_quants_all_sub) +
  geom_histogram(aes(x=width)) +
  theme_bw(16)

ggplot(data=agbi_quants_all_sub) +
  geom_point(aes(x=year, y=width)) +
  theme_bw(16)



agbi_quants_species$width = agbi_quants_species$agbi_hi - agbi_quants_species$agbi_lo

agbi_quants_species_sub = subset(agbi_quants_species, year %in% seq(year_lo, year_hi))

ggplot(data=agbi_quants_species_sub) +
  geom_histogram(aes(x=width)) +
  theme_bw(16) +
  facet_wrap(~species_id)

