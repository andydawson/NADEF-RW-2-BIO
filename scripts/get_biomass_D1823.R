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

if (update){
dat  = readRDS('data/D1823/D1823_input_update.RDS')
post = readRDS('output/D1823_output_update.RDS')
} else {
  dat  = readRDS('data/D1823/D1823_input.RDS')
  post = readRDS('output/D1823_output.RDS') 
}

names(post)
dim(post$d_latent)

d_post = post$d_latent
d_post[,1,]
N_iter = dim(d_post)[1]

d_post_mean = apply(d_post, c(2,3), mean)
dim(d_post_mean)

list2env(dat, envir = globalenv())
list2env(post, envir = globalenv())

# species_table = data.frame(species_ids = species_ids,
#                            genus = c('Thuja', 'Abies', 'Populus', 'Betula', 'Picea'),
#                            species = c('occidentalis', 'balsamea', 'tremuloides', 'papyrifera', 'glauca'))
species_table = read.csv('data/species_table.csv', stringsAsFactors = FALSE)

# logy[logy==(-999)] = NA
# y[y==(-999)] = NA

# ############################################################################################
# # 
# ############################################################################################
# 
# ## example
# bio_test = get_biomass(dbh = c(20, 24, 27),
#                        genus = 'Picea',
#                        species = 'glauca',
#                        coords = coords_D1823)
# 
# ## example
# biomass = array(NA, c(N_trees, N_years))
# 
# for (tree in 1:N_trees){
#   
#   tree_genus   = species_table[core2species[tree],'genus']
#   tree_species = species_table[core2species[tree],'species']
#   
#   biomass[tree,] = get_biomass(dbh = d_post_mean[tree,],
#                                genus = tree_genus,
#                                species = tree_species,
#                                coords = coords_D1823)
# }
# 
# ############################################################################################
# # 
# ############################################################################################
# 
# bio_df = data.frame(stat_id = core2tree, 
#                     species_id = species_ids[core2species], 
#                     biomass)
# 
# bio_melt = melt(bio_df, id.vars = c('stat_id', 'species_id'))
# bio_melt$year = years[as.numeric(substr(bio_melt$variable, 2, 5))]
# 
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
#             
# ############################################################################################
# # calculate biomass increment
# ############################################################################################
# 
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


############################################################################################
# 
############################################################################################

biomass_iter = array(NA, c(N_iter/2, N_trees, N_years))
biomass_iter = array(NA, c(50, N_trees, N_years))

for (iter in 1:50){#N_iter){
  print(iter)
  for (tree in 1:N_trees){
    print(tree)
    
    tree_genus   = species_table[d2species[tree],'genus']
    tree_species = species_table[d2species[tree],'species']
    
    biomass_iter[iter, tree,] = get_biomass(dbh = d_post[iter, tree,],
                                       genus = tree_genus,
                                       species = tree_species,
                                       coords = coords_D1823)
  }
}

biomass_iter

############################################################################################
# 
############################################################################################

# bio_df = data.frame(stat_id = core2tree, 
#                     species_id = species_ids[core2species], 
#                     biomass)

agb_melt = melt(biomass_iter)#, id.vars = c('stat_id', 'species_id'))
colnames(agb_melt) = c('iter', 'stat_id', 'year', 'agb')
agb_melt$year = years[agb_melt$year]
agb_melt$species_id = species_ids[core2species[agb_melt$stat_id]]

# bio_mean = apply(biomass_iter, c(1,2), mean)
# bio_quant = t(apply(biomass_iter, 2, function(x) quantile(x, c(0.025, 0.5, 0.975))))
# 
# bio_tree = data.frame(bio_mean = bio_mean, 
#                       bio_median = bio_quant[,2], 
#                       bio_lo = bio_quant[,1], 
#                       bio_hi = bio_quant[,3], 
#                       year = years)
library(dplyr)
agb_quants = agb_melt %>% 
  dplyr::group_by(stat_id, year, species_id) %>%
  dplyr::summarize(agb_mean = mean(agb), 
            agb_median = median(agb), 
            agb_lo = quantile(agb, c(0.025)),
            agb_hi = quantile(agb, c(0.975)), .groups="keep")


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
  facet_wrap(~species_id)
print(p)
dev.off()

agb_species = agb_melt %>% 
  group_by(year, iter, species_id) %>%
  summarize(agb = sum(agb), .groups="keep")

agb_quants_species = agb_species %>% 
  dplyr::group_by(year, species_id) %>%
  dplyr::summarize(agb_mean = mean(agb), 
                   agb_median = median(agb), 
                   agb_lo = quantile(agb, c(0.025)),
                   agb_hi = quantile(agb, c(0.975)), .groups="keep")

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
  summarize(agb = sum(agb), .groups="keep")

agb_quants_all = agb_all %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(bio_mean = mean(agb), 
                   bio_median = median(agb), 
                   bio_lo = quantile(agb, c(0.025)),
                   bio_hi = quantile(agb, c(0.975)), .groups="keep")

  
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
  dplyr::summarize(agbi_mean = mean(agbi), 
                   agbi_median = median(agbi), 
                   agbi_lo = quantile(agbi, c(0.025)),
                   agbi_hi = quantile(agbi, c(0.975)), .groups="keep")


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
  summarize(agbi = sum(agbi), .groups="keep")

agbi_quants_species = agbi_species %>% 
  dplyr::group_by(year, species_id) %>%
  dplyr::summarize(agbi_mean = mean(agbi), 
                   agbi_median = median(agbi), 
                   agbi_lo = quantile(agbi, c(0.025)),
                   agbi_hi = quantile(agbi, c(0.975)), .groups="keep")

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
  ylab('biomass increment (kg)') 
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
  facet_wrap(~species_id)
print(p)
dev.off()

agbi_all = agbi_melt %>% 
  group_by(year, iter) %>%
  summarize(agbi = sum(agbi), .groups="keep")

agbi_quants_all = agbi_all %>% 
  dplyr::group_by(year) %>%
  dplyr::summarize(agbi_mean = mean(agbi), 
                   agbi_median = median(agbi), 
                   agbi_lo = quantile(agbi, c(0.025)),
                   agbi_hi = quantile(agbi, c(0.975)), .groups="keep")

if (update){
pdf('figures/agbi_vs_year_overall_update.pdf', width=10, height=6)
} else {
  pdf('figures/agbi_vs_year_overall.pdf', width=10, height=6)
}
p <- ggplot() +
  geom_ribbon(data=agbi_quants_all, aes(x=year, ymin=agbi_lo, ymax=agbi_hi), fill='lightgrey') +
  geom_line(data=agbi_quants_all, aes(x=year, y=agbi_median)) +
  theme_bw(14) +
  xlab('year') +
  ylab('biomass increment (kg)') 
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
