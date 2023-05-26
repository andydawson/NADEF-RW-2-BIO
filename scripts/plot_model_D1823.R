library(ggplot2)
library(grid)
library(reshape2)
library(dplyr)
library(tidyr)
# library(ggdogs)

update = TRUE
interval_cut = TRUE

model = 'species_time'
# model = 'species_time_interval'
model = 'species_time_negd'

if (update){
  # dat = readRDS('data/D1823/D1823_input_update.RDS')
  dat = readRDS('data/D1823/D1823_input_update_interval.RDS')
  # post = readRDS('output/D1823_output_update.RDS')
  post = readRDS(paste0('output/D1823_output_update_', model, '.RDS'))
} else {
  # dat = readRDS('data/D1823/D1823_input.RDS')
  # post = readRDS('output/D1823_output.RDS') 
}

list2env(dat, envir = globalenv())
list2env(post, envir = globalenv())

#create dataframe of min, mean, and median, years when dbh went positive
d_min = apply(post$d_latent, c(2,3), min)

# for each tree, for each year, all d_latent are positive
min_year_negd = apply(d_min, 1, function(x) min(which(x>0)))

#create dataframe of min, mean, and median, years when dbh went positive
d_max = apply(post$d_latent, c(2,3), max)

# for each tree, for each year, all d_latent are positive
max_year_negd = apply(d_max, 1, function(x) min(which(x>0)))

#create dataframe of min, mean, and median, years when dbh went positive
d_mean = apply(post$d_latent, c(2,3), mean)

# for each tree, for each year, all d_latent are positive
mean_year_negd = apply(d_mean, 1, function(x) min(which(x>0)))

#create dataframe of min, mean, and median, years when dbh went positive
d_median = apply(post$d_latent, c(2,3), median)

# for each tree, for each year, all d_latent are positive
median_year_negd = apply(d_median, 1, function(x) min(which(x>0)))


summary_negd = data.frame(first_posd = max_year_negd, 
                          post_mean_posd = mean_year_negd, 
                          post_median_posd = median_year_negd, 
                          all_posd = min_year_negd)
summary_negd$first_posd = years[summary_negd$first_posd]
summary_negd$post_mean_posd = years[summary_negd$post_mean_posd]
summary_negd$post_median_posd = years[summary_negd$post_median_posd]
summary_negd$all_posd = years[summary_negd$all_posd]

# summary_negd = t(apply(summary_negd, 1, function(x) years[x]))

# core2stemids

summary_negd <- data.frame(stem_ids = core2stemids, summary_negd)

write.csv(summary_negd, 'data/negative_diameter_year.csv')

summary_negd_melt = melt(summary_negd, id.vars='stem_ids')

ggplot(data=summary_negd_melt) +
  geom_histogram(aes(x=value, y=after_stat(density))) +
  facet_wrap(~variable) + 
  theme_bw(16)




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
    
    if (year_end_tree != max(years_idx)){
      
      # post$x[,i,(year_end_tree+1):N_years] = NA
      # post$d_latent[,i,(year_end_tree+1):N_years] = NA 
      
      x[,i,(year_end_tree+1):N_years] = NA
      d_latent[,i,(year_end_tree+1):N_years] = NA 
      
    }
  }
}

names(post)
dim(post$x)

x_post = post$x
x_post[,1,1]

# x = apply(x_post, c(2,3), mean)

logy[logy==(-999)] = NA
y[y==(-999)] = NA

year_lo = min(years)
year_hi = max(years)

#######################################################################################################################################
#
#######################################################################################################################################

# which(!is.na(y[,122]))

# plot data and model DBH for each tree
if (update) {
  pdf(paste0('figures/dbh_vs_year_estimated_update_', model, '.pdf'), width=10, height=6)
} else {
  pdf(paste0('figures/dbh_vs_year_estimated_', model, '.pdf'), width=10, height=6)
}
for (i in 1:N_trees){
  
  print(i)
  
  stem_id = core2stemids[i]
  species_id = species_ids[core2species[i]]
  
  year_start = rw_year_start[i]
  year_end = rw_year_end[i]

  d_iter = d_latent[, i, ]

  d_mean = apply(d_iter, 2, mean, na.rm=TRUE)
  d_quant = t(apply(d_iter, 2, function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))

  dbh_tree = data.frame(d_mean = d_mean, 
                        d_median = d_quant[,2], 
                        d_lo = d_quant[,1], 
                        d_hi = d_quant[,3], 
                        year = years)

  idx_d_obs = which(d2tree == i)

  dbh_obs = data.frame(d_obs = d[idx_d_obs],
                       year = years[d2year[idx_d_obs]])

  # Create a text
  grob <- grobTree(textGrob(paste0('Tree ', i, '; Stem ID ', stem_id, '; Species ', species_id ), x=0.05,  y=0.9, hjust=0,
                            gp=gpar(col="black", fontsize=22)))

  p <- ggplot() +
    # geom_line(data=dbh_tree, aes(x=year, y=d_mean)) +
    geom_ribbon(data=dbh_tree, aes(x=year, ymin=d_lo, ymax=d_hi), fill='lightgrey') +
    geom_line(data=dbh_tree, aes(x=year, y=d_median)) +
    geom_point(data=dbh_obs, aes(x=year, y=d_obs), size=2) +
    # geom_dog(data=dbh_obs, aes(x=year, y=d_obs, dog='glasses'), size=2) +
    # ylim(c(0,500)) +
    xlab('year') +
    ylab('dbh (cm)') +
    xlim(c(year_lo, year_hi)) +
    theme_bw(16)  +
    # ggtitle(paste0('Tree ', i)) +
    annotation_custom(grob)

  print(p)

}
dev.off()
#######################################################################################################################################
#
#######################################################################################################################################

# plot data and model DBH for each tree
if (update) {
  pdf(paste0('figures/rw_vs_year_estimated_update_', model, '.pdf'), width=10, height=6)
} else {
  pdf(paste0('figures/rw_vs_year_estimated_', model, '.pdf'), width=10, height=6)
}
for (i in 1:N_trees){
  
  stem_id = core2stemids[i]
  species_id = species_ids[core2species[i]]

  x_iter = x[, i, ]

  x_mean = apply(x_iter, 2, mean)
  x_quant = t(apply(x_iter, 2, function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))

  rw_tree = data.frame(x_mean = x_mean, x_median = x_quant[,2], x_lo = x_quant[,1], x_hi = x_quant[,3], year = years)

  rw_obs = data.frame(x_obs = as.vector(t(y[i,])),
                       year = years)

  # Create a text
  grob <- grobTree(textGrob(paste0('Tree ', i, '; Stem ID ', stem_id, '; Species ', species_id ), x=0.05,  y=0.9, hjust=0,
                            gp=gpar(col="black", fontsize=22)))

  p <- ggplot() +
    # geom_line(data=dbh_tree, aes(x=year, y=d_mean)) +
    geom_ribbon(data=rw_tree, aes(x=year, ymin=x_lo, ymax=x_hi), fill='lightgrey') +
    geom_line(data=rw_tree, aes(x=year, y=x_median)) +
    geom_point(data=rw_obs, aes(x=year, y=x_obs), size=2) +
    # geom_dog(data=rw_obs, aes(x=year, y=x_obs, dog='glasses'), size=2) +
    # ylim(c(0,500)) +
    xlab('year') +
    ylab('rw (mm)') +
    xlim(c(year_lo, year_hi)) +
    theme_bw(16)  +
    # ggtitle(paste0('Tree ', i)) +
    annotation_custom(grob)

  print(p)

}
dev.off()

# #######################################################################################################################################
# # plot time effects
# #######################################################################################################################################
# 
# beta_t = post$beta_t
# 
# beta_t_quant = data.frame(t(apply(beta_t, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
# colnames(beta_t_quant) = c('lo', 'mid', 'hi')
# 
# beta_t_quant$year = years
# 
# ggplot(data=beta_t_quant) +
#   geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
#   geom_point(aes(x=year, y=mid)) + 
#   geom_linerange(aes(x=year, ymin=lo, ymax=hi)) +
#   xlab('year') +
#   ylab('beta_t') +
#   theme_bw(16)
# if (update) {
#   ggsave('figures/time_effect_estimated_update.pdf')
# } else {
#   ggsave('figures/time_effect_estimated.pdf')
# }

#######################################################################################################################################
# plot time effects
#######################################################################################################################################

beta_t = post$beta_t

foo = apply(beta_t, c(2,3), function(x) quantile(x, c(0.025, 0.5, 0.975)))
bar = melt(foo)
beta_t_quant = bar %>% pivot_wider(names_from = Var1, values_from = value)
colnames(beta_t_quant) = c('year', 'species_id', 'lo', 'mid', 'hi')

beta_t_quant$year = years[beta_t_quant$year]
beta_t_quant$species_id = species_ids[beta_t_quant$species_id]

# beta_t_quant = data.frame(t(apply(beta_t, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
# colnames(beta_t_quant) = c('lo', 'mid', 'hi')

# beta_t_quant$year = years

ggplot(data=beta_t_quant) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=year, y=mid)) + 
  geom_linerange(aes(x=year, ymin=lo, ymax=hi)) +
  xlab('year') +
  ylab('beta_t') +
  xlim(c(year_lo, year_hi)) +
  theme_bw(16) +
  facet_grid(species_id~.)
if (update) {
  ggsave('figures/time_species_effect_estimated_update.pdf')
} else {
  ggsave('figures/time_species_effect_estimated.pdf')
}

ggplot(data=beta_t_quant) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=year, y=mid)) + 
  geom_linerange(aes(x=year, ymin=lo, ymax=hi)) +
  xlab('year') +
  ylab('beta_t') +
  xlim(c(year_lo, year_hi)) +
  theme_bw(16) +
  facet_grid(species_id~.) 
if (update) {
  ggsave('figures/time_species_effect_estimated_update.pdf', width=10, height=8)
} else {
  ggsave('figures/time_species_effect_estimated.pdf', width=10, height=8)
}

# #######################################################################################################################################
# # plot time effects
# #######################################################################################################################################
# 
# if (!is.null(post$beta_k)){
# 
# beta_k = post$beta_k
# 
# beta_k_quant = data.frame(t(apply(beta_k, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
# colnames(beta_k_quant) = c('lo', 'mid', 'hi')
# 
# beta_k_quant$species_id = species_ids
# 
# ggplot(data=beta_k_quant) +
#   geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
#   geom_point(aes(x=species_id, y=mid)) + 
#   geom_linerange(aes(x=species_id, ymin=lo, ymax=hi)) +
#   xlab('species') +
#   ylab('beta_k') +
#   theme_bw(16)
# if (update) {
#   ggsave('figures/species_effect_estimated_update.pdf')
# } else {
#   ggsave('figures/species_effect_estimated.pdf')
# }
# 
# }

#######################################################################################################################################
# plot tree effect overall mean
#######################################################################################################################################

beta0 = post$beta0

beta0_quant = quantile(beta0, c(0.025, 0.5, 0.975))
names(beta0_quant) = c('lo', 'mid', 'hi')
beta0_quant = data.frame(tree=1, t(beta0_quant))

ggplot(data=beta0_quant) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=tree, y=mid)) + 
  geom_linerange(aes(x=tree, ymin=lo, ymax=hi)) +
  xlab('tree') +
  ylab('beta') +
  theme_bw(16)
if (update) {
  ggsave('figures/individual_effect_estimated_update.pdf')
} else {
  ggsave('figures/individual_effect_estimated.pdf')
}

#######################################################################################################################################
# plot tree effects
#######################################################################################################################################

beta = post$beta

beta_quant = data.frame(t(apply(beta, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
colnames(beta_quant) = c('lo', 'mid', 'hi')
beta_quant$tree = seq(1, nrow(beta_quant))
beta_quant$species = species_ids[core2species[beta_quant$tree]]

# ggplot(data=beta_quant) +
#   geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
#   geom_point(aes(x=tree, y=mid)) + 
#   geom_linerange(aes(x=tree, ymin=lo, ymax=hi)) +
#   xlab('tree') +
#   ylab('beta') +
#   theme_bw(16)

# beta_quant = beta_quant[order(beta_quant$species),]
# beta_quant$species = factor(beta_quant$species)

ggplot(data=beta_quant) +
  geom_hline(aes(yintercept=beta0_quant$mid), lty=2, lwd=1.2) +
  geom_point(aes(x=tree, y=mid, colour=species)) + 
  geom_linerange(aes(x=tree, ymin=lo, ymax=hi, colour=species)) +
  xlab('tree') +
  ylab('beta') +
  theme_bw(16) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
  

ggplot(data=beta_quant) +
  geom_hline(aes(yintercept=beta0_quant$mid), lty=2, lwd=1.2) +
  geom_point(aes(x=tree, y=mid, colour=species)) + 
  geom_linerange(aes(x=tree, ymin=lo, ymax=hi, colour=species)) +
  xlab('tree') +
  ylab('beta') +
  theme_bw(16) +
  coord_flip()

# # beta_quant$tree = factor(beta_quant$tree)
beta_quant = beta_quant %>% group_by(species) %>% arrange(mid, .by_group = TRUE)
beta_quant$tree = factor(beta_quant$tree, levels = beta_quant$tree)

ggplot(data=beta_quant) +
  geom_hline(aes(yintercept=beta0_quant$mid), lty=2, lwd=1.2) +
  geom_point(aes(x=tree, y=mid, colour=species)) + 
  geom_linerange(aes(x=tree, ymin=lo, ymax=hi, colour=species)) +
  xlab('tree') +
  ylab('beta') +
  theme_bw(16) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  facet_wrap(~species, scales="free_x")


# ggplot(data=beta_quant) +
#   geom_density(aes(x=beta0_quant$mid), lty=2, lwd=1.2) +
#   geom_point(aes(x=tree, y=mid, colour=species)) + 
#   geom_linerange(aes(x=tree, ymin=lo, ymax=hi, colour=species)) +
#   xlab('tree') +
#   ylab('beta') +
#   theme_bw(16) +
#   facet_wrap(~species, scales="free_x")

if (update) {
  ggsave('figures/individual_effect_estimated_update.pdf')
} else {
  ggsave('figures/individual_effect_estimated.pdf')
}

