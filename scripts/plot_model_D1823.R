library(ggplot2)
library(grid)
library(ggdogs)

if (update){
  dat = readRDS('data/D1823/D1823_input_update.RDS')
  post = readRDS('output/D1823_output_update.RDS')
} else {
  dat = readRDS('data/D1823/D1823_input.RDS')
  post = readRDS('output/D1823_output.RDS') 
}

names(post)
dim(post$x)

x_post = post$x
x_post[,1,1]

x = apply(x_post, c(2,3), mean)

list2env(dat, envir = globalenv())
list2env(post, envir = globalenv())

logy[logy==(-999)] = NA
y[y==(-999)] = NA

#######################################################################################################################################
#
#######################################################################################################################################

# plot data and model DBH for each tree
if (update) {
  pdf('figures/dbh_vs_year_estimated_update.pdf', width=10, height=6)
} else {
  pdf('figures/dbh_vs_year_estimated.pdf', width=10, height=6)
}
for (i in 1:N_trees){
  
  stem_id = core2stemids[i]
  species_id = species_ids[core2species[i]]

  d_iter = d_latent[, i, ]

  d_mean = apply(d_iter, 2, mean)
  d_quant = t(apply(d_iter, 2, function(x) quantile(x, c(0.025, 0.5, 0.975))))

  dbh_tree = data.frame(d_mean = d_mean, d_median = d_quant[,2], d_lo = d_quant[,1], d_hi = d_quant[,3], year = years)

  idx_d_obs = which(d2tree == i)

  dbh_obs = data.frame(d_obs = d[idx_d_obs],
                       year = years[d2year[idx_d_obs]])

  # Create a text
  grob <- grobTree(textGrob(paste0('Tree ', i, '; Stem ID ', stem_id, '; Species ', species_id ), x=0.1,  y=0.9, hjust=0,
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
  pdf('figures/rw_vs_year_estimated_update.pdf', width=10, height=6)
} else {
  pdf('figures/rw_vs_year_estimated.pdf', width=10, height=6)
}
for (i in 1:N_trees){

  x_iter = x[, i, ]

  x_mean = apply(x_iter, 2, mean)
  x_quant = t(apply(x_iter, 2, function(x) quantile(x, c(0.025, 0.5, 0.975))))

  rw_tree = data.frame(x_mean = x_mean, x_median = x_quant[,2], x_lo = x_quant[,1], x_hi = x_quant[,3], year = years)

  rw_obs = data.frame(x_obs = as.vector(t(y[i,])),
                       year = years)

  # Create a text
  grob <- grobTree(textGrob(paste0('Tree ', i), x=0.1,  y=0.9, hjust=0,
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
    theme_bw(16)  +
    # ggtitle(paste0('Tree ', i)) +
    annotation_custom(grob)

  print(p)

}
dev.off()

#######################################################################################################################################
# plot time effects
#######################################################################################################################################

beta_t = post$beta_t

beta_t_quant = data.frame(t(apply(beta_t, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
colnames(beta_t_quant) = c('lo', 'mid', 'hi')

beta_t_quant$year = years

ggplot(data=beta_t_quant) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=year, y=mid)) + 
  geom_linerange(aes(x=year, ymin=lo, ymax=hi)) +
  xlab('year') +
  ylab('beta_t') +
  theme_bw(16)
if (update) {
  ggsave('figures/time_effect_estimated_update.pdf')
} else {
  ggsave('figures/time_effect_estimated.pdf')
}

#######################################################################################################################################
# plot tree effects
#######################################################################################################################################

beta = post$beta

beta_quant = data.frame(t(apply(beta, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
colnames(beta_quant) = c('lo', 'mid', 'hi')
beta_quant$tree = seq(1, nrow(beta_quant))

ggplot(data=beta_quant) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=tree, y=mid)) + 
  geom_linerange(aes(x=tree, ymin=lo, ymax=hi)) +
  xlab('tree') +
  ylab('beta') +
  theme_bw(16)
if (update) {
  ggsave('figures/time_effect_estimated_update.pdf')
} else {
  ggsave('figures/time_effect_estimated.pdf')
}

