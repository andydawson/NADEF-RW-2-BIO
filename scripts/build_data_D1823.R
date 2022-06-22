library(ggplot2)

meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)

rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)

# subset the data to smaller region
ggplot(data=meta) + 
  geom_point(aes(x=x,y=y)) 

hi = 60
lo = 40
meta_sub = meta[which((meta$x<hi)&(meta$x>lo)&(meta$y<hi)&(meta$y>lo)),]
# subset the data to smaller region
ggplot(data=meta_sub) + 
  geom_point(aes(x=x,y=y)) 

meta_sub = meta_sub[which(meta_sub$dbh>10),]
# subset the data to smaller region
ggplot(data=meta_sub) + 
  geom_point(aes(x=x,y=y)) 

meta_sub = meta_sub[which(!is.na(meta_sub$dbh)),]
meta_sub = meta_sub[which(meta_sub$year %in% c(1994, 2004, 2011, 2019)),]

length(unique(meta_sub$stem_id))

rw_sub = rw[which(rw$stem_id %in% meta_sub$stem_id),]
length(unique(rw_sub$stem_id))

rw_stem_ids = unique(rw_sub$stem_id)

# meta_sub = meta_sub[which(meta_sub$stem_id %in% rw_stem_ids),]

stem_ids = unique(meta_sub$stem_id)
N_trees = length(stem_ids)
species_ids = unique(meta_sub$species_id)

stat_ids = seq(1, N_trees)
meta_sub$stat_id = match(meta_sub$stem_id, stem_ids)
meta_sub$species_code = match(meta_sub$species_id, species_ids)

year_lo = 1950
year_hi = 2019
years = seq(year_lo, year_hi)

N_years = length(years)
years_idx = seq(1, N_years)

N_cores = nrow(rw_sub)

d = meta_sub$dbh
d2tree = stat_ids[match(meta_sub$stem_id, stem_ids)]
d2year = match(meta_sub$year, years)
d2species = meta_sub$species_code
N_dbh = length(d)

rw_stat_id = stat_ids[match(rw_sub$stem_id, stem_ids)]
rw_sub = data.frame(stat_id = rw_stat_id, rw_sub)
rw_sub = rw_sub[order(rw_sub$stat_id),]
y = data.frame(matrix(NA, nrow=N_trees, ncol=ncol(rw_sub)))
y[match(rw_sub$stem_id, stem_ids),] = rw_sub

# rw_sub = rw_sub[match(rw_sub$stem_id, stem_ids),]
y = y[,which(substr(colnames(rw_sub), 2, 5) %in% years)]
if (year_hi>2013) {
  y = data.frame(y, matrix(-999, nrow=N_trees, ncol=year_hi-2013))
}

logy = log(y)
# idx_order = match(rw_sub$stem_id, stem_ids)
# logy = logy[idx_order,]
# core2tree = stat_ids[match(rw_sub$stem_id, stem_ids)]
core2tree = stat_ids
core2species = meta_sub$species_code[match(rw_sub$stat_id, meta_sub$stat_id)]

logy[is.na(logy)] = -999

rw_year_start = apply(logy, 1, which.min)
rw_year_end   = apply(logy, 1, which.max)

# int<lower=0> N_trees;
# int<lower=0> N_years;
# int<lower=0> N_species;
# int<lower=0> N_cores;
# 
# matrix[N_cores, N_years] y;
# int<lower=0> core2tree[N_cores];
# 
# real d[N_trees];
# int<lower=0> d2tree[N_trees];
# int<lower=0> d2year[N_trees];

N_trees
N_years
N_cores
N_dbh
y
logy
core2tree
d
d2tree
d2year

saveRDS(list(N_trees = N_trees, 
             N_years = N_years,
             # N_species
             N_cores = N_cores,
             N_dbh = N_dbh,
             y = y,
             logy = logy,
             rw_year_start = rw_year_start,
             rw_year_end = rw_year_end,
             core2tree = core2tree,
             d = d,
             d2tree = d2tree,
             d2year = d2year,
             sig_d_obs = 0.02
             ),
        file='data/D1823/D1823_input.RDS')

###############################################################################################################################

incr = rw_sub[,which(substr(colnames(rw_sub), 2, 5) %in% years)]
incr = data.frame(stem_id=rw_sub$stem_id, incr)

incr_data = melt(incr, id.vars=c('stem_id'))
colnames(incr_data) = c('stem_id', 'year', 'incr')
incr_data$plot   = 1#as.numeric(substr(incr_data$id, 3, 3))
# incr_data$stem_id = incr_data$stem_id 
# incr_data$stem_id     = as.numeric(substr(incr_data$stem_id, 2, 5))
incr_data$year = substr(incr_data$year, 2, 5)

tree_ids = as.numeric(unique(incr_data$stem_id))
N_trees = length(tree_ids)
stat_ids = seq(1, N_trees)

year_lo = 1950
year_hi = 2013
years = seq(year_lo, year_hi)
N_years = year_hi - year_lo + 1

incr_data$stat_id = stat_ids[match(incr_data$stem_id, tree_ids)]
# for (n in 1:nrow(incr_data)){
#   print(n)
#   incr_data$taxon[n] = as.vector(treeMeta$Species[which((as.numeric(substr(treeMeta$Site, 3, 3))==incr_data$plot[n])&
#                                                           (treeMeta$Tree.Number == incr_data$id[n]))])
# }

incr_data = incr_data[which(incr_data$year %in% years),]

# order by tree and year
incr_data = incr_data[order(incr_data$stat_id, incr_data$year),]
# incr_data = incr_data[which(!is.na(incr_data$incr)),]
incr_data$year = match(incr_data$year, years) #as.numeric(incr_data$year) - year_start + 1
N_inc   = nrow(incr_data) # number of measurements
m2t     = incr_data$year
m2tree  = incr_data$stat_id
m2plot  = incr_data$plot
# m2taxon = taxaMatch$number[match(incr_data$taxon, taxaMatch$species)]
Xobs    = incr_data$incr
Xobs[Xobs==0] = 0.0001
logXobs = log(Xobs)

year_idx = data.frame(year_start=as.numeric(aggregate(year~stat_id, data=incr_data, FUN=min, na.rm=TRUE)[,2]), year_end=as.numeric(aggregate(year~stat_id, incr_data, max)[,2]))
# year_idx[,2] = rep(N_years, nrow(year_idx))
# year_idx = year_idx - year_start + 1


# # make pdbh
# pdbh = aggregate(year~stat_id+plot+stem_id, incr_data, na.rm=TRUE)
# # pdbh = pdbh[which(pdbh$year %in% c(1994, 2004, 2011, 2019)),]
# pdbh = pdbh[order(pdbh$stat_id),]
# 
# for (n in 1:nrow(pdbh)){
#   pdbh$dbh[n] = treeMeta$dbh[which((as.numeric(substr(treeMeta$Site, 3, 3))==pdbh$plot[n])&
#                                      (treeMeta$Tree.Number == pdbh$id[n]))]
#   pdbh$distance[n] = treeMeta$Distance[which((as.numeric(substr(treeMeta$Site, 3, 3))==pdbh$plot[n])&
#                                                (treeMeta$Tree.Number == pdbh$id[n]))]
# }

pdbh = meta_sub[, c('stem_id', 'year', 'dbh')]
pdbh$stat_id = stat_ids[match(pdbh$stem_id, tree_ids)]

N_pdbh = nrow(pdbh)
logPDobs = log(pdbh$dbh)
pdbh_tree_id = pdbh$stat_id
# logPDobs[is.na(logPDobs)] = -999
pdbh$year = match(pdbh$year, years)
pdbh_year_id = pdbh$year
# distance = pdbh$distance

idx_stack = data.frame(meas=numeric(0), tree_id=numeric(0), year=numeric(0))
n = 1
for (tree in 1:N_trees){
  year = seq(year_idx[tree,1], year_idx[tree,2])
  meas = seq(n, n+length(year)-1)
  n = n + length(year)
  idx_stack = rbind(idx_stack, data.frame(meas=meas, tree_id=rep(tree, length(year)), year=year))
}

idx_tree = which(!duplicated(idx_stack$tree_id))
idx_tree = data.frame(idx_tree, c(idx_tree[-1]-1, nrow(idx_stack)))

x2tree  = idx_stack$tree_id
x2year  = idx_stack$year 

N_vals   = nrow(idx_stack)

meas2x = vector(length=N_inc)
for (i in 1:N_inc) {
  print(i)
  id = incr_data$stat_id[i]
  year = incr_data$year[i]
  
  meas2x[i] = which((idx_stack$tree_id == id) & (idx_stack$year == year))
}

# pdbh$year = rep(N_years, nrow(pdbh))

pdbh2val = vector(length=N_pdbh)
for (i in 1:N_pdbh){
  id = pdbh$stat_id[i]
  year = pdbh$year[i]
  
  print(i)
  which((idx_stack$tree_id == id) & (idx_stack$year == year))
  
  pdbh2val[i] = which((idx_stack$tree_id == id) & (idx_stack$year == year))
}

N_pdbh = nrow(pdbh)

site_dir <- file.path('sites',site)
if (!file.exists(site_dir)){
  dir.create(site_dir)
}
dir.create(file.path(site_dir,'data'))
dir.create(file.path(site_dir,'output'))
dir.create(file.path(site_dir, 'figures'))

saveRDS(list(N_trees=N_trees, 
             N_years=N_years,
             N_vals=N_vals,
             N_inc = N_inc,
             N_pdbh = N_pdbh,
             logXobs=logXobs, 
             logPDobs=logPDobs,
             year_idx=year_idx, 
             # N_taxa=N_taxa,
             pdbh_year=pdbh_year_id,
             idx_tree =idx_tree, 
             pdbh2val=pdbh2val,
             x2tree=x2tree,
             x2year=x2year,
             meas2x = meas2x, 
             # m2taxon = m2taxon,
             # taxon = taxon,
             # taxaMatch=taxaMatch,
             # plot_id = plot_id,
             years = years,
             m2tree = m2tree,
             m2t = m2t,
             sig_d_obs = 0.02),
        file='data/D1823/D1823_stacked_input.RDS')


# saveRDS(list(N_trees = N_trees, 
#              N_years = N_years,
#              # N_species
#              N_cores = N_cores,
#              N_dbh = N_dbh,
#              y = y,
#              logy = logy,
#              rw_year_start = rw_year_start,
#              rw_year_end = rw_year_end,
#              core2tree = core2tree,
#              d = d,
#              d2tree = d2tree,
#              d2year = d2year,
#              sig_d_obs = 0.02
# ),
# file='data/D1823/D1823_input.RDS')