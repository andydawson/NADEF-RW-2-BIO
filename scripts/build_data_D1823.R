library(ggplot2)

#test 2

# for original data: update = FALSE
# for updated data: update = TRUE
update = TRUE
data_name = 'interval'

# meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)
# rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)
meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)
rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)

if (update) {
  meta = read.csv('data/D1823/D1823_meta_update.csv', stringsAsFactors = FALSE)
  # meta$dbh[which(meta$year == 2021)] = meta[which(meta$year == 2021),'dbh']/10
  
  rw = read.csv('data/D1823/D1823_rw_update.csv', stringsAsFactors = FALSE)
}


# meta = meta[which(meta$status_id %in% c('A', 'AS', 'AL')

# subset the data to smaller region
ggplot(data=meta) + 
  geom_point(aes(x=x,y=y)) 

# hi = 100
# lo = 0
hi = 70
lo = 30
meta_sub = meta[which((meta$x<hi)&(meta$x>lo)&(meta$y<hi)&(meta$y>lo)),]
# subset the data to smaller region
ggplot(data=meta_sub) + 
  geom_point(aes(x=x,y=y)) 

meta_sub = meta_sub[which(meta_sub$dbh>10),]
# subset the data to smaller region
ggplot(data=meta_sub) + 
  geom_point(aes(x=x,y=y)) 

meta_sub = meta_sub[which(!is.na(meta_sub$dbh)),]
meta_sub = meta_sub[which(meta_sub$stem_id != ""),]
# meta_sub = meta_sub[which(meta_sub$year %in% c(1994, 2004, 2011, 2019)),]

length(unique(meta_sub$stem_id))

rw_sub = rw[which(rw$stem_id %in% meta_sub$stem_id),]
length(unique(rw_sub$stem_id))

rw_stem_ids = unique(rw_sub$stem_id)

# meta_sub = meta_sub[which(meta_sub$stem_id %in% rw_stem_ids),]

stem_ids = unique(meta_sub$stem_id)
N_trees = length(stem_ids)
species_ids = unique(meta_sub$species_id)
N_species = length(species_ids)

stat_ids = seq(1, N_trees)
meta_sub$stat_id = match(meta_sub$stem_id, stem_ids)
meta_sub$species_code = match(meta_sub$species_id, species_ids)

# rw_sub[,4:ncol(rw_sub)]
# 
# rw_first_idx = apply(rw_sub[,4:ncol(rw_sub)], 1, function(x) which(!is.na(x))[1])
# rw_years = as.numeric(substr(colnames(rw_sub)[4:ncol(rw_sub)], 2, 5))
# year_lo = rw_years[min(rw_first_idx)]

year_lo = 1900

year_hi = 2019
if (update){
  year_hi = 2021
}
years = seq(year_lo, year_hi)

meta_sub = meta_sub[which(meta_sub$year %in% years),]

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

rw_year_max = max(as.numeric(substr(colnames(rw_sub), 2, 5)[4:ncol(rw_sub)]))

y = data.frame(matrix(NA, nrow=N_trees, ncol=ncol(rw_sub)))
# y[match(rw_sub$stem_id, stem_ids),] = rw_sub
y[rw_sub$stat_id,] = rw_sub


# meta_sub$species_code[match(rw_sub$stat_id, meta_sub$stat_id)]

# rw_sub = rw_sub[match(rw_sub$stem_id, stem_ids),]
y = y[,which(substr(colnames(rw_sub), 2, 5) %in% years)]
if (!update){
  if (year_hi>2013) {
    y = data.frame(y, matrix(-999, nrow=N_trees, ncol=year_hi-2013))
  }
} 
if ((update)&(year_hi>2013)&(rw_year_max<year_hi)){
  y = data.frame(y, matrix(-999, nrow=N_trees, ncol=year_hi-2013))
}
# if (update & max())

# y[which(y==0)] = NA

logy = log(y)
# idx_order = match(rw_sub$stem_id, stem_ids)
# logy = logy[idx_order,]
# core2tree = stat_ids[match(rw_sub$stem_id, stem_ids)]
core2tree = stat_ids
core2species = meta_sub$species_code[match(core2tree, meta_sub$stat_id)]
core2stemids = stem_ids

# rw_year_start = rep(which(years==year_lo), N_trees)
# rw_year_end   = rep(which(years==year_hi), N_trees)
rw_year_start = rep(NA, N_trees)
rw_year_end   = rep(NA, N_trees)
for (i in 1:N_trees){

  rw_tree = logy[i,]

  if (any(!is.na(rw_tree))){
    rw_tree_years = which(!is.na(rw_tree))

    rw_year_start[i] = min(rw_tree_years)
    rw_year_end[i] = max(rw_tree_years)

    d_idx = which(d2tree == i)
    d_year_start = min(d2year[d_idx])
    d_year_end   = max(d2year[d_idx])

    if (d_year_start < rw_year_start[i]){
      rw_year_start[i] = d_year_start #- 10?
    }

    if (d_year_end < rw_year_end[i]){
      rw_year_end[i] = d_year_end #- 10?
    }

  } else {
    d_idx = which(d2tree == i)
    rw_year_start[i] = min(d2year[d_idx]) - 50
    rw_year_end[i] = max(d2year[d_idx])
  }

  if ((rw_year_end[i] == 120)|(d_year_end == 120)){
    rw_year_end[i] == 122
  }
}



# rw_year_start = apply(logy, 1, which.min)
# sapply(rw_year_start, function(x) if(x==0){x=1})
# 
# rw_year_end   = apply(logy, 1, which.max)

logy[is.na(logy)] = as.integer(-999)

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
core2species
d
d2tree
d2year

head(logy)

if (update) {
  fname = paste0('data/D1823/D1823_input_update_', data_name, '.RDS')
} else {
  fname = paste0('data/D1823/D1823_input_', data_name, '.RDS')
}

saveRDS(list(N_trees = N_trees, 
             N_years = N_years,
             N_species = N_species,
             N_cores = N_cores,
             N_dbh = N_dbh,
             y = y,
             logy = logy,
             rw_year_start = rw_year_start,
             rw_year_end = rw_year_end,
             core2tree = core2tree,
             core2species = core2species,
             core2stemids = core2stemids,
             d = d,
             d2tree = d2tree,
             d2year = d2year,
             d2species = d2species,
             sig_d_obs = 0.02,
             species_ids = species_ids,
             years = years
),
file=fname)

