library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

species_table = data.frame(species_raw = c('Ab', 'Pg', 'To', 'Pt'),
                           species_id = c('ABA', 'PGL', 'TOC', 'PTR'))


################################################################################
## read in raw census data
################################################################################

dat = read.csv('data/D1823/D1823_census_raw.csv', stringsAsFactors = FALSE)

colnames(dat)[1:4] = c('parcel', 'species_id', 'stem_id', 'ancient_id')

var_names = c('dbh', 'status', 'canopy_class')
var_names2 = c('dbh', 'status', 'canopy_class', 'greenness', 'decomp')
var_names3 = c('dbh', 'status', 'canopy_class', 'greenness', 'decomp', 'damage')

colnames(dat)[5:7] = c(paste0('1994_', var_names))
colnames(dat)[8:12] = c(paste0('2004_', var_names2))
colnames(dat)[13:18] = c(paste0('2011_', var_names3))
colnames(dat)[19:24] = c(paste0('2019_', var_names3))
colnames(dat)[25:26] = c('x', 'y') 

dat = dat[-1,]

################################################################################
##
################################################################################

dat_melt = melt(dat, id.vars=c('parcel', 'species_id', 'stem_id', 'ancient_id', 'x', 'y'))
dat_melt$year = as.numeric(substr(dat_melt$variable, 1, 4))

dat_melt$variable = as.character(dat_melt$variable)
dat_melt$variable = substr(dat_melt$variable, 6, 17)

dat_wide = dat_melt %>%
  group_by(parcel, species_id, stem_id, ancient_id, x, y, year) %>%
  pivot_wider(names_from=variable, values_from=value)

dat_wide = data.frame(dat_wide)
# dat_wide$stem_id = as.numeric(dat_wide$stem_id)
dat_wide$dbh = as.numeric(dat_wide$dbh)

dat_wide$species_id = toupper(dat_wide$species_id)

dat_wide$plot_id = 'D1823'
dat_wide = dat_wide[,c('plot_id', 'stem_id', 'ancient_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status')]
colnames(dat_wide)[which(dat_wide =='status')] = 'status_id'

dat_wide = dat_wide[which(!is.na(dat_wide$dbh)),]

write.csv(dat_wide, 'data/D1823/D1823_meta.csv', row.names=FALSE)

################################################################################
##
################################################################################

## aussenac 2017
## long core data

# read in ????
fnames = list.files('data/auss-2017/06_TRD/D1823/')

rw_long = data.frame(species_id=character(0),
                     year=numeric(0),
                     long_id=character(0),
                     value=numeric(0))
for (i in 1:length(fnames)){
  rwl_dat = read.table(paste0('data/auss-2017/06_TRD/D1823/', fnames[i]))
  rwl_dat = data.frame(species_id=substr(fnames[i],1,2), year = rownames(rwl_dat), rwl_dat)
  rwl_dat_melt = melt(rwl_dat, id.vars = c('species_id', 'year'))
  colnames(rwl_dat_melt) = c('species_id', 'year', 'long_id', 'value')
  
  rwl_dat_melt$year = as.numeric(rwl_dat_melt$year)
  rw_long = rbind(rw_long, rwl_dat_melt)
}

rw_long$species_id = species_table[match(rw_long$species_id, species_table$species_raw), 'species_id']
rw_long$long_id = substr(rw_long$long_id, 3, 8)

length(unique(rw_long$long_id))

trees_long = unique(rw_long$long_id)
n_trees_long = length(unique(trees_long))


dbh_long = read.csv('data/auss-2017/05_DBH/dbhD1823.csv', stringsAsFactors = FALSE)[,2:3]
colnames(dbh_long) = c('long_id', 'dbh')

dbh_long$species_id = substr(dbh_long$long_id, 1, 2)
dbh_long$species_id = species_table[match(dbh_long$species_id, species_table$species_raw), 'species_id']

# want to add a column to rw_long to map to census data stem_id
# why only 173 unique long_ids now?
length(unique(dbh_long$long_id))

length(unique(rw_long$long_id))

long_id_missing = unique(rw_long[which(!(rw_long$long_id %in% dbh_long$long_id)), 'long_id'])
length(long_id_missing)

all_long = merge(rw_long, dbh_long, all.x=TRUE)

length(unique(all_long$long_id))

# rw_dat_melt

head(all_long)

# n_trees = length(unique(rw_dat_melt$short_id))
trees_long = unique(all_long$long_id)
n_trees_long = length(unique(trees_long))


all_long$stem_id = substr(all_long$long_id, 3, 6)

all_long$plot_id = 'D1823'

all_long$x = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('x')])
all_long$y = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('y')])
all_long$status_id = dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('status')]


df_match_id = all_long[,c('plot_id', 'stem_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status_id')]

df_match_id$dbh = df_match_id$dbh/10

df_match_id = df_match_id[which(!is.na(df_match_id$dbh)),]

rw_dat_wide = rw_long %>% 
  group_by(species_id, long_id) %>% 
  arrange(year) %>% 
  pivot_wider(names_from=year, values_from=value)

rw_dat = data.frame(rw_dat_wide)  

rw_dat = data.frame(stem_id = substr(rw_dat$long_id, 3, 6),
                    rw_dat)
rw_dat$species_id = species_table[match(rw_dat$species_id, species_table$species_raw), 'species_id']

rw_dat = rw_dat[,c('stem_id', 'species_id', colnames(rw_dat)[4:ncol(rw_dat)])]
  
colnames(rw_dat)[3:ncol(rw_dat)] = substr(colnames(rw_dat)[3:ncol(rw_dat)], 2,5)


write.csv(rw_dat, 'data/D1823/D1823_rw.csv', row.names=FALSE)


