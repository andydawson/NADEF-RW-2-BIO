library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

species_table = data.frame(species_raw = c('Ab', 'Pg', 'To', 'Pt'),
                           species_id = c('ABA', 'PGL', 'TOC', 'PTR'))


## raw data

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

species_table = data.frame(species_raw = c('Ab', 'Pg', 'To', 'Pt'),
                           species_id = c('ABA', 'PGL', 'TOC', 'PTR'))
# 
# dat_wide$species_id = species_table[match(dat_wide$species_id, species_table$species_raw),'species_id']

dat_wide$plot_id = 'D1823'
dat_wide = dat_wide[,c('plot_id', 'stem_id', 'ancient_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status')]
colnames(dat_wide)[which(dat_wide =='status')] = 'status_id'

dat_wide = dat_wide[which(!is.na(dat_wide$dbh)),]

write.csv(dat_wide, 'data/D1823/D1823_meta.csv', row.names=FALSE)
## maleki data

# # read in stem meta data
# mal_stem = read.csv('data/maleki-2021/hectare_stems.csv')
# 
# # read in repeat census data
# mal_census = read.csv('data/maleki-2021/hectare_stem_meas.csv')
# 
# # merge coords a measurements into one data frame
# # want something like: PLOT ID, STEM ID, SPECIES, X, Y, YEAR, DBH
# mal_all = merge(mal_census, mal_stem)
# mal_all = mal_all[,c('plot_id', 'stem_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status_id')]
# 
# # save data frame
# write.csv(mal_all, 'data/maleki-2021/maleki_all.csv', row.names=FALSE)
# 
# unique(mal_all$plot_id)
# 
# mal_all = mal_all[which(mal_all$plot_id %in% 'H1823'),]

# detach(package:plyr)
# detach(package:dplyr)
# detach(package:ggplot2)

library(dplyr)
# # mal_dbh2011 = mal_all %>% 
# #   group_by(plot_id, stem_id, species_id, x, y) %>% 
# #   dplyr::summarize(dbh2011=max(dbh, na.rm=TRUE), .groups='keep')
# 
# mal_dbh2011 = mal_all[which(mal_all$year == 2011), c('plot_id', 'stem_id', 'dbh')]
# colnames(mal_dbh2011) = c('plot_id', 'stem_id', 'dbh2011')
# 
# mal_dbh2011$dbh2011 = mal_dbh2011$dbh2011*10
# 
# # foo = merge(mal_all, mal_dbh2011)
# 
# mal_all <-  dplyr::left_join(x = mal_all, y = mal_dbh2011, by = c("plot_id", "stem_id"))
# 
# 
# length(unique(mal_all$stem_id))
# 
# ggplot() +
#   geom_point(data=mal_all, aes(x=x, y=y))
# 
# ## aussenac 2019
# ## short core data
# 
# # read in ????
# fnames = list.files('data/auss-2019/tree/D1823/')
# fname = fnames[2]
# 
# rw_dat = read.csv(paste0('data/auss-2019/tree/D1823/', fnames[1]))
# for (i in 2:length(fnames)){
#   rw_dat = rbind(rw_dat, read.csv(paste0('data/auss-2019/tree/D1823/', fnames[i])))
# }
# 
# sp_trans = data.frame(sp_mal = c('ABA', 'BPA', 'TOC', 'PTR', 'PGL', 'PMA', 'ALN', 'ASP', 'SALIX'), 
#                       sp_auss = c('Ab', NA, 'To', 'Pt', 'Pg', NA, NA, NA, NA))
# 
# rw_dat$Sp = sp_trans[match(rw_dat$Sp, sp_trans$sp_auss), 'sp_mal']
# 
# colnames(rw_dat) = tolower(colnames(rw_dat))
# colnames(rw_dat)[1:3] = c('row', 'short_id', 'species_id')
# 
# rw_short = melt(rw_dat, id.vars = c('row', 'short_id', 'species_id', 'dbh2011', 'x', 'y'))
# rw_short$year = as.numeric(substr(rw_short$variable, 2,5))
# 
# length(unique(rw_dat$short_id))
# 
# rw_dat_meta = rw_dat[,c('short_id', 'species_id', 'x', 'y', 'dbh2011')]
# 
# 
# # foo = merge(mal_all, coords)
# 
# ggplot() +
#   geom_point(data=mal_all, aes(x=x, y=y), colour='grey') +
#   geom_point(data=rw_dat, aes(x=x, y=y), colour='blue') +
#   theme_bw()
# 
# # df_match_id <-  dplyr::right_join(x = mal_all, y = rw_dat_meta, by = c("species_id", "x", "y"))
# # df_match_id <-  dplyr::right_join(x = mal_all, y = rw_dat_meta, by = c("species_id", "x", "y", "dbh2011"))
# df_match_id <-  dplyr::left_join(x = mal_all, y = rw_dat_meta, by = c("species_id", "x", "y", "dbh2011"))
# df_match_id$dbh = df_match_id$dbh*10
# # df_match_id$stem_id = substr(df_match_id$stem_id, 7, )


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
# 

rw_long$species_id = species_table[match(rw_long$species_id, species_table$species_raw), 'species_id']
rw_long$long_id = substr(rw_long$long_id, 3, 8)

length(unique(rw_long$long_id))

# df4 <-  dplyr::right_join(x = rw_dat_melt, y = rw_long, by = c("species_id", "year", "value"))

# n_trees = length(unique(rw_dat_melt$short_id))
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

# 
# df_match_id$long_id =NA
# probs = 0
# success = 0
# 
# for (i in 1:n_trees_long){
#   
#   tree_long = trees_long[i]
#   
#   all_long_sub = all_long[which(all_long$long_id == tree_long),]
#   
#   idx_match = which((df_match_id$species_id == all_long_sub$species_id[1]) & (df_match_id$dbh2011 == all_long_sub$dbh[1]))
#   
#   if(length(idx_match)==0){
#     print(i)
#     print('No match')
#   }
#   
#   df_match_tree = df_match_id[idx_match,]
# 
#   # if more than one plot x species x dbh match, need to get short_id from rw_short
#   if (length(unique(df_match_tree$stem_id)) > 1) {
#     
#     # print(i)
#     # print('nooooooooooooo')
#     
#     all_long_sub = all_long_sub[which(!is.na(all_long_sub$value)),]    
# 
#     cumsum(all_long_sub$value)*2
#     
#     idx_match_rw = which((rw_short$species_id == all_long_sub$species_id[1]) &
#                            (rw_short$dbh2011 == all_long_sub$dbh[1]) &
#                            (rw_short$year == 2013) &
#                            (rw_short$value == all_long_sub[which(all_long_sub$year == 2013), 'value']))
#     
#     # idx_match_rw = which((rw_short$species_id == all_long_sub$species_id[1]) &
#     #                        (rw_short$dbh2011 == all_long_sub$dbh[1]))# &
#     #                        (rw_short$year == 2013) &
#     #                        (rw_short$value == all_long_sub[which(all_long_sub$year == 2013), 'value']))
#     
#     # print(idx_match_rw)
#     
#     if (length(idx_match_rw) == 0){
#       # print(i)
#       # print("Problem tree")
#       probs = probs + 1
#     }
# 
#     short_id = rw_short[idx_match_rw, 'short_id']
#     
#     idx_match = which((df_match_id$species_id == all_long_sub$species_id[1]) & 
#                         (df_match_id$dbh2011 == all_long_sub$dbh[1]) & 
#                         (df_match_id$short_id == short_id))
#     
#     # 
#     # df_match_id[idx_match, 'long_id'] = rw_short$
#     
#   } 
#   
#   # print(i)
#   # print(idx_match)
#   success = success+1
#   df_match_id[idx_match, 'long_id'] = all_long_sub$long_id[1]
# }
# 
# probs
# 
# # some trees have long core data but not short core data?!
# # need to figure out how to match these to census data, could try to match dbh values from previous census year
# # leave this for now
# # email aussenac?
# 
# 
# ggplot() +
#   geom_point(data=df_match_id, aes(x=x, y=y), colour='grey') +
#   geom_point(data=subset(df_match_id, !is.na(long_id)), aes(x=x, y=y), colour='blue') +
#   # geom_point(data=subset(df_match_id, !is.na(long_id) & !is.na(stem_id)), aes(x=x, y=y), colour='red') +
#   theme_bw()
# 
# 
# df_match_id[which((!is.na(df_match_id$short_id)) & (is.na(df_match_id$long_id))),]
# 

all_long$x = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('x')])
all_long$y = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('y')])
all_long$status_id = dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('status')]


# = data.frame(all_long, 
#                  x = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('x')]),
#                  y = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('y')]),
#                  status_id = dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('status')])

# all_long = data.frame(all_long, 
#                  x = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('x')]),
#                  y = as.numeric(dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('y')]),
#                  status_id = dat_wide[match(all_long$stem_id, dat_wide$stem_id), c('status')])
# colnames(all_long)[which(colnames(all_long)=='status')] = 'status_id'

df_match_id = all_long[,c('plot_id', 'stem_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status_id')]

# df_match_id$stat_id = 

# df_match_id$stem_id = as.numeric(substr(df_match_id$stem_id, 7, 12))

df_match_id$dbh = df_match_id$dbh/10

df_match_id = df_match_id[which(!is.na(df_match_id$dbh)),]

# write.csv(df_match_id, 'data/D1823/D1823_meta.csv', row.names=FALSE)

## SUBSET 

rw_long_wide = rw_long %>% 
  group_by(species_id, long_id) %>% 
  arrange(year) %>% 
  pivot_wider(names_from=year, values_from=value)

rw_long_wide = data.frame(rw_long_wide)  

# rw_short_wide = rw_short[,c('species_id', 'short_id', 'year', 'value')] %>% 
#   group_by(species_id, short_id) %>% 
#   dplyr::arrange(year) %>% 
#   pivot_wider(names_from=year, values_from=value)
# 
# rw_short_wide = data.frame(rw_short_wide)  

## df_match_id stat_id for model

rw_dat = rw_long_wide#bind_rows(rw_long_wide, rw_short_wide)
# rw_dat = rw_dat[,c(ncol(rw_dat), 1:(ncol(rw_dat)-1))]

# if have both short and long cores, keep long, remove short
# short_id_remove = unique(df_match_id[which(!is.na(df_match_id$short_id) & !is.na(df_match_id$long_id)), 'short_id'])

# rw_dat = rw_dat[which(!(rw_dat$short %in% short_id_remove)),]

# if have long but no census data match? these were already dropped if any, whew
# df_match_id[which(!is.na(df_match_id$long_id) & is.na(df_match_id$stem_id)), ]

#
# df_match_id[which(!is.na(df_match_id$stem_id) & is.na(df_match_id$long_id)), ]

# length(unique(which(!is.na(rw_dat$long_id) & is.na(rw_dat$stem_id))))
# length(unique(which(!is.na(rw_dat$long_id) & !is.na(rw_dat$stem_id))))
# length(unique(which(is.na(rw_dat$long_id) & !is.na(rw_dat$short_id) & !is.na(rw_dat$stem_id))))



rw_dat = data.frame(stem_id = substr(rw_dat$long_id, 3, 6),
                    rw_dat)
rw_dat$species_id = species_table[match(rw_dat$species_id, species_table$species_raw), 'species_id']

rw_dat = rw_dat[,c('stem_id', 'species_id', colnames(rw_dat)[4:ncol(rw_dat)])]
  
colnames(rw_dat)[3:ncol(rw_dat)] = substr(colnames(rw_dat)[3:ncol(rw_dat)], 2,5)



write.csv(rw_dat, 'data/D1823/D1823_rw.csv', row.names=FALSE)

# for (i in 1:nrow(rw_dat)){
#   print(rw_dat[i,])
#   askYesNo('next?')
# }

# 
# dat = read.csv('data/D1823/D1823_rw.csv', header=TRUE, stringsAsFactors = FALSE)
# colnames(dat)[5:ncol(dat)] = substr(colnames(dat)[5:ncol(dat)], 2,5)

# ggplot() +
#   geom_point(data=df_match_id, aes(x=x, y=y), colour='grey') +
#   geom_point(data=subset(df_match_id, !is.na(long_id)), aes(x=x, y=y), colour='blue') +
#   geom_point(data=subset(df_match_id, !is.na(long_id) & is.na(stem_id)), aes(x=x, y=y), colour='red') +
#   theme_bw()


