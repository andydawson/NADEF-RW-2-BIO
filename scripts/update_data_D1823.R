# script to prepare updated data for the Bayesian model, --version1.01, by xxxx

#######################################################################################################################################
# 
#######################################################################################################################################

# clear the R environment
rm(list = ls())

# load packages
library(dplR)
library(ggplot2)
library(plyr)
library(reshape2)
library(dplyr)

# read original metadata
meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)
colnames(meta)[which(colnames(meta)=='status')] = 'status_id'

# dbh$x = meta[match(dbh$stem_id, meta$stem_id), c('x')]
# meta = meta[,c(1,2,5,6,7,8,9)]

# read original ring width data
rw  = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)

# read id table, where does this file come from？
id_translate = read.csv('data/id_translate.csv', stringsAsFactors = FALSE)

# read new dbh file
dbh = read.csv('data/D1823/D1823_dbh.csv', stringsAsFactors = FALSE)

#######################################################################################################################################
# Update meta: add additional DBH measurements for NADEF remeasured trees
#######################################################################################################################################

# match new stem ids with Malecki ids
# dbh$stem_id = id_translate[match(dbh$stem_id, id_translate$mal), 'census']
dbh$stem_id = dbh$census_id
dbh$stem_id[dbh$stem_id == "C713"] = "I75"

# create data frame with new(?) stem ids and mean dbh
dbh = data.frame(dbh[,c(1,3)], dbh=rowMeans(dbh[,4:8]))

# add year
dbh$year = 2021
#add plot id
dbh$plot_id = 'D1823'

# add location
dbh$x = meta[match(dbh$stem_id, meta$stem_id), c('x')]
dbh$y = meta[match(dbh$stem_id, meta$stem_id), c('y')]

# rename species code column
colnames(dbh)[which(colnames(dbh) == 'species_code')] = 'species_id'

# create empty column for ancient(malecki?) ids
dbh$ancient_id = NA

# create new dataframe from new "dbh" measurements in 2022
dbh_meta = dbh[,c('plot_id', 'stem_id', 'ancient_id', 'species_id', 'x', 'y', 'year', 'dbh')]
dbh_meta$status_id = 'v'
dbh_meta$canopy_class = NA
dbh_meta$greenness = NA

# create new data frame from "meta"
meta = meta[,c('plot_id', 'stem_id', 'ancient_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status_id', 'canopy_class', 'greenness')]

# correct for errornous dbh for C460
meta[which((meta$stem_id == 'C460')&(meta$year == 2019)), 'dbh'] = meta[which((meta$stem_id == 'C460')&(meta$year == 2019)), 'dbh'] /10

#write.csv(meta, 'data/D1823/D1823_meta.csv', row.names=FALSE)

#combine new "dbh_meta" and "meta" data frames
colnames(meta)
colnames(dbh_meta)
meta_new = rbind(meta, dbh_meta)
meta_new = meta_new %>% group_by(stem_id, year) %>% arrange(stem_id, year)

meta_new = meta_new[which(!duplicated(meta_new)),]

write.csv(meta_new, 'data/D1823/D1823_meta_update.csv', row.names=FALSE)

length(unique(meta_new$stem_id[!is.na(meta_new$dbh)]))
length(unique(meta$stem_id[!is.na(meta$dbh)]))

#######################################################################################################################################
# Ring-width data: read in and reformat Aussenac 2017 ring-width data
#######################################################################################################################################

# create empty list for new rwl files
fnames = list.files(path="data/ring-width/", pattern=".rwl$")

# read rwl files for new ring width data
dat = list()
for (i in 1:length(fnames)){
  fname = fnames[i]
  print(fname)
  # if (i ==3) {
  #   fname = fnames[2]
  #   print(fname)
  # }
  dat[[i]] = read.rwl(paste0("data/ring-width/", fname))
}

#combine rwl files into data frame
dat_all = combine.rwl(dat)

#create new data frame with tree ids
# ids = read.ids(dat_all, stc = c(3,4,1))
#create matrix of rw increments organized by tree x year
# dat_all = treeMean(dat_all, ids=ids)
#transforms from matrix to table
dat_all = t(dat_all)

#create list of tree ids
rw_ids = rownames(dat_all)
#match tree ids to Aussenac ids(malecki?)
rw_ids = id_translate[match(rw_ids, id_translate$mal), 'census']
#add id columns to table 
dat_all = data.frame(stem_id = rw_ids, species_id = meta[match(rw_ids, meta$stem_id), 'species_id'], dat_all)
dat_all$stem_id[dat_all$stem_id == "C713"] = "I75"

#create new data frame from table
rw_new = dat_all
rw_ids%in% rw$stem_id

## read in, check, and format original RW data
## we will add our RW data to this file

# read in original ring width data
rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)

#translate ids
idx_zero = which(substr(rw$stem_id, 1, 1) == 0)
rw$stem_id[idx_zero] = substr(rw$stem_id[idx_zero], 2, 4)
idx_zero = which(substr(rw$stem_id, 1, 1) == 0)
rw$stem_id[idx_zero] = substr(rw$stem_id[idx_zero], 2, 3)

length(which(rw$stem_id %in% meta_new$stem_id))

#not sure what this does??
# check whether the same tree has been sampled twice？
rw[which(!(rw$stem_id %in% meta_new$stem_id)), 'stem_id']
meta_new[which(substr(meta_new$stem_id,1,1)=="I"),'stem_id']
idx_izero = which((substr(rw$stem_id, 1, 1) == "I")&(substr(rw$stem_id, 2, 2)=='0'))
rw$stem_id[idx_izero] = paste0("I", substr(rw$stem_id[idx_izero], 3, 4))
idx_izero = which((substr(rw$stem_id, 1, 1) == "I")&(substr(rw$stem_id, 2, 2)=='0'))
rw$stem_id[idx_izero] = paste0("I", substr(rw$stem_id[idx_izero], 3, 3))

length(which(rw$stem_id %in% meta_new$stem_id))

rw[which(!(rw$stem_id %in% meta_new$stem_id)), 'stem_id']
meta_new[which(substr(meta_new$stem_id,1,1)=="I"),'stem_id']

# these trees have RW IDS that are the Ancient IDs in the census
# update them to actual census IDs
# not related to our sampling
rw$stem_id[which(rw$stem_id == 'T15')] = '4337'
rw$stem_id[which(rw$stem_id == 'T29')] = '3433'
rw$stem_id[which(rw$stem_id == 'T42')] = '3931'
rw$stem_id[which(rw$stem_id == 'T65')] = '845'
rw$stem_id[which(rw$stem_id == 'I14')] = '4540'
rw$stem_id[which(rw$stem_id == 'I24')] = '4541'
rw$stem_id[which(rw$stem_id == 'I81')] = '4543'
rw$stem_id[which(rw$stem_id == 'I106')] = '842'
rw$stem_id[which(rw$stem_id == 'I134')] = '4547'
rw$stem_id[which(rw$stem_id == 'I135')] = '4546'
rw$stem_id[which(rw$stem_id == 'I138')] = '4359'
rw$stem_id[which(rw$stem_id == 'I141')] = '4334'
rw$stem_id[which(rw$stem_id == 'I142')] = '4348'
rw$stem_id[which(rw$stem_id == 'I151')] = '4372'
rw$stem_id[which(rw$stem_id == 'I186')] = '4350'
rw$stem_id[which(rw$stem_id == 'I208')] = '4373'
rw$stem_id[which(rw$stem_id == 'I221')] = '4565'
rw$stem_id[which(rw$stem_id == 'I229')] = '4377'
rw$stem_id[which(rw$stem_id == 'C136')] = 'dps32'

rw[which(!(rw$stem_id %in% meta_new$stem_id)), 'stem_id']

# # don't need this, they are all in oh my god
# rw = rw[which(rw$stem_id %in% meta_new$stem_id),]

#write.csv(rw, 'data/D1823/D1823_rw.csv', row.names=FALSE)

# create new data matrix for increment values (final years all have -999?)
rw_add = matrix(-999, nrow=nrow(rw), ncol=8)
colnames(rw_add) = paste0('X', seq(2014, 2021))
rw_expand = data.frame(rw, rw_add)
# rw_expand = rw_expand[,!(colnames(rw_expand) %in%  paste0('X', seq(1852, 1858)))]

# create data frame for overlapping trees
# the rw and rw_new should have the same number of columns (years)
rw_add2 = matrix(NA, nrow=20, ncol=15)
colnames(rw_add2) = paste0('X', seq(1852, 1866))
rw_new <- cbind(rw_new[, 1:2], rw_add2, rw_new[, -1:-2])

#create data frame for overlapping(?) trees
rw_in = rw_new[which(rw_new$stem_id %in% rw_expand$stem_id),]
idx_match = match(rw_in$stem_id, rw_expand$stem_id)
rw_expand$stem_id[idx_match]
#why don't we average them instead of replacing them/？
rw_expand[idx_match,] = rw_in

#Create list for new trees (ids)
id_out = rw_new[which(!(rw_new$stem_id %in% rw_expand$stem_id)), 'stem_id']

#replace ids
meta_new[which(meta_new$stem_id %in% id_out),]
#create new data frame for non-overlapping trees
rw_out = rw_new[which(!(rw_new$stem_id %in% rw_expand$stem_id)),]
rw_out$stem_id
#match selected trees
dbh_rw = data.frame(rw_out$stem_id, dbh_rw = rowSums(rw_out[,3:ncol(rw_out)],na.rm=TRUE)/10*2)
rw_out[rw_out == 0] = 0.001
rw_expand = rbind(rw_expand, rw_out)
rw_update = rw_expand

write.csv(rw_update, 'data/D1823/D1823_rw_update.csv', row.names=FALSE)

                
