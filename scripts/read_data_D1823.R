# script to read original data from Maleki et al., (2021) and Aussenac et al., (2017)
# for the Bayesian model, --version1.01, by xxxx

#######################################################################################################################################
# 
#######################################################################################################################################

# clear the R environment
rm(list = ls())

# load packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

# table for converting species id
species_table = data.frame(species_raw = c('Ab', 'Pg', 'To', 'Pt'),
                           species_id = c('ABA', 'PGL', 'TOC', 'PTR'))



#######################################################################################################################################
# Census data: read in raw census data and reformat
#######################################################################################################################################

## read in census raw data Maleki et al., (2021) 
# this data file is complete, it includes dbh data from Aussenac et al., 2017.
dat = read.csv('data/D1823/D1823_census_raw_edit.csv', stringsAsFactors = FALSE)

# rename columns from french to English
colnames(dat)[1:4] = c('parcel', 'species_id', 'stem_id', 'ancient_id') 

var_names = c('dbh', 'status', 'canopy_class')
var_names2 = c('dbh', 'status', 'canopy_class', 'greenness', 'decomp')
var_names3 = c('dbh', 'status', 'canopy_class', 'greenness', 'decomp', 'damage')

colnames(dat)[5:7] = c(paste0('1994_', var_names))
colnames(dat)[8:12] = c(paste0('2004_', var_names2))
colnames(dat)[13:18] = c(paste0('2011_', var_names3))
colnames(dat)[19:24] = c(paste0('2019_', var_names3))
colnames(dat)[25:26] = c('x', 'y') 

# remove french column names
dat = dat[-1,]

# at this point "dat" is the census data in wide format
length(unique(dat$ancient_id))
#117
length(unique(dat$stem_id))
#2415


N_dup = length(which(duplicated(dat$stem_id)))
dup_new_ids = paste0('D', seq(1, N_dup))

dup_ids = dat[which(duplicated(dat$stem_id)), 'stem_id'] 

dat[which(duplicated(dat$stem_id)), 'stem_id'] = dup_new_ids

# melt dataframe, each possible observation becomes a row
dat_melt = melt(dat, id.vars=c('parcel', 'species_id', 'stem_id', 'ancient_id', 'x', 'y'))

# split variable column into observation type and year
dat_melt$year = as.numeric(substr(dat_melt$variable, 1, 4))
dat_melt$variable = as.character(dat_melt$variable)
dat_melt$variable = substr(dat_melt$variable, 6, 17)

# add columns for greenness, decomp, and damage
dat_wide = dat_melt %>%
  group_by(parcel, species_id, stem_id, ancient_id, x, y, year) %>%
  pivot_wider(names_from = variable, values_from = value)

dat_wide = data.frame(dat_wide)
dat_wide$dbh = as.numeric(dat_wide$dbh)
dat_wide$species_id = toupper(dat_wide$species_id)
# species_table = data.frame(species_raw = c('Ab', 'Pg', 'To', 'Pt'),
#                            species_id = c('ABA', 'PGL', 'TOC', 'PTR'))

dat_wide$plot_id = 'D1823'
dat_wide = dat_wide[,c('plot_id', 'stem_id', 'ancient_id', 'species_id', 'x', 'y', 'year', 'dbh', 'status', 'canopy_class', 'greenness')]
colnames(dat_wide)[which(colnames(dat_wide) =='status')] = 'status_id'
dat_wide = dat_wide[which(!is.na(dat_wide$dbh)),]

# write dataframe to the D1823 data folder in github repo
write.csv(dat_wide, 'data/D1823/D1823_meta.csv', row.names=FALSE)


#######################################################################################################################################
# Ring-width data: read in and reformat Aussenac 2017 ring-width data
#######################################################################################################################################

## Aussenac 2017 - long core data, i.e., ring width
# read in a list of files in
fnames = list.files('data/auss-2017/06_TRD/D1823/')
#create empty dataframe for rw data with long species id
rw_long = data.frame(species_id=character(0),
                     year=numeric(0),
                     long_id=character(0),
                     value=numeric(0))
#populate dataframe
for (i in 1:length(fnames)){
  rwl_dat = read.table(paste0('data/auss-2017/06_TRD/D1823/', fnames[i]))
  rwl_dat = data.frame(species_id=substr(fnames[i],1,2), year = rownames(rwl_dat), rwl_dat)
  rwl_dat_melt = melt(rwl_dat, id.vars = c('species_id', 'year'))
  colnames(rwl_dat_melt) = c('species_id', 'year', 'long_id', 'value')
  
  rwl_dat_melt$year = as.numeric(rwl_dat_melt$year)
  rw_long = rbind(rw_long, rwl_dat_melt)
}
# 
#rename species to standard species id
rw_long$species_id = species_table[match(rw_long$species_id, species_table$species_raw), 'species_id']
rw_long$long_id = substr(rw_long$long_id, 3, 8)

#rw_long to wide format
rw_dat = rw_long %>% 
  group_by(species_id, long_id) %>% 
  arrange(year) %>% 
  pivot_wider(names_from=year, values_from=value)

rw_dat = data.frame(stem_id = substr(rw_dat$long_id, 3, 6),
                    rw_dat)

rw_dat$species_id = species_table[match(rw_dat$species_id, species_table$species_raw), 'species_id']
rw_dat = rw_dat[,c('stem_id', 'species_id', colnames(rw_dat)[4:ncol(rw_dat)])]
colnames(rw_dat)[3:ncol(rw_dat)] = substr(colnames(rw_dat)[3:ncol(rw_dat)], 2,5)

write.csv(rw_dat, 'data/D1823/D1823_rw.csv', row.names=FALSE)

# note that only 112 trees are in the census dataset somehow
# why is this?!
length(which(rw_dat$stem_id %in% dat_wide$stem_id))

rw_dat$stem_id[which(!(rw_dat$stem_id %in% dat_wide$stem_id))]
