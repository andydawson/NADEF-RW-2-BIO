library(dplR)
library(ggplot2)
library(plyr)

############################################################################################
# read original dataset
############################################################################################

meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)
rw   = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)

############################################################################################
# read new metadata, update original metadata
############################################################################################

# read dbh file
dbh = read.csv('data/D1823/D1823_dbh.csv', stringsAsFactors = FALSE)
dbh_melt = melt(dbh, id.vars=c('stem_id', 'species_code'))
dbh_meta = meta[match(dbh_melt$stem_id, meta$stem_id), c('plot_id', 'short_id', 'long_id', 'x', 'y', 'status_id')]

dbh_meta = data.frame(dbh_meta, dbh_melt)
dbh_meta$year = 2021
colnames(dbh_meta)[which(colnames(dbh_meta) == 'species_code')] = 'species_id'
colnames(dbh_meta)[which(colnames(dbh_meta) == 'value')] = 'dbh'

dbh_meta = dbh_meta[, colnames(meta)]

meta_new = rbind(meta, dbh_meta)

write.csv(meta_new, 'data/D1823/D1823_meta_update.csv', row.names=FALSE)

############################################################################################
# read new rw data, update original rw data
############################################################################################

# read rwl files
fnames = list.files(path="data/ring-width/", pattern=".rwl$")
fnames = fnames[c(1,2,4)]

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

# dat = lapply(fnames, function(x) read.rwl(paste0("alberta-transect/rwl/", x)))
dat_all = combine.rwl(dat)
# colnames(dat.all) = paste0('0', rep(seq(1, 9), each=2), rep(c('A', 'B'), times=9))

ids = read.ids(dat_all, stc = c(3,4,1))
dat_all = treeMean(dat_all, ids=ids)
dat_all = t(dat_all)

dat_all = data.frame(year=rownames(dat_all), dat_all)
colnames(dat_all) = paste0('X', colnames(dat_all))

# stem_id short_id long_id species_id 

rw_meta = meta_new[match(as.numeric(rownames(dat_all)), meta_new$stem_id), 
         c('stem_id', 'short_id', 'long_id', 'species_id')]
rw_new = data.frame(rw_meta, dat_all)

rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)

rw_update = rbind.fill(rw, rw_new)

write.csv(rw_update, 'data/D1823/D1823_rw_update.csv', row.names=FALSE)

                