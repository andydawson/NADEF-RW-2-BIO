library(dplR)
library(ggplot2)
library(plyr)
library(reshape2)
library(dplyr)

############################################################################################
# read original dataset
############################################################################################

meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)
colnames(meta)[which(colnames(meta)=='status')] = 'status_id'

# dbh$x = meta[match(dbh$stem_id, meta$stem_id), c('x')]
# meta = meta[,c(1,2,5,6,7,8,9)]

rw   = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)


id_translate = read.csv('data/id_translate.csv', stringsAsFactors = FALSE)

############################################################################################
# read new metadata, update original metadata
############################################################################################

# read dbh file
dbh = read.csv('data/D1823/D1823_dbh.csv', stringsAsFactors = FALSE)
dbh$stem_id = id_translate[match(dbh$stem_id, id_translate$mal), 'census']

dbh = data.frame(dbh[,1:2], dbh=rowMeans(dbh[,3:7]))
# dbh$dbh = dbh$dbh

dbh$year = 2021
dbh$plot_id = 'D1823'
# dbh$old_id = meta[match(dbh$stem_id, meta$stem_id), c('y')]



dbh$x = meta[match(dbh$stem_id, meta$stem_id), c('x')]
dbh$y = meta[match(dbh$stem_id, meta$stem_id), c('y')]

# 
# dbh_melt = melt(dbh, id.vars=c('stem_id', 'species_code'))
# # dbh_meta = meta[match(dbh_melt$stem_id, meta$stem_id), c('plot_id', 'x', 'y', 'status_id')]
# dbh_meta = meta[match(dbh_melt$stem_id, meta$stem_id), c('plot_id', 'x', 'y')]
# 
# # dbh_meta$x = meta[match(dbh_melt$stem_id, meta$stem_id), 'x']
# # dbh_meta$x = meta[match(dbh_melt$stem_id, meta$stem_id), 'y']
# 
# dbh_meta = cbind(dbh_meta, dbh_melt)
# dbh_meta$year = 2021
colnames(dbh)[which(colnames(dbh) == 'species_code')] = 'species_id'
# colnames(dbh_meta)[which(colnames(dbh_meta) == 'value')] = 'dbh'
# meta = meta[,c('plot_id', 'stem_id', 'species_id', 'x', 'y', 'year', 'dbh')]
# dbh_meta = dbh_meta[, colnames(meta)]
# # dbh_meta$x = meta[match(dbh_meta$stem_id, meta$stem_id),'x']
# # dbh_meta$y = meta[match(dbh_meta$stem_id, meta$stem_id),'y']

dbh$ancient_id = NA


dbh_meta = dbh[,c('plot_id', 'stem_id', 'ancient_id', 'species_id', 'x', 'y', 'year', 'dbh')]
meta = meta[,c('plot_id', 'stem_id', 'species_id', 'ancient_id', 'x', 'y', 'year', 'dbh')]

write.csv(meta, 'data/D1823/D1823_meta.csv', row.names=FALSE)

meta_new = rbind(meta, dbh_meta)

meta_new %>% group_by(stem_id, year) %>% arrange(stem_id, year)

# meta_new = meta_new %>% group_by(stem_id, year) %>% arrange(stem_id, year)
# meta_new = data.frame(meta_new)

# meta_new = meta_new[which(nchar(meta_new$stem_id)>0),]

# meta_new = meta

meta_new = meta_new[which(!duplicated(meta_new)),]

write.csv(meta_new, 'data/D1823/D1823_meta_update.csv', row.names=FALSE)

############################################################################################
# read new rw data, update original rw data
############################################################################################

# read rwl files
fnames = list.files(path="data/ring-width/", pattern=".rwl$")
# fnames = fnames[c(1,2,4)]

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

# dat_all = data.frame(year=rownames(dat_all), dat_all)
# colnames(dat_all) = paste0('X', colnames(dat_all))

# stem_id short_id long_id species_id

# rw_meta = meta_new[match(as.numeric(rownames(dat_all)), meta_new$stem_id),
#          c('stem_id', 'species_id')]
# rw_meta = rw_meta[which(!is.na(rw_meta$stem_id)),]
# 
# dat_all = dat_all[which(rownames(dat_all) %in% rw_meta$stem_id),]
rw_ids = rownames(dat_all)
rw_ids = id_translate[match(rw_ids, id_translate$mal), 'census']
dat_all = data.frame(stem_id = rw_ids, species_id = meta[match(rw_ids, meta$stem_id), 'species_id'], dat_all)

# rw_new = merge(rw_meta, dat_all, by=c('stem_id'))

rw_new = dat_all

rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)
idx_zero = which(substr(rw$stem_id, 1, 1) == 0)
rw$stem_id[idx_zero] = substr(rw$stem_id[idx_zero], 2, 4)
idx_zero = which(substr(rw$stem_id, 1, 1) == 0)
rw$stem_id[idx_zero] = substr(rw$stem_id[idx_zero], 2, 3)

length(which(rw$stem_id %in% meta_new$stem_id))

rw[which(!(rw$stem_id %in% meta_new$stem_id)), 'stem_id']

meta_new[which(substr(meta_new$stem_id,1,1)=="I"),'stem_id']

idx_izero = which((substr(rw$stem_id, 1, 1) == "I")&(substr(rw$stem_id, 2, 2)=='0'))
rw$stem_id[idx_izero] = paste0("I", substr(rw$stem_id[idx_izero], 3, 4))
idx_izero = which((substr(rw$stem_id, 1, 1) == "I")&(substr(rw$stem_id, 2, 2)=='0'))
rw$stem_id[idx_izero] = paste0("I", substr(rw$stem_id[idx_izero], 3, 3))

length(which(rw$stem_id %in% meta_new$stem_id))

rw[which(!(rw$stem_id %in% meta_new$stem_id)), 'stem_id']
meta_new[which(substr(meta_new$stem_id,1,1)=="I"),'stem_id']

# meta_new[match(rw[which(!(rw$stem_id %in% meta_new$stem_id)), 'stem_id'], meta_new$ancient_id), 'stem_id']

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

rw = rw[which(rw$stem_id %in% meta_new$stem_id),]

write.csv(rw, 'data/D1823/D1823_rw.csv', row.names=FALSE)


rw_add = matrix(-999, nrow=nrow(rw), ncol=8)
colnames(rw_add) = paste0('X', seq(2014, 2021))

rw_expand = data.frame(rw, rw_add)
rw_expand = rw_expand[,!(colnames(rw_expand) %in%  paste0('X', seq(1852, 1858)))]

# rw_update = rbind.fill(rw, rw_new)
# 
# rw_update = rw

# rw_update = rw_new
rw_in = rw_new[which(rw_new$stem_id %in% rw_expand$stem_id),]
idx_match = match(rw_in$stem_id, rw_expand$stem_id)

rw_expand[idx_match,] = rw_in

id_out = rw_new[which(!(rw_new$stem_id %in% rw_expand$stem_id)), 'stem_id']
meta_new[which(meta_new$stem_id %in% id_out),]


rw_out = rw_new[which(!(rw_new$stem_id %in% rw_expand$stem_id)),]

dbh_rw = data.frame(rw_out$stem_id, dbh_rw = rowSums(rw_out[,3:ncol(rw_out)],na.rm=TRUE)/10*2)

# works
# rw_expand = rbind(rw_expand, rw_out[c(1:4),])

# works
# rw_expand = rbind(rw_expand, rw_out[5:9,])

# does not work
# rw_expand = rbind(rw_expand, rw_out[c(4:9),])

# # works
# rw_expand = rbind(rw_expand, rw_out[c(1:3, 5:9),])

rw_out[which(rw_out == 0)] = 0.001

rw_expand = rbind(rw_expand, rw_out)

rw_update = rw_expand


write.csv(rw_update, 'data/D1823/D1823_rw_update.csv', row.names=FALSE)

                