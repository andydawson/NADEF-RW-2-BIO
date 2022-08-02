library(dplR)
library(dplyr)
library(plyr)

meta = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)
meta = meta[,c('plot_id', 'stem_id', 'species_id', 'x', 'y', 'year', 'dbh')]

# read dbh file
dbh = read.csv('data/D1823/D1823_dbh.csv', stringsAsFactors = FALSE)
dbh = data.frame(dbh[,1:2], dbh=rowMeans(dbh[,3:7]))

dbh$year = 2021
dbh$plot_id = 'D1823'
dbh$stem_id = id_translate[match(dbh$stem_id, id_translate$mal), 'census']

dbh$dbh = dbh$dbh*10

dbh$x = meta[match(dbh$stem_id, meta$stem_id), c('x')]
dbh$y = meta[match(dbh$stem_id, meta$stem_id), c('y')]

colnames(dbh)[which(colnames(dbh)=='species_code')] = 'species_id'

dbh = dbh[,c('plot_id', 'stem_id', 'species_id', 'x', 'y', 'year', 'dbh')]
meta_new = rbind(meta, dbh)

# meta_new = meta

write.csv(meta_new, 'data/D1823/D1823_meta_update.csv', row.names = FALSE)


rw = read.csv('data/D1823/D1823_rw.csv', stringsAsFactors = FALSE)


# read rwl files
fnames = list.files(path="data/ring-width/", pattern=".rwl$")

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


dat_all = data.frame(stem_id = rownames(dat_all), dat_all)

id_translate = read.csv('data/id_translate.csv', stringsAsFactors = FALSE)

dat_all$stem_id = id_translate[match(dat_all$stem_id, id_translate$mal), 'census']

dat_all$species_id = meta[match(dat_all$stem_id, meta$stem_id), 'species_id']

rw_update = rbind.fill(rw, dat_all)

rw_update = rw

write.csv(rw_update, 'data/D1823/D1823_rw_update.csv', row.names = FALSE)