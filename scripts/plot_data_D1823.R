library(reshape2)

dat = read.csv('data/D1823/D1823_meta.csv')









########################################################################################################################
dat = readRDS('data/D1823/D1823_input.RDS')
list2env(dat, envir = globalenv())

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

years = as.numeric(substr(colnames(y), 2, 5))

rw_dat = data.frame(year = years, t(y))
rw_melt = melt(rw_dat, id.vars='year')

ggplot(data=rw_melt) +
  geom_line(aes(x=year, y=value, colour=variable))

rw_sum = 2*colSums(rw_dat[,2:ncol(rw_dat)])
d*10

core2tree
