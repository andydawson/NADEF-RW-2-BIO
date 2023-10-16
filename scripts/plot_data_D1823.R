library(reshape2)
library(dplyr)
library(tidyr)

meta_update = read.csv('data/D1823/D1823_meta_update.csv')
meta_update = meta_update[,which(!(colnames(meta_update)%in% c("ancient_id", "species_id")))]

rw_update   = read.csv('data/D1823/D1823_rw_update.csv')

meta_wide = meta_update %>% 
  group_by(stem_id, x, y) %>% 
  distinct() %>%
  tidyr::pivot_wider(names_from = year, values_from = dbh) %>%
  unnest()

# some duplicates
rw_update[which(duplicated(rw_update$stem_id)), 'stem_id']

rw_update[which(rw_update$stem_id == "405"),]

# 
rw_update = rw_update[which(!duplicated(rw_update$stem_id)), ]


rw_sum = data.frame(stem_id = rw_update$stem_id, dbh_rw = rowSums(rw_update[,3:ncol(rw_update)], na.rm=TRUE)/10*2)

meta_wide = merge(meta_wide, rw_sum)
meta_wide$dbh_diff = meta_wide$`2019` - meta_wide$dbh_rw

hist(meta_wide$dbh_diff)
meta_wide[which(meta_wide$dbh_diff>20),]


meta_wide[which(!is.na(meta_wide$`2021`)),]

# why do they not aligh with earlier census measurements?
# different species_id or ancient_id

meta_update[which(meta_update$stem_id=='T60'),]



dbh = read.csv('data/D1823/D1823_dbh.csv')
dbh$sd = apply(dbh[,3:7], 1, sd)
dbh$mean = apply(dbh[,3:7], 1, mean)

ggplot(data=dbh) +
  geom_point(aes(x=mean, y=sd), size=3) +
  theme_bw(16) +
  xlab('Mean DBH (cm)') +
  ylab('Standard deviation DBH (cm)') +
  ylim(c(0, 1.2))
# ggsave('')

foo = melt(dbh, id.vars = c('stem_id', 'species_code'))

ggplot(data=foo) +
  geom_point(aes(x=stem_id, y=value))
