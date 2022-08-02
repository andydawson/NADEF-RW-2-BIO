
# meta_ha1923 = read.csv('data/H1923/HA1923_meta.csv', stringsAsFactors = FALSE)

meta_d1823 = read.csv('data/D1823/D1823_meta.csv', stringsAsFactors = FALSE)

# meta_d1847 = read.csv('data/D1847/D1847_meta.csv', stringsAsFactors = FALSE)

head(meta_ha1923)

# dat = rbind(meta_d1823, meta_d1847)

ggplot(data=subset(dat, year==2019)) +
  geom_histogram(aes(x=dbh, y=..density..)) +
  facet_grid(plot_id~year) +
  theme_bw(14)

ggplot(data=subset(dat, year==2019)) +
  geom_density(aes(x=dbh), fill='grey') +
  facet_grid(plot_id~year) +
  theme_bw(14)

ggplot(data=subset(dat, year==2019)) +
  geom_histogram(aes(x=dbh, y=..density..)) +
  facet_grid(plot_id~species_id) +
  theme_bw(14)

ggplot(data=subset(dat, year==2019)) +
  geom_histogram(aes(x=dbh, y=..density.., colour=plot_id)) +
  facet_grid(.~species_id) +
  theme_bw(14)

ggplot(data=subset(dat, year==2019)) +
  geom_density(aes(x=dbh, fill=plot_id)) +
  facet_grid(.~species_id) +
  theme_bw(14)

ggplot(data=subset(dat, year==2019)) +
  geom_boxplot(aes(x=species_id, y=dbh, colour=plot_id)) +
  # facet_grid(.~species_id) +
  theme_bw(14)
# 
# ggplot(data=subset(dat, (year==2019)&(dbh>100))) +
#   geom_boxplot(aes(x=species_id, y=dbh, colour=plot_id)) +
#   # facet_grid(.~species_id) +
#   theme_bw(14)

# ggplot(data=meta_d1823) +
#   geom_boxplot(aes(x=factor(year), y=dbh))#, colour=species_id))
# 
# ggplot(data=meta_d1823) +
#   geom_histogram(aes(x=dbh, y=..density..)) +
#   facet_grid(year~.)


