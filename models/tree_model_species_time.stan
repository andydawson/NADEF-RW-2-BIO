data {
  int<lower=0> N_trees;   // number of trees
  int<lower=0> N_years;   // number of years
  int<lower=0> N_species; // number of species
  int<lower=0> N_cores;   // number of cores
  int<lower=0> N_dbh;     // number of dbh measurements

  matrix[N_trees, N_years] logy;   // matrix of logged ring-width measurements; -999 represents missing values
  int<lower=0> core2tree[N_trees]; // index vector of tree number corresponding with each core
  int<lower=0> core2species[N_trees]; // index vector of species number corresponding with each core

  // int<lower=0> rw_year_start[N_trees];
  // int<lower=0> rw_year_end[N_trees];

  real<lower=0> d[N_dbh];  // vector of dbh measurements
  int<lower=0> d2tree[N_dbh];  // index vector of tree number corresponding with each dbh measurement
  int<lower=0> d2year[N_dbh];  // index vector of year number corresponding with each dbh measurement

  real<lower=1e-6> sig_d_obs; // dbh measurement uncertainty, represented as standard deviation

}
transformed data {
}
parameters {

  real beta0;
  real beta[N_trees];
  real<lower=1e-6> beta_sd;
  
  matrix[N_years, N_species] beta_t;
  real<lower=1e-6> beta_t_sd;

  //real beta_k[N_species];
  //real<lower=1e-6> beta_k_sd;

  real<lower=1e-6> sig_x;
  real<lower=1e-6> sig_x_obs;

  //real<lower=1e-6> sig_d_obs;

  real<lower=0,upper=10> d_init[N_trees];

  matrix<lower=1e-6>[N_trees, N_years] x;

}
transformed parameters {
  matrix<lower=1e-6>[N_trees, N_years] d_latent;

  for (tree in 1:N_trees){
    d_latent[tree,1] = d_init[tree] + 2 * x[tree, 1] / 10 ;
    for (year in 2:N_years) {
      d_latent[tree,year] = d_latent[tree, year-1] + 2 * x[tree, year] / 10 ;
    }
  }

}
model {

  // tree effect mean and dispersion priors
  beta0     ~ normal(0, 1000);
  beta_sd   ~ uniform(1e-6, 1000);

  // ring-width data model dispersion prior
  sig_x_obs ~ uniform(1e-6, 2.0);
  //sig_d_obs ~ uniform(1e-6, 10);

  // latent ring-width process dispersion prior
  sig_x     ~ uniform(1e-6, 1000);

  // temporal species effect dispersion prior
  beta_t_sd ~ uniform(1e-6, 1000);

  //// species effect dispersion prior
  //beta_k_sd ~ uniform(1e-6, 1000);

  
  for(tree in 1:N_trees) {

    // latent dbh initial value prior 
    d_init[tree] ~ uniform(0, 10);

    // tree effect prior
    beta[tree] ~ normal(beta0, beta_sd);
  }

  // temporal species effect prior 
  for(year in 1:N_years) {
    beta_t[year,] ~ normal(0, beta_t_sd);
  }

  // latent ring-width process model 
  for (tree in 1:N_trees) {
    for (year in 1:N_years) {
      x[tree, year] ~ lognormal(beta[tree] + beta_t[year, core2species[tree]], sig_x);
    }
  }

  // ring-width data model
  for (tree in 1:N_trees) {
    for (year in 1:N_years) {
      if (logy[tree,year] == -999){
      } else {
	logy[tree, year] ~ normal(log(x[tree, year]), sig_x_obs);
      }
    }
  }

  // dbh data model 
  for (i in 1:N_dbh) {
    log(d[i]) ~ student_t(3.0, log(d_latent[d2tree[i],d2year[i]]), sig_d_obs);
  }
}
