data {
  int<lower=0> N_trees;
  int<lower=0> N_years;
  //int<lower=0> N_species;
  int<lower=0> N_cores;
  int<lower=0> N_dbh;

  //matrix[N_cores, N_years] y;
  /* matrix[N_cores, N_years] logy; */
  /* int<lower=0> core2tree[N_cores]; */
  /* int<lower=0> rw_year_start[N_cores]; */
  /* int<lower=0> rw_year_end[N_cores]; */

  matrix[N_trees, N_years] logy;
  int<lower=0> core2tree[N_trees];

  // int<lower=0> rw_year_start[N_trees];
  // int<lower=0> rw_year_end[N_trees];

  real<lower=0> d[N_dbh];
  int<lower=0> d2tree[N_dbh];
  int<lower=0> d2year[N_dbh];

  real<lower=1e-6> sig_d_obs;

}
transformed data {
}
parameters {

  real beta0;
  real beta[N_trees];
  real<lower=1e-6> beta_sd;
  
  real beta_t[N_years];
  real<lower=1e-6> beta_t_sd;

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

    //print(d_latent[tree,1]);

    for (year in 2:N_years) {

      d_latent[tree,year] = d_latent[tree, year-1] + 2 * x[tree, year] / 10 ;

      //print(d_latent[tree,year]);

    }
    // print(d_latent[tree,]);

  }

}
model {

  beta0     ~ normal(0, 1.0/0.00001);
  sig_x_obs ~ uniform(1e-6, 2.0);
  //sig_d_obs ~ uniform(1e-6, 10);

  sig_x  ~ uniform(1e-6, 1000);
  beta_sd   ~ uniform(1e-6, 1000);
  beta_t_sd ~ uniform(1e-6, 1000);

  // print(beta0);
  
    
  for(tree in 1:N_trees) {
    d_init[tree] ~ uniform(0, 10);
    // print(d_init[tree]);
    beta[tree] ~ normal(beta0, beta_sd);
  }
  
  for(year in 1:N_years) {
    beta_t[year] ~ normal(0, beta_t_sd);
  }
  
  for (tree in 1:N_trees) {
    //print(tree);
    //print(x[tree,]);
    for (year in 1:N_years) {
      x[tree, year] ~ lognormal(beta[tree] + beta_t[year], sig_x); // beta_k[tree2species[tree]]
    }
  }

  /* for (core in 1:N_cores) { */
  /*   for (year in rw_year_start[core]:rw_year_end[core]) { */
  /*     if (logy[core,year] == -999){ */
  /*     } else { */
  /*       logy[core, year] ~ normal(log(x[core2tree, year]), sig_x_obs); */
  /*     } */
  /*   } */
  /* } */

  /* for (tree in 1:N_cores) { */
  /*   for (year in 1:N_years) { */
  /*   // for (year in rw_year_start[core]:rw_year_end[core]) { */
  /*     if (logy[tree,year] == -999){ */
  /*     } else { */
  /*       logy[tree, year] ~ normal(log(x[tree, year]), sig_x_obs); */
  /*     } */
  /*   } */
  /* } */

  /* for (core in 1:N_cores) { */
  /*   for (year in 1:N_years) { */
  /*   // for (year in rw_year_start[core]:rw_year_end[core]) { */
  /*     if (logy[core,year] == -999){ */
  /*     } else { */
  /*       logy[core, year] ~ normal(log(x[core2tree[core], year]), sig_x_obs); */
  /*     } */
  /*   } */
  /* } */


  for (tree in 1:N_trees) {
    for (year in 1:N_years) {
    // for (year in rw_year_start[core]:rw_year_end[core]) {
      if (logy[tree,year] == -999){
      } else {
	// print(logy[tree,year]);
	// print(x[tree,year]);
	//print(x[;
        // logy[tree, year] ~ normal(log(x[core2tree[tree], year]), sig_x_obs);
	logy[tree, year] ~ normal(log(x[tree, year]), sig_x_obs);
      }
    }
  }
  
  /* for (tree in 1:N_trees) { */
  /*   for (year in 1:N_years) { */
  /*     log(d[tree, year]) ~ student_t(3, log(d_latent[tree, year]), sig_d); */
  /*   } */
  /* } */

  for (i in 1:N_dbh) {
    log(d[i]) ~ student_t(3.0, log(d_latent[d2tree[i],d2year[i]]), sig_d_obs);
  }
}
