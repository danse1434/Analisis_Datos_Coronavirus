// Modelo de Inhibión de Hill
data {
  int<lower=0> N; 
  int<lower=0> Npred;
  real x[N]; 
  real Y[N]; 
  real xpred[Npred];
  
  // Previos
  real muImax;
  real<lower=0> sdImax;
  
  real muIC50;
  real<lower=0> sdIC50;
  
  real mugamma;
  real<lower=0> sdgamma;
  
  real<lower=0> scatau;
  real<lower=0> loctau;
} 
parameters {
  real<lower=0.00, upper=1.5> Imax; // Inhibición máxima
  real<lower=0.00, upper=100> IC50; // conc. inhibitoria media
  real<lower=0.00, upper=5.0> gamma; // Coef. Hill
  real<lower=0> tau; 
} 
transformed parameters {
  real sigma; 
  real m[N];
  for (i in 1:N)  
  m[i] = Imax / (1 + ((x[i]/IC50)^gamma)); // Modelo IC50 por GraphPad
  sigma = 1 / sqrt(tau); 
} 
model {
  // Hiperparámetros
  Imax ~ normal(muImax, sdImax); 
  IC50 ~ normal(muIC50, sdIC50);
  gamma ~ normal(mugamma, sdgamma);
  tau ~ gamma(scatau, loctau); 
  // Verosimilitud
  Y ~ normal(m, sigma);   
}
generated quantities{
  real Y_mean[Npred]; 
  real Y_pred[Npred]; 
  for(i in 1:Npred){
    // Distribución de parámetros a posteriori de la media
    Y_mean[i] = Imax / (1 + ((xpred[i]/IC50)^gamma)); //Modelo de simulación
    // Distribución predictiva a posteriori 
    Y_pred[i] = normal_rng(Y_mean[i], sigma);   
  }
}
