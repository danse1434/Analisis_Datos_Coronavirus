// Modelo de Inhibión de Hill
data {
  int<lower=0> N; 
  int<lower=0> Npred;
  real x[N]; 
  real Y[N]; 
  real xpred[Npred];
  
} 
parameters {
  real<lower=0.00, upper=1> Imax; // Inhibición máxima
  real<lower=0.00, upper=100> IC50; // conc. inhibitoria media
  real gamma; // original gamma in the JAGS example  
  real<lower=0> tau; 
} 
transformed parameters {
  real sigma; 
  real m[N];
  for (i in 1:N)  //R ~ I(1 - Imax * (C / (C + IC50)))
    m[i] = 1 - (Imax * pow(x[i], gamma)/(pow(x[i], gamma) + IC50)); //Modelo Hill inhibitorio
  sigma = 1 / sqrt(tau); 
} 
model {
  // Hiperparámetros
  Imax ~ normal(1.0, 0.1); 
  IC50 ~ normal(2.8, 1.0); 
  gamma ~ normal(1.0, 0.5);
  tau ~ gamma(.0001, .0001); 
  // Verosimilitud
  Y ~ normal(m, sigma);   
}
generated quantities{
  real Y_mean[Npred]; 
  real Y_pred[Npred]; 
  for(i in 1:Npred){
    // Distribución de parámetros a posteriori de la media
    Y_mean[i] = 1 - (Imax * pow(xpred[i], gamma)/(pow(xpred[i], gamma) + IC50)); //Modelo de simulación
    // Distribución predictiva a posteriori 
    Y_pred[i] = normal_rng(Y_mean[i], sigma);   
}
}
