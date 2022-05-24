// Modelo de Ejemplo 1
//
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> J;         // número de escuelas 
  real y[J];              // efectos de tratamientos estándar
  real<lower=0> sigma[J]; // SE de efectos estimados
}

// Los parámetros aceptados por el modelo
// acepta dos parámetors 'mu' y 'sigma'
parameters {
  real mu;                // Efecto de tratamiento poblacional
  real<lower=0> tau;      // SE en efectos de tratamientos
  vector[J] eta;          // Desviación no escalada de mu por escuela
}
// Parámetros transformados
transformed parameters {
  vector[J] theta = mu + tau * eta; // Efectos de tratamiento de escuela
}

model {
  target += normal_lpdf(eta | 0,1); // log-densidad previa
  target += normal_lpdf(y | theta, sigma); // log-verosimilitud
}

