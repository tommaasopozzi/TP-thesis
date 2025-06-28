#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <ctime>  


using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// Funzione per campionare da una dirichlet in cpp
// [[Rcpp::export]]
arma::vec rdirichlet_cpp(const arma::vec& alpha) {
  int k = alpha.n_elem;
  arma::vec result(k);
  
  // Campiono da una gamma
  for(int i = 0; i < k; i++) {
    result(i) = R::rgamma(alpha(i), 1.0);
  }
  
  result = result / sum(result);
  
  return result;
}

// [[Rcpp::export]]
List gibbs_lcm_standard_cpp(const arma::mat& Y, int G, int niter, int nburn, 
                            double b, double c, int progress_interval = 50, bool save_probs = true){
  
  int n = Y.n_rows;
  int d = Y.n_cols;
  
  // Trovo la categoria massima per ogni variabile
  arma::vec mj(d);
  for(int j = 0; j < d; j++) {
    mj(j) = Y.col(j).max();
  }
  int max_mj = mj.max();
  
  Rcpp::Rcout << "Dataset: " << n << " observations, " << d << " variables\n";
  
  //Inizializzazione
  arma::mat out_cluster(niter, n);
  arma::mat out_tau(niter, G); 
  arma::cube out_alpha(d, max_mj, G); 
  arma::field<arma::cube> all_alpha(niter);
  arma::field<arma::cube> out_prob(niter);
  
 
  for(int m = 0; m < niter; m++) {
    all_alpha(m) = arma::cube(d, max_mj, G, arma::fill::zeros);
    out_prob(m) = arma::cube(n, G, 1, arma::fill::zeros);
  }
  
  // Inizializzo i parametri 
  arma::cube alpha(d, max_mj, G, arma::fill::zeros); //Creo un cubo a 3 dimensioni con d colonne, max(mj) righe per G gruppi 
  for(int g = 0; g < G; g++) { //loop sulle classi latenti 
    for(int j = 0; j < d; j++) { //loop sulle variabili 
      arma::vec alpha_init(mj(j), arma::fill::ones); //campiono i valori iniziali da una Dirichlet(1) (uniforme)
      arma::vec alpha_j = rdirichlet_cpp(alpha_init);
      for(uword k = 0; k < mj(j); k++) {
        alpha(j, k, g) = alpha_j(k); //salvo nel cubo
      }
    }
  }
  
  // Iniziallizzo mixing proportions
  arma::vec tau = rdirichlet_cpp(arma::vec(G, arma::fill::ones));
  
  // Inizializzo assegnazione ai cluster 
  arma::vec z(n);
  for(int i = 0; i < n; i++) {
    z(i) = R::runif(0, 1) * G + 1;  // Assegnazione casuale da 1 a G
  }
  z = floor(z);  
  
  Rcpp::Rcout << "Start Gibbs sampling...\n";
  Rcpp::Rcout << "Parameters: G=" << G << " groups, niter=" << niter 
        << " iterations, hyperparameters b=" << b << ", c=" << c << "\n";
  
  
  // Gibbs
  for(int m = 0; m < niter; m++) {
    
    if (m % 1000 == 0) Rcpp::Rcout << "\033cIteration " << m << " / " << niter << "\n";
    
    //Aggiorno l'assegnazione ai cluster per ciascuna osservazione
    for(int i = 0; i < n; i++) {// loop sulle osservazioni
      arma::vec temp_prob(G, arma::fill::zeros);
      
      for(int g = 0; g < G; g++) { //loop sui cluster 
        double log_prob = log(tau(g));
        
        for(int j = 0; j < d; j++) {
          
          int y_ij = Y(i, j);
          if(y_ij > 0 && y_ij <= mj(j)) {
            log_prob += log(alpha(j, y_ij-1, g)); 
          }
        }
        
        temp_prob(g) = exp(log_prob); //aggiorno le probabilità
      }
      
      // Normalizzo 
      temp_prob = temp_prob / sum(temp_prob);
      
      // Campiono le nuova allocazione nei cluster 
      double u = R::runif(0, 1); //Campiono con il metodo dell'inverse CDF per ridurre i tempi computazionali
      double cumsum = 0; //in questo modo riesco a campionare da una categorical distribution evitando di passare 
      int new_z = 0; //dalla funzione sample di R
      
      for(int g = 0; g < G; g++) {
        cumsum += temp_prob(g);
        if(u <= cumsum) {
          new_z = g + 1;
          break;
        }
      }
      
      // Assegnazione al nuovo cluster 
      z(i) = new_z;
      
      // Salvo le probabilities per stephens alghoritm
      for(int g = 0; g < G; g++) {
        out_prob(m)(i, g, 0) = temp_prob(g);
      }
    }
    
    // Aggiornamento dei parametri alpha
    for(int g = 0; g < G; g++) { //loop per le variabili latenti 
      for(int j = 0; j < d; j++) { //loop per le variabili 
        arma::vec counts(mj(j), arma::fill::zeros); //Inizializzo il conteggio a zero per la variabili j 
        //sarebbe un vettore di dimensione m_j con tutti zero 
        for(int i = 0; i < n; i++) { //loop sulle osservazioni per effettuare il conteggio 
          if(z(i) == g+1 && arma::is_finite(Y(i, j))) {  //Conta, per ogni osservazione i, quante volte appare ciascun livello della variabile j, nel cluster g
            int y_ij = Y(i, j); //valore osservato 
            if(y_ij > 0 && y_ij <= mj(j)) {
              counts(y_ij-1) += 1;  
            }
          }
        }
        
        // Aggiornamento parametri della distribuzione degli alpha
        arma::vec new_params_alpha = c + counts;
        arma::vec new_alpha = rdirichlet_cpp(new_params_alpha);
        
        // Salvo gli alpha (necessario?)
        for(uword k = 0; k < mj(j); k++) {
          alpha(j, k, g) = new_alpha(k);
        }
      }
    }
    
    // Aggiorno i parametri tau 
    arma::vec n_g(G, arma::fill::zeros);
    for(int i = 0; i < n; i++) {
      n_g(z(i)-1) += 1; // conto quante osservazioni per ciascun cluster
    }
    
    tau = rdirichlet_cpp(b + n_g);
    
    // Salvo i risulti nelle matrici di output
    out_cluster.row(m) = z.t();
    out_tau.row(m) = tau.t();
    all_alpha(m) = alpha;
    
  
  }
  
  // Restituisco i risultati
  arma::mat cluster = out_cluster.rows(nburn, niter-1);
  arma::mat tau_res = out_tau.rows(nburn, niter-1);
  
  // Extract alpha slices after burn-in
  arma::field<arma::cube> alpha_res(niter-nburn);
  arma::field<arma::cube> prob_res(niter-nburn);
  for(int m = 0; m < niter-nburn; m++) {
    alpha_res(m) = all_alpha(m + nburn);
    prob_res(m) = out_prob(m + nburn);
  }
  
  return List::create(
    Named("cluster") = cluster,
    Named("alpha") = alpha_res,
    Named("tau") = tau_res,
    Named("prob") = prob_res
  );
}


// [[Rcpp::export]]
double kronecker_delta(double x, double y) {
  return x == y ? 1.0 : 0.0;
}

// [[Rcpp::export]]
double log_sum_exp(const arma::vec& log_probs) {
  double max_log = log_probs.max();
  return max_log + std::log(sum(exp(log_probs - max_log)));
}

template <typename T>
T clamp_local(T x, T lower, T upper) {
  return std::min(std::max(x, lower), upper);
}

// [[Rcpp::export]]
arma::vec pz_given_y(const arma::vec& y_i, const arma::vec& tau, 
                     const arma::mat& a, const arma::mat& epsilon, 
                     const arma::vec& m_levels) {
  int G = tau.n_elem;
  int J = m_levels.n_elem;
  
  arma::vec log_probs(G);
  const double min_eps = 1e-10;
  
  for (int g = 0; g < G; g++) {
    double logp = std::log(std::max(tau(g), min_eps));
    
    for (int j = 0; j < J; j++) {
      double delta = kronecker_delta(y_i(j), a(g, j));
      double eps = clamp_local(epsilon(g, j), min_eps, 1.0 - min_eps);
      double m_j = std::max(m_levels(j), 2.0);
      
      
      double prob_match = eps;
      double prob_nomatch = (1.0 - eps) / (m_j - 1.0);
      
      if (delta > 0.5) { 
        logp += std::log(std::max(prob_match, min_eps));
      } else { 
        logp += std::log(std::max(prob_nomatch, min_eps));
      }
    }
    
    log_probs(g) = logp;
    
    if (!std::isfinite(log_probs(g))) {
      log_probs(g) = -1e6; // valore molto negativo ma finito
    }
  }
  
  double logZ = log_sum_exp(log_probs);
  arma::vec probs = arma::exp(log_probs - logZ);
  
  for (int g = 0; g < G; g++) {
    if (!std::isfinite(probs(g)) || probs(g) < 0) {
      probs(g) = min_eps;
    }
  }
  
  double sum_probs = arma::sum(probs);
  if (sum_probs <= 0 || !std::isfinite(sum_probs)) {
    probs.fill(1.0 / G); // probabilità uniformi se tutto fallisce
  } else {
    probs /= sum_probs;
  }
  
  return probs;
}

// [[Rcpp::export]]
double rtrunc_beta_inv(double shape1, double shape2, double a = 0.0, double b = 1.0) {
  if (shape1 <= 0.0 || shape2 <= 0.0 || !std::isfinite(shape1) || !std::isfinite(shape2)) {
    Rcpp::warning("shape1 and shape2 must be finite and > 0");
    return 0.5; 
  } 
  
  if (a < 0.0 || b > 1.0 || a >= b) {
    Rcpp::warning("Invalid truncation interval: check that 0 <= a < b <= 1");
    return 0.5;
  } 
  
  double p_a = R::pbeta(a, shape1, shape2, 1, 0);
  double p_b = R::pbeta(b, shape1, shape2, 1, 0);
  
  if (!std::isfinite(p_a) || !std::isfinite(p_b) || p_a >= p_b || (p_b - p_a) < 1e-10) {
    return (a + b) / 2.0; 
  }
  
  double u = R::runif(p_a, p_b);
  double q = R::qbeta(u, shape1, shape2, 1, 0);
  
  if (!std::isfinite(q) || q < a || q > b) {
    return (a + b) / 2.0;
  }
  
  return q;
}

// [[Rcpp::export]]
arma::vec rdirichlet(const arma::vec& alpha) {
  int n = alpha.n_elem;
  arma::vec sample(n);
  
  double sum_sample = 0.0;
  
  for (int i = 0; i < n; i++) {
    double alpha_i = std::max(alpha(i), 1e-6);
    sample(i) = R::rgamma(alpha_i, 1.0);
    if (!std::isfinite(sample(i)) || sample(i) <= 0) {
      sample(i) = 1e-6;
    }
    sum_sample += sample(i);
  }
  
  if (sum_sample <= 0 || !std::isfinite(sum_sample)) {
    sample.fill(1.0 / n);
  } else {
    sample = sample / sum_sample;
  }
  
  return sample;
}

// [[Rcpp::export]]
List epsilon_g_j_cpp(const arma::mat& Y, int G, double b, double c, 
                     int niter, int nburn, bool save_probs = true) {
  int n = Y.n_rows;
  int d = Y.n_cols;
  
  arma::vec mj(d);
  for (int j = 0; j < d; j++) {
    mj(j) = Y.col(j).max();
  }
  
  arma::mat c_out = arma::zeros(niter, n);
  
  arma::cube all_probs;
  if (save_probs) {
    all_probs = arma::cube(n, G, niter, arma::fill::zeros);
  }
  
  arma::mat a(G, d);
  for (int g = 0; g < G; g++) {
    for (int j = 0; j < d; j++) {
      // Campiono uniformemente tra 1 e mj[j]
      a(g, j) = std::floor(R::runif(1.0, mj(j) + 1.0));
      a(g, j) = std::max(1.0, std::min(a(g, j), mj(j)));
    }
  }
  
  arma::vec tau = rdirichlet(arma::ones(G));
  
  arma::mat eps(G, d);
  for (int g = 0; g < G; g++) {
    for (int j = 0; j < d; j++) {
      double upper_bound = (mj(j) - 1.0) / mj(j);
      eps(g, j) = rtrunc_beta_inv(1.0, 1.0, 0.01, std::min(0.99, upper_bound - 0.01));
    }
  }
  
  std::vector<arma::mat> a_history;
  std::vector<arma::mat> eps_history;
  std::vector<arma::vec> tau_history;
  
  if (save_probs) {
    a_history.push_back(a);
    eps_history.push_back(eps);
    tau_history.push_back(tau);
  }
  
  for (int m = 0; m < niter; m++) {
    arma::mat temp_prob(n, G);
    
    for (int i = 0; i < n; i++) {
      arma::vec y_i = Y.row(i).t();
      arma::vec probs = pz_given_y(y_i, tau, a, eps, mj);
      temp_prob.row(i) = probs.t();
      
      if (save_probs) {
        for (int g = 0; g < G; g++) {
          all_probs(i, g, m) = probs(g);
        }
      }
      
      double sum_probs = arma::sum(probs);
      if (sum_probs <= 0 || !std::isfinite(sum_probs)) {
        probs.fill(1.0 / G);
      }
      
      NumericVector prob_r = wrap(probs);
      IntegerVector range = seq_len(G);
      int cluster = as<int>(sample(range, 1, false, prob_r)) - 1;  
      c_out(m, i) = cluster + 1; 
    }
    
    arma::vec n_g = arma::zeros(G);
    for (int g = 0; g < G; g++) {
      for (int i = 0; i < n; i++) {
        if (c_out(m, i) == g + 1) {
          n_g(g)++;
        }
      }
    }
    
    for (int g = 0; g < G; g++) {
      if (n_g(g) == 0) continue;
      
      for (int j = 0; j < d; j++) {
        double v_gj = 0.0;
        for (int i = 0; i < n; i++) {
          if (c_out(m, i) == g + 1 && Y(i, j) == a(g, j)) {
            v_gj += 1.0;
          }
        }
        
        
        double shape1 = std::max(1e-6, n_g(g) - v_gj + (mj(j) - 1.0) * (c - 1.0) + 1.0);
        double shape2 = std::max(1e-6, v_gj + c);
        double upper_bound = (mj(j) - 1.0) / mj(j);
        
        eps(g, j) = rtrunc_beta_inv(shape1, shape2, 0.01, std::min(0.99, upper_bound - 0.01));
        
        arma::vec counts = arma::zeros(mj(j));
        arma::vec gamma_gj = arma::zeros(mj(j));
        
        for (int h = 0; h < mj(j); h++) {
          for (int i = 0; i < n; i++) {
            if (c_out(m, i) == g + 1 && Y(i, j) == h + 1) {
              counts(h) += 1.0;
            }
          }
          
          // CORREZIONE 10: Calcolo più stabile del gamma
          double base = ((mj(j) - 1.0) * (1.0 - eps(g, j))) / eps(g, j);
          base = std::max(base, 1e-10);
          gamma_gj(h) = std::pow(base, counts(h));
          
          if (!std::isfinite(gamma_gj(h))) {
            gamma_gj(h) = 1e-10;
          }
        }
        
        double sum_gamma = arma::sum(gamma_gj);
        if (sum_gamma <= 0 || !std::isfinite(sum_gamma)) {
          gamma_gj.fill(1.0 / mj(j));
        } else {
          gamma_gj = gamma_gj / sum_gamma;
        }
        
        NumericVector rho_r = wrap(gamma_gj);
        IntegerVector range = seq_len(mj(j));
        int new_a = as<int>(sample(range, 1, false, rho_r));
        a(g, j) = new_a;
      }
    }
    
    
    arma::vec dirichlet_params = arma::ones(G) * std::max(b, 1e-6) + n_g;
    tau = rdirichlet(dirichlet_params);
    
    if (save_probs) {
      a_history.push_back(a);
      eps_history.push_back(eps);
      tau_history.push_back(tau);
    }
    
    if ((m + 1) % 100 == 0) {
      Rcpp::Rcout << "Iteration " << (m + 1) << "/" << niter << std::endl;
    }
  }
  
  arma::mat cluster_out = c_out.rows(nburn, niter - 1);
  
  List result = List::create(
    Named("cluster") = cluster_out,
    Named("tau") = tau,
    Named("a") = a,
    Named("epsilon") = eps
  );
  
  if (save_probs) {
    arma::cube probs_out = all_probs.slices(nburn, niter - 1);
    result["cluster_probs"] = probs_out;
    
    List a_list(niter - nburn);
    List eps_list(niter - nburn);
    List tau_list(niter - nburn);
    
    for (int i = nburn; i < niter; i++) {
      a_list[i - nburn] = a_history[i];
      eps_list[i - nburn] = eps_history[i];
      tau_list[i - nburn] = tau_history[i];
    }
    
    result["a_history"] = a_list;
    result["epsilon_history"] = eps_list;
    result["tau_history"] = tau_list;
  }
  
  return result;
}

// [[Rcpp::export]]
List run_parsimonious_model(const NumericMatrix& Y_r, int G, double b, double c, 
                            int niter, int nburn) {
  arma::mat Y = as<arma::mat>(Y_r);
  return epsilon_g_j_cpp(Y, G, b, c, niter, nburn);
}


// [[Rcpp::export]]
arma::vec rdirichlet_cpp(const arma::vec& alpha) {
  int k = alpha.n_elem;
  arma::vec result(k);
  
  for(int i = 0; i < k; i++) {
    result(i) = R::rgamma(alpha(i), 1.0);
  }
  
  double sum_result = arma::sum(result);
  if (sum_result == 0.0) {.
    result.fill(1.0 / k);
  } else {
    result = result / sum_result;
  }
  
  return result;
}

// [[Rcpp::export]]
Rcpp::List gibbs_lcm_standard_tune_cpp(const arma::mat& Y, int G, int niter, int nburn, 
                                       const arma::vec& hyper_b,       
                                       const arma::cube& hyper_c,     
                                       int progress_interval = 50, bool save_probs = true){
  
  
  int n = Y.n_rows;
  int d = Y.n_cols;
  
  arma::vec mj(d);
  for(int j = 0; j < d; j++) {
    mj(j) = Y.col(j).max();
  }
  int max_mj = mj.max();
  
  Rcpp::Rcout << "Dataset: " << n << " observations, " << d << " variables\n";
  
  //Inizializzo le matrici contenti i cluster 
  arma::mat out_cluster(niter, n);
  arma::mat out_tau(niter, G); //inizializzazione del vettore dei parametri tau
  arma::field<arma::cube> all_alpha(niter);
  arma::field<arma::cube> out_prob(niter); 
  
  for(int m = 0; m < niter; m++) {
    all_alpha(m) = arma::cube(d, max_mj, G, arma::fill::zeros);
    if (save_probs) {
      out_prob(m) = arma::cube(n, G, 1, arma::fill::zeros);
    }
  }
  
  // Inizializzo i parametri 
  arma::cube alpha(d, max_mj, G, arma::fill::zeros); //Creo un cubo a 3 dimensioni con d righe, max(mj) colonne per G slice
  for(int g = 0; g < G; g++) { //loop sulle classi latenti 
    for(int j = 0; j < d; j++) { //loop sulle variabili 
      arma::vec alpha_init_params(mj(j), arma::fill::ones); //campiono i valori iniziali da una Dirichlet(1) (uniforme)
      arma::vec alpha_j = rdirichlet_cpp(alpha_init_params);
      for(arma::uword k = 0; k < mj(j); k++) {
        alpha(j, k, g) = alpha_j(k); //salvo nel cubo
      }
    }
  }
  
  // Iniziallizzo mixing proportions
  arma::vec tau_init_params(G, arma::fill::ones);
  arma::vec tau = rdirichlet_cpp(tau_init_params);
  
  // Inizializzo assegnazione ai cluster 
  arma::vec z(n);
  for(int i = 0; i < n; i++) {
    z(i) = static_cast<int>(R::runif(0, G-1) + 1.0); // Assegnazione casuale da 1 a G
  }
  
  Rcpp::Rcout << "Start Gibbs sampling...\n";
  Rcpp::Rcout << "Parameters: G=" << G << " groups, niter=" << niter 
              << " iterations.\nHyperparameters for tau and alpha are now vectors/cubes.\n"; 
  
  // Main Gibbs sampling loop
  for(int m = 0; m < niter; m++) {
    
    if (m > 0 && m % progress_interval == 0) { // Modificato per usare progress_interval
      Rcpp::Rcout << "Iteration " << m << " / " << niter << "\n";
      Rcpp::checkUserInterrupt(); // Permette di interrompere il loop da R
    }
    
  
    for(int i = 0; i < n; i++) {
      arma::vec temp_log_prob(G, arma::fill::zeros);
      
      for(int g = 0; g < G; g++) {  
        double current_log_prob_val = log(tau(g));
        
        for(int j = 0; j < d; j++) {
          int y_ij = static_cast<int>(Y(i, j)); 
          if(y_ij > 0 && y_ij <= mj(j)) {
            if (alpha(j, y_ij-1, g) > 0) { 
              current_log_prob_val += log(alpha(j, y_ij-1, g)); 
            } else {
              current_log_prob_val += -std::numeric_limits<double>::infinity(); // Penalize if alpha is zero
            }
          }
        
        }
        temp_log_prob(g) = current_log_prob_val;
      }
      
      // Normalizzo in modo numericamente stabile (log-sum-exp trick)
      double max_log_prob = temp_log_prob.max();
      arma::vec temp_prob = arma::exp(temp_log_prob - max_log_prob);
      temp_prob = temp_prob / sum(temp_prob);
      
      // Campiono le nuova allocazione nei cluster 
      double u_rand = R::runif(0, 1); 
      double cumsum_prob = 0; 
      int new_z_val = 0; 
      
      for(int g = 0; g < G; g++) {
        cumsum_prob += temp_prob(g);
        if(u_rand <= cumsum_prob) {
          new_z_val = g + 1; 
          break;
        }
      }
      if (new_z_val == 0) new_z_val = G; 
      
      z(i) = new_z_val;
      
      if (save_probs) {
        for(int g = 0; g < G; g++) {
          out_prob(m)(i, g, 0) = temp_prob(g);
        }
      }
    }
    
    // Aggiornamento dei parametri alpha
    for(int g = 0; g < G; g++) { //loop per le classi latenti 
      for(int j = 0; j < d; j++) { //loop per le variabili 
        arma::vec counts(mj(j), arma::fill::zeros); 
        for(int i = 0; i < n; i++) { 
          if(z(i) == (g+1) && arma::is_finite(Y(i, j))) { 
            int y_ij = static_cast<int>(Y(i, j)); 
            if(y_ij > 0 && y_ij <= mj(j)) {
              counts(y_ij-1) += 1;  
            }
          }
        }
        
        // Estrai i parametri di hyper_c per la variabile j, classe g corrente
        arma::vec current_hyper_c_params(mj(j));
        for(arma::uword k_cat = 0; k_cat < mj(j); k_cat++) {
          current_hyper_c_params(k_cat) = hyper_c(j, k_cat, g);
        }
        
        arma::vec new_params_alpha_posterior = current_hyper_c_params + counts;
        arma::vec new_alpha_j_g = rdirichlet_cpp(new_params_alpha_posterior);
        
        for(arma::uword k_cat = 0; k_cat < mj(j); k_cat++) {
          alpha(j, k_cat, g) = new_alpha_j_g(k_cat);
        }
        // I valori di alpha(j, k_cat, g) per k_cat >= mj(j) rimarranno zero (dall'inizializzazione)
      }
    }
    
    // Aggiorno i parametri tau 
    arma::vec n_g(G, arma::fill::zeros);
    for(int i = 0; i < n; i++) {
      n_g(z(i)-1) += 1; // conto quante osservazioni per ciascun cluster
    }
    
    tau = rdirichlet_cpp(hyper_b + n_g); 
    
    // Salvo i risulti nelle matrici di output
    out_cluster.row(m) = z.t();
    out_tau.row(m) = tau.t();
    all_alpha(m) = alpha;
    
  }
  
  Rcpp::Rcout << "Gibbs sampling completed.\n";
  
  
  arma::mat cluster_res = out_cluster.rows(nburn, niter-1);
  arma::mat tau_res = out_tau.rows(nburn, niter-1);
  
  int num_saved_iters = niter - nburn;
  arma::field<arma::cube> alpha_res(num_saved_iters);
  arma::field<arma::cube> prob_res; 
  
  if (save_probs) {
    prob_res.set_size(num_saved_iters);
  }
  
  for(int iter_idx = 0; iter_idx < num_saved_iters; iter_idx++) {
    alpha_res(iter_idx) = all_alpha(iter_idx + nburn);
    if (save_probs) {
      prob_res(iter_idx) = out_prob(iter_idx + nburn);
    }
  }
  
  Rcpp::List results = Rcpp::List::create(
    Rcpp::Named("cluster") = cluster_res,
    Rcpp::Named("alpha") = alpha_res,
    Rcpp::Named("tau") = tau_res
  );
  
  if (save_probs) {
    results["prob"] = prob_res;
  }
  
  return results;
}



double log_sum_exp_cpp(const arma::vec& log_probs) {
  if (log_probs.empty()) {
    return -arma::datum::inf; 
  }
  
  if (log_probs.has_nan()) {
    return arma::datum::nan;
  }
  
  double max_val = log_probs.max();
  
  if (max_val == arma::datum::inf) {
    return arma::datum::inf;
  }
  if (max_val == -arma::datum::inf) {
    return -arma::datum::inf;
  }
  

  double sum_exp_terms = arma::sum(arma::exp(log_probs - max_val));
  
  if (sum_exp_terms <= 0.0) { 

    return -arma::datum::inf;
  }
  return max_val + std::log(sum_exp_terms);
}


// [[Rcpp::export]]
arma::mat calculate_log_likelihood_matrix_cpp(
    const arma::field<arma::cube>& alpha_samples, 
    const arma::mat& tau_samples,                
    const arma::mat& Y,                           
    const arma::vec& mj                          
) {
  
  int S_samples = static_cast<int>(tau_samples.n_rows);
  int G_classes = static_cast<int>(tau_samples.n_cols);
  int N_obs = static_cast<int>(Y.n_rows);
  int D_vars = static_cast<int>(Y.n_cols);
  
  if (alpha_samples.n_elem != static_cast<arma::uword>(S_samples)) {
    Rcpp::stop("Il numero di campioni alpha non corrisponde al numero di campioni tau.");
  }
  
  arma::mat log_lik_matrix(S_samples, N_obs, arma::fill::zeros);
  
  for (int s = 0; s < S_samples; ++s) {
    const arma::cube& current_alpha = alpha_samples(s); // Cubo: (var_idx, cat_idx_0based, class_idx)
    arma::rowvec current_tau = tau_samples.row(s);    // Riga: (1 x G)
    

    if (current_alpha.n_slices != static_cast<arma::uword>(G_classes) || 
        current_alpha.n_rows != static_cast<arma::uword>(D_vars)) {
      Rcpp::Rcerr << "Attenzione: discrepanza di dimensioni per l'esempio alpha " << s + 1 
                  << ". G atteso: " << G_classes << ", G trovato: " << current_alpha.n_slices
                  << ". D atteso: " << D_vars << ", D trovato: " << current_alpha.n_rows
                  << ". Riga saltata." << std::endl;
      log_lik_matrix.row(s).fill(arma::datum::nan); 
      continue;
    }
    
    for (int i = 0; i < N_obs; ++i) {
      arma::vec log_probs_obs_i_given_class(G_classes);
      
      for (int g = 0; g < G_classes; ++g) {
        double current_tau_g = current_tau(g);
        double log_p_joint_sg; // log P(y_i, z_i=g | theta_s)
        
        if (current_tau_g <= 0.0 || !arma::is_finite(current_tau_g)) {
          log_p_joint_sg = -arma::datum::inf;
        } else {
          log_p_joint_sg = std::log(current_tau_g);
        }
        
        for (int j = 0; j < D_vars; ++j) {
          if (log_p_joint_sg == -arma::datum::inf) {
            break; // La probabilità per questa classe g è già zero
          }
          
          int y_ij_val = static_cast<int>(Y(i, j)); // Y è 1-indexed
          int current_mj = static_cast<int>(mj(j));
          
          // current_alpha è (var_idx, cat_idx_0based, class_idx)
          if (y_ij_val >= 1 && y_ij_val <= current_mj) {
            // y_ij_val - 1 per l'indicizzazione 0-based della categoria in alpha
            double alpha_val = current_alpha(j, y_ij_val - 1, g); 
            if (alpha_val <= 0.0 || !arma::is_finite(alpha_val)) {
              log_p_joint_sg = -arma::datum::inf;
              break; 
            }
            log_p_joint_sg += std::log(alpha_val);
          } else {
            // Valore Y(i,j) non valido o fuori range atteso per la variabile j
            log_p_joint_sg = -arma::datum::inf;
            break;
          }
        }
        log_probs_obs_i_given_class(g) = log_p_joint_sg;
      }
      // log_lik_matrix(s, i) è log P(y_i | theta_s)
      log_lik_matrix(s, i) = log_sum_exp_cpp(log_probs_obs_i_given_class);
    }
  }
  return log_lik_matrix;
}

// [[Rcpp::export]]
Rcpp::List calculate_waic_cpp(const arma::mat& log_lik_matrix) {
  if (log_lik_matrix.empty() || log_lik_matrix.n_rows == 0 || log_lik_matrix.n_cols == 0) {
    Rcpp::warning("Matrice di log-verosimiglianza vuota o non valida fornita a calculate_waic_cpp.");
    return Rcpp::List::create(
      Rcpp::Named("waic") = arma::datum::nan,
      Rcpp::Named("p_waic") = arma::datum::nan,
      Rcpp::Named("lppd") = arma::datum::nan,
      Rcpp::Named("message") = "Input log_lik_matrix vuoto o non valido."
    );
  }
  
  int S_samples = log_lik_matrix.n_rows;
  int N_obs = log_lik_matrix.n_cols;
  
  if (S_samples <= 1) {
    Rcpp::warning("Il calcolo WAIC richiede più di 1 campione MCMC per la varianza.");
    return Rcpp::List::create(
      Rcpp::Named("waic") = arma::datum::nan,
      Rcpp::Named("p_waic") = arma::datum::nan,
      Rcpp::Named("lppd") = arma::datum::nan,
      Rcpp::Named("message") = "Necessari >1 campioni MCMC."
    );
  }
  
  arma::vec lppd_i(N_obs);
  arma::vec p_waic_i(N_obs);
  lppd_i.fill(arma::datum::nan); // Inizializza a NaN
  p_waic_i.fill(arma::datum::nan);
  
  for (int i = 0; i < N_obs; ++i) {
    arma::vec log_lik_col_i = log_lik_matrix.col(i);
    
    arma::uvec finite_indices = arma::find_finite(log_lik_col_i);
    
    if (finite_indices.n_elem == 0) {
      lppd_i(i) = -arma::datum::inf; 
      p_waic_i(i) = arma::datum::nan;
      // Rcpp::Rcout << "Attenzione: Osservazione " << i+1 << " non ha valori di log-verosimiglianza finiti." << std::endl;
      continue;
    }
    arma::vec current_col_finite = log_lik_col_i(finite_indices);
    int S_current_col = current_col_finite.n_elem;
    
    if (S_current_col == 0) { 
      lppd_i(i) = -arma::datum::inf; 
      p_waic_i(i) = arma::datum::nan;
      continue;
    }
    
    lppd_i(i) = log_sum_exp_cpp(current_col_finite) - std::log(static_cast<double>(S_current_col));
    
    if (S_current_col > 1) {
      p_waic_i(i) = arma::var(current_col_finite); // Varianza campionaria (normalizza per N-1)
    } else {
      p_waic_i(i) = arma::datum::nan; // Impossibile calcolare la varianza con <=1 campione finito
    }
  }
  
  // Filtra NaN prima di sommare, per i casi in cui alcune osservazioni non hanno prodotto valori validi
  double lppd_sum = arma::sum(lppd_i(arma::find_finite(lppd_i)));
  double p_waic_sum = arma::sum(p_waic_i(arma::find_finite(p_waic_i)));
  
  std::string message = "";
  if (!arma::is_finite(lppd_sum) || !arma::is_finite(p_waic_sum)){
    message = "lppd_sum o p_waic_sum non è finito. WAIC potrebbe essere NA/Inf.";
    Rcpp::warning(message);
  }
  
  double waic = -2.0 * lppd_sum + 2.0 * p_waic_sum;
  
  return Rcpp::List::create(
    Rcpp::Named("waic") = waic,
    Rcpp::Named("p_waic") = p_waic_sum,
    Rcpp::Named("lppd") = lppd_sum,
    Rcpp::Named("lppd_i") = lppd_i,     
    Rcpp::Named("p_waic_i") = p_waic_i, 
    Rcpp::Named("message") = message
  );
}









