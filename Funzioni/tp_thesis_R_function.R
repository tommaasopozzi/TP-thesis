alpha_k_jh = function(k, h, m_j, delta){
  h_check <- ((k-1) %% m_j) + 1
  if(h_check == h){
    return(1 / m_j + (1 - delta)*(m_j-1)/m_j)
  }else{
    return((1-1/m_j-(1-delta)*(m_j-1)/m_j)/(m_j-1))
  }
}

prob_poLCA <- function(K, d, m_vec, delta = 0.1) {
  prob_list <- vector("list", d)
  
  for (j in 1:d) {
    m_j <- m_vec[j]
    mat <- matrix(0, nrow = K, ncol = m_j)
    for (k in 1:K) {
      for (h in 1:m_j) {
        mat[k, h] <- alpha_k_jh(k = k, h = h, m_j = m_j, delta = delta)
      }
    }
    prob_list[[j]] <- mat
  }
  
  return(prob_list)
}


gibbs_lcm_standard <- function(Y, G, niter, nburn, b, c, progress_interval = 50) {
  
  start_time <- Sys.time()
  
  n <- nrow(Y)
  d <- ncol(Y)
  mj <- apply(Y, 2, max)
  cat(sprintf("Dataset: %d observation, %d variable\n", n, d))
  out_cluster <- matrix(0, nrow = niter, ncol = n)
  out_tau <- matrix(0, nrow = niter, ncol = G)
  out_alpha <- array(0, dim = c(niter, d, max(mj), G))
  out_prob <- array(0, dim = c(niter, n, G))
  
  alpha <- array(0, dim = c(d, max(mj), G)) 
  for(g in 1:G) {
    for(j in 1:d) {
      alpha[j, 1:mj[j], g] <- rdirichlet(1, rep(1, mj[j])) #Inizio simulando con valori casuali dalla dirichlet
    }
  }
  
  tau <- rdirichlet(1, rep(1, G)) #Inizializzo con valori casuali dalla dirichlet 
  z <- sample(1:G, size = n, replace = TRUE, prob = tau) #Inizializzo le allocazioni in maniera casuale 
  cat("Start Gibbs sampling...\n")
  cat(sprintf("Parameters: G=%d gruppi, niter=%d iterations, hyperparameters b=%g, c=%g\n", 
              G, niter, b, c))
  pb <- txtProgressBar(min = 1, max = niter, style = 3)
  
  for(m in 1:niter) {
    
    if (m %% progress_interval == 0 || niter == 1) {
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      estimated_total <- elapsed * (niter / m)
      remaining <- estimated_total - elapsed
      
      cat("\nIterazione", m, "di", niter, 
          sprintf("(%.1f%%)", m/niter*100),
          sprintf("- Tempo trascorso: %.2f min, Stimato rimanente: %.2f min\n", 
                  as.numeric(elapsed), as.numeric(remaining)))
    }
    
    
    for(i in 1:n) {
      temp_prob <- rep(0, G)
      
      for(g in 1:G) {
        log_prob <- log(tau[g])
        for(j in 1:d) {
          if(!is.na(Y[i,j])) {
            log_prob <- log_prob + log(alpha[j, Y[i,j], g])
          }
        }
        temp_prob[g] <- exp(log_prob)
      }
      
      temp_prob <- temp_prob / sum(temp_prob)
      z[i] <- sample(1:G, size = 1, prob = temp_prob)
      out_prob[m, i, ] <- temp_prob
    }
    for(g in 1:G) {
      for(j in 1:d) {
        counts <- rep(0, mj[j])
        for(h in 1:mj[j]) {
          counts[h] <- sum(Y[z == g, j] == h, na.rm = TRUE)
        }
        alpha[j, 1:mj[j], g] <- rdirichlet(1, c + counts)
      }
    }
    
    n_g <- tabulate(z, nbins = G)
    tau <- rdirichlet(1, b + n_g)
    
    out_cluster[m, ] <- z
    out_tau[m, ] <- tau 
    out_alpha[m, , , ] <- alpha
    setTxtProgressBar(pb, m)
  }
  close(pb)
  
  return(list(
    cluster = out_cluster[-c(1:nburn), ],
    alpha = out_alpha[-c(1:nburn), , , ],
    tau = out_tau[-c(1:nburn), ],
    prob = out_prob[-c(1:nburn), , ]
  ))
}

# Funzione di supporto per il campionamento dalla distribuzione Dirichlet
rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  result <- matrix(0, nrow = n, ncol = k)
  
  for(i in 1:n) {
    gamma_samples <- rgamma(k, shape = alpha, scale = 1)
    result[i,] <- gamma_samples / sum(gamma_samples)
  }
  
  if(n == 1) {
    return(as.vector(result))
  } else {
    return(result)
  }
}


point_Binder <- function(partition_matrix){
  
  n <- ncol(partition_matrix)
  PSM <- matrix(0, ncol = n, nrow = n)
  dist_Binder <- c()
  
  for(i in 1:n){
    for(j in 1:i){
      PSM[i,j] <- PSM[j,i] <- mean(partition_matrix[,i] == partition_matrix[,j])
    }
  }
  for(i in 1:nrow(partition_matrix)){
    temp_mat <- matrix(as.numeric(sapply(partition_matrix[i,], 
                                         function(z) z == partition_matrix[i,])), ncol = n)
    dist_Binder[i] <- sum((temp_mat - PSM)^2)
  }
  point_estimate <- partition_matrix[which.min(dist_Binder),]
  
  return(point_estimate)
}

H <- function(partizione){
  N <- n
  kn <- max(partizione)
  nj <- c()
  for (j in 1:kn){
    nj[j] <- sum(partizione == j)
  }
  somma <- sum(nj*log(nj))
  log(N, base = 2) - 1/N*somma
}


kronecker_delta <- function(x, y) {
  as.numeric(x == y)
}


pz_given_y <- function(y_i, tau, a, epsilon, m_levels) {
  L <- length(tau)  # numero di classi
  G <- length(tau)
  J <- length(m_levels)  # numero di variabili (componenti in y_i)
  
  log_tau <- log(tau)
  log_probs <- numeric(G)
  for (g in 1:G) {
    logp <- log_tau[g]
    for (j in 1:J) {
      delta <- kronecker_delta(y_i[j], a[g,j])
      eps <- epsilon[g, j]
      m_j <- m_levels[j]
      
      term <- (eps / ((m_j - 1) * (1 - eps)))
      logp <- logp + log(1 - eps) + (1 - delta) * log(term)
    }
    log_probs[g] <- logp
  }
  
  logZ <- log_sum_exp(log_probs)
  probs <- exp(log_probs - logZ)
  return(probs)  # vettore di probabilità per ogni g
}


pz_given_y_2 <- function(y_i, tau, a, epsilon, m_levels) {
  L <- length(tau)  # numero di classi
  G <- length(tau)
  J <- length(m_levels)  # numero di variabili (componenti in y_i)
  
  log_tau <- log(tau)
  log_probs <- numeric(G)
  for (g in 1:G) {
    logp <- log_tau[g]
    for (j in 1:J) {
      delta <- kronecker_delta(y_i[j], a[j])
      eps <- epsilon[j]
      m_j <- m_levels[j]
      
      term <- (eps / ((m_j - 1) * (1 - eps)))
      logp <- logp + log(1 - eps) + (1 - delta) * log(term)
    }
    log_probs[g] <- logp
  }
  
  logZ <- log_sum_exp(log_probs)
  probs <- exp(log_probs - logZ)
  return(probs)  # vettore di probabilità per ogni g
}


log_sum_exp <- function(log_probs) {
  max_log <- max(log_probs)
  max_log + log(sum(exp(log_probs - max_log)))
}

rtrunc_beta_inv <- function(n, shape1, shape2, a = 0, b = 1) {
  # Controllo validità dei parametri
  if (shape1 <= 0 || shape2 <= 0) {
    warning("shape1 e shape2 devono essere > 0")
    return(rep(NA, n))
  }
  if (a < 0 || b > 1 || a >= b) {
    warning("Intervallo di troncamento non valido: controlla che 0 <= a < b <= 1")
    return(rep(NA, n))
  }
  
  # Calcolo dei quantili corrispondenti a a e b
  p_a <- pbeta(a, shape1, shape2)
  p_b <- pbeta(b, shape1, shape2)
  
  if (is.na(p_a) || is.na(p_b) || p_a >= p_b) {
    warning("Problema nei quantili: pbeta(a) >= pbeta(b) oppure NA")
    return(rep(NA, n))
  }
  
  # Campionamento uniforme nell'intervallo [p_a, p_b]
  u <- runif(n, p_a, p_b)
  q <- qbeta(u, shape1, shape2)
  
  return(q)
}



#Algoritmo modelli parsimoniosi 


epsilon_g_j <- function(Y, G, b, c, niter, nburn){
  n <- nrow(Y)
  d <- ncol(Y)
  mj <- apply(Y, 2, max)
  
  # Inizializzo le matrici in cui salvare i risultati 
  c_out <- matrix(0, nrow = niter, ncol = n)
  
  # Inizializzazione dei parametri 
  a <- matrix(NA, nrow = G, ncol = d)
  for(g in 1:G){
    for(j in 1:d){
      a[g,j] <- sample(1:mj[j], size = 1)
    }
  }
  
  # Initializzo tau
  tau <- rdirichlet(1, rep(1, G))
  
  # Initializzo epsilon  
  eps <- matrix(NA, nrow = G, ncol = d)
  for(g in 1:G){
    for(j in 1:d){
      eps[g, j] <- rtrunc_beta_inv(n = 1, shape1 = 1, shape2 = 1, a = 0, b = (mj[j]-1)/mj[j])
    }
  }
  
  for(m in 1:niter){
    #Calcolo delle probabilità per fare il sample dei cluster 
    temp_prob <- matrix(0, ncol = G, nrow = n)
    for(i in 1:n){
      temp_prob[i, ] <- pz_given_y(y_i = Y[i, ], tau = tau, a = a, epsilon = eps, m_levels = mj)
      c_out[m, i] <- sample(1:G, size = 1, prob = temp_prob[i, ])
    }
    
    #Inizializzo vettore per calcolare la numerosità di ciasun cluster
    n_g <- numeric(G)
    for(g in 1:G){
      n_g[g] = sum(c_out[m, ] == g) #Calcolo di n_g
    }
    
    
    for(g in 1:G){
      
      if(n_g[g] == 0) next #Salto l'iterazione se il cluster è vuoto, altrimenti riscontro problemi computazionali
      
      for(j in 1:d){
        
        v_gj <- sum(Y[c_out[m, ] == g, j] == a[g, j], na.rm = TRUE) #Calcolo di v_g^j
        
        
        eps[g, j] <- rtrunc_beta_inv(
          n = 1, 
          shape1 = n_g[g] - v_gj + (mj[j] - 1) * (c - 1) + 1, 
          shape2 = v_gj + c,
          a = 0, 
          b = (mj[j] - 1) / mj[j]
        )
        
        
        counts <- numeric(mj[j]) 
        gamma_gj <- numeric(mj[j])
        
        for(h in 1:mj[j]){
          counts[h] <- sum(Y[c_out[m, ] == g, j] == h, na.rm = TRUE) #calcolo di u_g^j
          gamma_gj[h] <- (((mj[j] - 1) * (1 - eps[g, j])) / (eps[g, j]))^(counts[h])
        }
        
        rho_gj <- gamma_gj / sum(gamma_gj) #calcolo di rho per campionare a
        a[g, j] <- sample(1:mj[j], size = 1, prob = rho_gj)
      }
    }
    
    
    tau <- rdirichlet(1, b + n_g) #campionamento dalla dirichlet per i tau 
    
    cat(paste("\014", m, "/", niter, "\n"))
  }
  
  
  return(list(
    cluster = c_out[(nburn+1):niter, ],
    tau = tau, #Ritorna solo l'ultimo tau (non essenziale)
    a = a, #Ritorna solo l'ultimo a (non essenziale)
    epsilon = eps #Ritorna solo l'ultimo epsilon (non essenziale)
  ))
}



epsilon_j <- function(Y, G, b, c, niter, nburn){
  n <- nrow(Y)
  d <- ncol(Y)
  mj <- apply(Y, 2, max)
  
  # Inizializzo le matrici in cui salvare i risultati 
  c_out <- matrix(0, nrow = niter, ncol = n)
  
  # Inizializzazione dei parametri 
  a <- numeric(d)
  for(j in 1:d){
      a[j] <- sample(1:mj[j], size = 1)
    }
  
  # Initializzo tau
  tau <- rdirichlet(1, rep(1, G))
  
  # Initializzo epsilon  
  eps <- numeric(d)
  for(j in 1:d){
      eps[j] <- rtrunc_beta_inv(n = 1, shape1 = 1, shape2 = 1, a = 0, b = (mj[j]-1)/mj[j])
  }
  
  for(m in 1:niter){
    #Calcolo delle probabilità per fare il sample dei cluster 
    temp_prob <- matrix(0, ncol = G, nrow = n)
    for(i in 1:n){
      temp_prob[i, ] <- pz_given_y_2(y_i = Y[i, ], tau = tau, a = a, epsilon = eps, m_levels = mj)
      c_out[m, i] <- sample(1:G, size = 1, prob = temp_prob[i, ])
    }
    
    #Inizializzo vettore per calcolare la numerosità di ciasun cluster
    n_g <- numeric(G)
    for(g in 1:G){
      n_g[g] = sum(c_out[m, ] == g) #Calcolo di n_g
    }
    
    
    for(g in 1:G){
      
      if(n_g[g] == 0) next #Salto l'iterazione se il cluster è vuoto, altrimenti riscontro problemi computazionali
      
      for(j in 1:d){
        
        v_j <- sum(Y[, j] == a[j], na.rm = TRUE) #Calcolo di v^j
        
        
        eps[j] <- rtrunc_beta_inv(
          n = 1, 
          shape1 = n-v_j+G*(mj[j]-1)*(c-1)+1, 
          shape2 = v_j+G*(c-1)+1,
          a = 0, 
          b = (mj[j] - 1) / mj[j]
        )
        
        
        counts <- numeric(mj[j]) 
        gamma_j <- numeric(mj[j])
        
        for(h in 1:mj[j]){
          counts[h] <- sum(Y[, j] == h, na.rm = TRUE) #calcolo di u_g^j
          gamma_j[h] <- (((mj[j] - 1) * (1 - eps[j])) / (eps[j]))^(counts[h])
        }
        
        rho_j <- gamma_j / sum(gamma_j) #calcolo di rho per campionare a
        a[j] <- sample(1:mj[j], size = 1, prob = rho_j)
      }
    }
    
    
    tau <- rdirichlet(1, b + n_g) #campionamento dalla dirichlet per i tau 
    
    cat(paste("\014", m, "/", niter, "\n"))
  }
  
  
  return(list(
    cluster = c_out[(nburn+1):niter, ],
    tau = tau, #Ritorna solo l'ultimo tau (non essenziale)
    a = a, #Ritorna solo l'ultimo a (non essenziale)
    epsilon = eps #Ritorna solo l'ultimo epsilon (non essenziale)
  ))
}


epsilon_j <- function(Y, G, b, c, niter, nburn){
  n <- nrow(Y)
  d <- ncol(Y)
  mj <- apply(Y, 2, max)
  
  # Inizializzo le matrici in cui salvare i risultati 
  c_out <- matrix(0, nrow = niter, ncol = n)
  
  # Inizializzazione dei parametri 
  a <- numeric(d)
  for(j in 1:d){
    a[j] <- sample(1:mj[j], size = 1)
  }
  
  # Initializzo tau
  tau <- rdirichlet(1, rep(1, G))
  
  # Initializzo epsilon  
  eps <- numeric(d)
  for(j in 1:d){
    eps[j] <- rtrunc_beta_inv(n = 1, shape1 = 1, shape2 = 1, a = 0, b = (mj[j]-1)/mj[j])
  }
  
  for(m in 1:niter){
    #Calcolo delle probabilità per fare il sample dei cluster 
    temp_prob <- matrix(0, ncol = G, nrow = n)
    for(i in 1:n){
      temp_prob[i, ] <- pz_given_y_2(y_i = Y[i, ], tau = tau, a = a, epsilon = eps, m_levels = mj)
      c_out[m, i] <- sample(1:G, size = 1, prob = temp_prob[i, ])
    }
    
    #Inizializzo vettore per calcolare la numerosità di ciasun cluster
    n_g <- numeric(G)
    for(g in 1:G){
      n_g[g] = sum(c_out[m, ] == g) #Calcolo di n_g
    }
    
    
    for(g in 1:G){
      
      if(n_g[g] == 0) next #Salto l'iterazione se il cluster è vuoto, altrimenti riscontro problemi computazionali
      
      for(j in 1:d){
        
        v_j <- sum(Y[, j] == a[j], na.rm = TRUE) #Calcolo di v^j
        
        
        eps[j] <- rtrunc_beta_inv(
          n = 1, 
          shape1 = n-v_j+G*(mj[j]-1)*(c-1)+1, 
          shape2 = v_j+G*(c-1)+1,
          a = 0, 
          b = (mj[j] - 1) / mj[j]
        )
        
        
        counts <- numeric(mj[j]) 
        gamma_j <- numeric(mj[j])
        
        for(h in 1:mj[j]){
          counts[h] <- sum(Y[, j] == h, na.rm = TRUE) #calcolo di u_g^j
          gamma_j[h] <- (((mj[j] - 1) * (1 - eps[j])) / (eps[j]))^(counts[h])
        }
        
        rho_j <- gamma_j / sum(gamma_j) #calcolo di rho per campionare a
        a[j] <- sample(1:mj[j], size = 1, prob = rho_j)
      }
    }
    
    
    tau <- rdirichlet(1, b + n_g) #campionamento dalla dirichlet per i tau 
    
    cat(paste("\014", m, "/", niter, "\n"))
  }
  
  
  return(list(
    cluster = c_out[(nburn+1):niter, ],
    tau = tau, #Ritorna solo l'ultimo tau (non essenziale)
    a = a, #Ritorna solo l'ultimo a (non essenziale)
    epsilon = eps #Ritorna solo l'ultimo epsilon (non essenziale)
  ))
}


pearson_gamma_index <- function(dmat, clustering) {
  same_cluster <- outer(clustering, clustering, FUN = function(x, y) as.numeric(x != y))
  dist_vec <- as.vector(dmat)
  cluster_vec <- as.vector(same_cluster)
  cor(dist_vec, cluster_vec, method = "pearson")
}


convert_to_factors <- function(df) {
  df[] <- lapply(df, function(x) as.factor(x))
  return(df)
}

hamming_dist <- function(data) {
  n <- nrow(data)
  D <- matrix(0, n, n)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      D[i, j] <- sum(data[i, ] != data[j, ])
      D[j, i] <- D[i, j]  # simmetrica
    }
  }
  return(as.dist(D))
}



kmultinomial_gibbs <- function(x, k, lambda = 1, R = 1000, burn_in = 1000, nstart = 10, trace = FALSE) {
  
  n <- nrow(x)
  d <- ncol(x)
  
 
  stopifnot(k <= n)
  stopifnot(k >= 1)
  
  if (trace) {
    cat("Initialization of the algorithm\n")
  }
  
  G <- sample(1:k, n, replace = T)
  fit_map <- kmulticlass_R_function(X = x, G = G, freq = as.numeric(table(G)), k = k)
  G_map <- fit_map$Cluster
  freq_map <- as.numeric(table(G_map))
  if (trace) {
    cat("Starting the Gibbs sampling (R + burn-in) \n")
  }
  
  fit <- Gibbs_kmulticlass_C(R = R + burn_in, X = as.matrix(x), G = G_map, freq = freq_map, lambda = lambda, trace = T)
  
 
  fit$G <- fit$G[-c(1:burn_in), ]
  fit$lambda <- fit$lambda[-c(1:burn_in)]
  fit$loss <- fit$loss[-c(1:burn_in)]
  fit$prob <- fit$prob[,,(burn_in + 1):(R+burn_in)]
  
  fit$G_map <- G_map
  fit$loss_map <- fit_map$loss
  fit
}



elicit_hyperparameters_from_poLCA <- function(lcm_poLCA_model,
                                              data_original_factors,
                                              prior_strength_tau,
                                              prior_strength_alpha,
                                              G,
                                              min_hyper_value = 0.01) {
  
  # --- 1. Validazione degli input ---
  if (!inherits(lcm_poLCA_model, "poLCA")) {
    stop("lcm_poLCA_model deve essere un oggetto restituito da poLCA().")
  }
  if (!is.data.frame(data_original_factors) || !all(sapply(data_original_factors, is.factor))) {
    stop("data_original_factors deve essere un data.frame con tutte le colonne di tipo factor.")
  }
  if (!is.numeric(prior_strength_tau) || length(prior_strength_tau) != 1 || prior_strength_tau <= 0) {
    stop("prior_strength_tau deve essere uno scalare numerico positivo.")
  }
  if (!is.numeric(prior_strength_alpha) || length(prior_strength_alpha) != 1 || prior_strength_alpha <= 0) {
    stop("prior_strength_alpha deve essere uno scalare numerico positivo.")
  }
  if (!is.numeric(min_hyper_value) || length(min_hyper_value) != 1 || min_hyper_value <= 0) {
    stop("min_hyper_value deve essere uno scalare numerico positivo.")
  }
  
  # --- 2. Estrarre G (numero di classi) ---
  G_val <- G
  if (is.null(G_val) || !is.numeric(G_val) || G_val <= 0) {
    stop("Numero di classi (G) non trovato o non valido nell'oggetto poLCA.")
  }
  
  # --- 3. Costruire hyper_b per le proporzioni delle classi (tau) ---
  tau_hat <- lcm_poLCA_model$P # Vettore delle proporzioni stimate delle classi
  if (is.null(tau_hat) || !is.numeric(tau_hat) || length(tau_hat) != G_val || any(tau_hat < 0) || abs(sum(tau_hat) - 1.0) > 1e-6) {
    stop("Proporzioni delle classi (P) da poLCA non trovate, non valide o non sommano a 1.")
  }
  hyper_b <- prior_strength_tau * tau_hat
  hyper_b[hyper_b < min_hyper_value] <- min_hyper_value # Assicura che siano > 0
  
  # --- 4. Determinare d (numero variabili), mj_vec (categorie per variabile), max_mj ---
  poLCA_item_probs_list <- lcm_poLCA_model$probs # Lista di matrici (classi x categorie)
  if (is.null(poLCA_item_probs_list) || !is.list(poLCA_item_probs_list)) {
    stop("Probabilità item-risposta (lcm_poLCA_model$probs) non trovate o non valide.")
  }
  
  d_val_from_probs <- length(poLCA_item_probs_list)
  d_val_from_data <- ncol(data_original_factors)
  
  if(d_val_from_probs != d_val_from_data) {
    stop(paste("Mismatch nel numero di variabili: modello poLCA ha", d_val_from_probs, 
               "variabili (da $probs), ma data_original_factors ne ha", d_val_from_data))
  }
  d_val <- d_val_from_data # Numero di variabili
  
  # mj_vec (numero di categorie per ogni variabile)
  # Derivato dal numero di colonne nelle matrici di probabilità di poLCA
  mj_vec_from_probs <- sapply(poLCA_item_probs_list, ncol) 
  # Per controllo, confronta con nlevels dai dati originali
  mj_vec_from_data <- sapply(data_original_factors, nlevels)
  
  if (!identical(mj_vec_from_probs, mj_vec_from_data)) {
    warning(paste("C'è un Mismatch nel numero di categorie per variabile tra l'output di poLCA e i livelli dei factor nei dati forniti.\n",
                  "  Numero categorie da poLCA ($probs): (", paste(mj_vec_from_probs, collapse=", "), ")\n",
                  "  Numero categorie da nlevels(data): (", paste(mj_vec_from_data, collapse=", "), ")\n",
                  "Si utilizzerà mj_vec basato sull'output di poLCA, che è più autorevole per le sue stime di probabilità."))
    mj_vec <- mj_vec_from_probs
  } else {
    mj_vec <- mj_vec_from_data
  }
  
  if(any(mj_vec <= 0)) stop("Tutte le variabili devono avere almeno una categoria secondo mj_vec.")
  max_mj_val <- max(mj_vec)
  
  hyper_c_array <- array(min_hyper_value, dim = c(d_val, max_mj_val, G_val))
  
  variable_names <- names(poLCA_item_probs_list)
  if(is.null(variable_names) || length(variable_names) != d_val) variable_names <- colnames(data_original_factors)
  if(is.null(variable_names) || length(variable_names) != d_val) variable_names <- paste0("Var", 1:d_val)
  
  dimnames(hyper_c_array) <- list(
    Vars = variable_names,
    Cats = paste0("Cat", 1:max_mj_val),
    Classes = paste0("Class", 1:G_val)
  )
  
  for (j_idx in 1:d_val) { 
    num_categories_for_item_j <- mj_vec[j_idx]
    # prob_matrix_for_var_j è una matrice [Classi x Categorie] da poLCA
    prob_matrix_for_var_j <- poLCA_item_probs_list[[j_idx]]
    
 
    if(is.null(prob_matrix_for_var_j) || 
       !is.matrix(prob_matrix_for_var_j) ||
       nrow(prob_matrix_for_var_j) != G_val || 
       ncol(prob_matrix_for_var_j) != num_categories_for_item_j){
      warning(paste("Matrice delle probabilità per variabile '", variable_names[j_idx], "' (indice ", j_idx,
                    ") ha dimensioni inattese o è NULL. Atteso: ", G_val, " righe (classi) x ", num_categories_for_item_j,
                    " colonne (categorie). Ottenuto: ", if(is.null(prob_matrix_for_var_j)) "NULL" else paste(nrow(prob_matrix_for_var_j),"x",ncol(prob_matrix_for_var_j)),
                    ".\n  Si usa un prior uniforme (basato su min_hyper_value e prior_strength_alpha) per questa variabile in hyper_c.", sep=""))
      for(g_idx_fill in 1:G_val){
        for(k_idx_fill in 1:num_categories_for_item_j){
          default_prior_val <- prior_strength_alpha * (1/num_categories_for_item_j)
          hyper_c_array[j_idx, k_idx_fill, g_idx_fill] <- max(default_prior_val, min_hyper_value)
        }
      }
      next 
    }
    
    for (g_idx in 1:G_val) { 
      for (k_idx in 1:num_categories_for_item_j) { 
        alpha_gjk_from_poLCA <- prob_matrix_for_var_j[g_idx, k_idx]
        
        elicited_val <- prior_strength_alpha * alpha_gjk_from_poLCA
        hyper_c_array[j_idx, k_idx, g_idx] <- max(elicited_val, min_hyper_value)
      }
    }
  }
  

  df_numeric <- as.data.frame(lapply(data_original_factors, as.numeric))
  Y_numeric_for_bayesian <- data.matrix(df_numeric) 
  
  return(list(hyper_b = hyper_b,
              hyper_c = hyper_c_array,
              G = G_val,
              d = d_val,
              mj_vec = mj_vec, 
              max_mj = max_mj_val,
              Y_numeric_for_bayesian = Y_numeric_for_bayesian
  ))
}

kmulticlass_R_function <- function(X, G, freq, k = 3) {
  epsilon = 0.0000000001
  n = nrow(X)
  d = ncol(X)
  
  losses = numeric(k)
  convergence = F
  
  total_loss_old = -1
  C = max(apply(X, 2, max))
  x_tilde =  array(0, dim = c(C, d, k))
  
  total_loss = 0
  for (j in 1:k) {
    idx_cluster = which(G == j)
    x_tilde[, , j] = multiclass_centroid_C(x_k = X[idx_cluster, ], x = X, alpha = 0.5)$matrix_prob
    total_loss = total_loss + loss_multiclass_C(X[idx_cluster, ], x = X, alpha = 0.5)
    # cat("Loss_function : " total_loss, "\n")
  }
  
  while (!convergence) {
    for (i in 1:n) {
      for (g in 1:k) {
        prob = numeric(d)
        for (j in 1:d) {
          c <- as.integer(X[i, j])
          prob[j] <- max(x_tilde[c, j, g], epsilon)
        }
        losses[g] <- -sum(log(prob))
      }
      
      freq[G[i]] = freq[G[i]] - 1
      
      if (freq[G[i]] > 0) {
        G[i] = which.min(losses)
      }
      freq[G[i]] = freq[G[i]] + 1
    }
    total_loss = 0
    x_tilde =  array(0, dim = c(C, d, k))
    for (j in 1:k) {
      idx_cluster = which(G == j)
      x_tilde[, , j] = multiclass_centroid_C(x_k = X[idx_cluster, ], x = X, alpha = 0.3)$matrix_prob
      total_loss = total_loss + loss_multiclass_C(X[idx_cluster, ], x = X, alpha = 0.3)
    }
    if ((total_loss_old - total_loss) == 0) {
      convergence = T
    } else{
      total_loss_old = total_loss
      cat("Total_loss", total_loss, "\n")
    }
  }
  return(list("Cluster" = G,
              "Centers" = x_tilde,
              "Loss" = total_loss))
}



simulation_1_function <- function(n, tau, delta,m,  replica_id = NULL){
  G <- length(tau) #numero dei cluster
  d <-  length(m) #numero delle variabili 
  
  #Simulazione dei dati 
  prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
  sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)
  data <- sim.data$dat
  
  #Per l'approccio classico 
  data_factor <- convert_to_factors(data)
  
  #Classical approach
  
  start <- Sys.time()
  res_std <- mixmodCluster(data_factor, nbCluster = G, dataType = "qualitative", model = mixmodMultinomialModel(listModels = "Binary_pk_Ekjh"),
                           criterion = c("ICL"))
  end <- Sys.time()
  time_EM_std <- difftime(end, start, units = "secs")
  
  clust_freq <- res_std@results[[1]]@partition
  
  ari_freq <- adjustedRandIndex(clust_freq, sim.data$trueclass)
  nmi_freq <- NMI(sim.data$trueclass, clust_freq)
  
  start <- Sys.time()
  res_pars<- mixmodCluster(data_factor, nbCluster = G, dataType = "qualitative", model = mixmodMultinomialModel(listModels = "Binary_pk_Ekj"),
                           criterion = c("ICL"))
  end <- Sys.time()
  time_EM_pars <- difftime(end, start, units = "secs")
  
  clust_freq_pars <- res_pars@results[[1]]@partition
  
  ari_freq_pars <- adjustedRandIndex(clust_freq_pars, sim.data$trueclass)
  nmi_freq_pars <- NMI(sim.data$trueclass, clust_freq_pars)
  gc()
  
  #kmodes 
  
  start <- Sys.time()
  kmod <- kmodes(data_factor, modes = G, iter.max = 100)
  end <- Sys.time()
  time_kmodes <- difftime(end, start, units = "secs")
  
  kmod_clust <- kmod$cluster
  ari_kmodes <- adjustedRandIndex(kmod_clust, sim.data$trueclass)
  nmi_kmodes <- NMI(sim.data$trueclass, kmod_clust)
  
  #Bayesian approach
  
  start <- Sys.time()
  c_lcm_std <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 4)
  end <- Sys.time()
  time_lcm_std <- difftime(end, start, units = "secs")
  
  cluster_probs <- aperm(abind(c_lcm_std$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = c_lcm_std$cluster,          
                             p = cluster_probs,
                             K = G)
  c_step <- as.vector(out_std$clusters)
  gc()
  ari_lcm_std <- adjustedRandIndex(c_step, sim.data$trueclass)
  nmi_lcm_std <- NMI(sim.data$trueclass, c_step)
  
  #Parsimonious model 
  
  
  cat("Inizio modelli parsimoniosi \n")
  start <- Sys.time()
  c_pars <- run_parsimonious_model(Y_r = as.matrix(data), G = G, b = 4, c = 4, niter = 12000, nburn = 2000)
  end <- Sys.time()
  time_lcm_pars <- difftime(end, start, units = "secs")
  
  
  c_pars_probs <- aperm(c_pars$cluster_probs, c(3,1,2))
  
  cat("ls_step \n")
  out_pars <- label.switching(method = "STEPHENS",
                              z = c_pars$cluster,          
                              p = c_pars_probs,
                              K = G)
  c_opt_pars <- as.vector(unlist(out_pars$clusters))
  cat("\014")
  cat("Ari\n")
  ari_lcm_pars <- adjustedRandIndex(c_opt_pars, sim.data$trueclass)
  cat("Calcolo NMI \n")
  cat("Label stimate se esce il problema:", c_opt_pars, "\n")
  
  nmi_lcm_pars <- aricode::NMI(sim.data$trueclass, as.vector(c_opt_pars))
  cat("NMI_stimato:", nmi_lcm_pars, "\n")
  gc()
  
  #Generalized Bayes Product Partition model 
  
  start <- Sys.time()
  gbpp <- kmultinomial_gibbs(x = as.matrix(data), k = G, R = 10000, burn_in = 2000,trace = F)
  end <- Sys.time()
  time_gbpp <- difftime(end, start, units = "secs")
  
  
  gbpp_clust <- gbpp$G
  gbpp_prob <- aperm(gbpp$prob, c(3,1,2))
  
  out_gbpp <- label.switching(method = "STEPHENS",
                              z = gbpp_clust,
                              p = gbpp_prob,
                              K = G)
  

  ari_gbpp <- adjustedRandIndex(out_gbpp$clusters, sim.data$trueclass)
  nmi_gbpp <- NMI(as.vector(out_gbpp$clusters), sim.data$trueclass)
  
  return(list(
    "ARI_Kmodes" = ari_kmodes,
    "NMI_Kmodes" = nmi_kmodes,
    "Time_kmodes" = time_kmodes,
    "ARI_em" = ari_freq,
    "NMI_em" = nmi_freq,
    "Time_em" = time_EM_std,
    "ARI_em_pars" = ari_freq_pars,
    "NMI_em_pars" = nmi_freq,
    "Time_em_pars" = time_EM_pars,
    "ARI_bayes_std" = ari_lcm_std,
    "NMI_bayes_std" = nmi_lcm_std,
    "Time_bayes_std" = time_lcm_std,
    "ARI_bayes_pars" = ari_lcm_pars,
    "NMI_bayes_pars" = nmi_lcm_pars,
    "Time_bayes_pars" = time_lcm_pars,
    "ARI_gbpp" = ari_gbpp,
    "NMI_gbpp" = nmi_gbpp,
    "Time_gbpp" = time_gbpp))
}


run_simulation <- function(n, tau, delta, m, n_replicate = 100, output_file) {
  results <- vector("list", n_replicate)
  
  for(i in seq_len(n_replicate)){
    results[[i]] <- simulation_1_function(n = n, tau = tau, delta = delta, m = m)
    cat("Replica simulazione", i, "completata\n")
  }
  
  df_results <- do.call(rbind, lapply(results, as.data.frame))
  save(df_results, file = output_file)
  
  return(df_results)
}


simulation_ls <- function(n, tau, m, delta){
  
  
  G <- length(tau) #numero dei cluster
  d <-  length(m) #numero delle variabili 
  
  #Simulazione dei dati 
  prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
  sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)
  data <- sim.data$dat
  
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 4, save_probs = T)
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  start <- Sys.time()
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  end <- Sys.time()
  time_lcm_step <- difftime(end, start, units = "secs")
  
  
  psm <- comp.psm(model$cluster)
  
  start <- Sys.time()
  c_vi <- minVI(psm)$cl
  end <- Sys.time()
  time_lcm_vi <- difftime(end, start, units = "secs")  
  
  start <- Sys.time()  
  c_binder <- point_Binder(model$cluster)
  end <- Sys.time()
  time_lcm_binder <- difftime(end, start, units = "secs")  
  
  
  ari_step <- adjustedRandIndex(out_std$cluster, sim.data$trueclass)
  ari_vi <- adjustedRandIndex(c_vi, sim.data$trueclass)
  ari_binder <- adjustedRandIndex(c_binder, sim.data$trueclass)
  
  nmi_step <- NMI(as.vector(out_std$cluster),  sim.data$trueclass)
  nmi_vi <- NMI(c_vi, sim.data$trueclass)
  nmi_binder <- NMI(c_binder, sim.data$trueclass)
  
  
  
  return(list(
    "ARI_stephens" = ari_step,
    "NMI_step" = nmi_step,
    "Time_stephens" = time_lcm_step,
    "ARI_vi" = ari_vi,
    "NMI_vi" = nmi_vi,
    "Time_vi" = time_lcm_vi,
    "ARI_binder" = ari_binder,
    "NMI_binder" = nmi_binder,
    "Time_binder" = time_lcm_binder
  ))
}

run_simulation_ls <- function(n, tau, delta, m, n_replicate = 100, output_file) {
  results <- vector("list", n_replicate)
  
  for(i in seq_len(n_replicate)){
    results[[i]] <- simulation_ls(n = n, tau = tau, delta = delta, m = m)
    cat("Replica simulazione", i, "completata\n")
  }
  
  df_results <- do.call(rbind, lapply(results, as.data.frame))
  save(df_results, file = output_file)
  
  return(df_results)
}

prepare_hyperparameters_r <- function(true_P,
                                      true_probs_list,
                                      true_nresp,
                                      S_tau,
                                      S_alpha_type = "num_categories",
                                      S_alpha_value = 1.0) {

  if (!is.vector(true_P) || !is.numeric(true_P) || any(true_P < 0)) {
    stop("true_P deve essere un vettore numerico con valori non negativi.")
  }
  if (abs(sum(true_P) - 1.0) > 1e-6 && length(true_P) > 0) {
    warning("La somma di true_P non è 1. Gli iperparametri risultanti per tau saranno comunque proporzionali a true_P.")
  }
  if (!is.list(true_probs_list)) {
    stop("true_probs_list deve essere una lista di matrici.")
  }
  if (length(true_probs_list) > 0 && !all(sapply(true_probs_list, is.matrix))) {
    stop("Tutti gli elementi di true_probs_list devono essere matrici.")
  }
  if (!is.vector(true_nresp) || !is.numeric(true_nresp) || any(true_nresp <= 0)) {
    stop("true_nresp deve essere un vettore numerico con valori interi positivi.")
  }
  d <- length(true_probs_list)
  if (d != length(true_nresp)) {
    stop("La lunghezza di true_probs_list deve corrispondere alla lunghezza di true_nresp (numero di item).")
  }
  if (!S_alpha_type %in% c("num_categories", "constant")) {
    stop("S_alpha_type deve essere 'num_categories' o 'constant'.")
  }
  if (S_alpha_type == "constant" && (!is.numeric(S_alpha_value) || length(S_alpha_value) != 1 || S_alpha_value <= 0)) {
    stop("Se S_alpha_type è 'constant', S_alpha_value deve essere un singolo numero positivo.")
  }
  if (!is.numeric(S_tau) || length(S_tau) != 1 || S_tau <= 0) {
    stop("S_tau deve essere un singolo numero positivo.")
  }
  
  G <- length(true_P) # Numero di classi latenti
  if (G == 0 && d > 0) { 
    G <- nrow(true_probs_list[[1]])
    if(is.null(G)) stop("Impossibile determinare G (numero di classi).")
    warning("true_P è vuoto. G è stato derivato da true_probs_list. hyper_b non può essere calcolato se true_P non è fornito.")
    hyper_b_values <- numeric(0) 
  } else if (G == 0 && d == 0) {
    hyper_b_values <- numeric(0)
  }
  else {
    hyper_b_values <- S_tau * true_P
    if (any(hyper_b_values <= 0) && any(true_P > 0)) { 
      stop("I valori risultanti di hyper_b devono essere positivi. Controlla S_tau e true_P (es. true_P non può avere valori negativi che non sommano a zero).")
    }
  }
  

  if (d == 0) { # Nessun item
    max_mj <- 0
    hyper_c_values <- array(0, dim = c(0, 0, 0))
  } else {
    if (nrow(true_probs_list[[1]]) != G && G != 0) { 
      stop(paste0("Il numero di righe in true_probs_list[[1]] (", nrow(true_probs_list[[1]]),
                  ") non corrisponde a G derivato da true_P (", G, ")."))
    } else if (G == 0) { 
      G <- nrow(true_probs_list[[1]])
      if(is.null(G) || G==0) stop("G non può essere determinato né da true_P né da true_probs_list.")
    }
    
    
    max_mj <- max(true_nresp)
    hyper_c_values <- array(0, dim = c(d, max_mj, G))
    
    for (j_idx in 1:d) { 
      num_categories_item_j <- true_nresp[j_idx]
      
      if (ncol(true_probs_list[[j_idx]]) != num_categories_item_j) {
        stop(paste0("Errore per l'item ", j_idx, ": ncol(true_probs_list[[", j_idx, "]]) è ",
                    ncol(true_probs_list[[j_idx]]), " ma true_nresp[", j_idx, "] è ", num_categories_item_j, "."))
      }
      if (nrow(true_probs_list[[j_idx]]) != G) {
        stop(paste0("Errore per l'item ", j_idx, ": nrow(true_probs_list[[", j_idx, "]]) è ",
                    nrow(true_probs_list[[j_idx]]), " ma G è ", G, "."))
      }
      
      for (g_idx in 1:G) { # Loop sulle classi (da 1 a G in R)
        true_alpha_jg_vector <- true_probs_list[[j_idx]][g_idx, 1:num_categories_item_j]
        
        if (abs(sum(true_alpha_jg_vector) - 1.0) > 1e-6) {
          warning(paste0("La somma delle probabilità vere per l'item ", j_idx, ", classe ", g_idx,
                         " non è 1. I parametri hyper_c risultanti saranno comunque proporzionali."))
        }
        if (any(true_alpha_jg_vector < 0)) {
          stop(paste0("Trovate probabilità negative per l'item ", j_idx, ", classe ", g_idx, "."))
        }
        
        S_val_alpha_current <- 0
        if (S_alpha_type == "num_categories") {
          S_val_alpha_current <- num_categories_item_j 
        } else if (S_alpha_type == "constant") {
          S_val_alpha_current <- S_alpha_value
        }
        
        if (S_val_alpha_current <= 0) {
          stop("S_val_alpha_current (forza della prior per alpha) deve essere positiva.")
        }
        
        prior_params_for_alpha_jg <- S_val_alpha_current * true_alpha_jg_vector
        

        if (any(prior_params_for_alpha_jg <= 0 & true_alpha_jg_vector > 0)) {
          stop(paste0("Errore interno: una probabilità vera positiva per item ", j_idx, ", classe ", g_idx, 
                      " ha portato a un iperparametro non positivo."))
        }
        if (any(prior_params_for_alpha_jg <= 0 & true_alpha_jg_vector == 0)) {
          warning(paste0("Un iperparametro per alpha (item ", j_idx, ", classe ", g_idx,
                         ") è zero o negativo perché la probabilità vera corrispondente è zero. ",
                         "Assicurati che la tua funzione rdirichlet_cpp possa gestire parametri non positivi se questo è intenzionale."))
        }
        
        
        hyper_c_values[j_idx, 1:num_categories_item_j, g_idx] <- prior_params_for_alpha_jg
      }
    }
  }
  
  return(list(hyper_b = hyper_b_values, hyper_c = hyper_c_values))
}


prior_sim_function <- function(n, tau, m, delta){
  G <- length(tau) 
  d <-  length(m) 
  
  #Simulazione dei dati
  
  prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
  sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)
  data <- sim.data$dat
  
  
  #Tuning per b 
  
  #b = 0.1

  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 0.1, c = 4)
  
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  c_1 <- as.vector(out_std$clusters)
  gc()
  ari_b01 <- adjustedRandIndex(c_1, sim.data$trueclass)
  nmi_b01 <- NMI(sim.data$trueclass, c_1)
  
  
  # b = 1
  
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 1, c = 4)

  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  c_1 <- as.vector(out_std$clusters)
  gc()
  ari_1 <- adjustedRandIndex(c_1, sim.data$trueclass)
  nmi_1 <- NMI(sim.data$trueclass, c_1)
  
  
  
  
  start <- Sys.time()
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 10, c = 4)
  end <- Sys.time()
  time_2 <- difftime(end, start, units = "secs")
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  
  c_2 <- as.vector(out_std$clusters)
  gc()
  ari_2 <- adjustedRandIndex(c_2, sim.data$trueclass)
  nmi_2 <- NMI(sim.data$trueclass, c_2)
  
  
  
  start <- Sys.time()
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 50, c = 4)
  end <- Sys.time()
  time_3 <- difftime(end, start, units = "secs")
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  
  c_3 <- as.vector(out_std$clusters)
  gc()
  ari_3 <- adjustedRandIndex(c_2, sim.data$trueclass)
  nmi_3 <- NMI(sim.data$trueclass, c_2)  
  
  gc()
  
  
  #Tuning di c
  
  #c = 0.1
  
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 0.1)
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  c_1 <- as.vector(out_std$clusters)
  gc()
  ari_c01 <- adjustedRandIndex(c_1, sim.data$trueclass)
  nmi_c01 <- NMI(sim.data$trueclass, c_1)
  
  
  #c = 1
  
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 1)
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  c_1 <- as.vector(out_std$clusters)
  gc()
  ari_c1 <- adjustedRandIndex(c_1, sim.data$trueclass)
  nmi_c1 <- NMI(sim.data$trueclass, c_1)
  
  
  # c = 10
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 10)

  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  
  c_2 <- as.vector(out_std$clusters)
  gc()
  ari_c10 <- adjustedRandIndex(c_2, sim.data$trueclass)
  nmi_c10 <- NMI(sim.data$trueclass, c_2)
  
  
  #c = 50
  
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 50)
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  
  c_3 <- as.vector(out_std$clusters)
  gc()
  ari_c50 <- adjustedRandIndex(c_2, sim.data$trueclass)
  nmi_c50 <- NMI(sim.data$trueclass, c_2)  
  
  gc()
  
  
  
  
  #Inizio tuning empirical bayes
  
  polca_model <- poLCA(formula = as.matrix(data)~1, data = data, nclass = G)
  data_factor <- convert_to_factors(data)
  
  #Empirical bayes con forza pari a 1
  param_prior <- elicit_hyperparameters_from_poLCA(lcm_poLCA_model = polca_model, data_original_factors = data_factor, prior_strength_tau = 1,
                                                   prior_strength_alpha = 1, G = G)
  
  mod <- gibbs_lcm_standard_tune_cpp(Y = param_prior$Y_numeric_for_bayesian, G = param_prior$G, niter = 20000, nburn = 10000,
                                     hyper_b = param_prior$hyper_b, hyper_c = param_prior$hyper_c, progress_interval = 1000)
  
  gc()
  M <- nrow(mod$cluster)
  N <- ncol(mod$cluster)
  G <- ncol(mod$tau)
  p_array <- array(NA, dim = c(M, N, G))
  for (iter in 1:M) {
    p_array[iter, , ] <- mod$prob[[iter]][, , 1]
  }
  ls_output <- label.switching(
    method = "STEPHENS",
    z = mod$cluster,
    K = G,
    p = p_array
  )
  
  ari_eb <- adjustedRandIndex(as.vector(ls_output$clusters), sim.data$trueclass)
  nmi_eb <- NMI(as.vector(ls_output$clusters), sim.data$trueclass)
  
  gc()
  
  #Empirical bayes con forza pari a 10
  param_prior <- elicit_hyperparameters_from_poLCA(lcm_poLCA_model = polca_model, data_original_factors = data_factor, prior_strength_tau = 10,
                                                   prior_strength_alpha = 10, G = G)
  
  mod <- gibbs_lcm_standard_tune_cpp(Y = param_prior$Y_numeric_for_bayesian, G = param_prior$G, niter = 20000, nburn = 10000,
                                     hyper_b = param_prior$hyper_b, hyper_c = param_prior$hyper_c, progress_interval = 1000)
  
  
  M <- nrow(mod$cluster)
  N <- ncol(mod$cluster)
  G <- ncol(mod$tau)
  p_array <- array(NA, dim = c(M, N, G))
  for (iter in 1:M) {
    p_array[iter, , ] <- mod$prob[[iter]][, , 1]
  }
  ls_output <- label.switching(
    method = "STEPHENS",
    z = mod$cluster,
    K = G,
    p = p_array
  )
  
  ari_eb_2 <- adjustedRandIndex(as.vector(ls_output$clusters), sim.data$trueclass)
  nmi_eb_2 <- NMI(as.vector(ls_output$clusters), sim.data$trueclass)
  
  #Empirical bayes con forza pari a 30
  
  param_prior <- elicit_hyperparameters_from_poLCA(lcm_poLCA_model = polca_model, data_original_factors = data_factor, prior_strength_tau = 30,
                                                   prior_strength_alpha = 30, G = G)
  
  mod <- gibbs_lcm_standard_tune_cpp(Y = param_prior$Y_numeric_for_bayesian, G = param_prior$G, niter = 20000, nburn = 10000,
                                     hyper_b = param_prior$hyper_b, hyper_c = param_prior$hyper_c, progress_interval = 1000)
  
  gc()
  M <- nrow(mod$cluster)
  N <- ncol(mod$cluster)
  G <- ncol(mod$tau)
  p_array <- array(NA, dim = c(M, N, G))
  for (iter in 1:M) {
    p_array[iter, , ] <- mod$prob[[iter]][, , 1]
  }
  ls_output <- label.switching(
    method = "STEPHENS",
    z = mod$cluster,
    K = G,
    p = p_array
  )
  
  ari_eb_3 <- adjustedRandIndex(as.vector(ls_output$clusters), sim.data$trueclass)
  nmi_eb_3 <- NMI(as.vector(ls_output$clusters), sim.data$trueclass)
  
  #Empirical bayes con forza pari a 50
  
  param_prior <- elicit_hyperparameters_from_poLCA(lcm_poLCA_model = polca_model, data_original_factors = data_factor, prior_strength_tau = 50,
                                                   prior_strength_alpha = 50, G = G)
  
  mod <- gibbs_lcm_standard_tune_cpp(Y = param_prior$Y_numeric_for_bayesian, G = param_prior$G, niter = 20000, nburn = 10000,
                                     hyper_b = param_prior$hyper_b, hyper_c = param_prior$hyper_c, progress_interval = 1000)
  
  gc()
  M <- nrow(mod$cluster)
  N <- ncol(mod$cluster)
  G <- ncol(mod$tau)
  p_array <- array(NA, dim = c(M, N, G))
  for (iter in 1:M) {
    p_array[iter, , ] <- mod$prob[[iter]][, , 1]
  }
  ls_output <- label.switching(
    method = "STEPHENS",
    z = mod$cluster,
    K = G,
    p = p_array
  )
  
  ari_eb_4 <- adjustedRandIndex(as.vector(ls_output$clusters), sim.data$trueclass)
  nmi_eb_4 <- NMI(as.vector(ls_output$clusters), sim.data$trueclass)
  
  
  #Prior centrata sui veri valori dei parametri con forza pari a 10
  
  param_prior <- prepare_hyperparameters_r(true_P = sim.data$P, true_probs_list = sim.data$probs, true_nresp = sim.data$nresp,
                                           S_tau = 10,  S_alpha_value = 10)
  
  mod <- gibbs_lcm_standard_tune_cpp(Y = as.matrix(data), G = G, niter = 20000, nburn = 10000,
                                     hyper_b = as.vector(param_prior$hyper_b), hyper_c = param_prior$hyper_c, progress_interval = 1000)
  
  M <- nrow(mod$cluster)
  N <- ncol(mod$cluster)
  G <- ncol(mod$tau)
  p_array <- array(NA, dim = c(M, N, G))
  for (iter in 1:M) {
    p_array[iter, , ] <- mod$prob[[iter]][, , 1]
  }
  ls_output <- label.switching(
    method = "STEPHENS",
    z = mod$cluster,
    K = G,
    p = p_array
  )
  
  ari_true <- adjustedRandIndex(as.vector(ls_output$clusters), sim.data$trueclass)
  nmi_true <- NMI(as.vector(ls_output$clusters), sim.data$trueclass)  
  
  
  
  return(list(
    "ARI b = 0.1" = ari_b01,
    "NMi b = 0.1" = nmi_b01,
    "ARI b = 1" = ari_1,
    "NMI b = 1" = nmi_1, 
    "ARI b = 10" = ari_2,
    "NMI b = 10" = nmi_2,
    "ARI b = 50" = ari_3,
    "NMI b = 50" = nmi_3,
    "ARI c = 0.1" = ari_c01,
    "NMI c = 0.1" = nmi_c01,
    "ARI c = 1" = ari_c1,
    "NMI c = 1" = nmi_c1,
    "ARI c = 10" = ari_c10,
    "NMI c = 10" = nmi_c10, 
    "ARI c = 50" = ari_c50,
    "NMI c = 50" = nmi_c50,
    "ARI eb strenght = 1" = ari_eb,
    "NMI eb strenght = 1" = nmi_eb,
    "ARI eb strenght = 10" = ari_eb_2, 
    "NMI eb strenght = 10" = nmi_eb_2,
    "ARI eb strenght = 30" = ari_eb_3,
    "NMI eb strenght = 30" = nmi_eb_3,
    "ARI eb strenght = 50" = ari_eb_4,
    "NMI eb strenght = 50" = nmi_eb_4,
    "ARI true" = ari_true,
    "NMI true" = nmi_true
  ))
}


calculate_posterior_mean_alpha <- function(alpha_samples, na_rm = TRUE) {
  
  if (is.array(alpha_samples) && length(dim(alpha_samples)) == 4) {
    posterior_mean <- apply(alpha_samples, c(2, 3, 4), mean, na.rm = na_rm)
    
  } else if (is.list(alpha_samples)) {
    # Input è una lista di array 3D
    if (length(alpha_samples) == 0) {
      stop("La lista 'alpha_samples' è vuota.")
    }
    # Verifica che tutti gli elementi siano array 3D e abbiano le stesse dimensioni
    if (!all(sapply(alpha_samples, function(x) is.array(x) && length(dim(x)) == 3))) {
      stop("Tutti gli elementi della lista 'alpha_samples' devono essere array 3D.")
    }
    dims_first <- dim(alpha_samples[[1]])
    if (length(alpha_samples) > 1) {
      if (!all(sapply(alpha_samples[-1], function(x) all(dim(x) == dims_first)))) {
        stop("Tutti gli array 3D nella lista 'alpha_samples' devono avere le stesse dimensioni.")
      }
    }
    
    stacked_array <- simplify2array(alpha_samples)
    posterior_mean <- apply(stacked_array, c(1, 2, 3), mean, na.rm = na_rm)

    
  } else {
    stop("L'input 'alpha_samples' deve essere un array 4D o una lista di array 3D.")
  }
  
  return(posterior_mean)
}


prior_sim_function_2 <- function(n, tau, m, delta){
  G <- length(tau) 
  d <-  length(m) 
  
  #Simulazione dei dati
  
  prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
  sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)
  data <- sim.data$dat
  
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 4)
  
  
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  
  c_1 <- as.vector(out_std$clusters)
  gc()
  ari_b4c4 <- adjustedRandIndex(c_1, sim.data$trueclass)
  nmi_b4c4 <- NMI(sim.data$trueclass, c_1)
  
  return(list(
    "ARI b = 4" = ari_b4c4,
    "NMI b = 4" = nmi_b4c4
  )
  )
}


simulation_clust <- function(n, tau, delta, m){
  G <- length(tau) 
  d <-  length(m) 
  
  #Simulazione dei dati
  
  prob = prob_poLCA(K = G, d = d, m_vec = m, delta = delta)
  sim.data <- poLCA.simdata(N = n, probs = prob, P = tau)
  data <- sim.data$dat
  
  start <- Sys.time()
  model <- gibbs_lcm_standard_cpp(Y = as.matrix(data), G = G, niter = 12000, nburn = 2000, b = 4, c = 4)
  end <- Sys.time()
  time_lcm_std <- difftime(end, start, units = "secs")
  cluster_probs <- aperm(abind(model$prob, along = 3), c(3,1,2))
  
  
  out_std <- label.switching(method = "STEPHENS",
                             z = model$cluster,          
                             p = cluster_probs,
                             K = G)
  c_1 <- as.vector(out_std$clusters)
  gc()
  ari_std <- adjustedRandIndex(c_1, sim.data$trueclass)
  nmi_std <- NMI(sim.data$trueclass, c_1)
  
  
  
  
  
  start <- Sys.time()
  c_pars <- run_parsimonious_model(Y_r = as.matrix(data), G = G, b = 4, c = 4, niter = 12000, nburn = 2000)
  end <- Sys.time()
  time_lcm_pars <- difftime(end, start, units = "secs")
  c_pars_probs <- aperm(c_pars$cluster_probs, c(3,1,2))
  
  cat("ls_step \n")
  out_pars <- label.switching(method = "STEPHENS",
                              z = c_pars$cluster,          
                              p = c_pars_probs,
                              K = G)
  c_opt_pars <- as.vector(unlist(out_pars$clusters))
  
  ari_pars <- adjustedRandIndex(c_opt_pars, sim.data$trueclass)
  nmi_pars <- aricode::NMI(sim.data$trueclass, as.vector(c_opt_pars))
  gc()
  
  return(list(
    "ARI_std" = ari_std,
    "NMI_std" = nmi_std,
    "TIME_std" = time_lcm_std,
    "ARI_pars" = ari_pars,
    "NMI_pars" = nmi_pars,
    "TIME_pars" = time_lcm_pars
  ))
}

