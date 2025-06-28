#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;


// [[Rcpp::depends(RcppArmadillo)]]



// [[Rcpp::export]]
List multiclass_centroid_C(const arma::mat& x_k, const arma::mat& x, double alpha = 0.5) {
  int n_k = x_k.n_rows;
  int d = x.n_cols; // Use dimensionality from the full dataset x for consistency
  
  // Determine the number of classes (C[j]) for each feature j based on the full dataset x
  arma::ivec C(d);
  for (int j = 0; j < d; j++) {
    arma::vec current_col_x = x.col(j);
    if (current_col_x.is_empty() || x.n_rows == 0) {
      C[j] = 0; // No data in this column of x, so no classes
    } else {
      double max_val = current_col_x.max();
      // Ensure max_val is finite and positive (assuming classes are 1-indexed)
      if (!arma::is_finite(max_val) || max_val < 1.0) {
        C[j] = 0;
      } else {
        C[j] = static_cast<int>(max_val);
      }
    }
  }
  
  // Determine the maximum class label across all features to dimension matrix 'p'
  int max_overall_C = 0;
  if (C.n_elem > 0) {
    max_overall_C = C.max(); // Max value in the C vector
  }
  // Ensure p has at least one row if classes are 1-indexed and p(c-1,j) is used.
  // If no classes found (all C[j]=0), max_overall_C could be 0.
  if (max_overall_C <= 0) {
    max_overall_C = 1; // Default to 1 row to prevent 0-row matrix if no classes found.
    // This means p(0,j) is the only accessible row for prob.
  }
  
  arma::mat p(max_overall_C, d, arma::fill::zeros);
  
  for (int j = 0; j < d; j++) {
    if (C[j] == 0) { // If feature j has no defined classes in the full dataset x
      // Probabilities p(?,j) for this feature will remain 0.0 as initialized.
      continue;
    }
    
    for (int c_label = 1; c_label <= C[j]; c_label++) {
      double n_jc = 0.0; // Count of class c_label for feature j in cluster x_k
      
      if (n_k > 0) {
        // Only access x_k.col(j) if x_k is not empty.
        // We also assume x_k has 'd' columns, which should be true if derived from X.rows().
        if (j < x_k.n_cols) { // Defensive check, x_k.n_cols should equal d
          n_jc = arma::accu(x_k.col(j) == c_label);
        }
      }
      
      // Calculate probability P(x_kj = c_label)
      double denominator = static_cast<double>(n_k) + C[j] * alpha;
      
      if (denominator == 0.0) {
        // This happens if n_k=0 and C[j]*alpha=0.
        // If C[j]>0 (which it is, due to C[j]==0 check above) and alpha > 0 (default 0.5),
        // this path (denominator == 0) should not be taken if n_k=0.
        // It could happen if alpha=0 and n_k=0.
        // Fallback to uniform if C[j] > 0, otherwise 0.
        p(c_label - 1, j) = (C[j] > 0) ? (1.0 / C[j]) : 0.0;
      } else {
        p(c_label - 1, j) = (n_jc + alpha) / denominator;
      }
      // Indexing p(c_label - 1, j) is safe because:
      // c_label >= 1, so c_label - 1 >= 0.
      // c_label <= C[j], and C[j] <= max_overall_C (max value in C vector).
      // So, c_label - 1 < max_overall_C (number of rows in p).
    }
  }
  return List::create(Named("matrix_prob") = p, Named("classi") = C);
}




// [[Rcpp::export]]
double loss_multiclass_C_abbattuta(const arma::mat& x_k, const arma::mat& x,double alpha = 0.5) {
  List result = multiclass_centroid_C(x_k, x, alpha);
  arma::mat x_tilde = as<arma::mat>(result["matrix_prob"]);
  arma::ivec C = as<arma::ivec>(result["classi"]);
  
  int n_k = x_k.n_rows;
  int d = x_k.n_cols;
  double loss = 0.0;
  
  double epsilon = 1e-10;
  
  for (int i = 0; i < n_k; i++) {
    for (int j = 0; j < d; j++) {
      int c = static_cast<int>(x_k(i, j));//statistic_cast trasforma il valore della matrice in intero 
      double prob = std::max(x_tilde(c - 1, j), epsilon); 
      loss -= log(prob);
    }
  } 
  
  return loss;
}  



// [[Rcpp::export]]
double loss_multiclass_C(const arma::mat& x_k, const arma::mat& x, double alpha = 0.5) {
  List result = multiclass_centroid_C(x_k, x, alpha);
  arma::mat x_tilde = Rcpp::as<arma::mat>(result["matrix_prob"]); // Corretto
  // arma::ivec C_original = Rcpp::as<arma::ivec>(result["classi"]); // Non usato qui
  
  int n_k = x_k.n_rows;
  int d = x_k.n_cols; // Numero di colonne del cluster attuale
  double loss = 0.0;
  double epsilon = 1e-10;
  
  // Assicurati che d non ecceda le colonne di x_tilde
  // (dovrebbero corrispondere se x_k proviene da X.rows e x_tilde da x)
  int d_tilde = x_tilde.n_cols;
  
  for (int i = 0; i < n_k; i++) {
    for (int j = 0; j < d; j++) {
      if (j >= d_tilde) { // Se la colonna j di x_k non esiste in x_tilde
        // Questo non dovrebbe accadere se x_k.n_cols == x.n_cols == x_tilde.n_cols
        Rcpp::warning("Column index j=%d is out of bounds for x_tilde with %d columns.", j, d_tilde);
        loss -= log(epsilon); // Applica una penalità
        continue;
      }
      
      int c_val = static_cast<int>(x_k(i, j));
      
      if (c_val >= 1 && (c_val - 1) < x_tilde.n_rows) {
        double prob = std::max(x_tilde(c_val - 1, j), epsilon);
        loss -= log(prob);
      } else {
        // Rcpp::warning("Invalid class label %d for x_k(%d,%d). Index c-1=%d for x_tilde (rows=%d) is invalid.",
        //             c_val, i, j, c_val-1, x_tilde.n_rows);
        loss -= log(epsilon); // Applica una penalità per etichette non valide/out-of-bounds
      }
    }
  }
  return loss;
}



// [[Rcpp::export]]
List Gibbs_kmulticlass_C(int R, const arma::mat& X, arma::vec G, arma::vec freq, double lambda, bool trace){
  
  int n = X.n_rows;
  int K = freq.n_elem;
  int R_show = floor(R / 5);
  
  arma::vec prob(K);  
  arma::vec lprob(K);
  IntegerVector clusters = Range(1, K);
  bool skip;
  
  arma::mat G_out(R, n);
  arma::vec total_loss_out(R);
  arma::uvec idx_cluster;
  
  // inizializzo il cube per le probabilità: dimensione n x K x R
  arma::cube prob_out(n, K, R, arma::fill::zeros);
  
  // Calcolo della loss function iniziale
  double total_loss = 0;
  for(int j = 0; j < K; j++) {
    idx_cluster = find(G == j + 1); 
    total_loss += loss_multiclass_C(X.rows(idx_cluster), X);
  }
  
  // Inizio Gibbs Sampling
  for(int r = 0; r < R; r++) {
    
    for(int i = 0; i < n; i++) {
      
      // Rimuovo i dal cluster
      freq(G(i) - 1) -= 1; 
      
      // Se l’osservazione è unica nel cluster, la teniamo fissa
      if(freq(G(i) - 1) == 0){
        skip = true;
      } else { 
        skip = false;
      } 
      
      if(!skip){
        G(i) = arma::datum::nan;
        
        for(int j = 0; j < K; j++) {
          idx_cluster = find(G == j + 1); 
          arma::uvec idx_cluster_i(freq(j) + 1); 
          idx_cluster_i.head(freq(j)) = idx_cluster; 
          idx_cluster_i.tail(1) = i;
          lprob(j) = -lambda * (loss_multiclass_C(X.rows(idx_cluster_i), X) - loss_multiclass_C(X.rows(idx_cluster), X));
        } 
        
        lprob = lprob - max(lprob);
        prob = exp(lprob);
        prob /= sum(prob);
        
        // ✅ Salva le probabilità nel cube
        prob_out.slice(r).row(i) = trans(prob);
        
        G(i) = RcppArmadillo::sample(clusters, 1, TRUE, prob)[0];
      } else {
        // Se l’unica osservazione nel cluster, rimane dov’è e le probabilità sono fisse
        prob_out.slice(r).row(i).zeros();
        prob_out.slice(r)(i, G(i) - 1) = 1.0;
        
        freq(G(i) - 1) += 1;
        continue;
      }
      
      freq(G(i) - 1) += 1;
    }
    
    // Calcola loss dopo aggiornamenti
    total_loss = 0;
    for(int j = 0; j < K; j++) {
      idx_cluster = find(G == j + 1); 
      total_loss += loss_multiclass_C(X.rows(idx_cluster), X);
    } 
    
    if(trace) {
      if((r + 1) % R_show == 0) {
        Rprintf("Iteration: %i \n", r + 1);
      }
    } 
    
    G_out.row(r) = trans(G);
    total_loss_out(r) = total_loss;
  } 
  
  return(List::create(
      Named("G") = G_out, 
      Named("loss") = total_loss_out,
      Named("prob") = prob_out  
  ));
}
