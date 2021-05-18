/**
 * Este arquivo contém as funções necessárias para a 
 * simulação.
 * 
 * Os cenários de sensibilidade ao preço são representados
 * da seguinte forma: 1: estimado; 2: menos sensível; 3: mais sensível
 * 
 * Os cenários de efetividade da publicidade são representados
 * da seguinte forma: 1: estimado; 2: otimista; 3: pessimista
 * 
 * Por serem utilizadas específicamente para a simulação,
 * as funções neste arquivo não possuem verificações para erros.
 */
#include <Rcpp.h>
#include <truncnorm.h>
// [[Rcpp::depends(RcppDist)]]
using namespace Rcpp;

// Constantes
double P0 = 185.;
double AD0 = 2000.;
NumericVector VAR_COST = {103., 103. * 1.1, 103.* 1.15};
NumericVector VAR_COST_PROB  = {0.85, 0.10, 0.05};
  
double QW0 = .275;
double QW_SD = .025;
NumericVector QW_R = {-0.03, -0.02, -0.042};
  
double QR0 = 135;
NumericVector  QR_B = {40., 55., 22.};
double  QR_SD = 15.;

double q_MEAN = 14.32117;
double q_SD = 3.858328;
double REV_MEAN = 309022.3;
double REV_SD = 42359.15;

// Funções

// Calcula o valor da média da distribuição de W
// [[Rcpp::export]]
double mu_quotes_won (double p, int qw_scen = 1) {
  double r = QW_R[qw_scen - 1];
  double p_0 = P0 - log((1) / QW0  - 1) / -r;
  return(1 / (1 + exp(-r * (p - p_0))));
}

// Calcula o valor da média da distribuição de R
// [[Rcpp::export]]
double mu_quotes_received (double ad, int qr_scen = 1){
  double b = QR_B[qr_scen - 1];
  double a = QR0 - b * log(AD0);
  return(a + b * log(ad));
} 

// Gera amostras da distribuição de W
// [[Rcpp::export]]
NumericVector gen_qw_samples(double p, int qw_scen = 1, int iter = 1) {
  double mean = mu_quotes_won(p, qw_scen);
  NumericVector samples = rtruncnorm(iter, mean, QW_SD, 0, 1);
  return samples;
  }

// Gera amostras da distribuição de R
// [[Rcpp::export]]
NumericVector gen_qr_samples(double ad, int qr_scen = 1, int iter = 1) {
  double mean = mu_quotes_received(ad, qr_scen);
  NumericVector samples = rtruncnorm(iter, mean, QR_SD, 0, R_PosInf);
  return samples;
}

// Gera amostras da distribuição de Q
// [[Rcpp::export]]
NumericVector gen_qty_samples (double p, double ad, int qw_scen = 1, int qr_scen = 1, int iter = 1) {
  NumericVector orders =  gen_qr_samples(ad, qr_scen, iter) *
    gen_qw_samples(p, qw_scen, iter);
  NumericVector qty_per_bid = rtruncnorm(iter, q_MEAN, q_SD, 0, R_PosInf);
  return orders * qty_per_bid;
}

// Calcula o imposto devido dados RBT12 e o último faturamento
// [[Rcpp::export]]
double tax (double annual_rev, double rev) {
  double rate = NA_REAL;
  double disc = NA_REAL;
  
  if(annual_rev > 0 && annual_rev < 180e3) {
    rate = 4./100;
    disc = 0;
  } else if(annual_rev > 180e3 && annual_rev <= 360e3) {
    rate = 7.3 / 100.;
    disc = 5940.;
  } else if(annual_rev > 360e3 && annual_rev <= 720e3) {
    rate = 9.5 / 100.;
    disc = 13860.;
  } else if(annual_rev > 720e3 + 1 & annual_rev <= 1.8e6) {
    rate = 10.7 / 100.;
    disc = 22500.;
  } else if(annual_rev > 1.8e6 + 1 & annual_rev <= 3.6e6) {
    rate = 14.3 / 100.;
    disc = 87300.;
  } else if(annual_rev > 3.6e6 + 1 & annual_rev <= 4.8e6) {
    rate = 19.0 / 100.;
    disc = 378000.;
  }
  
  double tax = (annual_rev * rate - disc) / annual_rev;
  return(tax * rev);
}

// Calcula as estimativas da variação do lucro bruto, lucro líquido, 
// custo variável e imposto a pagar.
// [[Rcpp::export]]
List make_estimates (double p, double ad, int qw_scen = 1, int qr_scen = 1, int iter = 1000) {
  NumericVector base_qty = gen_qty_samples(P0, AD0, qw_scen, qr_scen, iter);
  NumericVector new_qty = gen_qty_samples(p, ad, qw_scen, qr_scen, iter );
  
  NumericVector base_rev = base_qty * P0;
  NumericVector new_rev = new_qty * p;
  NumericVector drev = new_rev - base_rev;
  
  NumericVector base_tax(iter);
  NumericVector new_tax(iter);
  
  for (int i = 0; i < iter; i++) {
    NumericVector mean_rev = rtruncnorm(12, REV_MEAN, REV_SD, 0, R_PosInf);
    NumericVector drev_effect = mean_rev + drev(i);
    base_tax(i) = tax(sum(mean_rev), mean_rev(11));
    new_tax(i) = tax(sum(drev_effect), drev_effect(11));
  }
  
  NumericVector dtax = new_tax - base_tax;
  
  NumericVector var_cost_samples = sample(VAR_COST, iter , true, VAR_COST_PROB);
  
  NumericVector base_cost = base_qty * var_cost_samples;
  NumericVector new_cost = new_qty * var_cost_samples;
  NumericVector dcost = new_cost - base_cost;
  
  NumericVector dprofit = drev - dcost - (ad - AD0);

  NumericVector dprofit_tax =
          dprofit - dtax;
  
  LogicalVector p_out = 
          is_na(dtax);

  double med_dprofit = median(na_omit(dprofit));
  double med_dprofit_tax = median(na_omit(dprofit_tax));
  double med_dcost = median(na_omit(dcost));
  double prob_out = double (sum(p_out)) / p_out.length(); 
  double med_dtax = median(na_omit(dtax));
  
  return(List::create(Named("p") = p, 
                      _["ad"] = ad,
                      _["qw_scen"] = qw_scen,
                      _["qr_scen"] = qr_scen,
                      _["dcost"] = med_dcost,
                      _["dprofit"] = med_dprofit,
                      _["dprofit_tax"] = med_dprofit_tax,
                      _["dtax"] = med_dtax,
                      _["p_out"] = prob_out));
}
