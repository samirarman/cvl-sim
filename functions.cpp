/**
 * NOTA: 
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

double QW0 = .275;
NumericVector QW_R = {-0.03, -0.02, -0.042};
  
double QR0 = 135;
NumericVector  QR_B = {40., 55., 22.};

double REV_MEAN = 309022.3;
double REV_SD = 42359.15;

// Funções

// [[Rcpp::export]]
double quotes_won (double p, int qw_scen = 1) {
  double r = QW_R[qw_scen - 1];
  double p_0 = P0 - log((1) / QW0  - 1) / -r;
  return(1 / (1 + exp(-r * (p - p_0))));
}

// [[Rcpp::export]]
double quotes_received (double ad, int qr_scen = 1){
  double b = QR_B[qr_scen - 1];
  double a = QR0 - b * log(AD0);
  return(a + b * log(ad));
} 

// Calcula o imposto devido dadas 13 amostras do faturamento
// bruto.
// [[Rcpp::export]]
double tax(NumericVector rev_samples) {
  double annual_rev = sum(rev_samples[Range(0,11)]);
  double rev = rev_samples[12];
  double rate = NA_REAL;
  double disc = NA_REAL;
  
  if(annual_rev > 0 && annual_rev < 180e3) {
    rate = 4.5/100;
    disc = 0;
  } else if(annual_rev > 180e3 && annual_rev <= 360e3) {
    rate = 7.8 / 100.;
    disc = 5940.;
  } else if(annual_rev > 360e3 && annual_rev <= 720e3) {
    rate = 10. / 100.;
    disc = 13860.;
  } else if(annual_rev > 720e3 + 1 && annual_rev <= 1.8e6) {
    rate = 11.2 / 100.;
    disc = 22500.;
  } else if(annual_rev > 1.8e6 + 1 && annual_rev <= 3.6e6) {
    rate = 14.7 / 100.;
    disc = 85500.;
  } else if(annual_rev > 3.6e6 + 1 && annual_rev <= 4.8e6) {
    rate = 30.0 / 100.;
    disc = 720000.;
  }
  
  double tax = (annual_rev * rate - disc) / annual_rev;
  return(tax * rev);
}

// [[Rcpp::export]]
List dtax(double drev, int iter = 1000) {
  NumericVector base_tax(iter);
  NumericVector new_tax(iter);
  
  for (int i = 0; i < iter; i++) {
    NumericVector mean_rev =
      rtruncnorm(13, REV_MEAN, REV_SD, 0, R_PosInf);
    NumericVector new_rev =
      rtruncnorm(13, REV_MEAN + drev, REV_SD, 0, R_PosInf);
    base_tax[i] = tax(mean_rev);
    new_tax[i] = tax(new_rev);
  } 

  NumericVector dtax = new_tax - base_tax;
  double med_dtax = mean(na_omit(dtax));
  LogicalVector p_out = is_na(dtax);
  double prob_out = double (sum(p_out)) / p_out.length(); 

  return(List::create(Named("dtax") = med_dtax, 
                      _["p_out"] = prob_out));
}