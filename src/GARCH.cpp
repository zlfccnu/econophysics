#include <Rcpp.h>
using namespace Rcpp;
//' GARCH model for CoViaR
//' @export
//' @param BETA
//' @param y
//' @return a list
// [[Rcpp::export]]
NumericVector caviar_GARCH(NumericVector BETA, NumericVector y, double empiricalQuantile, NumericVector VaR, int RowsOfy, int varPredict)
{
  int i;
  
  /* Initialize output variables */
  VaR[0] = empiricalQuantile;
  
  /* Start the loop */
  for(i = 1; i < RowsOfy; i++)
  {
    // Indirect GARCH
    VaR[i] =  sqrt(BETA[0] + BETA[1] * pow(VaR[i-1],2) + BETA[2] * pow(y[i-1],2));
    
  }
  
  if (varPredict == 1){
    VaR[RowsOfy] = sqrt(BETA[0] + BETA[1] * pow(VaR[RowsOfy-1],2) + BETA[2] * pow(y[RowsOfy-1],2));
  } 
  return VaR;
}
