#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <math.h>
using namespace Rcpp;
using namespace std;
//'Function to get the edgelist form a time series
//'@export
//'@param x a numeric vector convert from a time series
//'@param epsilon the threshold
//'@return a edgelist dataframe with weights with 2 columns
//'@references Geometrical invariability of transformation between a time series and a complex network, PHYSICAL REVIEW E 90, 012804 (2014)
// [[Rcpp::export]]
SEXP isometricTrans(NumericVector x,double epsilon){
  vector<vector<int > > edgelist(0);
  vector<int > temp(2,0);
  for(int i=1;i!=x.size();i++){
    for(int j=i+1;j<=x.size();j++){
      if(abs(x[i-1]-x[j-1])<epsilon){
        temp[0]=i;
        temp[1]=j;
        edgelist.push_back(temp);
      }
    }
  }
  return wrap(edgelist);
}
