#include <Rcpp.h>
#include <vector>
#include <iostream>
#include <math.h>
using namespace Rcpp;
using namespace std;
//'Function to get the edgelist form a time series
//'@export
//'@param x a numeric vector convert from a time series
//'@return a edgelist dataframe with weights with 3 columns
//'@references From time series to complex networks:The visibility graph,4972–4975, PNAS, April 1, 2008, vol. 105, no. 13
// [[Rcpp::export]]
SEXP weightedVisibilityGraph(NumericVector x){
  vector<double > weight(0);
  vector<int > firstNode(0);
  vector<int > secondNode(0);
  vector<double > real(0);
  for(int i=1;i!=x.size();i++){
    //numeric vertex label must start from 1
    firstNode.push_back(i);
    secondNode.push_back(i+1);
    weight.push_back(sqrt(1.0+pow(x[i+1]-x[i],2)));
    int j=i+2;//the next neighbor need evaluation
    while(j<=x.size()){
      //the real values of the timeseires
      for(int l=i+1;l!=j;l++){
        real.push_back(x[l-1]);
      }
      //the theoritical values of the timeseries
      bool compare=true;
      for(int k=i+1;k!=j;k++){
        double theory_temp=(x[j-1]+(x[i-1]-x[j-1])*(double(j-k)/double(j-i)));
        if(real[k-i-1]>=theory_temp){
          compare=false;
          break;
        }
        //cout<<theory_temp<<endl;
      }
      if(compare==true){
        firstNode.push_back(i);
        secondNode.push_back(j);
        weight.push_back(sqrt(pow(double(i-j),2)+pow(x[i+1]-x[i],2)));
      }
      //clear the temp vector
      real.clear();
      j++;
    }
  }
  return List::create(Named("x")=firstNode,Named("y")=secondNode,Named("weight")=weight);
}