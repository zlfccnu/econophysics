#include <Rcpp.h>
#include <vector>
#include <iostream>
using namespace Rcpp;
using namespace std;
//' Function to get the edgelist form a time series
//'@export
//'@param x a numeric vector convert from a time series
//'@return a edgelist dataframe without weights with 2 columns
// [[Rcpp::export]]
SEXP unweightVisibilityGraph(NumericVector x){
  vector<vector<int > > edgelist(0);
  vector<int > temp(2,0);
  vector<double > real(0);
  for(int i=1;i!=x.size();i++){
    //numeric vertex label must start from 1
    temp[0]=i;//vertex label of i and it's neighbor is connected
    temp[1]=i+1;//vertex label of the neighbor of i
    edgelist.push_back(temp);
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
        temp[0]=i;
        temp[1]=j;
        edgelist.push_back(temp);
      }
      //clear the temp vector
      real.clear();
      j++;
    }
  }
  return wrap(edgelist);
}
