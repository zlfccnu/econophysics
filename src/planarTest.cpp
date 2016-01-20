// We can now use the BH package
// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/graph/boyer_myrvold_planar_test.hpp> 
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/config.hpp>
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>
using namespace std;
using namespace boost;
using namespace Rcpp;
// here the identifer of the vertice should begin with 0
// the graph object of BGL is always begin with vertex 0
//' Test a graph is planar or not
//'@export
//'@param edgelist a edgelist dataframe without edge weights
//'@param VNum vertex number of a graph which related to the edgelist
//'@return FALSE OR TRUE
// [[Rcpp::export]]
int funcPlanarTest(Rcpp::NumericMatrix edgelist,int VNum){
  typedef adjacency_list<vecS,vecS,undirectedS,property<vertex_index_t, int> > Graph;
  Graph g(VNum);
  int n=edgelist.nrow();
  for(int i=0;i<n;i++){
    add_edge(edgelist(i,0),edgelist(i,1),g);
  }
   return boyer_myrvold_planarity_test(g);
}