#include <Rcpp.h>
using namespace Rcpp;
//' convert a data.frame to list
//'@export
//'@param x the dataframe to be converted
//'@return an list
// [[Rcpp::export]]
List df2list(const DataFrame& x) {
  std::size_t nrows = x.rows();
  std::size_t ncols = x.cols();
  CharacterVector nms = x.names();
  List res(no_init(nrows));
  for (std::size_t i = 0; i < nrows; ++i) {
    List tmp(no_init(ncols));
    for (std::size_t j = 0; j < ncols; ++j) {
      switch(TYPEOF(x[j])) {
      case INTSXP: {
        if (Rf_isFactor(x[j])) {
        IntegerVector t = as<IntegerVector>(x[j]);
        RObject t2 = wrap(t[i]);
        t2.attr("class") = "factor";
        t2.attr("levels") = t.attr("levels");
        tmp[j] = t2;
      } else {
        tmp[j] = as<IntegerVector>(x[j])[i];
      }
      break;
      }
      case LGLSXP: {
        tmp[j] = as<LogicalVector>(x[j])[i];
        break;
      }
      case CPLXSXP: {
        tmp[j] = as<ComplexVector>(x[j])[i];
        break;
      }
      case REALSXP: {
        tmp[j] = as<NumericVector>(x[j])[i];
        break;
      }
      case STRSXP: {
        tmp[j] = as<std::string>(as<CharacterVector>(x[j])[i]);
        break;
      }
      default: stop("Unsupported type '%s'.", type2name(x));
      }
    }
    tmp.attr("class") = "data.frame";
    tmp.attr("row.names") = 1;
    tmp.attr("names") = nms;
    res[i] = tmp;
  }
  res.attr("names") = x.attr("row.names");
  return res;
}
