#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerVector timesTwo(IntegerVector x) {
  Rcout << x << endl;
  for(int i = 0; i < (x.length() - 1); i++ ){
    Rcout << "Origin   = " << i << endl; // 100 2 3
    Rcout << "Next     = " << (i + 1) << endl; // 100 2 3
    Rcout << x << endl;
    if(x[i] > x[(i + 1)]){
      int temp = x[i];
      x[i] = x[(i + 1)];
      x[(i + 1)] = temp;
    }
  }
    return x;
}


/*** R
timesTwo(sample(1:10))
*/
