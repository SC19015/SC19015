#include <Rcpp.h>
using namespace Rcpp;
//' @title A random walk Metropolis sampler  using Rcpp
//' @description a random walk Metropolis sampler  using Rcpp
//' @param sigma the standerror of norm distribution
//' @param N the number of samples
//' @return sample a random sample of size N
//' @return reject the number of rejections
//' @examples
//' \dontrun{
//' Metr(1,100)
//' }
//' @export
// [[Rcpp::export]]
List Metr(double sigma , int N ) {
  NumericVector x(N);
  x[0] = 0;
  double u = 0, y = 0;
  int k = 0;
  for (int i = 1;i <N; i++) {
    y = rnorm(1, x[i-1], sigma)[0];
    u = runif(1)[0];
    if (u <= (0.5*exp(-abs(y))) / (0.5*exp(-abs(x[i-1])))){
      x[i] = y; 
    }else {
        x[i] = x[i-1];
        k++;
      }
  }
  return List::create(
    _["sample"] = x,
    _["reject"] = k
  );
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
