#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Nullable<NumericVector> smoothC(NumericVector x, double alpha, double beta, double lod)
{
  if(NumericVector::is_na(alpha) || NumericVector::is_na(beta)){return R_NilValue;}
  if(alpha <= 0 || alpha >= 1){stop("'alpha' must be in (0,1)");}
  if(beta <= 0 || beta > 1){stop("'beta' must be in (0,1]");}
  
  int n = x.size();
  
  NumericVector newx(n);
  newx[0] = x[0];
  
  for(int i = 1; i < n; i++)
  {
    newx[i] = newx[i - 1]*alpha + x[i]*beta;
    if(newx[i] < lod)
    {
      newx[i] = 0;
    }
  }
  return newx;
}


// [[Rcpp::export]]
NumericVector windowMeanC(NumericVector x, int window, double lod)
{
  int n = x.size();
  NumericVector newx(n);

  for(int i = 0; i < window; i++)
  {
    double tmp = 0;
    for(int j = 0; j <= i; j++)
    {
      tmp += x[j];
    }
    newx[i] = tmp / (i + 1);
  }
  
  for(int i = window; i < n; i++)
  {
    double tmp = 0;
    for(int j = i - window + 1; j <= i; j++)
    {
      tmp += x[j];
    }
    newx[i] = tmp / window;
  }
  
  for(int i = 0; i < n; i++)
  {
    if(newx[i] < lod)
    {
      newx[i] = 0;
    }
  }

  return newx;
}

/*** R
# any test code goes here
*/
