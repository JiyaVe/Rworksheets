#W18
library(Rcpp)

addR <- function(x,y)
{
  return(x+y)
}

cppFunction('int addC(int x, int y)
            {
              int sum = x + y;
              return sum;
            }')
addR(4,5)
addC(4,5)

library(rbenchmark)

benchmark(addR(3,4),addC(3,4))

#R
EucR <- function(x,y)
{
  rtn <- sqrt(sum( (x-y)^2 ))
  return(rtn)
}

#C++

cppFunction('double EucC(NumericVector x, NumericVector y) {
double track = 0;
int n = x.size();
for(int i = 0; i < n; i++){
track = track + pow( (x[i] - y[i]), 2);
}
track = sqrt(track);
return track;
}
')

x <- rnorm(1e4)

y <- rnorm(1e4)

EucR(x,y)
EucC(x,y)

all.equal(EucR(x,y),EucC(x,y))

#Problems
#1

benchmark(EucR(x,y),EucC(x,y))

#3
cppFunction('
NumericMatrix matAdd(NumericMatrix A,NumericMatrix B)
{
 
  int m = A.nrow();
  int n = A.ncol();
  NumericMatrix C(m,n) ;
  
  for(int i = 0;i < m;i++){
    for(int j = 0;j< n;j++)
    {
      C(i,j) = A(i,j) + B(i,j);
    }
  }
  return(C);
}
')

A <- matrix(c(1,1,1,1,1,1), nrow = 2,ncol = 3)
B <- matrix(c(2,1,1,1,1,1), nrow = 2,ncol = 3)

matAdd(A,B)

#4
cppFunction('
NumericVector colsums(NumericMatrix A,NumericMatrix B)            
            ')