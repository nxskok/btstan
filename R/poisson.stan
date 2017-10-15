data
{
  int<lower=1> nt;
  int<lower=1> ng;
  int x[ng,2];
  int y[ng,2];
}

parameters
{
  real o[nt];
  real d[nt];
}

model
{
  int t1;
  int t2;
  real eta1;
  real eta2;
  o~normal(0,3);
  d~normal(0,3);
  for (i in 1:ng) {
    t1=x[i,1];
    t2=x[i,2];
    eta1=exp(o[t1]-d[t2]);
    eta2=exp(o[t2]-d[t1]);
    y[i,1]~poisson(eta1);
    y[i,2]~poisson(eta2);
  }
}
