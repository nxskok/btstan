data {
  int w;
}

parameters {
  real p;
}

model {
  p ~ unif(0,1);
  w ~ binom(9,p);
}