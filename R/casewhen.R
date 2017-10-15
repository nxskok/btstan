# case when

d=tibble(x=1:6,g=c("a","a","a","b","b","b"))
d
d %>% mutate(y=case_when(
  g=="a" ~ x,
  g=="b" ~ -x
  ))
