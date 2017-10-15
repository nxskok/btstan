# football-data.co.uk
library(tidyverse)
library(stringr)
setwd("R")

# E0-2: England
# SC0-1: Scotland
# D1-2: Germany
# I1-2: Italy
# SP1-2: Spain
# F1-2: France
# G1 (only): Greece

# update the 1718 ones
# create Sweden and Greece 2 (or both greece, for consistency of names)

# update spreadsheets by copying from sw match page (for this season), then

make_fd_alike("sweden0-1718-sw.csv","SW0-1718.csv")
make_fd_alike("sweden1-1718-sw.csv","SW1-1718.csv")
make_fd_alike("sweden0-1617-sw.csv","SW0-1617.csv")
make_fd_alike("sweden1-1617-sw.csv","SW1-1617.csv")


make_all_league_df("SW") %>% print(n=Inf)

# also need to solidify procedure for updating spreadsheets

# starting from R folder
# perl ../get_this_season.pl 

# functions


make_fd_alike=function(copy_csv,out_name) {
  s0=read_csv(copy_csv,col_names=F)
  s0 %>% fill(X2) %>% mutate(sc=str_split(X4," - ")) %>% 
    mutate(FTHG=as.integer(map_chr(sc,~.[[1]]))) %>% 
    mutate(FTAG=as.integer(map_chr(sc,~.[[2]]))) %>% 
    rename(HomeTeam=X3) %>% 
    rename(AwayTeam=X5) %>% 
    rename(Date=X2) %>% 
    mutate(Div=out_name) %>% 
    select(Div,Date,HomeTeam,AwayTeam,FTHG,FTAG) %>% 
    write_csv(out_name)
}

# new makexy

make_xy=function(d) {
  tf=factor(c(d$HomeTeam,d$AwayTeam))
  tfn=as.numeric(tf)
  X=matrix(tfn,ncol=2)
  Y= d %>% select(FTHG,FTAG) %>% as.matrix()
  n=length(table(X))
  gp=table(tf)
  stan.lookup=data.frame(id=1:n,l=levels(tf))
  list(X=X,Y=Y,z=stan.lookup,gp=gp)
}

make_all_league_df=function(lg) {
  ff=find_files(lg)
  map_df(ff,make_league_df) 
}

make_league_df=function(f) {
  suppressMessages(read_csv(f)) %>% select(1:6) %>% filter(!is.na(FTHG))
}


find_files=function(lg) {
  #' find all the files for a particular league (hint: delete old ones)
  re=paste0(lg,"*-*.csv")
  Sys.glob(re)
}

