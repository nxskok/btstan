# functions for csv-style data 2016-10-10

#' data frame of values for a league from pred file
frompred=function(lg) { # league name has 3 chars
  cmd=paste0("grep ","\',",lg,",\'"," stats.txt")
  xx=system(cmd,intern = T)
  y=read.csv(textConnection(xx),header=F,stringsAsFactors = F,colClasses = c(V2="character"))
  names(y)=c("game","resnow","league","npred","rp")
  y
}

#' as above, but output csv suitable for copying in
newgames=function(lg) {
  frompred(lg) %>% select(c(resnow,game)) %>% 
    write.csv(quote=F,row.names=F) 
}

#' data frame of values from current as text
fromcur=function(txt) {
  z=read.csv(textConnection(txt),header=F,stringsAsFactors = F,colClasses = c(V3="character"))
  names(z)=c("t1","t2","res","stuff")
  z
}

#' return games for which resnow and res are different
diffgames=function(y,z,all.games=F) {
  if (all.games) {
    bind_cols(y,z) %>% mutate(row=1:n()) %>% select(row,resnow,res,game,t1,t2,rp)
  }
  else {
    bind_cols(y,z) %>% mutate(row=1:n()) %>%  
      filter(resnow!=res) %>% 
      select(row,resnow,res,game,t1,t2)
  }
}

#' return games with resnow and res different, from txt and league name
#' 
diff.games=function(txt,lg,all) {
  z=fromcur(txt)
  y=frompred(lg)
  diffgames(y,z,all)
}

#' get predictions for teams t1 and t2 in league lg
best.all=function(lg,t1,t2,ha) {
  pp=do.pd(lg$pars,t1,t2,ha)
  bestall(pp,ept,pt.ptf)
}

#' data frame d with t1 and t2 to pull out and feed into best.all
#' 
best.df=function(lg,d,ha) {
  if (nrow(d)==0) return("No rows changed.")
  lookup=1:6
  names(lookup)=c("X ","21","2 ","1 ","0 ","10")
  v=character(0)
  for (i in 1:nrow(d)) {
    t1=d$t1[i]
    t2=d$t2[i]
    r=d$resnow[i]
    # d$rp has list of non-bonus scores, pass on to best.all
    z=best.all(lg,t1,t2,ha)
    j=lookup[[r]]
    v[i]=z[j]
  }
  d$pred=v
  d
}

#d=diff.games(txt,'en ') # txt from leagues.R
#d
best.df(england,d,T)

