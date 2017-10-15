library(tidyverse)
library(htmltab)
library(lubridate)
library(stringr)
library(htmlTable)

url="http://www.sfl.ch/superleague/spielplan/saison/"
get_this(36,url)

weeks=20:36
lapply(weeks,get_this,url) %>% bind_rows() %>% mutate(game=1:n()) -> games
games

savegames(games)

# column V1 is date, V2 KO time, V3 is home team (remove trailing CAPS), V5 ditto

savegames=function(x) {
  games %>% transmute(
    thegame=str_c(kickoff,round,game,home,away,sep=",")
  ) %>% write.table("spl.txt",quote=F,row.names=F)
}

get_this=function(x,url) {
  y=htmltab(url,which=x,header=0)
  process_table(y,x)
}


process_table=function(x,w) {
  # x is data frame
  if (has_name(x,"V2")) {
    x %>% unite(dt,c(V1,V2)) %>% 
      mutate(kickoff=dmy_hm(dt)) -> y
  } else {
    x %>% mutate(dt=paste0(V1,".17 12:00")) %>% 
      mutate(kickoff=dmy_hm(dt)) -> y
  }
    y %>% mutate(home=str_replace(V3,"[A-Z]+$",""),
           away=str_replace(V5,"[A-Z]+$",""),
           round=w-19) %>% 
    select(kickoff:round)
}
