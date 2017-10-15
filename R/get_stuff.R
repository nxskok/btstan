library(rvest)
library(tidyverse)
library(stringr)
library(lubridate)


cutoff="2017-08-31 08:50" # set cutoff equal to now, adjust now
#cutoff="2017-08-24 17:40" # set cutoff equal to now, adjust now
now="2017-08-31 19:00"
get_leagues(cutoff)



# functions


date_of=function(dt) {
  # given a POSIXct date-time, as constructed by ymd_hms, make a text date
  sprintf("%4d-%02d-%02d",year(dt),month(dt),day(dt))
}

get_leagues=function(cutoff_dt) {
  lo=date_of(cutoff_dt)
  hi=date_of(Sys.Date())
  cc=newgamecomps(cutoff_dt)
  print(cc)
  ll=map(cc,oppo,lo,hi)
  for (i in ll) {
    print(i$name)
    if (!is.null(i$table)) print(i$table,n=Inf)
    print(i$games)
    readline(prompt="Press enter to continue")
  }
}

url_of_id=function(id) {
  str_c("http://scoresway.com/?sport=soccer&page=round&id=",id)
}

comp_name=function(id_wanted) {
  db=src_sqlite("/home/ken/sports/scoresway/soccer.db")
  comps=tbl(db,"comps")  
  comps %>% filter(
    id==id_wanted) %>% pull(name)
}

table_html=function(id) {
  # from league id get HTML table nodes
  url=url_of_id(id)
  html=read_html(url)
  sleep(5)
  html %>% html_nodes("table") # a list of tables
}

colours=function(tab) {
  # from table as HTML object get vector of colours, one per team
  classes=tab %>% html_nodes('td') %>% html_attr("class") 
  has_rank=str_subset(classes,"^rank")
  str_replace_all(has_rank,"rank","")
  m=str_split_fixed(has_rank,"-",n=3)
  str_c(m[,2],m[,3])
}

leaguetables=function(id) {
  # from id get league tables containing desired info
  url=url_of_id(id)
  tables=table_html(url)
  table_list=map(tables,html_table)
  colour_list=map(tables,colours)
  cols=map_dbl(table_list,ncol) 
  if (!any(cols==12)) return(tibble())
  tibble(cols=cols,table=table_list,colours=colour_list) %>% filter(cols==12) %>% 
    mutate(which=row_number()) %>% select(-cols) %>% unnest() %>% 
    select(which,colours,`#`,Team,MP,D,P)
}

matches=function(id,lo="2000-01-01",hi="2018-07-08") {
  url=url_of_id(id)
  url2=str_c(url,"&view=matches")
  tables=table_html(url2)
  matches=html_table(tables[[1]])
  matches=matches[,!duplicated(colnames(matches))]
  matches %>% select(-6) %>% 
    mutate(thedate=dmy(Date)) %>% 
    filter(thedate>=lo, thedate<=hi) %>% 
    mutate(`Score/Time`=str_replace_all(`Score/Time`,"\n","")) %>% 
    mutate(`Score/Time`=str_replace_all(`Score/Time`," {2,}",""))
}


process_result=function(x,reverse=F) {
  m=str_split_fixed(x,"-",2)
  what=case_when(
    m[,2]=="" ~ "sched",
    reverse   ~ str_c(as.numeric(m[,2]),as.numeric(m[,1]),sep=" - "),
    TRUE      ~ str_c(as.numeric(m[,1]),as.numeric(m[,2]),sep=" - ")
    )
  what
}

with_oppo=function(tab,m) {
  tab %>% left_join(m,by=c("Team"="Home team")) %>% 
    left_join(m,by=c("Team"="Away team")) %>% 
    mutate(oppres=case_when(
      !is.na(`Home team`) ~ str_c("@ ",`Home team`," ",process_result(`Score/Time.y`,reverse=T)),
      !is.na(`Away team`) ~ str_c("v ",`Away team`," ",process_result(`Score/Time.x`)),
      TRUE ~ ""
    )) %>% 
    select(c(which:P,oppres))
}

oppo=function(id,lo,hi) {
  tab=leaguetables(id)
  m=matches(id,lo,hi)
  name=comp_name(id)
  if (ncol(tab)==0) {
    return(list(id=id,name=name,games=m))
  } else {
    return(list(id=id,name=name,table=with_oppo(tab,m),games=m)
)
  }
}

newgamecomps=function(r=' ') {
  db=src_sqlite("/home/ken/sports/scoresway/soccer.db")
  scores=tbl(db,"scores")
  scores %>% filter(retrieved>r) %>% pull(comp) %>% unique()  
}

map(cc,oppo,"2017-07-07","2017-07-09")



############# jottings

compname(40208)
comp_names_urls(cc)
cc=newgamecomps('2017-07-09 10:42')
uu=urlfromcomps(cc)
uu
leaguetables(uu[1])
oppo(uu[2],'2017-07-07','2017-07-09')

url="http://www.scoresway.com/?sport=soccer&page=competition&id=1344"
url="http://www.scoresway.com/?sport=soccer&page=competition&id=1119"
url="http://www.scoresway.com/?sport=soccer&page=round&id=39664"
url="http://www.scoresway.com/?sport=soccer&page=competition&id=28"
url="http://www.scoresway.com/?sport=soccer&page=competition&id=33"
url="http://www.scoresway.com/?sport=soccer&page=competition&id=1133"

oppo(url,lo="2017-07-07",hi="2017-07-08")

db=src_sqlite("/home/ken/sports/scoresway/soccer.db")
scores=tbl(db,"scores")
scores %>% filter(retrieved>' ') %>% pull(comp) %>% unique()
