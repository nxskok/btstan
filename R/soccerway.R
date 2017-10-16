# get match info from soccerway
library(tidyverse)
library(rvest)
library(stringr)
library(anytime)
library(lubridate)

# leagues

leagues=read_csv("../europe.csv")
leagues

leagues %>% filter(level>1) %>% pull(comp) -> lev2


matches_by_id(id = 38654, what = "round")
(xx=map_df(lev2,table_by_id,"round"))



# testing safely/possibly

dc=date_comps("2017-10-13")
dc

dc=date_comps('2017-09-23') # do look-ahead eg 5 days and collect all the comps
dc


# get leagues with matches up to a week ahead

mydates=seq(as.Date("2017-10-10"),as.Date("2017-10-15"),"days")
mydates
dc=map(mydates,date_comps)
dc=unique(unlist(dc))
dc


games
(n=length(dc))

dc[ij]
#xx=table_by_comp_id(732)
(xx=map_df(dc[ij],table_by_comp_id))
xx %>% count(country,comp_name) %>% print(n=Inf)

dim(games)
games=combine(games,xx) 
games
saveRDS(games,"games.rds")

# get competition ids for several days and combine
# also need to think about getting list of matches (country is problem then: look up country later)


(games=combine(games,xx) )
saveRDS(games,"games.rds")

league=41547
(xx=table_by_id(league,"round")) # get latest page of results from scoresway

(m2=matches_in(games,league)) # matches in current database
(mm=missing_matches(games,league))
(xx=get_game_numbers(mm))
(xx=back_n(m2,300))

# if I find games from another league, I've (probably) gone back far enough. Or if the first thing in xx is the same league, 
# I should keep going.

# get latest page for several "rounds"

ll=lgs$`2017`
(leagues=ll[!is.na(ll)])
(xx=map_df(leagues,table_by_id,"round"))

second_divs=c()

(xx=back_n_league(39293,20,games))
ij=1:20
leagues[ij]
(xx=map_df(leagues[ij],back_n_league,25,games))

# fill in missing: these are games that I already have, collecting results; this is good.


tibble(days=seq(0.1,3,0.1)) %>% mutate(which=map(days,get_which,games)) %>% 
  mutate(rows=map_dbl(which,nrow)) %>% ggplot(aes(x=days,y=rows))+geom_point()+geom_line()
# get_which(0.9,games)

xx=get_results(games,3)
xx %>% select(-(1:3)) %>% filter(str_detect(score," - ")) %>% arrange(comp) %>% print(n=Inf)
xx %>% filter(str_detect(score," - ")) %>% count(country) %>% print(n=Inf)

dim(games)
games %>% mutate(is_res=str_detect(score," - ")) %>%  count(is_res)
games=combine(games,xx) 
saveRDS(games,"games.rds")
dim(games)
games %>% mutate(is_res=str_detect(score," - ")) %>%  count(is_res)


# get games that I don't already have, by anti-joining a list of matches to the ones in games
# this is good, uses dc from above
# chokes on leagues with no matches


dc
(n=length(dc))

# check for any problems

# the error that this gets (if any) is the first "list[2]"

tibble(comp=dc) %>% 
  mutate(matches=map(comp,matches_by_comp_id)) -> dd
dd %>% mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
nogood=which(!dd2$istib)
nogood

# get leagues

n
gets=setdiff(1:n,nogood)
gets
length(gets) 
tibble(comp=dc[gets]) %>% mutate(matches=map(comp,matches_by_comp_id)) %>%  # print(n=Inf)
  unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% get_games() -> xx # then combine, above
xx %>% select(-(1:3))
xx %>% count(country) %>% print(n=Inf)


# fill in missing countries

# check how far I need to go back by league (think I have them all now)

ll=lev2
ll

n_back=50
tibble(id=ll) %>% mutate(first=map_dbl(id,first_match,games)) %>% 
  filter(!is.na(first)) %>% 
  mutate(matches=map(first,match_search,n_back)) %>% unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% 
  mutate(country=NA_character_) %>% 
  get_games() -> xx

xx %>% count(comp) -> d
tibble(comp=ll) %>% left_join(d) %>% filter(!is.na(n)) %>% print(n=Inf) # counts how many of the searched-for comps had games found


games %>% arrange(comp,country) %>% print(n=Inf)

# fill in missing matches in a league (that I do not already have)

# experimenting with fill

d=tribble(
  ~x, ~y,
  1, "a",
  1, NA,
  1, "a",
  2, "b",
  2, NA,
  2, "b",
  3, NA
)
d %>%  arrange(x,y) %>% group_by(x) %>% fill(y)

games %>% arrange(comp,country) %>% group_by(comp) %>% fill(country) -> ggg
ggg %>% count(country) %>% print(n=Inf)
games=ggg

games %>% group_by(t1) %>% count(country) %>% filter(is.na(country))
games %>% group_by(t1) %>% count(country) %>% filter(t1 %in% c(78,83,87,89))


###################### functions

match_search=function(i,n) {
  seq.int(i-n,i,length.out=n+1)
}

first_match=function(compn,g) {
  g %>% filter(comp==compn) %>% summarize(f=first(match)) %>% pull(f)
}

not_in=function(matchlist,games) {
  matchlist[!(matchlist %in% games$match)]
}

get_results=function(d,since_days) {
  d %>% 
    filter(!str_detect(score," - ")) %>% 
    filter(time_stamp<Sys.time()) %>% 
    mutate(since=Sys.time()-time_stamp) %>% 
    filter(since<ddays(since_days)) %>% # or a bit more
    filter(since>dhours(2)) %>% # finished
    select(match,country,t1_name,t2_name,score,time_stamp,since) %>% 
    get_games()
}

get_which=function(since_days,d) {
  d %>% 
    filter(!str_detect(score," - ")) %>% 
    filter(time_stamp<Sys.time()) %>% 
    mutate(since=Sys.time()-time_stamp) %>% 
    filter(since<ddays(since_days)) %>% # or a bit more
    filter(since>dhours(2)) %>% # finished
    select(match,country,t1_name,t2_name,score,time_stamp,since) 
}

game_is_missing=function(games) {
  games %>% filter(!str_detect(score," - "))
}

back_n_league=function(ll,n,games) {
  print(ll)
  m2=matches_in(games,ll)
  if (length(m2)>0) {
    back_n(m2,n)
  } else {
    empty_match(0)
  }
}

back_n=function(j,n) {
  # j is a previous set of game numbers
  m=min(j)
  p=(m-n):(m-1)
  get_game_numbers(p)
}

get_game_numbers=function(j) {
  map_df(j,game_data,NA_character_)
}

missing_matches=function(games,comp) {
  mm=matches_in(games,comp)
  pp=min(mm):max(mm)
  pp[!(pp %in% mm)]
}

matches_in=function(games,this_comp) {
  games %>% filter(comp==this_comp) %>% pull(match)
}

read_html_safe=possibly(read_html,otherwise=NA)

combine=function(m1,m2) {
  m2 %>% bind_rows(m1) %>% group_by(match) %>% summarize_all(first)
}

table_by_comp_id_safe=safely(table_by_comp_id,otherwise=NA)

table_by_comp_id=function(id) {
  matches=matches_by_comp_id(id)
  map_df(matches$matches,game_data,matches$country)
}

table_by_id=function(id,what) {
  matches=matches_by_id(id,what)
  print(matches)
  map_df(matches$matches,game_data,matches$country)
}

get_games=function(d) {
  map2_df(d$match,d$country,game_data)
}


matches_by_id=function(id,what) {
  print(id)
  base_url_1="http://www.scoresway.com/?sport=soccer&page="
  base_url_1a="&id="
  base_url_2="&view=matches"
  url=paste0(base_url_1,what,base_url_1a,id,base_url_2)
  print(url)
  html=read_html_safe(url)
  if (is.na(html)) return(NA)
  html %>% html_nodes("table") %>% html_nodes("td") %>% html_nodes("a") %>% html_attr("href") -> hrefs
  # find the ones that refer to a match
  if (is.null(hrefs)) return(list(matches=0,country=""))
  if (length(hrefs)==0) {
    return(list(matches=0,country=""))
  }
  hrefs %>% str_split("&",simplify=T) %>%  as.data.frame() %>% 
    mutate(is_match=(V2=="page=match")) %>% 
    mutate(is_not_event=!str_detect(V3,"events$")) %>% 
    filter(is_match,is_not_event) %>% 
    mutate(id=extract_id(V3)) %>% pull(id) -> ids
  print(ids)
  html %>% html_nodes("h2") %>% .[2] %>% html_text() -> country
  list(matches=unique(ids),country=country)
  
}

matches_by_comp_id=function(id) {
  base_url_1="http://www.scoresway.com/?sport=soccer&page=competition&id="
  base_url_2="&view=matches"
  url=paste0(base_url_1,id,base_url_2)
  html=read_html_safe(url)
  if (is.na(html)) return(list(matches=0,country=""))
  html %>% html_nodes("table") %>% html_nodes("td") %>% html_nodes("a") %>% html_attr("href") -> hrefs
  # find the ones that refer to a match
  if (length(hrefs)==0) {
    return(list(matches=numeric(0),country=""))
  }
  hrefs %>% str_split("&",simplify=T) %>%  as.data.frame() %>% 
    mutate(is_match=(V2=="page=match")) %>% 
    mutate(is_not_event=!str_detect(V3,"events$")) %>% 
    filter(is_match,is_not_event) %>% 
    mutate(id=extract_id(V3)) %>% pull(id) -> ids
  html %>% html_nodes("h2") %>% .[2] %>% html_text() -> country
  tibble(matches=unique(ids),country=country)
  }


date_of_match=function(dds) { # get date and kickoff time
  dds[2] %>% html_nodes("a") %>% html_attr("href")
}

date_comps=function(the_date) { # get the competitions on this date
  base_url="http://www.scoresway.com/?sport=soccer&page=matches&date="
  url=paste0(base_url,the_date)
  html=read_html_safe(url)
  if (is.na(html)) return(NA)
  html %>% html_nodes("th") %>% html_nodes("a") %>% html_attr("href") -> today_comps
  extract_id(today_comps)
}

get_game=function(mid) { # this reads the website
  match_base_url="http://www.scoresway.com/?sport=soccer&page=match&id="
  url=paste0(match_base_url,mid)
  read_html_safe(url)
}

empty_match=function(match_number=-1) {
  # same format as tibble returned by game_data
  tibble(match=match_number,t1=0,t2=0,comp=-1,country="",t1_name="none",t2_name="none",score="none",
         comp_name="none",time_stamp=anytime(0,tz="America/Toronto"),retrieved=Sys.time())
}

game_data=function(mid,ctry="") {
  html=get_game(mid)
  if (anyNA(html)) return(empty_match(mid))
  html %>% html_nodes("h3") -> game_info
  if (is.null(game_info)) return(empty_match(mid))
  if (length(game_info)==0) return(empty_match(mid))
  if (is.na(game_info)) return(empty_match(mid))
  # print(game_info)
  # 1 and 3 are teams, 2 is score or kickoff time
  # get id and team name
  txt=map_chr(game_info,game_text) # t1 name, score/ko, t2 name
  num=game_ids(game_info) # t1 id, t2 id
  html %>% html_nodes("dl") %>% html_nodes('dd') -> dds
  dds[1] %>% html_text() -> comp_text
  dds[1] %>% html_nodes("a") %>% html_attr("href") -> comp_info
  comp_id=extract_id(comp_info)
  dds[2] %>% html_nodes("span") %>% html_attr("data-value") -> time_stamp
  time_stamp=as.numeric(time_stamp)
  tibble(match=as.integer(mid),t1=as.integer(num[1]),t2=as.integer(num[2]),
         comp=as.integer(comp_id),country=ctry,
         t1_name=txt[1],t2_name=txt[3],score=txt[2],comp_name=comp_text,
         time_stamp=anytime(time_stamp,tz="America/Toronto"),retrieved=Sys.time())
}

game_text=function(game_inf) {
  game_inf %>% html_text() %>% trimws()
}

extract_id=function(s) {
  s %>%  
    str_extract("id=\\d+") %>% 
    str_extract("\\d+") %>% 
    as.numeric()
}

game_ids=function(game_inf) {
  game_inf %>% html_nodes("a") %>% html_attr("href") -> info
  extract_id(info)
}

