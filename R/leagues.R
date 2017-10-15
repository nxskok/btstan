lgs=read.csv("leagues.csv",header=T)
lgs
v=c(lgs[,3],lgs[,4],lgs[,5])
v=v[!is.na(v)]
v
library(rstan)
con=get.con()
pp=run2(con,v,100)
options(width=120):
pp[30000:40000,]
pr=grepl("Steaua",pp[,1],perl=T)
table(pr)
pp[pr,]

# don't use run2, but instead get predictives singly


run4=function(comp,nit=10000,add_country=F)
{
  d=make_all_league_df(comp)
  xy.list=make_xy(d)
  model.2=sample.model(p.sc,xy.list,nit)
  pars=rstan::extract(model.2)
  date=Sys.time()
  list(pars=pars,xy=xy.list,date=date)
}

con
comp
lgs
v

l=run4(con,v,100)
names(l)
l$xy$gp
dim(l$pars$o)
do.predict(l$pars,39,30)
head(pars)
names(pars)
summary(l$pars$h)

# list ratings

list.rat=function(xx,sort=T)
    {
        teams=xx$xy$z$l
        n=length(teams)
        id=1:n
        off=apply(xx$pars$o,2,mean)
        off=round(off,2)
        def=apply(xx$pars$d,2,mean)
        def=round(def,2)
        tot=off+def
        if (sort) {
          o=order(-tot)
        } 
        else {
          o=id
        }
        data.frame(team=teams[o],id=id[o],off=off[o],def=def[o],sum=tot[o],diff=off[o]-def[o])
}

list_teams=function(x) {
  n=length(x$xy$z$l)
  d=data.frame(x$xy$z$l,1:n)
  n3=ceiling(n/3)
  dd=data.frame(t1=d[1:n3,1],n1=d[1:n3,2],t2=d[(n3+1):(2*n3),1],n2=d[(n3+1):(2*n3),2],t3=d[(2*n3+1):(3*n3),1],n3=d[(2*n3+1):(3*n3),2])
  dd
}

do.preds=function(lg,txt,pt.fn,detab=F) {
  z=fromcur(txt)
  if (detab) z$stuff=gsub("\t"," ",z$stuff)
  v=numeric(0)
  for (i in 1:nrow(z)) {
    pp=do.pd(lg$pars,z[i,1],z[i,2],T)
    v[i]=best1(pp,ept,pt.cp)
  }
  z$pred=v
  z
}


#########################################################################
# start here
#########################################################################


# prelims
library(tidyverse)
library(dbplyr)
library(rstan)  
library(lubridate)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# con=get.con() for the moment
setwd("R")
system("./stats.sh >stats.txt")
stats=read_csv("stats.txt")
stats %>% print(n=Inf)


setwd("..")

###################################################################
# send rpushbullet note
####################################################################

library(RPushbullet)
pbPost(type="note",title="Done",body="the code is run")

#####################################################################

# accessing database with dplyr (check if I have all the matches yet)



db=src_sqlite("/home/ken/sports/scoresway/soccer.db")
scores=tbl(db,"scores")
teams=tbl(db,"teams")
comptab=tbl(db,"comps")
head(comptab)
db

# get comps from below

# this is how to get matching comps

tbl(db,sql("select * from comps where name like '%greece%' and name like '% 2017/2018 %' and id>35000 order by name")) %>% print(n=100)


td=today()
td5=today()+ddays(5)
td2=today()-ddays(2)
scores %>% filter(comp %in% comps,date>=td2,date<td5) %>% 
  left_join(teams,by=c("t1"="id")) %>% # looking up teams
  left_join(teams,by=c("t2"="id")) %>% 
  left_join(comptab,by=c("comp"="id")) %>% # looking up competition
  select(match,date,name.x,name.y,result,score,name,comp) %>% 
  arrange(date) %>% 
  print(n=500,width=Inf)

# this is a much smoother method of accessing the database
# regular expression seems to need "like" in sql though

####################################################################
# euro

lgs=read_csv("leagues.csv") # will need updating
v=c(lgs[[3]],lgs[[4]],lgs[[5]]) 
v=v[!is.na(v)]
comps=v
comps
euro=run4(con,comps,1000,add_country = T)

#######################################################################
# most recent rating dates

leaguenames=c("euro","england","france","germany","greece","italy","scotland","spain","sweden")

get_date=function(x) { as.character(get(x)$date) }
tibble(league=leaguenames,dt=map_chr(leaguenames,get_date))

######################################################################

#idea: make data frame of leagues and years, pull out ones needed
#for all leagues, including top divisions for these leagues and top division for those part of europe

######################################################################

# england
comps=c(31553,31555,31556,31557,31471,31472,31473,35992,36636,36637,36638,36639,36640,36641,41547,42070,41820)
england=run4(con,comps)
# fa cup: add successive cup rounds. Resp leagues 2016, cup 2016

comps=c(
        31553,31555,31471,31474,31419,31413:31414,32388:32390,32417:32420,32422:32423, # leagues 2016
        31650,32925,31651,33074,31652,33220,31653,33322,31654,33348,31655,33394,31656,33516,31657,33879,31658,34185,34515,31659:31663,34883,34765, # facup 2016
        36444,36445,
        36447,36448,
        36425,36426,
        36429,36430,
        36433,36434,
        36437,36438,
        36424,36427,
        36428,36431,
        36432,36435,
        36436,36439,
        36440,36441,
        36442,
        36449,
        36446, # facup 2017
        35992,36638,36641,36644,35949,36276,36273,35629,35623,35626,35603,35604,35597,35598,35599,35600
        ) # leagues 2017 (take out 2015 at some point, maybe after 1st round)
facup=run4(con,comps,1000)
# france
comps=c(31546,31517,35879,35875,41646,41647)
france=run4(con,comps)
# germany
comps=c(31432,31469,31545,35823,35824,36073,41485,41486)
germany=run4(con,comps)
# greece
comps=c(32039,32040,33184,36669,36670,38508,42568) # needs football league
greece=run4(con,comps)
# italy
comps=c(31554,31798,36003,37192,42011) 
italy=run4(con,comps)
# scotland
comps=c(31205,31206,31207,31567,31568,31578,35877,35878,35425,36070,36071,36072,41506,41507,40975)
scotland=run4(con,comps)
# spain ## start here for transcription to ss
comps=c(31781,32028,35880,37748,41509,42973)
spain=run4(con,comps)
# sweden
comps=c(30329,30330,33927,33944,39293,39292)
sweden=run4(con,comps)
# switzerland
comps=c(31601,31602,36501,36890)
switz=run4(con,comps)
# europe int'l (euro 2012/2016, wcq 2014)
comps=c(24218,24219,11138,16313,15652,15653,13552,13555,13556,13557,31060:31064) # check
intl=run4(con,comps)
# europe u21
comps=c(20361,28875,28877,29653,29655,30630,30631,30632,30633,30634)
u21=run4(con,comps)
# women's world cup
comps=c(13377,13559:13562,29233,29235,29809,28267,24667,25689,25835,29159,29161,29163,20080:20081,28991,28993,22888:22893,19562,23097,23768,23098,23100,23099,20925:20930)
wwc=run4(con,comps)
# copa america (includes concacaf)
comps=c(
  20898:20901,31003:31007, # concacaf gold cup 2013 & 2015
  15678:15679, 30437, 30644, # concacaf wc qual (2014 & 2018)
  28219, 28221, 28223, 28225, 28227, # copa america 2015
  15676, 31495, # s americawc qual 2014 & 2018
  16351:16356 # world cup finals 2014 (includes other teams, to ignore, but 
              # where did the other teams on the list come from?)
)
ca=run4(con,comps)


ll=read_csv("leagues.csv")
ll %>% gather(year,comp,`2015`:`2017`) %>% write_csv("leagues2.csv")

thedate=function(s) {
  x=get(s)
  x$date
}

show=function(league) {
  league$xy$z$l %>% paste(1:length(.),.) %>% print(quote=F)
}

scotland$xy$z$l %>% paste(1:length(.),.)

save.image()
thecomps=c("euro","england","facup","france","germany","greece","italy","scotland","spain","sweden","switz")
datetime=as.POSIXct(sapply(thecomps,thedate),origin="1970-01-01")
data.frame(dt=sort(datetime))

#############################################################

# new ratings


#############################################################
# perl get_this_season.pl

england=run4("E")
france=run4("F")
germany=run4("D")
greece=run4("G")
italy=run4("I")
scotland=run4("SC")
spain=run4("SP")
sweden=run4("SW")
# sweden: update spreadsheet(s), convert spreadsheet to fd format:
# fd.R
# then sweden=run4("SW")
# greece 2nd division also (not started yet)
# for greece may be some translation of names (omit?)


#############################################################

system("perl stats.pl euc")
system("perl stats.pl eue")
system("perl stats.pl en")
system("perl stats.pl en2")
system("perl stats.pl fr")
system("perl stats.pl de")
system("perl stats.pl gr")
system("perl stats.pl it")
system("perl stats.pl sc")
system("perl stats.pl es")
system("perl stats.pl se")


# select.pred(l)



list.rat(euro)
list.rat(euro, sort=F)
x=list.rat(euro)
x %>% slice(1:30)

euro$xy$z$l
euro$date
system("perl stats.pl euc")
stats %>% filter(league=="euc ") # %>% slice(5:16)
pp=pd(euro) ; bestall(pp,ept,pt.ptf)

# newgames('euc')
# show(euro)
txt="
44,114,21,Apoel Nic v Bor Dortmd                       
237,674,X ,Feyenoord v Shakt Donsk                      
463,512,10,Man City v Napoli                            
494,101,10,Monaco v Besiktas                            
466,435,21,NK Maribor v Liverpool                       
615,587,X ,RB Leipzig v FC Porto                        
617,783,10,Real Madrid v Tottenham                      
720,672,X ,Spartak Moscow v Sevilla                     
600,63,21,FK Qarabag v Atl Madrid                      
36,594,21,Anderlecht v Paris SG                        
87,542,10,Barcelona v Olympiakos                       
93,141,10,Bayern Mun v Celtic                          
99,464,21,Benfica v Man Utd                            
144,633,10,Chelsea v Roma                               
163,89,0 ,CSKA v Basel                                 
362,732,10,Juventus v Sporting                          
"
d=diff.games(txt,'euc',F)
best.df(euro,d,T)

# newgames('eue')
# show(euro)
txt="
56,458,X ,FC Astana v Maccabi Tel-Aviv                 
91,383,X ,BATE Bor v 1. FC Köln                        
161,53,21,Crvena Zvezda v Arsenal                      
303,740,1 ,Hapoel Be'er Sheva v Steaua Buc              
385,650,X ,Konyaspor v FC RB Salzb                      
450,838,X ,Lugano v Viktoria Plzen                      
544,844,10,Marseille v Vitória Guimarães                
523,413,X ,Nice v Lazio                                 
552,61,1 ,Östersunds FK v Ath Bilbao                   
894,842,X ,SV Zulte Waregem v Vitesse                   
823,618,2 ,Vardar v Real Sociedad                       
882,635,10,Zenit St P v Rosenborg                       
891,316,2 ,Zorya Luhansk v Hertha BSC                   
484,14,10,Milan v AEK Athens                           
60,45,10,Atalanta v Apollon Limassol                  
68,627,10,Austria Vienna v HNK Rijeka                  
205,872,10,Dynamo Kiev v Young Boys                     
227,543,X ,Everton v Lyon                               
322,346,10,Hoffenheim v Istanbul Basaksehir             
678,441,2 ,Sheriff Tiraspol v Lokomotiv Moscow          
697,567,X ,Skenderbeu v Partizan Belgrade               
730,447,0 ,Sporting Braga v Ludo Razgd                  
839,705,10,Villarreal v Slavia Prague                   
890,381,2 ,Zlín v FC Copenhagen                      
"
d=diff.games(txt,'eue',F)
best.df(euro,d,T)

# newgames('en ')
# show(england)
txt="
35,37,X ,Liverpool v Man Utd                          
15,70,X ,Burnley v West Ham                           
23,20,21,Crystal Palace v Chelsea                     
36,62,10,Man City v Stoke                             
64,30,0 ,Swansea v Huddersfield                       
66,9,10,Tottenham v Bournemouth                      
68,2,21,Watford v Arsenal                            
12,26,X ,Brighton v Everton                           
60,41,XS ,Southampton v Newcastle                      
34,69,0 ,Leicester v West Brom                
"
d=diff.games(txt,'en ',F)
best.df(england,d,T)

# newgames('en2')
# show(england)
txt="
5,18,2 ,Birmingham v Cardiff                         
13,16,10,Bristol City v Burton                        
4,38,X ,Barnsley v Middlesbrough                     
8,58,21,Bolton v Sheff Wed                           
11,39,0 ,Brentford v Millwall                         
28,51,0 ,Fulham v Preston                             
33,53,10,Leeds v Reading                              
43,31,0 ,Norwich v Hull                               
57,32,0 ,Sheff Utd v Ipswich                          
63,52,X ,Sunderland v QPR                             
72,3,X ,Wolves v Aston Villa                         
24,44,0 ,Derby v Nottm Forest            
"
d=diff.games(txt,'en2',F)
best.df(england,d,T)


# newgames('fr ')
# show(france)
txt="
20,23,X ,Lyon v Monaco                                
13,32,21,Dijon v Paris SG                             
10,4,0 ,Caen v Angers                                
14,36,X ,Guingamp v Rennes                            
18,42,0 ,Lille v Troyes                               
38,22,10,Saint-Étienne v Metz                         
40,3,0 ,Toulouse v Amiens                            
7,26,0 ,Bordeaux v Nantes                            
24,27,2 ,Montpellier v Nice                           
39,21,21,Strasbourg v Marseille                 
"
d=diff.games(txt,'fr ',F)
best.df(france,d,T)

# newgames('de ')
# show(germany)
txt="
35,12,0 ,Stuttgart v 1. FC Köln                       
2,14,10,Bayern Mun v SC Freiburg                     
17,10,X ,Hannover v Eintracht Frankfurt               
19,33,X ,Hertha BSC v Schalke                         
20,1,10,Hoffenheim v FC Augsburg                     
26,16,0 ,Mainz 05 v Hamburger SV                      
7,30,10,Bor Dortmd v RB Leipzig                      
25,38,10,Bayer Levkn v VfL Wolfsburg                  
37,27,X ,Werder Bremen v B Gladbach                   
"
d=diff.games(txt,'de ',F)
best.df(germany,d,T)

# newgames('gr ')
# show(greece)
txt="
5,16,10,PAS Giannina v Platanias                     
4,3,0 ,Atromitos Athens v Asteras Tripolis          
14,11,X ,Panionios v Olympiakos                       
15,8,10,PAOK Salonika v Lamia                        
2,7,0 ,Apollon Smyrnis v Kerkyra                    
13,10,0 ,Panetolikos v Levadiakos                     
18,1,1 ,Xanthi v AEK Athens                          
12,9,10,Panathinaikos v Larissa                      
"
d=diff.games(txt,'gr ',F)
best.df(greece,d,T)


# newgames('it ')
# show(italy)
txt="
21,23,10,Juventus v Lazio                             
33,25,X ,Roma v Napoli                                
16,42,10,Fiorentina v Udinese                         
6,37,10,Bologna v SPAL                               
8,19,X ,Cagliari v Genoa                             
14,40,21,Crotone v Torino                             
35,2,X ,Sampdoria v Atalanta                         
36,11,X ,Sassuolo v Chievo                            
20,24,0 ,Inter Milan v Milan                          
44,5,10,Hellas Verona v Benevento                 
"
d=diff.games(txt,'it ',F)
best.df(italy,d,T)

# concorso pronostici

# do.preds(italy,txt,pt.cp)
  
  
# newgames('sc ')
# show(scotland)
txt="
23,21,X ,St Johnstone v Rangers                       
4,6,10,Celtic v Dundee                              
10,17,X ,Hamilton v Motherwell                        
12,1,X ,Hibernian v Aberdeen                         
18,14,0 ,Partick Thistle v Kilmarnock                 
22,11,X ,Ross County v Hearts                         
"
d=diff.games(txt,'sc ',F)
best.df(scotland,d,T)

# newgames('es ')
# show(spain)
txt="
15,25,10,Espanyol v Levante                           
5,36,X ,Ath Bilbao v Sevilla                         
16,34,21,Getafe v Real Madrid                         
1,38,2 ,Alavés v Real Sociedad                       
6,7,X ,Atl Madrid v Barcelona                       
13,21,X ,Eibar v Deportivo La Coruña                  
18,45,X ,Girona v Villarreal                          
28,23,X ,Malaga v Leganés                             
9,42,X ,Real Betis v Valencia                        
22,11,X ,Las Palmas v Celta Vigo                      
"
d=diff.games(txt,'es ',F)
best.df(spain,d,T)


# newgames('se ')
# show(sweden)
txt="
17,20,X ,Halmstads BK v IFK Göteborg                  
16,1,0 ,BK Häcken v AFC United                       
30,27,0 ,Östersunds FK v Örebro SK                    
2,21,10,AIK v Jönköpings Södra IF                    
10,15,10,IF Elfsborg v GIF Sundsvall                  
31,9,1 ,IK Sirius v Djurgårdens IF                   
26,24,1 ,IFK Norrköping v Malmö FF                    
22,18,X ,Kalmar FF v Hammarby IF                      
"
d=diff.games(txt,'se ',F)
best.df(sweden,d,T)



