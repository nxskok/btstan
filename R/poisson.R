# fit poisson model for soccer scores via
# stan
# y_ij|beta ~ poisson(exp(home+o_i-d_j))
# home, o's, d's prior normal
# data y[] n x 2 of scores by team
# team numbers in x, likewise n x 2

# DON'T USE THIS!
#library(rstan)
#p.sc=stan_model("poisson.stan")
p.sc
xx=c(1,2,2,3,3,1,1,3)
x=matrix(xx,ncol=2,byrow=T)
x
yy=c(2,0,1,1,1,2,3,0)
y=matrix(yy,ncol=2,byrow=T)
y
p.1=sampling(p.sc,list(x=x,y=y,ng=4,nt=3),iter=10)
p.1
pars=extract(p.1)
names(pars)
pars$o[,3]
l1=exp(pars$o[,1]-pars$d[,2])
l2=exp(pars$o[,2]-pars$d[,1])
plot(l1,l2)
# predictive distribution for 1 vs 2
y1=rpois(length(l1),l1)
y2=rpois(length(l2),l2)
yt=table(y1,y2)
sum(yt)
sum(y1>y2)
sum(y1==y2)
sum(y1<y2)

# make a function to do p.d. for i vs j

#' Title
#'
#' @param i 
#' @param j 
#' @param pars 
#' @param ha 
#'
#' @return
#' @export
#'
#' @examples
pd=function(i,j,pars,ha=T)
{
    if (ha) {
        l1=exp(pars$o[,i]-pars$d[,j]+pars$h)
    }
    else {
        l1=exp(pars$o[,i]-pars$d[,j])
    }
    l2=exp(pars$o[,j]-pars$d[,i])
    y1=rpois(length(l1),l1)
    y2=rpois(length(l2),l2)
    yt=table(y1,y2)
    ns=sum(yt)
    c(sum(y1>y2)/ns,
      sum(y1==y2)/ns,
      sum(y1<y2)/ns)
}

pd(1,2,pars)
pd(1,3,pars)
pd(2,3,pars)

# rating list, organize by sum of o+d

nt=3
s=numeric(nt)
o=numeric(nt)
d=numeric(nt)
for (i in 1:nt)
{
  o[i]=mean(pars$o[,i])
  d[i]=mean(pars$d[,i])
  s[i]=mean(o[i]+d[i])  
}
s
ord=order(-s)
ord
dd=data.frame(o=o,d=d,s=s)
dd[ord,]

# get some soccer data

library(RSQLite)
m=dbDriver("SQLite")
con=dbConnect(m,"/home/ken/sports/scoresway/soccer.db")
query="select * from teams where name='Grasshopper'"
rs=dbSendQuery(con,query)
fetch(rs,n=-1)
# teams 2178 and 5411
query="select max(date), comp from scores where t1=2178 or t2=2178 group by comp"
rs=dbSendQuery(con,query)
fetch(rs,n=-1)
# apparently 21467
query="select * from scores where comp=21467"
rs=dbSendQuery(con,query)
d=fetch(rs,n=-1)
head(d)
teams=unique(c(d$t1,d$t2))
teams
names=numeric(0)
for (i in teams)
{
  query=paste("select name from teams where id =",i)
  rs=dbSendQuery(con,query)
  dd=fetch(rs,n=-1)
  print(dd[1,1])
  names=c(names,dd[1,1])
}
ltab=data.frame(teams,names)
ltab

which(2175 == teams)
zz=d$t2[1:10]
zz
which(zz[1]==teams)
ltab
zz
teams
names
tf=factor(c(d$t1,d$t2))
tf
tfn=as.numeric(tf)
tfn
levels(tf)
X=matrix(as.numeric(tfn),ncol=2)
head(X)
names(d)
d$score
Y=matrix(as.numeric(unlist(strsplit(d$score, " - "))),ncol=2,byrow=T)
dim(X)
dim(Y)
ch.1=sampling(p.sc,list(x=X,y=Y,ng=180,nt=10),iter=10000)
ch.1
pars=extract(ch.1)

# play with data.table

install.packages("data.table")
library(data.table)
ltab
ltab.dt=data.table(ltab)
ltab.dt
tables()
setkey(ltab.dt,teams)
ltab.dt
ltab.dt["2181"]
teams

# try out merge (on data frames)

ids=1:4
names=c("alpha","beta","gamma","delta")
df=data.frame(id=ids,name=names)
df
id2=c(2,3,3,2,4,1)
x=c(10,10,12,11,12,6)
df2=data.frame(id=id2,x=x)
merge(df,df2) # that works
# try the by thing

df2=data.frame(idd=id2,x=x)
merge(df,df2) # no error but wrong

merge(df,df2,by.x="id",by.y="idd")

# ltab has the lookup of team names with database ids

ltab
n=length(levels(tf))
stan.lookup=data.frame(id=1:n,l=levels(tf))
stan.lookup

mm=merge(ltab,stan.lookup,by.x="teams",by.y="l")
mm
om=apply(pars$o,2,mean)
om
dm=apply(pars$d,2,mean)
dm
tot=om+dm
o=order(-tot)
means=data.frame(mm$names,om,dm,diff=om-dm)
means
means[o,]

###############################

# attempt at functions

# get database connection to soccer database

get.con=function()
{
  library(RSQLite)
  m=dbDriver("SQLite")
  dbConnect(m,"/home/ken/sports/scoresway/soccer.db")
}



# make compiled model object USE THIS

compiled.model=function()
{
    # home advantage added
  model='
data
{
  int<lower=1> ng;
  int<lower=1> nt;
  int x[ng,2];
  int y[ng,2];
}
  
  parameters
{
  real o[nt];
  real d[nt];
  real h;
}
  
  model
{
  int t1;
  int t2;
  real eta1;
  real eta2;
  real nu1;
  real nu2;
  o~normal(0,1);
  d~normal(0,1);
  h~normal(0,1);
  for (i in 1:ng)
{ 
  t1=x[i,1];
  t2=x[i,2];
  nu1=h+o[t1]-d[t2];
  nu2=o[t2]-d[t1];
  eta1=exp(nu1);
  eta2=exp(nu2);
  y[i,1]~poisson(eta1);
  y[i,2]~poisson(eta2);
}
}  
  '
  stan_model(model_code=model)
}

X
table(X)
length(table(X))

# sample from posterior

sample.model=function(p.sc,xylist,nit=10000)
{
  X=xylist$X
  Y=xylist$Y
  ng=nrow(X)
  nt=length(table(X))
  sampling(p.sc,list(x=X,y=Y,ng=ng,nt=nt),iter=nit)
}

# posterior mode estimates

optim.model=function(p.sc,xylist)
{
  X=xylist$X
  Y=xylist$Y
  ng=nrow(X)
  nt=max(X)
  optimizing(p.sc,list(x=X,y=Y,ng=ng,nt=nt),iter=10000)  
}

# function to run it all

library(rstan)
p.sc=compiled.model()
comp=21450

matching.leagues=function(con,st)
{
  query=paste0("select c.id, c.name, max(s.date) from comps c, scores s where c.name like \'",st,"%\' and s.comp=c.id group by c.id")
  rs=dbSendQuery(con,query)
  d=fetch(rs,n=-1)  
  dbClearResult(rs)
  o=order(d[,2],d[,3])
  d=d[o,]
  row.names(d)=1:(nrow(d))
  d
}

get.games=function(con,league.id,add_country=F)
{
  v=numeric(0)
  # think of league.id as a vector
  # make one query using where ... in
  # take vector of leagues, make comma-separated list with no quotes,
  # then glue commas on the ends
  comps=paste0("(",noquote(paste(league.id,collapse=",")),")")
  query=paste("select s.date, th.name, th.country, ta.name, ta.country, s.score from scores s, teams th, teams ta where s.comp in",comps,"and s.t1=th.id and s.t2=ta.id")
#  dbClearResult(rs)
  rs=dbSendQuery(con,query)
  d=fetch(rs,n=-1)
  dbClearResult(rs)
  if (add_country) {
    tnh=paste0(d[,2]," (",d[,3],")")
    tna=paste0(d[,4]," (",d[,5],")")
    d1=data.frame(date=d[,1],name1=tnh,name2=tna,score=d[,6])
    return(d1)
  }
  else
  {    
    tnh=d[,2]
    tna=d[,4]
    d1=data.frame(date=d[,1],name1=tnh,name2=tna,score=d[,6])
    return(d1)
  }
}
  

make.xy=function(d)
{
  ok=grep("-",d[,4])
  d=d[ok,]
  tf=factor(c(as.character(d[,2]),as.character(d[,3])))
  tfn=as.numeric(tf)
  X=matrix(as.numeric(tfn),ncol=2)
  Y=matrix(as.numeric(unlist(strsplit(as.character(d[,4]), " - "))),ncol=2,byrow=T)
  n=length(table(X))
  gp=table(tf)
  stan.lookup=data.frame(id=1:n,l=levels(tf))
  list(X=X,Y=Y,z=stan.lookup,gp=gp)
}

# revised run

run=function(con,comp)
{
  d=get.games(con,comp)
  xy.list=make.xy(d)
  model.2=optim.model(p.sc,xy.list)
  ll=length(model.2$par)
  llp=(ll-1)/2
  om=model.2$par[1:llp]
  dm=model.2$par[(llp+1):(2*llp)]
  hm=model.2$par[ll]
  tm=om+dm
  o=order(-tm)
  res=data.frame(xy.list$gp,round(om,2),round(dm,2),round(tm,2),round(om-dm,2))
  colnames(res)=c("team","games","attack","defence","total","diff")
  res=res[o,]
  row.names(res)=1:(nrow(res))
  list(res,round(hm,2))
}

ml=matching.leagues(con,"Germany -")
ml
rows=c(1:4,16:23,35:60,91:156,233:251) # Germany
rows=c(63:78,122:127) # Romania
comps=ml[rows,1]
comps
run(con,comps)
d=get.games(con,comps)
d
table(c(d[,2],d[,3]))
dim(d)
xy.list=make.xy(d)
xy.list
names(xy.list)
dim(xy.list$X)
dim(xy.list$Y)
dim(xy.list$z)
dim(xy.list$gp)
head(xy.list$gp)
con=get.con()
library(rstan)
tf
ml=matching.leagues(con,"Austria -")
ml
rows=c(4,5,23,24)
comps=ml[rows,1]
comps
run(con,comps)
ml=matching.leagues(con,"Switzerland -")
ml
rows=c(52:53,146:147)
comps=ml[rows,1]
comps
run(con,comps)


run2=function(con,comp,nit=10000)
{
  d=get.games(con,comp)
  xy.list=make.xy(d)
  model.2=sample.model(p.sc,xy.list,nit)
  pars=extract(model.2)
  pred.list(pars,xy.list)
}

run3=function(con,comp)
  {
      d=get.games(con,comp)
      xy.list=make.xy(d)
      model.3=optim.model(p.sc,xy.list)
      model.3
  }

do.predict=function(pars,i,j)
{
  l1=exp(pars$h+pars$o[,i]-pars$d[,j])
  l2=exp(pars$o[,j]-pars$d[,i])
  nr=nrow(pars$o)
  r1=rpois(nr,l1)
  r2=rpois(nr,l2)
  tb=table(paste(r1,r2,sep="-"))
  o=order(-tb)
  d=r1-r2
  dt=table(cut(d,c(-100,-0.5,0.5,100)))
  dt=dt/sum(dt)
  names(dt)=c("L","D","W")
#  list(head(tb[o]/nr,n=12),dt[3:1])
  list(dist=tb[o]/nr,prob=dt[3:1])
}

select.pred.old=function(l)
    {
        print(l$xy$z)
        tp=scan(n=2)
        do.predict(l$pars,tp[1],tp[2])
    }

do.pd=function(pars,i,j,ha)
{
    diff1=pars$o[,i]-pars$d[,j]
    diff2=pars$o[,j]-pars$d[,i]
    if (ha) {
        nu1=diff1+pars$h
        nu2=diff2
    }
    else {
        nu1=diff1+pars$h/2
        nu2=diff2+pars$h/2
    }
    l1=exp(nu1)
    l2=exp(nu2)
#    print(c(mean(l1),mean(l2)))
    nr=nrow(pars$o)
    r1=rpois(nr,l1)
    r2=rpois(nr,l2)
    v=cbind(r1,r2)
    tab=table(v[,1],v[,2])
    w=as.data.frame(tab)
    w[,3]=w[,3]/nr
    w
}


pd=function(l,ha=T)
    {
        tp=scan(n=2)
        do.pd(l$pars,tp[1],tp[2],ha)
    }

res=function(v)
    {
        if (v[1]>v[2]) return(2)
        if (v[1]==v[2]) return(1)
        if (v[1]<v[2]) return(0)
    }

pt.ptf=function(x,y,r=numeric(0))
    {
      # x is vector of predicted scores, y of exact (or vv)
      # goal difference can go here
        pres=res(x) %in% r
        if (x[1]==y[1] && x[2]==y[2]) return(3+2*pres)
        if (res(x)==res(y)) return(1+2*pres)
        0
    }

pt.spl=function(x,y)
    {
        s=0
        if (x[1]==y[1]) s=s+2
        if (x[2]==y[2]) s=s+2
        if (res(x)==res(y)) s=s+1
        s
    }

pt.cp=function(x,y)
    {
        x=as.numeric(x)
        y=as.numeric(y)
        max1=20
        max2=8
        min1=0
        off1=abs(x[1]-y[1])
        off2=abs(x[2]-y[2])
        offd=abs((x[1]-x[2])-(y[1]-y[2]))
        if (res(x)==res(y))
            {
                pt=max1-off1-off2-offd
                pt=max(pt,max2)
            }
        else
            {
                pt=max2-off1-off2
                pt=max(pt,min1)
            }
        pt
    }


ept=function(score,pd,pt.fn,...)
    {
        # expected points for score in predictive distribution given by pd
        w=apply(pd[,1:2],1,pt.fn,score,...)
        sum(w*pd[,3])
    }

best10=function(pp,ept,pt.fn,...)
    {
        epp=apply(pp[,1:2],1,ept,pp,pt.fn,...)
        o=order(-epp)
        y=cbind(pp[o,],epp[o])
        names(y)=c("s1","s2","prob","expected")
        y[1:10,]
    }

anf=function(f) {
  as.numeric(levels(f)[f])
}

best1=function(pp,ept,pt.fn,...)
{
  epp=apply(pp[,1:2],1,ept,pp,pt.fn,...)
  o=order(-epp)
  y=cbind(pp[o,],epp[o])
  v=c(anf(y[1,1]),anf(y[1,2]))
  paste(v,collapse="-")
}

bestall=function(pp,ept,pt.fn) {
  v=list(4,2:1,2,1,0,1:0) # has to be a list
  x=numeric(6)
  for (i in 1:6) {
    x[i]=best1(pp,ept,pt.fn,v[[i]])
  }
  names(x)=v
  x
}

best.neutral=function(pp,ept,pt.fn) {
  v=list(4,2:1,2,1,0,1:0) # has to be a list
  w=list(4,1:0,0,1,2,2:1) # opposite for other way around
  I=nrow(pp)
  J=length(v)
  pq=cbind(pp[,2],pp[,1],pp[,3])
  x=matrix(NA,I,J)
  for (i in 1:I) {
    s1=pp[i,1]
    s2=pp[i,2]
    for (j in 1:J) {
      expected=ept(c(s1,s2),pp,pt.fn,v[[j]])
      x[i,j]=expected
    }
    # rearrange pp with teams opposite way around
    for (j in 1:J) {
      expected=ept(c(s2,s1),pq,pt.fn,w[[j]])
      x[i,j]=(x[i,j]+expected)/2
    }
  }
  rownames(x)=paste(pp[,1],pp[,2],sep="-")
  colnames(x)=v
  # find row max for each column
  row.max=numeric(J)
  for (j in 1:J) {
    row.max[j]=rownames(x)[which.max(x[,j])]
  }
  names(row.max)=v
  row.max
}


dp.df=function(x)
    {
        p=x
        nm=names(x)
        ss=strsplit(nm,"-")
        m=matrix(as.integer(unlist(ss)),ncol=2,byrow=T)
        m
        cbind(m,p)
    }

res=function(x,y)
    {
        ifelse(x>y,"W",ifelse(x==y,"D","L"))
    }

exp.score=function(s,dist)
    {
        v=(dist[,1]==s[1] & dist[,2]==s[2])
        p.exact=sum(v*dist[,3])
        res.in=res(s[1],s[2])
        res.mat=res(dist[,1],dist[,2])
        w=(res.in==res.mat)
        p.result=sum(w*dist[,3])
        3*p.exact+p.result
    }

select.pred=function(l)
    {
        while (1)
            {
                tp=scan(n=2)
                if (length(tp)<2) break
                opps=l$xy$z$l[tp]
                dpr=do.predict(l$pars,tp[1],tp[2])
                dd=dpr$dist
                dp=dpr$prob
                df=dp.df(dd)
                vv=apply(df[,1:2],1,exp.score,df)
                o=order(-vv)
                print(list(opps,vv[o],dp))
            }
    }




do.predict.list=function(pars,i,j)
{
  l1=exp(pars$h+pars$o[,i]-pars$d[,j])
  l2=exp(pars$o[,j]-pars$d[,i])
  nr=nrow(pars$o)
  r1=rpois(nr,l1)
  r2=rpois(nr,l2)
  tb=table(paste(r1,r2,sep="-"))
}

pred.all=function(pars,xy.list)
{
  nm=names(xy.list$gp)
  n=length(nm)
  ss=matrix("",n,n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      v=do.predict(pars,i,j)
      nv=names(v)
      ss[i,j]=nv[1]
    }
  }
  a4=abbreviate(nm,4)
  rownames(ss)=a4
  colnames(ss)=a4
  ss
}

pred.list=function(pars,xy.list)
{
  nm=names(xy.list$gp)
  n=length(nm)
  h=character(0)
  a=character(0)
  sc=character(0)
  pr=numeric(0)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      h=c(h,nm[i])
      a=c(a,nm[j])
      v=do.predict.list(pars,i,j)
      w=sort(-v)
      sc=rbind(sc,names(w)[1:3])
      pr=rbind(pr,-w[1:3])
    }
  }
  data.frame(home=h,away=a,scores=sc,probs=pr)
}

pp=pred.all(pars,xy.list)
pp
str(xy.list)
names(xy.list$gp)

# predict leagues

# this bit first

library(rstan)
con=get.con()

ml=matching.leagues(con,"Switzerland -")
ml
rows=c(146:147)
comps=ml[rows,1]
comps
run2(con,comps)
res=.Last.value
res$pp
d
xy.list=make.xy(d)
str(xy.list)
pp=.Last.value
pp[1:20,]

ml=matching.leagues(con,"Greece -")
ml
rows=c(60:62,94,97)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp$pp

ml=matching.leagues(con,"England -")
ml
rows=c(516:517,18:20,21:23)
comps=ml[rows,1]
comps
pp=run2(con,comps)
dim(pp$pp)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
noquote(pp$pp[,1:18])
noquote(pp$pp[,19:36])


ml=matching.leagues(con,"Scotland -")
ml
rows=c(25,66:68,98:99)
comps=ml[rows,1]
comps
pp=run2(con,comps)
noquote(pp$pp)

ml=matching.leagues(con,"Wales -")
ml
rows=c(44:45)
comps=ml[rows,1]
comps
pp=run2(con,comps)
noquote(pp$pp)

library(rstan)
con=get.con()

ml=matching.leagues(con,"Switzerland -")
ml
rows=c(158:159,62,63)
comps=ml[rows,1]
comps
pp=run2(con,comps)
dim(pp$pp)
noquote(pp$pp[,16:21])


ml=matching.leagues(con,"Germany -")
ml
rows=c(4:5,23:24)
comps=ml[rows,1]
comps
pp=run2(con,comps)
noquote(pp$pp)

ml=matching.leagues(con, "Spain -")
ml
rows=c(185:187,184,112:113)
comps=ml[rows,1]
comps
pp=run2(con,comps)
names(pp)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
noquote(pp$pp[,1:20])
noquote(pp$pp[,21:40])
noquote(pp$pp[,])

ml=matching.leagues(con, "France -")
ml
rows=c(190:191,195:196)
comps=ml[rows,1]
comps
pp=run2(con,comps)
names(pp)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
noquote(pp$pp[,1:20])
noquote(pp$pp[,21:40])
noquote(pp$pp[,])

ml=matching.leagues(con, "Italy -")
ml
rows=c(243,241,282:284,272,274:275)
comps=ml[rows,1]
comps
pp=run2(con,comps)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
noquote(pp$pp[,1:20])
noquote(pp$pp[,21:40])
noquote(pp$pp[,])

library(rstan)
con=get.con()

ml=matching.leagues(con, "Switzerland -")
ml
rows=c(158,159,62,63)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp

ml=matching.leagues(con,"England -")
ml
rows=c(21:24,399:402,537,538)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp


ml=matching.leagues(con, "Scotland -")
ml
rows=c(102:104,25,26)
comps=ml[rows,1]
comps
pp=run2(con,comps)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
noquote(pp$pp[1:15,1:10])
noquote(pp$pp[,17:21])

ml=matching.leagues(con, "England -")
ml
rows=c(21:24,398:401,532)
comps=ml[rows,1]
comps
pp=run2(con,comps)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
length(nn)
noquote(pp$pp[1:15,1:15])
noquote(pp$pp[,17:21])
pp$pp

ml=matching.leagues(con, "France -")
ml
rows=c(192,193,197,198)
comps=ml[rows,1]
comps
pp=run2(con,comps)
nn=names(pp$teams)
cbind(nn,abbreviate(nn,4))
length(nn)
noquote(pp$pp[,21:40])
noquote(pp$pp[,17:21])
pp$pp

##############################################

only need this now

##############################################

library(rstan)
con=get.con()

options(width=120)



ml=matching.leagues(con,"England -")
ml
rows=c(24:27,405:408,543,544)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp

ml=matching.leagues(con,"France -")
ml
rows=c(210,211,215,216)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp


ml=matching.leagues(con,"Germany -")
ml
rows=c(4,5,27,28)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp

ml=matching.leagues(con,"Greece -")
ml
rows=c(94,95,98,60:62)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp

ml=matching.leagues(con,"Italy -")
ml
rows=c(250:251,290:293)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp


ml=matching.leagues(con,"Spain -")
ml
rows=c(115,116,192:195)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp

ml=matching.leagues(con,"Scotland -")
ml
rows=c(106:108,26,27,74:76)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp


ml=matching.leagues(con,"Switzerland -")
ml
rows=c(160:161,62,63)
comps=ml[rows,1]
comps
pp=run2(con,comps)
pp
