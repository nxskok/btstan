
                                        # stuff from btstan.R


compile.model=function()
{
  model='
data
{
  int<lower=2> N; // number of games
int<lower=1> D; // number of teams
int<lower=1,upper=3> y[N]; // results
int x[N,2]; // indices of teams playing
}

parameters
{
vector[D] beta;
real<lower=0> cc;
}

model
{
real nu;
int y1;
int y2;
vector[2] d;
beta~normal(0,2);
cc~normal(0,10);
for (i in 1:N)
{
y1 <- x[i,1];
y2 <- x[i,2];
nu <- beta[y1]-beta[y2];
d[1] <- -cc;
d[2] <- cc;
y[i] ~ ordered_logistic(nu,d);
}
}'
  stan_model(model_code=model)
}


get.res=function(fname)
{
  res=read.table(fname,sep=":",quote="\"",stringsAsFactors=F)
  res
}

makexy=function(res)
{
  f=factor(c(res[,2],res[,3]))
  ll=split.half(f)
  x=cbind(as.numeric(ll[[1]]),as.numeric(ll[[2]]))
  y=res[,4]+1
  list(x=x,y=y,names=levels(f))
}

estimate=function(xylist,bto.sc)
{
  stopifnot(is.list(xylist))
  xx=xylist[[1]]
  yy=xylist[[2]]
  N=length(yy)
  K=3
  D=max(xx)
  v=optimizing(bto.sc,list(N=length(yy),K=3,D=max(xx),y=yy,x=xx))
  #        v=optimizing(bto.sc,c("N","K","D","y","x"))
  theta=v$par
  n=length(theta)
  beta=theta[1:(n-1)]
  names(beta)=xylist[[3]]
  cc=theta[n]
  ll=list(beta=beta,cc=cc)
  ll
}

game.count=function(x,tf=rep(T,nrow(x)))
{
  gc=table(c(x[tf,1],x[tf,2]))
  gc
  gg=numeric(max(x))
  gg[as.numeric(names(gc))]=gc
  gg
}

keep1=function(x,ng,tf=rep(T,nrow(x)))
{
  # indicate which rows of x have both teams playing
  # at least ng games for which tf is T (not prev eliminated)
  gp=game.count(x,tf)
  c1=gp[x[,1]]
  c2=gp[x[,2]]
  pp=(pmin(c1,c2)>=ng)
  pp=ifelse(is.na(pp),F,pp)
  pp
}

keep.n=function(x,ng)
{
  tf=rep(T,nrow(x))
  for (i in 1:3)
  {
    tf=keep1(x,ng,tf)
    print(table(tf))
  }
  tf
}

make.clean=function(fname,ng)
{
  res=get.res(fname)
  xy=makexy(res)
  tf=keep.n(xy$x,ng)
  res=res[tf,]
  xy=makexy(res)
  xy
}

split.half=function(x)
{
  sf=ceiling(2*seq_along(x)/length(x))
  split(x,sf)
}



elo.scale=function(rat)
{
  round(1500+173.7178*rat)
}


run.rating=function(fname,model,ng=0,sampling=F)
{
  # eliminate teams with fewer than ng games
  xy=make.clean(fname,ng)
  gc=game.count(xy$x)
  res=estimate(xy,model)
  df=data.frame(team=names(res$beta),gc,elo.scale(res$beta))
  o=order(-res$beta)
  dg=df[o,]
  row.names(dg)=1:length(gc)
  names(dg)=c("Team","Games","Rating")
  list(tab=dg,cc=res$cc)
}


pred=function(i,j,ll)
{
  eta=ll$beta[i]-ll$beta[j];
  cc=c(-ll$cc,ll$cc)
  xx=outer(eta,cc,"-")
  p=plogis(xx)
  p=cbind(1,p,0)
  t(-apply(p,1,diff))
}


pred.all=function(ll)
{
  n=length(ll$beta)
  ans=array(0,c(n,n,3))
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      ans[i,j,]=pred(i,j,ll)
    }
  }
  ans
}

estimate_bayes=function(xylist,bto.sc)
{
  stopifnot(is.list(xylist))
  xx=xylist[[1]]
  yy=xylist[[2]]
  N=length(yy)
  K=3
  D=max(xx)
  v=sampling(bto.sc,list(N=length(yy),K=3,D=max(xx),y=yy,x=xx))
  #        v=optimizing(bto.sc,c("N","K","D","y","x"))
  ss=extract(v)
  ss$cc
  theta=v$par
  n=length(theta)
  beta=theta[1:(n-1)]
  names(beta)=xylist[[3]]
  cc=theta[n]
  ll=list(beta=beta,cc=cc)
  ll
}





                                        # college hockey


get.results=function(url)
    {
        library(XML)
        tbls=readHTMLTable(url,header=F)
        # want v's 5 8 6 9 and 12
        attach(tbls[[1]])
        d=tbls[[1]][V6!="" & V12!="EX",c(5,8,6,9)]
        detach(tbls[[1]])
        d
    }

rr=function(x,y)
    {
        x=as.integer(x)
        y=as.integer(y)
        ifelse(x>y,2,ifelse(x==y,1,0))
    }

make.input.df=function(d)
    {
        t1=as.character(d[,1])
        t2=as.character(d[,2])
        r=rr(d[,3],d[,4])
        data.frame(1,t1,t2,r,stringsAsFactors=F)
    }

rate=function(dd)
    {
        ll=makexy(dd)
        est=estimate(ll,bt.sc)
        v=elo.scale(est$beta)
        o=order(-v)
        df=data.frame(team=names(v)[o],rating=v[o])
        row.names(df)=NULL
        df

    }

make.ratings=function(url)
    {
        dx=get.results(url)
        d=make.input.df(dx)
        rate(d)
    }

get.cfl.ratings=function(url)
    {
        library(XML)
        tbls=readHTMLTable(url,header=T)
        d=tbls[[2]]
        cc=complete.cases(d) & d[,7]=="Highlights"
        dd=d[cc,c(2,4,3,5)]
        dd=dd[-(1:9),] # get rid of pre season games
        dd
        dd[,3]=as.character(dd[,3])
        dd[,4]=as.character(dd[,4])
        d3=make.input.df(dd)
        rate(d3)
    }

# usa today functions


extract.score=function(x)
    {
        t1=x$V2[1]
        t1=gsub('[\t\r\n_]',"",t1)
        t1=gsub('\\([0-9]+\\).',"",t1)
        t2=x$V2[2]
        t2=gsub('[\t\r\n_]',"",t2)
        t2=gsub('\\([0-9]+\\).',"",t2)
        s1=x$V3[1]
        s2=x$V3[2]
        data.frame(t1,t2,s1,s2)
    }

ymd=function(d)
    {
        # d is a POSIXlt object
#        sprintf("%04d%02d%02d",d$year+1900,d$mon+1,d$mday)
        c(d$year+1900,d$mon+1,d$mday)
    }

ymd2=function(d)
    {
        # d is a POSIXlt object
        sprintf("%04d%02d%02d",d$year+1900,d$mon+1,d$mday)
#        c(d$year+1900,d$mon+1,d$mday)
    }



results=function(league,date)
    {
        usa.base="http://content.usatoday.com/sportsdata/scores"
        yyy=ymd(date)
        url=paste(usa.base,league,yyy[1],yyy[2],yyy[3],sep="/")
        tbls=readHTMLTable(url,header=F)
        d=numeric(0)
        for (i in 1:length(tbls))
            {
                dl=extract.score(tbls[[i]])
                d=rbind(d,dl)
            }
        sl=grepl('[0-9]',d$s1)
        d[sl,]
    }

results2=function(date,league)
    {
        results(league,as.POSIXlt(date))
    }

make.date.list=function(start.date,end.date)
    {
        d1=as.POSIXlt(start.date)
        d2=as.POSIXlt(end.date)
        seq(d1,d2,by=1*24*60*60)
    }

get.all=function(date.list,lg)
    {
        result.list=lapply(date.list,results2,lg)
        dates.numeric=ymd2(as.POSIXlt(date.list))
        names(result.list)=dates.numeric
        result.list
    }



############################################################
# stuff to run here
# put functions above
############################################################

# run this first

library(rstan)
# bt.sc=compile.model()
library(XML)
library(RCurl)

# college hockey

url.m1="http://www.uscho.com/scoreboard/division-i-men/20142015/composite-schedule/"
url.w1="http://www.uscho.com/scoreboard/division-i-women/2014-2015/composite-schedule/"
url.m3="http://www.uscho.com/scoreboard/division-iii-men/20142015/composite-schedule/"
url.w3="http://www.uscho.com/scoreboard/division-iii-women/20142015/composite-schedule/"

make.ratings(url.m1)
make.ratings(url.w1)

# cfl

cfl.url="http://www.cfl.ca/schedule/year/2014/time_zone/0"
get.cfl.ratings(cfl.url)

# usatoday


dates=make.date.list("2013-11-27 12:00","2013-12-01 12:00")
dates
nhl=get.all(dates,"nhl")
nhl
dates=make.date.list("2013-11-07 12:00","2013-11-09 12:00")
dates
nhl2=get.all(dates,"nhl")
nhl2
nhl=c(nhl,nhl2)
# next: check which ones we already have, and grab only the new ones, combining into one list

names(nhl)
is.null(nhl$`20131120`)

new.dates %in% names(nhl)
