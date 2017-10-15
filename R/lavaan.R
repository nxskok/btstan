library(lavaan)
data(PoliticalDemocracy)
pd.1=princomp(PoliticalDemocracy,cor=T)
pd.1
plot(pd.1,type="l")
pd.2=factanal(PoliticalDemocracy,2,scores="r")
pd.2
biplot(pd.2$scores,pd.2$loadings)
model= 'politics =~ y1+y2+y3+y4+y5+y6+y7+y8
        industry=~x1+x2+x3'
pd.3=cfa(model,data=PoliticalDemocracy)
summary(pd.3,fit.measures=T)
show(pd.3)
# doesn't fit
modelt= 'politics60 =~ y1+y2+y3+y4
        politics65=~ y5+y6+y7+y8
        industry=~x1+x2+x3'
pd.4=cfa(modelt,data=PoliticalDemocracy)
show(pd.4)
anova(pd.3,pd.4)

###### the kids

kids=read.table("/home/ken/teaching/d29/notes/rex2.txt",header=T)
kids
km=as.matrix(kids)
km2=list(cov=km,n.obs=145)
km2
kids.f2=factanal(factors=2,covmat=km2)
kids.f2

## cfa

model=" verbal =~ para+sent+word
        math =~ add+dots"
kids.2=cfa(model,sample.cov=km,sample.nobs =145)
kids.2

model2=" ability =~ para+sent+word+add+dots"
kids.3=cfa(model2,sample.cov=km,sample.nobs =145)
kids.3

anova(kids.3,kids.2)
