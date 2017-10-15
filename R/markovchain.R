library(markovchain)
states=LETTERS[1:3]
states
A=matrix(c(0.5,0.4,0.1,0,0.3,0.7,0,0.6,0.4),byrow=T,ncol=3,dimnames=list(states,states))
A
mc=new("markovchain",states=states,transitionMatrix=A,name="mc")
plot(mc)
transientStates(mc)
steadyStates(mc)

initialState=c(40,40,40)
show(mc)

timesteps <- 100
sir.df <- data.frame( "timestep" = numeric(),
                      "A" = numeric(), "B" = numeric(),
                      "C" = numeric(), stringsAsFactors=FALSE)
for (i in 0:timesteps) {
  newrow <- as.list(c(i,round(as.numeric(initialState * mc ^ i),0)))
  sir.df[nrow(sir.df) + 1, ] <- newrow
}
sir.df

summary(mc)

s1=rmarkovchain(100,mc,t0="A")
s2=rmarkovchain(100,mc,t0="B")
s3=rmarkovchain(100,mc,t0="C")
s1
s2
s3
S