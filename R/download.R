dl=function(league) {
  base="predictthefootball.com/?lang=en_us"
  file=paste0("http://",league,".",base)
  download.file(file,paste(league,".txt",sep="."))
}

dl("facup")

######################################################
# jottings
########################################################

prefixes=c("facup")
files
download.file

download.file("http://swiss-soccer.net/spl/spl.txt","spppl.txt")
