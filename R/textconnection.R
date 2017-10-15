con=textConnection("
a b c
1 3 a
1 4 v
2 1 v
2 5 a
                   ")

con
mydata=read.table(con,header=T)
mydata

con=textConnection("
688 687 #0 Shakhtar Donetsk v Sevilla                     0     eue      18         2-1:1-1:1-2:2-0:2-2
860 441 # Villarreal v Liverpool                         0     eue      18         1-1:2-1:0-0:1-2:1-3
441 860 #10 Liverpool v Villarreal                         10    eue      14         2-0:2-1:1-0:1-1:3-0
687 688 #10 Sevilla v Shakhtar Donetsk                     0     eue      14     1-1:2-1:1-0:2-0:3-0:3-1
")
eu=read.table(con,header=F)
eu
