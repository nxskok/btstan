library(tidyverse)
library(rvest)
# get euto soccer stuff from scoresway

url="http://www.scoresway.com/?sport=soccer&page=round&id=42380"
html=read_html(url)
html %>% html_table() %>% .[[1]] -> tab
names(tab)=c("Day","Date","Home","Score","Away","i1","i2")
tab %>% select(2:5)

%>% filter(Day!="Aggr.") %>% select(1:5) %>% 
