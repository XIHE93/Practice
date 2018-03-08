pra
================

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

\`\`\`{r 1} library(choroplethr) library(choroplethrMaps) library(data.table) library(tidyverse) setwd("C:/Users/xi97x/Desktop/479") dat=read\_csv("2017.txt") dat%&gt;%as.tbl

``{r 2} keep = c("STATE\_CODE\_001", "STRUCTURE\_NUMBER\_008" , "COUNTY\_CODE\_003", "LAT\_016", "LONG\_017", "TOLL\_020" , "ADT\_029" , "YEAR\_ADT\_030" , "YEAR\_BUILT\_027" , "DECK\_COND\_058" , "SUPERSTRUCTURE\_COND\_059", "SUBSTRUCTURE\_COND\_060" , "CHANNEL\_COND\_061","CULVERT\_COND\_062", "DATE\_OF\_INSPECT\_090" , "FRACTURE\_092A" , "UNDWATER\_LOOK\_SEE\_092B" , "SPEC\_INSPECT\_092C" )

x = select(dat, one\_of(keep)) x*S**T**A**T**E*<sub>*C*</sub>*O**D**E*<sub>001</sub> = *a**s*.*n**u**m**e**r**i**c*(*x*STATE\_CODE\_001) x*C**O**U**N**T**Y*<sub>*C*</sub>*O**D**E*<sub>003</sub> = *a**s*.*n**u**m**e**r**i**c*(*x*COUNTY\_CODE\_003) Bridge = mutate (x,fips=STATE\_CODE\_001\*1000+COUNTY\_CODE\_003)

wi = mutate(Bridge, cond = pmin(SUPERSTRUCTURE\_COND\_059, SUBSTRUCTURE\_COND\_060, CHANNEL\_COND\_061,CULVERT\_COND\_062, na.rm = T))

good = 5:9
==========

bad = 2:4
=========

fail = 0:1
==========

cond "condition" is the minimum of the given ratings.
=====================================================

rateIt = function(cond){ \# gives a good to fail rating for cond. rate = rep("good", length(cond)) rate\[cond&lt;5\] = "bad" rate\[cond &lt;2\]= "fail" return(rate) } wi*r**a**t**e* = *r**a**t**e**I**t*(*w**i*cond)

load unempleyment data
======================

library(blscrapeR) library(dplyr) library(choroplethr)

unemployment = get\_bls\_county() %&gt;% as.tbl save(unemployment, file = "unemployment.RData") load("unemployment.RData") unemployment*f**i**p**s* = *a**s*.*n**u**m**e**r**i**c*(*u**n**e**m**p**l**o**y**m**e**n**t*fips)

data=wi %&gt;% inner\_join(unemployment, by = "fips")

data*u**n**e**m**p**l**o**y**e**d*<sub>*r*</sub>*a**t**e* = *a**s*.*n**u**m**e**r**i**c*(*d**a**t**a*unemployed\_rate) data = data%&gt;% mutate(good = (rate == "good")) ``{r 3} new=data %&gt;% group\_by(fips)%&gt;% summarize(unemployedrate=mean(unemployed\_rate),con=mean(good))

lm=lm(unemployedrate~con,new) plot(y=new*u**n**e**m**p**l**o**y**e**d**r**a**t**e*, *x* = *n**e**w*con) abline(lm) \`\`\`
