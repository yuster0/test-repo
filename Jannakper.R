library(data.table)
flights <- fread("~/Dropbox/flights14.csv")
flights
dim(flights)
ans <- flights[origin=="JFK" & month== 6L]
head(ans)
DT= data.table(ID=c("b","b","b","a","a","c"), a=1:6, b=7:12, c=13:18)
DT


class(DT$ID)
DT[ i,j, by]
rm(DT)
library(reshape2)
library(tidyr)


mytable

library(dplyr)
perdatn[,"year"]
dim(perdatn)
head(perdatn)
perdatn[, sum("c_baby_birth" == "Macerated stillbirth")]
perdatn[perdatn$c_baby_birth=="Macerated stillbirth",]
glimpse(perdatn)
use(perdatn)
load(perdatn)

install.packages("maptools")
twoby2(perdatn$part_use, perdatn$c_baby_birth)
twoby2(perdatn$foetus_pres, perdatn$c_baby_birth)
twoby2(perstat$anc_visits,perstat$c_baby_birth) 
twoby2(perstat$pre_rupmemb,perstat$c_baby_birth)
twoby2(perstat$mec_liq, perstat$c_baby_birth) 
twoby2(perstat$ab_fhs, perstat$c_baby_birth)
twoby2(perstat$cord_prol, perstat$c_baby_birth)
twoby2(perstat$off_liq, perstat$c_baby_birth)
twoby2(perstat$pre_eclampsia,perstat$c_baby_birth)
twoby2(perstat$locstat,perstat$c_baby_birth)
twoby2(perstat$loc_del,perstat$c_baby_birth)
twoby2(perstat$rup_uterus,perstat$c_baby_birth)
twoby2(perstat$obs_lab,perstat$c_baby_birth)
twoby2(perstat$Mgravrec,perstat$c_baby_birth)
twoby2(perstat$rmgest,perstat$c_baby_birth)
## Macerated stillbirth
twoby2(permsb$rmgest,permsb$c_baby_birth) ## etreme term vs te outcome alive
twoby2(permsb$iden_dang_sign,permsb$c_baby_birth)
twoby2(permsb$pre_eclampsia,permsb$c_baby_birth)
twoby2(permsb$Mgravrec,permsb$c_baby_birth)
twoby2(permsb$off_liq,permsb$c_baby_birth)
twoby2(permsb$iden_dang_sign,permsb$c_baby_birth)
twoby2(permsb$anc_visits,permsb$c_baby_birth)
twoby2(permsb$r_fesup,permsb$c_baby_birth)
twoby2(permsb$obs_lab,permsb$c_baby_birth)
library(lubridate)
perdatn %>% ## with corrected date
  mutate_at(vars(event_date,del_date,c_deldate,doa,dod,c_dodt),dmy hms)) %>% 
  mutate(date = coalesce(event_date,del_date,c_deldate,doa,dod,c_dodt))

perdatn$c_dodt <- as.Date(perdatn$c_dodt)
perdatn$lifeperiod <- perdatn$c_dodt - perdatn$c_deldate
glimpse(perdatn)
use(perstat)
cs(c_baby_birth,locstat)
cs(c_baby_birth,anc_visits)
cs(c_baby_birth,part_use)
cs(c_baby_birth,pre_rupmemb)
cs(c_baby_birth,off_liq)
cs(c_baby_birth,loc_del)
cs(c_baby_birth,pre_eclampsia)
cs(c_baby_birth,rup_uterus)
cs(c_baby_birth,obs_lab)
cs(c_baby_birth, Mgravrec)
cs(c_baby_birth, rmgest)
cs(c_baby_birth, iden_dang_sign)r_fesup
cs(c_baby_birth, r_fesup)
cs(c_baby_birth, obs_lab)

## Macerated stillbirth
use(permsb)

cs(c_baby_birth, rmgest)
cs(c_baby_birth, Mgravrec)
cs(c_baby_birth,off_liq)

permsb <- perdatn %>%
  filter (!(c_baby_birth=="Fresh still birth")) %>% 
  filter(c_baby_birth =="Alive" > 1) %>%


## recoding and releveling
permsb$rmgest <- as.factor(permsb$rmgest)
permsb$rmgest <- relevel( permsb$rmgest, ref="Ext preterm")
permsb$Mgravrec <- as.factor(permsb$Mgravrec)
permsb$rmgest <- relevel( permsb$Mgravrec, ref="Multiparous")
permsb$rmgest <- relevel( permsb$Mgravrec, ref="Nulliparous")
permsb$anc_visits <- as.factor(permsb$anc_visits)
permsb$anc_visits <- relevel(permsb$anc_visits, ref = "Twice")
permsb$anc_visits <- relevel(permsb$anc_visits, ref = "None")

###


  
perstat$locstat <- relevel(perstat$lab_loc_st, ref="Tier3/4")
perstat$locstat <- relevel(perstat$lab_loc_st, ref="Home")

perstat$c_baby_birth <- relevel( perstat$c_baby_birth, ref="Alive")
perstat$foetus_pres <- relevel(perstat$foetus_pres,ref="Transverse")
perstat$foetus_pres <- relevel(perstat$foetus_pres,ref="Breech")
perstat$anc_visits <- relevel(perstat$anc_visits, ref = "Twice")
perstat$anc_visits <- relevel(perstat$anc_visits, ref = "None")
perstat$loc_del <- relevel(perstat$loc_del, ref = "Provincial Hospital")
perstat$loc_del <- relevel(perstat$loc_del, ref = "Home")
perstat$anc_visits <- as.factor(perstat$anc_visits)
perstat$lab_loc_st<- as.factor(perstat$lab_loc_st)
perstat$c_baby_birth <- as.factor( perstat$c_baby_birth)
perstat$foetus_pres <- as.factor(perstat$foetus_pres)
perstat$loc_del <- as.factor(perstat$loc_del)
perstat
glimpse(perstat)  


  
