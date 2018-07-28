library(dplyr)
library(tidyr)
library(tidyverse)
library(tidyselect)
library(sf)

perdatn <- readr::read_csv("~/Dropbox/Peri2018.csv")
perdatn %>% tbl_df() ## change the data to dataframe
glimpse(perdatn) ## use glimpse to view the data
## change dates from character to date
perdatn <- perdatn %>% ## with corrected date
  mutate_at(vars(date_event,Doa_death,doa_del,dod), dmy) %>% 
  mutate(date = coalesce(date_event,Doa_death,doa_del,dod)) ## conbine to one date 
## Use glimpse to view changed data 
glimpse(per)
## See the distribution of perinatal death
perdatn %>% ## reported no of perinatal deaths per facility PGH 869/954 cases (91%)
  group_by(Org_unit) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## reported no of perinatal deaths by year in 2015 we had 431  deaths
  group_by(Year) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ## reported no of perinatal deaths by month in november we  highest deaths at 75 (12%), february lowest
  group_by(month) %>% 
  select(year = 2014) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per <- per %>% ## with corrected date
  mutate_at(vars(date_event,Doa_death,doa_del,dod), dmy) %>% 
  mutate(date = coalesce(date_event,Doa_death,doa_del,dod))   
glimpse(per)
describe(per_cn)

## run frequency tables
per_cn %>% ## run frequency table(1)
  group_by(year)%>% ## in 2014 we had 274(45%))
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(ii) delivery by location
  group_by(Loc_del)%>% ## total of 752/954(78%)) delivered in Nakuru PGH
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(iii) localtion where labour began
  group_by(loc_labr)%>% ## 350/606 experience labour at home  534/ 958 (56%)) and 418/958 (44% )began at Health-centre or sub-county hospital
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(iii) delivery by localtion
  group_by(c_baby_birth)%>% ## 597/958 (62%) were born alive, 361/958 (38%) born stillbirth
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq)) 

per %>% ##(iv) number of pre-natal visits
  group_by(anc_visits)%>% ## 280/958 (24% ) and  a third of the cases at least aattended anc atleast 3 or more times, 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per%>% ##(v) foetus presentation
  group_by(foetus_pres)%>% ## 725/958 (76% ) had cephalic presentation
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(vi) pregnancy type 
  group_by(preg_type)%>% ## 783 /958 (82% ) were singleton and 17(2%) were multiple pregnancies
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

## recode gestation weeks 
per$gest2 <- case_when(per$gest <= 1 ~ 'Gest zero',
                       between(per$gest, 20, 21) ~ 'Gest 20=21',
                       between(per$gest, 22, 23) ~ 'Gest 22=23',
                       between(per$gest, 24, 27) ~ ' Gest 24=27',
                       between(per$gest, 28, 31) ~ 'Gest 28=31 ',
                       between(per$gest, 32, 36) ~ 'Gest 32=36',
                       per$gest >= 37 ~ 'post mature')
per$gest2 <- as.factor(per$gest2) ## order as a factor.
glimpse(per)

per$gravrec <- as.factor(per$gravrec)
per %>% ##(vi) pregnancy type 
  group_by(gest2)%>% ## 352 /958 (37% ) gestation not imputted, were early premature delivery and 29% post mature delivery
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
## recode number of pregnancies women had
per$gravrec <- case_when(per$grav <= 1 ~ 'Nulliparous',
                         per$grav >=2 ~ 'Multiparous')
per %>% ##(vi) recode number of gravida 
  group_by(gravrec)%>% ## 518/958 (54 % ) were multiparous
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))


## risk factors that may have lead to perinatal death
per %>% ##(vii) cord prolapse
  group_by(cord_prol)%>% ## 30 /603 (5% ) presented with cord prolapse
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(vii) did newborn present with prematurity
  group_by(c_d_pretm)%>% ## 160/603 (26% ) were born premature
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(vii) did newborn present with birth defect
  group_by(c_d_birthdef)%>% ## 3/603 (4% ) were born with birth defect
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

per %>% ##(vii) did foetus present with abnormal fetal heart sound
  group_by(fhs_abn)%>% ## 80/603 (13% ) presented with abnormal fetal heart
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

## Convert variables to factors
glimpse(per_cn)
levels(per$dgn_ident)
per_cn$dgn_ident<- as.factor(per_cn$dgn_ident)
library(magrittr)
per %<>%
  mutate_each_(funs(factor(.)),per)

per_cn %>% mutate_if(is.character,funs(factor(.)))

## convert variables from charactor to factors#
per[sapply(per, is.character)] <- lapply(per [sapply(per, is.character)], 
                                         as.factor)

##two by two table analysis#
glimpse(per)
pertabfp <- table(per$foetus_pres, (per$c_baby_birth)) ## by stratifying condition at birth by fetus presentation 
pertabfp ## bulk had cephalic presentation
pertabft <- table(per$c_baby_birth, (per$fhs_abn))
pertabft

table(per$c_baby_birth, per$gravrec, deparse.level = 2,  useNA = "ifany") # 117 is Nalliparous 
table(per$parity,per$lab_rupm, deparse.level = 2,  useNA = "ifany")
table(per$tod_newborn, per$parity, deparse.level = 2,  useNA = "ifany")
table(per$c_baby_birth, per$pre_eclampsia, deparse.level = 2,  useNA = "ifany")
table(per$gravrec, per$mod,deparse.level = 2, useNA = "ifany") ## consider 46% were Nulliparous does CS have impact
table(per$gravrec, per$part_use ,deparse.level = 2, useNA = "ifany")

library(epiR)

library(Epi)
twoby2( per$loc_labr, per$c_baby_birth)
twoby2( per$Loc_del, per$c_baby_birth)
twoby2( per$mod, per$c_baby_birth)
twoby2( per$anc_visits, per$c_baby_birth)
twoby2( per$anc_attend, per$c_baby_birth)
twoby2( per$preg_type, per$c_baby_birth)
twoby2( per$pre_eclampsia, per$c_baby_birth)
twoby2(per$foetus_pres,per$c_baby_birth)
twoby2(per$preg_type,per$c_baby_birth)
twoby2(per$tod_newborn,per$c_baby_birth)
twoby2(per$C_d_asphy,per$c_baby_birth)
twoby2(per$c_d_birthdef,per$c_baby_birth)
twoby2(per$c_d_pretm,per$c_baby_birth)
twoby2(per$cord_prol,per$c_baby_birth)
twoby2(per$part_use,per$c_baby_birth)
twoby2(per$liq_fsmell,per$c_baby_birth)
twoby2(per$liq_mecon,per$c_baby_birth)
twoby2(per$lab_obstr,per$c_baby_birth)
twoby2(per$lab_rupm,per$c_baby_birth)
twoby2(per$i_photo_therapy,per$c_baby_birth)

library(epiR)
Odds_Ratio <- epi.2by2(pertabft, method = "case.control", conf.level = 0.95)
glimpse(per)
library(nnet)
table(per$c_baby_birth)
per$cbay<- as.factor(c_baby_birth)
per$cbay = relevel(per$c_baby_birth , ref = "Fresh still birth")
permulti1 = multinom(cbay ~ bwt + parity + gest , data=per)
summary(permulti1)

library(epicalc)
use(per)
logistic.display(glm(c_baby_birth=="Fresh still birth" ~ fhs_abn, binomial))

cbaby <- glm(gest~ c_baby_birth, family = poisson, data = per)
