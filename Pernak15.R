library(data.table)
library(psych)
library(dplyr)
library(sf)
library(lubridate)
perdatm <- readr::read_csv("~/Dropbox/perin18.csv")
perdatm %>% tbl_df() ## change the data to dataframe
glimpse(perdatm) ## use glimpse to view the data

## change date to date format
perdatm <- perdatm %>% ## with corrected date
  mutate_at(vars(event_date,doa,d_comp,c_deldate,del_date,dod),dmy) %>% 
  mutate(date = coalesce(event_date,doa,c_deldate,d_comp,del_date,dod))
glimpse(perdatm)
## Analysis part Descritptive part
library(prettyR)
describe(perdatn,num.desc = c("mean", "sd", "median", "min","max", "valid.n"))
### Summary of each variables
perdatm %>% ## 1. Distribution of perinatal deaths per year
  group_by(preg_type) %>% 
  summarise(count = n(),
   lowCI = ci(preg_type)[1],
   hiCI = ci(preg_type)[2]) 
  
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
library(prevalence)
propCI(x = 915, n = 59152)
  
glimpse(perdatn)
  
library(rjags)
perdatn %>% ## 2. Distribution of perinatal deaths per health facility
  group_by(M_gravida ) %>%
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
#24.79935 95% C.I[15.16072-34.4378]
p <- 275/11089 
#24.79935 95% C.I[15.16072-34.4378]
p + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/1000)*p*(1-p))
## PGH 2014
## PGH 2015
p <- 270/11234
## 24  95% C.I [14.54-33.5]
p + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/1000)*p*(1-p))
## PGH 2016
p <- 258/10280
p
## 25  95% C.I [15.4-34.8]
p + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/1000)*p*(1-p))
### 2017
p <- 100/3008
p
## 33.2  95% C.I [22.1-44.4]
p + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/1000)*p*(1-p))




perdatm %>% ## 3. Distribution of perinatal deaths per managing authority
  group_by(M_auth) %>% 
  summarise(freq = n()) %>% 
  mutate(rfreq = freq/sum(freq)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm %>% ## 4. Distribution of perinatal deaths per level of care
  group_by(care_level) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
## recode gestation weeks 
perdatm$Mgest <- case_when(perdatm$M_gest <= 1 ~ 'Gest zero',
                       between(perdatm$M_gest, 18, 21) ~ 'Gest 18=21',
                       between(perdatm$M_gest, 22, 23) ~ 'Gest 22=23',
                       between(perdatm$M_gest, 24, 27) ~ ' Gest 24=27',
                       between(perdatm$M_gest, 28, 31) ~ 'Gest 28=31 ',
                       between(perdatm$M_gest, 32, 36) ~ 'Gest 32=36',
                       perdatm$M_gest >= 37 ~ 'post mature')
library(tidyselect)
              
perdatm$Mgravrec <- case_when(perdatm$M_gravida <= 1 ~ 'Nulliparous',
                            between(perdatm$M_gravida,1,2) ~ 'Para 2+',
                            perdatm$M_gravida >= 3 ~ 'Para 3+')

perdatn %>% ## 5. Distribution of perinatal deaths per gestational weeks
  group_by(M_gravida,Year) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm$bwtrec<- case_when(perdatm$bwt <= 2500 ~ '<2500',
                         perdatm$bwt >=2500 ~ '>=2500')


perdatm %>% ## 5. Distribution of perinatal deaths per gestational weeks
  group_by(bwtrec,c_baby_birth) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
p <- ggplot(data=perdatm, aes(x=bwtrec, y=rfreq, fill=rfreq)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#999999", "#E69F00"))

p
library(tidyverse)



perdatm %>% ## 5. Distribution of perinatal deaths per gestational weeks
  group_by(Mgravrec,Year) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

## recode number of pregnancies women had
perdatm$parirec <- case_when(perdatm$M_parity <= 1 ~ 'Nulliparous',
                             perdatm$M_parity >=2 ~ 'Multiparous')
perdatm %>% ## 6. Distribution of perinatal deaths as per recode number of gravida 
  group_by(parirec)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm %>% ## 7. Distribution of perinatal deaths as per recode number of gravida 
  group_by(preg_type)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm %>% ## 8. Presentation of foetus at time of delivery
  group_by(foetus_pres)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm %>% ## 9. Distribution of  perinatal death per time of newborn death 
  group_by(t_nb_death)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 11 . Did mother receive antenatal care 
  group_by(anr_attend,Year)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 12 . If yes what facility did  mother receive antenatal care 
  filter( anr_attend == "Yes") %>% 
  group_by(anc_facility)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm %>% ## 12 . If yes what facility did  mother receive antenatal care 
  filter( anr_attend == "Yes") %>% 
  group_by(anc_visits)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
t <- table(perdatn$anc_visits)
t <- as.data.frame(t)
t2 <- prop.table(t)*100
t2
barplot(t,ylab="Number of perinatal death", main="Perinatal death: No. of antenatal visits")
library(RColorBrewer)
p <- ggplot(data=t, aes(x=(Var1), y=Freq, fill=Var1)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Freq), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired") +
  theme_minimal()
library(scales)
ggplot(t) +
  geom_bar(aes(reorder(Var1, -Freq), fill=Var1),
           col="red", alpha = .2, stat="identity") +
  scale_fill_gradient("Percentage", low = "green", high = "red") +
  labs(title="Histogram for Age") +
  labs(x="Age", y="Percentage")

ggplot(t) +
     geom_bar(aes(reorder(Var1, -Freq),Freq, fill=Var1),
                          col="red", alpha = .2, stat="identity") +
     scale_fill_gradient("Percentage", low = "green", high = "red") +
     labs(title="Histogram for Age") +
     labs(x="Age", y="Percentage")
library(ggthemes)
ggplot(data=t, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity") +
  theme_tufte()

perdatn %>% 
  filter(anr_attend == "Yes") %>%
  group_by(anc_visits) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  ggplot + aes( x = anc_visits, y = freq,fill = freq ) + 
  geom_bar(stat = "identity",position = position_dodge(0.2), width = 0.5) +
  labs(title="Perinatal deaths vs Number of Antenatal visit") +
  labs(x="Antenatal Visit", y="No. of Perinatal Deaths") +
  theme_tufte()

ggplot(t, aes(x = reorder(Var1, -Freq), y = Freq, fill = Freq)) + 
  geom_bar(stat = "identity",position = position_dodge(0.2), width = 0.5) +
  labs(title="Perinatal deaths vs Number of Antenatal visit") +
  labs(x="Antenatal Visit", y="No. of Perinatal Deaths") +
  theme_tufte()







p <- ggplot(data=t, aes(x=reorder(Var1), y=Freq)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Freq), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Reds") +
  theme_minimal()
p +labs(title="Number of Antenatal visit", 
        x="Antenatal visit ", y = "Number of deaths")

p + scale_fill_manual(values=c('#999999','#E69F00'))

library(ggproto)

install.packages("ggproto")



perdatn %>% ## 12 . If yes what facility did  mother receive antenatal care 
  group_by( anr_attend,Year) %>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
glimpse(perdatn)
names(perdatn)
library(dplyr)
perdatn %>% ## 11 . Did mother receive antenatal care 
  filter(anr_attend == "Yes") %>% 
  group_by(Year,r_tt)%>% 
  summarise(freq = n()) %>% 
  mutate(r.freq = freq/sum(freq)*100) %>%     ## relative frequency 
  arrange(desc(r.freq))
perdatn %>% ## 13(a) . did they receive Tetanus toxoid
  filter( anr_attend == "Yes") %>% 
  group_by(iden_dang_sign)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 13(b) . did they receive iron supplement 
  filter( anr_attend == "Yes") %>% 
  group_by(r_fesup)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 13(C) . did they ARVs for HIV infected
  filter( inf_hiv== "Yes") %>% ## for HIV positive
  group_by(r_arv)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 14 . Were any danger signs identified 
  filter( anr_attend == "Yes") %>% 
  group_by(iden_dang_sign)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 14 .  If yes specified danger signs  
  filter( anr_attend == "Yes") %>% 
  filter( anc_specdg_sign== "Yes") %>% 
  group_by(anti_hyp)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 15 .  What action were taken
  filter( anr_attend == "Yes") %>% 
  filter( anc_specdg_sign== "Yes") %>% 
  group_by(referral)%>% ## (a) Referral
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 15 .  What action were taken
  filter( anr_attend == "Yes") %>% 
  filter( anc_specdg_sign== "Yes") %>% 
  group_by(anti_mal)%>% ## (b) Anti-malarials
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 15 .  What action were taken
  filter( anr_attend == "Yes") %>% 
  filter( iden_dang_sign == "Yes") %>% 
  group_by(anti_bio)%>% ## (c) Anti-biotics
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% ## 15 .  What action were taken
  filter( anr_attend == "Yes") %>% 
  filter( iden_dang_sign == "Yes") %>% 
  group_by(anti_hyp)%>% ## (d) Anti-hypertensive
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% 
  group_by(loc_del)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))
  
perdatn %>% 
    filter (loc_del =="Medical Clinic") %>%
    group_by(part_use)%>% 
    summarise(count = n()) %>% 
    mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
    arrange(desc(rfreq))
 
   perdatn %>% 
    group_by(c_baby_birth)%>% 
    summarise(count = n()) %>% 
    mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
    arrange(desc(rfreq))

   perdatn %>% 
     group_by(cbaby)%>% 
     summarise(count = n()) %>% 
     mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
     arrange(desc(rfreq)) 

perdatm$cbaby <- case_when(perdatm$c_baby_birth =="Fresh still birth"  ~ 'stillbirth',
                              perdatm$c_baby_birth  =="Alive"~ 'Neonatal death',
                               perdatm$c_baby_birth =="Macerated stillbirth" ~ 'stillbirth')  
 
perdatm$recbwt <- case_when(perdatm$bwt <= 500 ~ '<=500',
                          between(perdatm$bwt,501,1000) ~ '501-1000',
                          between(perdatm$bwt, 1001, 1500) ~ '1001-1500',
                          between(perdatm$bwt, 1501, 2500) ~ '1501-2500',
                          between(perdatm$bwt, 2501, 3500) ~ ' 2501-3500',
                          between(perdatm$bwt, 3501, 4000) ~ '3501-4000 ',
                          perdatm$bwt >= 4001 ~ '>= 4001')  
   
perdatm$Mgest <- case_when(perdatm$M_gest <= 22 ~ '<22',
                           between(perdatm$M_gest, 22, 27)~'22-27',
                           between(perdatm$M_gest, 28, 31)~'28-31',
                           between(perdatm$M_gest, 32, 36)~'32-36',
                           between(perdatm$M_gest, 37, 41)~'37-41',
                           perdatm$M_gest >= 42 ~ '42+')   
   
perdatn$mgest <- case_when(perdatn$M_gest <= 22 ~ 'Under 22',
                            between(perdatn$M_gest,22,27) ~ '22=27',
                            between(perdatn$M_gest, 28, 31) ~ '28=31',
                            between(perdatn$M_gest, 32, 36) ~ '32=36',
                            between(perdatn$M_gest, 37, 41) ~ ' 37=41',
                            perdatn$M_gest >= 42 ~ '42+')     


perdatm %>% 
  group_by(lab_loc_st, cbaby)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatm %>% 
  filter (!(loc_del=="Home")) %>% 
  filter (!(loc_del=="BBA")) %>% 
  group_by(pre_rupmemb,c_baby_birth) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))


perdatn %>% 
  filter (!(loc_del=="Home")) %>% 
  filter (!(loc_del=="BBA")) %>% 
  group_by(ab_fhs,cbaby) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))
## Same above with macerated stillbirth
perdatn %>% 
  filter (!(loc_del=="Home")) %>% 
  filter (!(loc_del=="BBA")) %>% 
  group_by(ab_fhs,c_baby_birth) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

###


perdatn %>% 
  filter (!(loc_del=="Home")) %>% 
  filter (!(loc_del=="BBA")) %>% 
  group_by(obs_lab,cbaby) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

perdatn %>%
  group_by(obs_lab,cbaby) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

perdatn %>% 
  filter (!(loc_del=="Home")) %>% 
  filter (!(loc_del=="BBA")) %>% 
  group_by(rup_uterus,cbaby) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

perdatn %>%
  group_by(antepart_uterus,cbaby) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))




library(ggthemes)
     
perdatm %>% 
  group_by(bwtrec, cbaby)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  ggplot + aes(x=bwtrec, y=rfreq, fill=cbaby) +
  geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
  geom_text(aes(label = paste(rfreq)) +
                position = position_dodge(width = 0.9), 
                vjust = -0.5, hjust = 0.5, size = 5) +
  theme(axis.text=element_text(size=14),
           axis.title=element_text(size=14,face="bold")) + 
     scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
                          theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.background = element_blank())                          
 
perdatn %>% 
      group_by(bwtrec, c_baby_birth)%>% ## you can replace with c_baby_birth
      summarise(count = n()) %>% 
      mutate(rfreq = count/sum(count)*100) %>%
      arrange(desc(rfreq)) %>% 
      ggplot + aes(x=bwtrec, y=rfreq, fill=c_baby_birth) + ## replace with c_baby_birth
      geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
      theme(axis.text=element_text(size=14),
             axis.title=element_text(size=14,face="bold")) + 
      scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
                          theme(panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank()) 

perdatn %>% 
  group_by(recbwt, c_baby_birth)%>% 
  summarise(count = n()) %>% y
  mutate(rfreq = count/sum(count)*100) %>%
  arrange(desc(rfreq)) %>% 
  ggplot + aes(x=recbwt, y=rfreq, fill=c_baby_birth) +
  geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
### Similar to above with categorised birth weight    
perdatn %>% 
  group_by(recbwt, c_baby_birth)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%
  arrange(desc(rfreq)) %>% 
  ggplot + aes(x=recbwt, y=rfreq, fill=c_baby_birth) +
  geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())     




perdatn %>% 
  group_by(M_gest, cbaby)%>% 
  summarise(count = n()) %>%
  ggplot + aes(x=M_gest, y=count, fill=cbaby) +
  geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())    
 
perdatn %>% 
  group_by(Mgest, c_baby_birth)%>% 
  summarise(count = n()) %>%
  ggplot + aes(x=Mgest, y=count, fill=c_baby_birth) +
  geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

perdatn %>% 
  group_by(recbwt, cbaby)%>% 
  summarise(count = n()) %>%
  ggplot + aes(x=recbwt, y=count, fill=cbaby) +
  geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#56B4E9")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())    

    
      
   
   geom_bar(stat="identity",width = 0.5, position=position_dodge(), colour="black") + 
     theme_bw(base_size = 12) + 
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"))
   summarise(count = n()) %>% 
     ggplot + aes( x =preg_type, y = count,fill = count ) + 
     geom_bar(stat = "identity",position = position_dodge(0.2), width = 0.5) +
     geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
     labs(title="Distribution of perinatal death per type of pregnancy") +
     labs(x="type of pregnancy", y="No. of Perinatal Deaths")+
     theme_classic()+
     coord_flip()  
   
   
   
   
 table(perdatm$recbwt, perdatm$c_baby_birth)  
   
 p <- ggplot(data=perdatm, aes(x=recbwt, y=c_baby_birth, fill=c_babybirth)) +
   geom_bar(stat="identity", position=position_dodge(), colour="black") +
   scale_fill_manual(values=c("#999999", "#E69F00"))   
   
p 
perdatn %>% 
  filter (loc_del =="Home") %>%
  group_by(who_del)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))   
   
perdatn %>% 
  group_by(who_del)%>% 
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))     
   
   
perdatn %>% 
  filter (!(loc_del=="Home")) %>% 
  filter (!(loc_del=="BBA")) %>% 
  group_by(part_use) %>% 
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

perdatn %>% 
  filter (!(mod=="Caesarian Section")) %>%
  group_by(foetus_pres, c_baby_birth) %>% 
  summarise(n=n()) %>% 
 mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

perdatn %>% 
  group_by(preg_type) %>% 
  summarise(n=n()) %>% 
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))
class(perdatn)
library(ggplot2)
install.packages("qplot")
library(qplot)
perdatn %>% 
  group_by(mod) %>% 
  summarise(n=n())
library(dplyr)
perdatn %>% 
  group_by(preg_type)%>% 
  summarise(count = n()) %>% 
  ggplot + aes( x =preg_type, y = count,fill = count ) + 
  geom_bar(stat = "identity",position = position_dodge(0.2), width = 0.5) +
  geom_text(aes(label = count), vjust = -0.3, size = 3.5) +
  labs(title="Distribution of perinatal death per type of pregnancy") +
  labs(x="type of pregnancy", y="No. of Perinatal Deaths")+
  theme_classic()+
  coord_flip()

perdatn %>% 
  group_by(cbaby) %>%
  summarise(count = n()) 
  
qplot(preg_type,c_baby_birth, data = perdatn)

install.packages("ggplot2")
## Risk factor analysis
perdatm$cr_baby <- case_when(perdatm$cbaby =="stillbirth"  ~ '0',
                             perdatm$cbaby =="Neonatal death"~ '1')
                           
library(prettyR)
perdatn %>% 
  group_by(cr_baby) %>%
  summarise(count = n()) 
tab <- table(perdatn$cr_baby, perdatn$part_use)
tab <- table(perdatn$c_baby_birth, perdatn$part_use)
tab
library(epiR)
library(epicalc)
barplot(tab, besides=T, legend=T)
use(perdatm)
epi.2by2(tab, method = "cohort.count", conf.level = 0.95) ## the odds of having stillbirth with no use of partograph 1.11 times odds of having neonatal death
tab2 <- table(cr_baby, referral)
tab2 
epi.2by2(tab2, method = "cohort.count", conf.level = 0.95)

barplot(tab2, besides=T, legend=T)
epi.2by2(tab2, method = "cohort.count", conf.level = 0.95)
glimpse(perdatm)   

table(perdatm$c_baby_birth  ,perdatm$foetus_pres, deparse.level = 2, useNA = "ifany")
table("baby condition" = perdatm$c_baby_birth, "foetus presentation" = perdatm$foetus_pres, useNA = "ifany")
options(digits = 2)
tab <- table(perdatn$c_baby_birth  ,perdatn$foetus_pres, deparse.level = 2, useNA = "ifany")
prop.table(tab,1)
prop.table(tab,2)
by(subset(perdatm, select = c(c_baby_birth , foetus_pres,mod )), perdatm$preg_type, mean, na.rm = TRUE)
library(Hmisc)
summary(cr_baby ~ part_use + mod + preg_type , method = "reverse", data = perdatm)
perdatm$cr_baby <- as.numeric(perdatm$cr_baby)
perdatm$foetus_pres <- as.numeric(perdatm$foetus_pres)
perdatm$preg_type <- as.numeric(perdatm$preg_type)

hist(perdatm$M_gest, xlab = "Gestation", ylab = "Frequency", main = "Histogram of gestational weeks")
perdatm$c_baby <- ifelse(perdatm$c_baby_birth > 2, 0, 1)
table(perdatm$c_baby, perdatm$c_baby_birth, deparse.level = 2, useNA = "ifany")
## Univariate analysis
library(Epi)
twoby2(perdatm$cr_baby,perdatm$foetus_pres) ## Foetus presentation
twoby2(perdatm$c_baby_birth, perdatm$preg_type) ## pregnacy type (single, twin)
twoby2(1 - perdatm$cr_baby, perdatm$foetus_pres) ## Foetus presentation
twoby2(1-perdatn$cr_baby, perdatn$ab_fhs) ## abnormal foetal heart sound
twoby2(perdatn$c_baby, perdatn$liq_fsmell)
twoby2(-perdatn$cr_baby, perdatn$mec_liq)
twoby2(perdatn$cr_baby, perdatn$part_use)
twoby2(1-perdatn$cr_baby,perdatn$lab_loc_st)
twoby2(0-perdatm$cr_baby, perdatm$lab_loc_st)
twoby2(perdatn$cr_baby,perdatn$part_use)
twoby2(1-perdatn$cr_baby, perdatn$anc_visits) ## Change reference group to None
twoby2(1-perdatn$cr_baby, perdatn$loc_del)
perdatm$cr_baby <- as.numeric(perdatm$cr_baby, na.omit=TRUE)
perdatm$anc_visits <- relevel(perdatm$anc_visits, ref="Four times")
perdatm$anc_visits <- as.factor(perdatm$anc_visits)
perdatn$cord_prol<- relevel(perdatn$cord_prol, ref="Yes")
perdatn$cord_prol <- as.factor(perdatn$cord_prol)
glm2 <- glm(cr_baby ~ anc_visits, family=binomial, data=perdatn)
summary(glm2)
logistic.display(glm2)

glm3 <- glm(c_baby_birth=="Macerated stillbirth"~ anc_visits=="Once", family=binomial, data=perdatn)
summary(glm3)
logistic.display(glm3)
exp(coef(glm3))
exp(cbind(OR=coef(glm3), confint(glm3)))
glm4 <- glm(c_baby_birth=="Fresh still birth"~ referral, family=binomial, data=perdatn)
summary(glm4)
logistic.display(glm4)
exp(coef(glm3))
exp(cbind(OR=coef(glm4), confint(glm4)))

glm5 <- glm(c_baby_birth=="Fresh still birth"~ lab_loc_st, family=binomial, data=perdatn)
summary(glm5)
logistic.display(glm5)
exp(coef(glm3))
exp(cbind(OR=coef(glm5), confint(glm5)))


glm4 <- glm(c_baby_birth=="Fresh still birth"~perdatn$cord_prol=="Yes", 
            family=binomial, data=perdatn)
summary(glm4)
logistic.display(glm4)
exp(coef(glm3))
exp(cbind(OR=coef(glm4), confint(glm4)))
table(perdatn$c_baby_birth, perdatn$loc_del)

glm6 <- glm(c_baby_birth=="Fresh still birth"~perdatn$anc_visits, 
            family=binomial, data=perdatn)
summary(glm6)
exp(cbind(OR=coef(glm6), confint(glm6)))

glm7 <- glm(c_baby_birth=="Macerated stillbirth"~perdatn$anc_visits, 
            family=binomial, data=perdatn)
summary(glm7)
exp(cbind(OR=coef(glm7), confint(glm7)))



perdatn$anc_visits <- as.factor(perdatn$anc_visits)
perdatn$anc_visits <- relevel(perdatn$anc_visits,ref ="Four")
perdatn$c_baby_birth <- as.factor(perdatn$c_baby_birth)
perdatn$c_baby_birth <- relevel(perdatn$c_baby_birth, ref="Macerated stillbirth")
twoby2(perdatn$c_baby_birth=="Fresh still birth", perdatn$anc_visits)
twoby2( perdatn$anc_visits=="Four times",perdatn$c_baby_birth) ## More than four times
table(perdatn$anc_visits, perdatn$c_baby_birth)
twoby2(perdatn$cr_baby, perdatn$referral)
twoby2(perdatm$c_baby, perdatm$cord_prol)
twoby2(perdatn$c_baby, perdatn$liq_mecon)
twoby2(perdatm$cr_baby, perdatm$anc_specdg_sign)
perdatm$loc_del

perdatm %>% 
  group_by(loc_del) %>%
  summarise(count = n()) 

twoby2(perdatm$cr_baby, perdatm$loc_del=="Dispensary")          
perdatm$loc_del <- as.factor(perdatm$loc_del)
perdatm$loc_del <- relevel(perdatm$loc_del, ref="Provincial Hospital")
twoby2(perdatm$cr_baby, perdatm$anc_visits=="More than four times")
perdatm$referral <- as.factor(perdatm$referral)
perdatm$loc_del <- relevel(perdatm$loc_del, ref="Provincial Hospital")

twoby2(perdatm$cr_baby, perdatm$referral=="Yes")
cc(perdatm$cr_baby, perdatm$referral=="Yes")

cc(perdatm$cr_baby, perdatm$recbwt)
twoby2(perdatm$cr_baby, perdatm$recbwt)

cc(perdatm$cr_baby, perdatm$loc_del=="Home")
cc(perdatm$cr_baby, perdatm$lab_loc_st=="Tier2")
cc(perdatm$cr_baby, perdatm$anc_visits=="None")
cc(perdatm$cr_baby, perdatm$anc_visits=="Twice")
cc(perdatn$c_baby_birth=="Fresh still birth", perdatn$referral=="Yes")

cc(perdatn$c_baby_birth=="Fresh still birth", perdatn$lab_loc_st)
cc(perdatn$c_baby_birth=="Macerated stillbirth", perdatn$anc_visits)
cc(perdatn$c_baby_birth=="Fresh still birth", perdatn$who_del=="Doctor") # Fresh still birth # Macerated stillbirth
cc(perdatn$c_baby_birth=="Fresh still birth", perdatn$part_use=="No") 
table(perdatn$c_baby_birth, perdatn$who_del)

cc(perdatm$cr_baby, perdatm$cord_prol)
mhor(perdatm$cr_baby,perdatm$pre_rupmemb,perdatm$pre_eclampsia)
mhor(perdatm$cr_baby, perdatm$pre_rupmemb)
mhor(perdatn$cr_baby,perdatn$recbwt, perdatn$pre_rupmemb)
mhor(perdatn$cr_baby, perdatn$pre_eclampsia, perdatn$iden_dang_sign)
twoby2(perdatn$cr_baby, perdatn$pre_rupmemb)

glm2 <- glm(cr_baby ~ referral, family=binomial, data=perdatm)
logistic.display(glm2)
glm5 <- glm(cr_baby ~ part_use*cord_prol, family=binomial, data=perdatn)
logistic.display(glm5)
library(nnet)
logistic.display(glm(c_baby_birth=="Fresh stillbirth" ~ preg_type, binomial))
multi1 <- multinom(c_baby_birth ~ cord_prol); multi1
tabpct(c_baby_birth, lab_loc_st, graph=FALSE)
lab_loc_st <- as.factor(lab_loc_st)
tabpct(lab_loc_st,c_baby_birth)
perdatn$nocbaby <- !is.na(perdatn$c_baby_birth)
perdatn$labst <- !is.na(perdatn$lab_loc_st)
multi1 <- multinom(nocbaby ~ labst) 
mlogit.display(multi1)
use(perdatn)
View(Ectopic)
options(scipen = 999)
mod <- glm(nocbaby ~ cord_prol + pre_eclampsia + pre_rupmemb + Age + bwt+labst,data = perdatn,family = "binomial")
summary(mod, decimal =1)
exp(coefficients(mod))
exp(confint(mod))
logistic.display(mod, decimal = 3)

plot(mod, 1)
library(epiDisplay)
data("Ectopic")
glimpse(Ectopic)
install.packages("epiDisplay")
library(epiDisplay)
twoby2(perdatn$c_baby, perdatn$yes_dangersign)
twoby2(perdatn$c_baby, perdatn$pre_eclampsia)
twoby2(perdatn$c_baby, perdatn$r_tt)
twoby2(1-perdatn$c_baby, perdatn$rup_uterus)
twoby2(1- perdatn$c_baby, perdatn$referral)
twoby2(1- perdatn$c_baby, perdatn$dgn_ident)
twoby2(1- perdatn$c_baby, perdatn$lab_rupm)
twoby2(1- perdatn$c_baby, perdatn$anaemia)
twoby2(1- perdatn$c_baby, perdatn$fhs_abn)
twoby2(1- perdatn$c_baby, perdatn$grav)
cc(0-perdatn$cr_baby, perdatn$ab_fhs)
## recode gestation to gestrec
perdatn %>% mutate(gestrec= ifelse(perdatn$gest<37, 'Premature', 'mature'))

cc(case, eclair.eat)
glimpse(perdatn)

library(epicalc)
## Use epicalc to calculate MHOR
mhor(perdatn$cr_baby,perdatn$cord_prol)
mhor(perdatn$c_baby,perdatn$lab_rupm,perdatn$fhs_abn)
mhor(1-perdatn$c_baby,perdatn$lab_rupm,perdatn$fhs_abn)
mhor(0-perdatn$c_baby,perdatn$liq_mecon,perdatn$fhs_abn)
mhor(perdatn$c_baby,perdatn$liq_mecon, perdatn$rup_uterus)
mhor(perdatn$c_baby,perdatn$part_use, perdatn$rup_uterus)

perdatn$anc_visits <- as.factor(perdatn$anc_visits)
perdatn$anc_visits <- relevel(perdatn$anc_visits,ref = "None")
contrasts(perdatn$anc_visits)
## Logistic regression
glm0 <- glm(c_baby ~ anc_visits, family=binomial, data=perdatn)
summary(glm0)
logistic.display(glm0)
glimpse(perdatn)
perdatn$loc_labr <- as.factor(perdatn$loc_labr)
preg_type <- relevel(perdatn$preg_type, ref =1)

perdatn$preg_type <- as.factor(perdatn$preg_type)

m <- glm(c_baby ~ preg_type*loc_labr+bwt, family=binomial, data = perdatn)
logistic.display(m, decimal=1)
m1 <- glm(c_baby ~ preg_type*preg_type+lab_rupm, family=binomial, data = perdatn)
logistic.display(m1, decimal=1)

m2 <- glm(c_baby ~ preg_type*preg_type+pre_eclampsia, family=binomial, data = perdatn)
logistic.display(m2, decimal=1)

m3 <- glm(cr_baby ~ foetus_pres*part_use +pre_eclampsia, family=binomial, data = perdatn)
logistic.display(m3, decimal=1)

m4 <- glm(cr_baby ~ part_use*referral +pre_eclampsia, family=binomial, data = perdatn)
summary(m4, decimal=1)
logistic.display(m4, decimal=1)
glimpse(perdatn)

cc(perdatn$c_baby_birth=="Alive", perdatn$anc_visits)
cc(perdatn$cr_baby, perdatn$anc_visits)

md1 <- glm(c_baby ~  ancvisits + loc_labr , data = perdatn, family = "binomial")
summary(md1, decimal=1)
mod1 <- glm(c_baby ~ preg_type+pre_eclampsia + anc_visits + loc_labr+bwt + ante_partum, data = perdatn, family = "binomial")
summary(mod1, decimal=1)
logistic.display(mod1, decimal=1)
plot(mod1,2)
plot(mod1,1)

drop1(mod1, .~., test = "Chisq")
library(prettyR)
plot(naclus(perdatn))
glimpse(perdatn)
library(Hmisc)
summary(perdatn)
library(ggplot2)
ggplot(perdatn) +
  geom_histogram(aes(x=gest),
                 binwidth=5, fill="gray")
library(mgcv)
form.lin <- as.formula("cr_baby ~ bwt + anc_visits + loc_del + antepart_uterus + anc_visits  + preg_type")
linmodel <- lm(form.lin, data=perdatn)
summary(linmodel)
coefficients(linmodel)
form.glin <- as.formula("c_baby_birth ~ s(bwt) + s(ante_partum) + s(Loc_del) + s(anc_visits) + s(preg_type)")
glinmodel <- gam(form.glin, data=perdatn)
glinmodel$converged
library(tidyverse)
perdatn$gestrec <- case_when(per$gest <37 ~ 'Premature ',per$gest >= 37 ~ 'Mature')
## dealing with missing values
mean(perdatn$bwt)
mean(perdatn$gest)
mean(perdatn$gest, na.rm = TRUE)
mean(perdatn$bwt, na.rm = TRUE)
aggregate(data = perdatn, bwt~gest, mean, na.rm = TRUE)
aggregate(data=perdatn, gest~ bwt, mean, na.rm = TRUE)
ave_gest <- ave(perdatn$gest, perdatn$bwt,FUN = function(x) mean(x, na.rm = TRUE))
ave_bwt <- ave(perdatn$bwt, perdatn$gest, FUN = function(x) mean(x, na.rm = TRUE))
perdatn$gest <- ifelse(is.na(perdatn$gest), ave_gest, perdatn$gest)
perdatn$bwt <- ifelse(is.na(perdatn$bwt), ave_bwt, perdatn$bwt)
summary(perdatn$bwt)
summary(perdatn$gest)
rm(p.1, p5_2a,p5_2b)

perdatn %>% ##(vi) pregnancy type 
  group_by(anc_visits)%>% ## 783 /958 (82% ) were singleton and 17(2%) were multiple pregnancies
  summarise(count = n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>%     ## relative frequency 
  arrange(desc(rfreq))

perdatn %>% 
  group_by(year) %>% 
  summarise(count=n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>% 
   arrange(desc(rfreq))

### Perinatal rate per 1000 livebirths
library(prevalence)
propCI(x = 918, n = 57685)
perdatn %>% 
  group_by(Org_unit) %>% 
  summarise(count=n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>% 
  arrange(desc(rfreq))

perdatn %>% 
  group_by(c_baby_birth) %>% 
  summarise(count=n()) %>% 
  mutate(rfreq = count/sum(count)*100) %>% 
  arrange(desc(rfreq))

propCI(x = 533, n = 57685) 
library(rvest)
url <- "https://www.theguardian.com/commentisfree/2011/oct/28/bad-science-diy-data-analysis"
cancerdata=data.frame( readHTMLTable(url, which = 1))

url <- "http://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
poptable <- readHTMLTable(url, which = 1)
library(readr)
cancerdata <- readr::read_csv("~/Dropbox/cancerdata.csv")
cancerdata

names(cancerdata)
head(cancerdata)
tail(cancerdata)
class(cancerdata$Number)
class(cancerdata$Number)
plot(Number ~ Population,data=cancerdata)
plot(Number ~ Population,data=cancerdata, main='Bowel Cancer Occurrence by Population')
plot(Number ~ Population,data=cancerdata, main='Bowel Cancer Occurrence by Population',ylab='Number of deaths')
require(ggplot2)
ggplot(cancerdata)+geom_point(aes(x=Population,y=Number))+labs(title='Bowel Cancer Data')+ylab('Number of Deaths')
p=cancerdata$Rate/100000
p
number <- sample(1:379, 10, replace = TRUE)
p.se <- sqrt((p*(1-p)) / (number))
df <- data.frame(p, number, p.se)

library(ggplot2)

set.seed(1)
p <- runif(100)
number <- sample(1:1000, 100, replace = TRUE)
p.se <- sqrt((p*(1-p)) / (number))
df <- data.frame(p, number, p.se)

## common effect (fixed effect model)
p.fem <- weighted.mean(p, 1/p.se^2)

## lower and upper limits for 95% and 99.9% CI, based on FEM estimator
number.seq <- seq(0.001, max(number), 0.1)
number.ll95 <- p.fem - 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul95 <- p.fem + 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ll999 <- p.fem - 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul999 <- p.fem + 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
dfCI <- data.frame(number.ll95, number.ul95, number.seq, p.fem)

## draw plot
fp <- ggplot(aes(x = number, y = p), data = df) +
  geom_point(shape = 1) +
  geom_line(aes(x = number.seq, y = number.ll95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul95), data = dfCI) +
  geom_hline(aes(yintercept = p.fem), data = dfCI) +
  scale_y_continuous(limits = c(0,1.1)) +
  xlab("number") + ylab("p") + theme_bw() 
fp

df <-
  read.table(text = "
             school_id year sdq_emotional
             1060 7 4
             1060 7 5
             1060 7 7
             1060 7 6
             1060 7 4
             1060 7 7
             1060 7 8
             1115 7 5
             1115 7 9
             1115 7 3
             1136 7 1
             1136 7 8
             1136 7 5
             1136 7 9
             1135 7 4
             1139 7 7
             1139 7 3
             2371 7 6
             2371 7 3
             2372 7 4
             2372 7 1
             2378 7 6
             2378 7 7
             2378 7 5", header=TRUE)

# Format the data
df1 <- plyr::count(df, c('school_id'))
df2 <- merge(df,df1, by= c("school_id"))
df <- df2 

M3 <- aggregate(df$sdq_emotional[df$freq > 10], by=list(df$school_id[df$freq > 10]),mean,na.rm=T) 
S3 <- aggregate(df$sdq_emotional[df$freq > 10], by=list(df$school_id[df$freq > 10]),na.rm =T)

CG_PLOT1 <- merge(M3,S3,by="Group.1")
names(CG_PLOT1) <- c("School","Mean","Size")
LINE3 <- data.frame(M3=rep(mean(df$sdq_emotional,na.rm=T),max(CG_PLOT1$Size)+25), 
                    SD3=rep(sd(df$sdq_emotional,na.rm=T),max(CG_PLOT1$Size)+25))
                    
                    
                    
library(prevalence)

prop(x=141, n= 26459)
                    
                    
                    
                    
                    
                    
N3=sqrt(1:(max(CG_PLOT1$Size)+25))
ID <- 1060

filling3 <- rep("white",nrow(CG_PLOT1))
filling3[CG_PLOT1$School ==ID]<-"green"

# Build the graph
ggplot(data = CG_PLOT1) + 
  geom_line(data = LINE3, aes(x = 1:(max(CG_PLOT1$Size) + 25), 
                              y = M3 + qnorm(0.975) * SD3 / N3), size = 1, colour = "steelblue2",
            linetype = 5) +
  geom_line(data = LINE3, aes(x = 1:(max(CG_PLOT1$Size) + 25), 
                              y = M3 - qnorm(0.975) * SD3 / N3), size = 1, colour = "steelblue2",
            linetype = 5) +
  geom_segment(xend = max(CG_PLOT1$Size)+25,yend=mean(LINE3$M3,na.rm=T))+
aes(x = 1, y = mean(LINE3$M3,na.rm=T), size=1, colour="steelblue2") +
  geom_point(data = CG_PLOT1, aes(x = Size, y = Mean), size = 2,
             colour = "black", shape = 21,fill = filling3) + 
  ylim(0, 8)


library(grid)
library(dplyr)
quanti <- c("M_gravida", "M_parity")

rm(quanti)
table(perdatn$M_gravida, perdatn$M_parity)

library(ggplot2)
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_classic() 

p







