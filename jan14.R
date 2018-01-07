install.packages("sjPlot")
library(sjmisc)
library(sjlabelled)
library(sjPlot)
data(efc)
set_theme(geom.label.angle = 90)
sjp.xtab(efc$e42dep, efc$e16sex, vjust = "center", hjust = "bottom")
data(efc)
efc.val <- get_labels(efc)
efc.var <- get_label(efc)
sjp.xtab(efc$e42dep, efc$e16sex, title = efc.var['e42dep'],
         axis.labels = efc.val[['e42dep']], legend.title = efc.var['e16sex'],
         legend.labels = efc.val[['e16sex']])
library(dplyr)
data(efc)
sjplot(efc, e42dep, c172code, fun = "frq")
efc %>% sjplot(e42dep, c172code, fun = "grpfrq")
efc %>%
  group_by(e16sex, c172code) %>%
  select(e42dep, e16sex, c172code) %>%
  sjplot(wrap.title = 100) # no line break for subtitles
library(sjmisc)
data(efc)
sjt.frq(efc$e42dep)
sjmisc::frq(efc$e42dep)
glimpse(efc)
sjt.frq(efc$e42dep, title = "Dependency",
        value.labels = c("independent", "slightly dependent",
                         "moderately dependent", "severely dependent"))
efc$services <- sjmisc::dicho(efc$tot_sc_e, dich.by = 0, as.num = TRUE)
fit1 <- glm(services ~ neg_c_7 + c161sex + e42dep,
            data = efc, family = binomial(link = "logit"))
fit2 <- glm(services ~ neg_c_7 + c161sex + e42dep,
            data = efc, family = binomial(link = "probit"))
fit3 <- glm(services ~ neg_c_7 + c161sex + e42dep,
            data = efc, family = poisson(link = "log"))


sjt.glm(fit1, fit2, fit3, string.est = "Estimate",show.aic = TRUE, show.family = TRUE)
devtools::install_github("ropenscilabs/styles")
devtools::install_github("fkeck/editheme")
library(default)
library(editheme)
list_pal()
library(edi)

set_base_sty("Clouds Midnight")
par(mfrow = c(2, 3))
hist(rnorm(100))
plot(iris$Sepal.Length, iris$Petal.Length)
barplot(1:9, names.arg = LETTERS[1:9])
boxplot(iris$Sepal.Length ~ iris$Species)
image(volcano)

perdatn %>%
  filter( anr_attend == "Yes") %>% 
  with(table(Year,r_fesup)) %>%
  prop.table(margin=2)*100
  
  summarise (n=n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%")) %>% 
  arrange(desc(rel.freq))

  plot_df <-perdatn %>% group_by(Year) %>%
    do(
      plots = ggplot(data = .) + aes(x = Org_unit, y = anr_attend) +
        geom_point() + ggtitle(.$Org_unit)
    )
  
  # show plots
  plot_df$plots

  ggplot(perdatn, aes(x = c_baby_birth)) +
    geom_bar()
  
  ggplot(perdatn, aes(x = mod, fill = foetus_pres),alpha=0.8) +
    geom_bar() +
    coord_flip() +
    theme_tufte()
  
  library(ggthemes)
  
  
  perdatn %>% 
    with(table(Year, Month)) %>%
    prop.table(margin=2)*100

library(data.table)
perdatn <-  as.data.table(perdatn)
perdatn[, .N, by=Org_unit]
perdatn[, .N, by=care_level]
perdatn[, .N, by=preg_type]
perdatn[, .N, by=preg_type][order(-N)]
perdatn

install.packages("spData")


