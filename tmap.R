## Step1: Load library tools
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(rgeos)
library(dplyr)
library(RColorBrewer)
## Step2: Read shapefiles and data files
Nakuru <- st_read("~/desktop/Nakuru/data/Nakuruw.shp")
summary(Nakuru)
## Step3: Convert vaiable to Percentage
Nakuru$COV_ANC_4T <- Nakuru$COV_ANC_4T*100
Nakuru$COV_DEL <- Nakuru$COV_DEL*100
Nakuru$COV_ANC_1S <- Nakuru$COV_ANC_1S*100
Nakuru$COV_PMTCT <- Nakuru$COV_PMTCT_*100
summary(Nakuru)
glimpse(Nakuru)
## Step 4: Plot the map
qtm(Nakuru, fill = "COV_ANC_4T")
tm_shape(Nakuru) + tm_fill("COV_ANC_4T")
tm_shape(Nakuru) + tm_fill("COV_ANC_4T", style = "quantile", palette = "Reds")
tm_shape(Nakuru) + tm_fill("COV_DEL", style = "quantile",n=12, palette = "Reds")

qtm(Nakuru)
## You can use this one
tm_shape(Nakuru) + tm_fill("COV_ANC_4T", style = "quantile", n = 8, palette = "Reds", 
                           legend.hist = TRUE)+ tm_borders(alpha = .4)+ tm_scale_bar() + 
                            tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
                                    scale=0.5, size.lowerbound = .01)

## Legend setting 
tm_shape(Nakuru) + tm_fill("COV_ANC_4T", style = "quantile",n=7, palette = "Reds") +  
                                        tm_legend(position=c(35.649, -0.787), frame=TRUE)

  
  ##
tm_shape(Nakuru) + tm_fill("COV_ANC_4T",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds",
                           legend.hist = TRUE) + tm_borders(alpha = .4)+ tm_scale_bar() + tm_legend(position=c(35.649, -0.787), frame=TRUE)+
                            tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
                              scale=0.5, size.lowerbound = .01)
##1
tm_shape(Nakuru) + tm_fill("COV_ANC_4T",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds",
                           legend.hist = TRUE) + tm_borders(alpha = .4)+ tm_scale_bar() + 
  tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01)

### Step 5a: Legend position use this one  for ANC coverage
tm_shape(Nakuru) + tm_fill("COV_ANC_4T",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds", 
                           legend.position = c(35.649, -0.787),
                           title=c('4th Visit(%)'),
                            legend.hist = TRUE) + tm_borders(alpha = .4)+ tm_scale_bar(position=c("left", "bottom")) + 
                            tm_text("NAME_3", size="AREA", col = "black", root=4, shadow = FALSE, 
                            scale=0.5, size.lowerbound = .01) +
                            tm_layout(title = "ANC Coverage", frame = FALSE)

### Step 5b: Legend position use this one 
tm_shape(Nakuru) + tm_fill("COV_ANC_4T",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds", 
                legend.hist = TRUE) + tm_borders(alpha = .4)+ tm_scale_bar(position=c("left", "bottom")) + 
                       tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
                          scale=0.5, size.lowerbound = .01) +
                             tm_layout(title = "ANC Coverage", frame = FALSE)

tm_shape(Nakuru) + tm_fill("COV_DEL",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds", 
                           legend.hist = FALSE) + tm_borders(alpha = .4)+ tm_scale_bar(position=c("left", "bottom")) + 
  tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01) +
  tm_layout(title = "Skill Delivery Coverage", frame = FALSE)

### tweaked map
tm_shape(Nakuru) + tm_fill("COV_DEL", title = "Skilled Coverage(%)",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds", 
                           legend.hist = FALSE) + tm_borders(alpha = .4)+ tm_scale_bar(position=c("left", "bottom")) + 
  tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01) +
  tm_layout(title = "Skill Delivery Coverage", frame = FALSE)
##tm_text('iso_a3',size = .5, col = "black", bg.color = "white")


tm_shape(Nakuru) + tm_fill("COV_DEL", title = "Skilled Coverage(%)",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "-RdYlBu", 
                           legend.hist = FALSE) + tm_borders(alpha = .4)+ tm_scale_bar(position=c("left", "bottom")) + 
  tm_text("NAME_3", size="AREA", col = "black", bg.color = "white", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01) +
  tm_layout(title = "Skill Delivery Coverage", frame = FALSE)
## Producing two maps
tm_shape(Nakuru) + tm_fill(col=c("COV_ANC_1S","COV_ANC_4T"), 
                           title=c('ANC 1st Visit(%)'), 
                           style = "fixed",breaks = c(0, 25, 50, 75, 100,125, 150, Inf),legend.hist = TRUE) +
  tm_text("NAME_3", size="AREA", col = "black", bg.color = "white", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01) +
                           tm_borders()+ tm_layout(panel.labels=c("Coverage 1st ANC Visit Per Ward")) + 
  tm_scale_bar(position=c("left", "bottom"))

glimpse (Nakuru)

### taking Legend up 
tm_shape(Nakuru) + tm_fill(col=c("COV_ANC_1S","COV_ANC_4T"), 
                           title=c('1st(%)','4th(%)'), 
                           style = "fixed",breaks = c(0, 25, 50, 75, 100,125, 150, Inf),legend.hist = TRUE) +
tm_text("NAME_3", size="AREA", col = "black", bg.color = "white", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01) +
    tm_borders()+ tm_layout(legend.position = c("left","bottom"),legend.hist.width = .7, panel.labels=c("Coverage 1st ANC Visit Per Ward","Coverage 4th ANC Visit Per Ward")) +
  tm_scale_bar(position=c("left", "bottom"))


### Produce step by step map

tm_shape(Nakuru) + tm_fill("COV_PMTCT",style = "fixed",breaks = c(0, 25, 50, 75, 100,125, Inf), palette = "Reds", 
                           legend.hist = TRUE) + tm_borders(alpha = .4)+ tm_scale_bar(position=c("left", "bottom")) + 
  tm_text("NAME_3", size="AREA", col = "grey20", root=4, shadow = FALSE, 
          scale=0.5, size.lowerbound = .01) +
  tm_layout(title = "PMTCT Identified HIV Positive", frame = FALSE)


spacy_initialize()
d <-  spacy_parse("Bob smith gave Alice his login information.", dependency = TRUE)
library(spacyr)
install.packages("koRpus")
library(koRpus)







