#Required#
library(tidyverse)#data manipulations
library(haven)#read SPSS file
library(essurvey)#download ESS data
library(codebook)#extract metadata
library(countrycode)
library(scales)



## Reading Round 8 SPSS file ##
ess_data <- read_sav("./data/ESS8e02.sav")#reading round 8


tab_worry <- ess_data %>%
  group_by(cntry) %>%
  count (wrclmch, wt = pspwght) %>%
  filter(!is.na(wrclmch)) %>%
  mutate(percent = prop.table(n)*100) %>%
  dplyr::select(-n) %>%
  spread(wrclmch, percent) %>%
  mutate(worried = `4` + `5`) %>%
  mutate (country = countrycode(cntry, "iso2c", "country.name")) %>%
  arrange(worried)

ggplot(data = tab_worry, aes(x = reorder(country, worried), y = worried))+
  geom_bar(stat="identity", fill = "#1F9D9F" ) +
  geom_text(aes(label=round(worried)), size = 4, colour = "grey40", vjust= 0.25, hjust=-0.25) +
  theme(axis.text.x = element_text(size=11, colour="grey30",
                                   hjust=0.5,vjust=0.95),
        axis.text.y = element_text(size=14, colour="grey20")) +
  labs(x = "", y = "") +
  coord_flip()+
  ylim(0,60)

likert(tab_worry, as.percent = TRUE, positive.order = TRUE)
