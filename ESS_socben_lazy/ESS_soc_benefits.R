#Required#
library(tidyverse)#data manipulations
library(haven)#read SPSS file
library(essurvey)#download ESS data
library(codebook)#extract metadata
library(countrycode)
library(scales)


## Reading Round 8 SPSS file ##
ess_data <- read_sav("./data/ESS8e02.sav")#reading round 8


lazy <- ess_data %>%
  group_by(cntry) %>%
  count(sblazy, wt = pspwght) %>%
  filter(!is.na(sblazy)) %>%
  mutate(perc = prop.table(n)*100,
         country = countrycode(cntry, "iso2c", "country.name"))

ord_lazy <- lazy %>%
  dplyr::select(-n) %>%
  spread(sblazy, perc) %>%
  mutate(agree = `1`+`2`) %>%
  arrange(desc(agree)) %>%
  dplyr::select(cntry, agree)

lazy <- left_join(lazy, ord_lazy, by = "cntry")

library(forcats)

ggplot(lazy, aes(x=fct_reorder(country, agree), y=perc, fill=as.factor(sblazy))) +
  geom_bar(stat="identity") +
  geom_text(aes(x = country, y = perc, label = round(perc), group = sblazy),
            position = position_stack(vjust = .5), size = 3.5, colour = "grey90",
            data = lazy[lazy$cntry %in% c("IE", "PT", "RU"),])+
  scale_y_continuous("", expand = c(0, 0))+
  scale_fill_manual(values=c('#01665E', '#5AB4AC', '#CCCDCC', '#FC8D59','#D73027')) +
  scale_x_discrete() +
  theme(legend.position="bottom",
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_flip()
