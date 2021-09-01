#Required#
library(tidyverse)#data manipulations
library(haven)#read SPSS file
library(essurvey)#download ESS data
library(codebook)#extract metadata
library(countrycode)
library(scales)


####Reading ESS datasets####
tess_data <- read_sav(file = "./data/ESS1-8e01.sav")

#codebook#
codebook <- codebook_table(tess_data)


## Reading Round 8 SPSS file ##
ess_data <- read_sav("./data/ESS8e02.sav")#reading round 8


##### 13) Social benefits - equal society ######


equal <- ess_data %>%
  group_by(cntry) %>%
  count(sbeqsoc, wt = pspwght) %>%
  filter(!is.na(sbeqsoc)) %>%
  mutate(perc = prop.table(n)*100,
         country = countrycode(cntry, "iso2c", "country.name"))

ord_equal <- equal %>%
  dplyr::select(-n) %>%
  spread(sbeqsoc, perc) %>%
  mutate(agree = `1`+`2`) %>%
  arrange(desc(agree)) %>%
  dplyr::select(cntry, agree)

equal <- left_join(equal, ord_equal, by = "cntry")

library(forcats)

ggplot(equal, aes(x=fct_reorder(country, agree), y=perc, fill=as.factor(sbeqsoc))) +
  geom_bar(stat="identity") +
  geom_text(aes(x = country, y = perc, label = round(perc), group = sbeqsoc),
            position = position_stack(vjust = .5), size = 3.5, colour = "grey90",
            data = equal[equal$cntry %in% c("IE", "FI", "RU"),])+
  scale_y_continuous("", expand = c(0, 0))+
  scale_fill_manual(values=c('#01665E', '#5AB4AC', '#CCCDCC', '#FC8D59','#D73027')) +
  scale_x_discrete() +
  theme(legend.position="bottom",
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=13),
        panel.background = element_rect(fill = "white", colour = "grey50")) +
  coord_flip()
