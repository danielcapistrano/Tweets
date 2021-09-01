# Tweet on ESS 9 

library(tidyverse)
library(openxlsx)
library(essurvey)
library(ggalt)
library(countrycode)



df_ess9 <- import_rounds(9, "danielcapistrano@gmail.com")

df_ess9 %>%
  group_by(cntry) %>%
  summarise(Education = mean(stfedu, na.rm = TRUE),
            Health = mean(stfhlth, na.rm = TRUE)) %>%
  mutate(diff = Education - Health,
         country = countrycode(cntry, "iso2c", "country.name")) %>%
  ggplot(aes(x=Education, xend=Health, y=reorder(country, diff))) + 
  geom_dumbbell(color="grey60", size=1, 
                colour_x ="darkblue", size_x = 5,
                colour_xend ="darkorange", size_xend = 5)+
  theme(axis.text.y = element_text(size=12, colour="grey20")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 10)) +
  labs(x = "Mean value",
       y = "",
       title = "Average level of satisfaction (2018)",
       caption = "Source: European Social Survey")

