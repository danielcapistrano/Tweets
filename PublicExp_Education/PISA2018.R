# Tweet on PISA 2018

library(tidyverse)
library(openxlsx)

# Loading data ####

tb_expenditure <- read.xlsx("/Users/UCD/danielcapistrano@gmail.com/Academia/6_Public/Tweets/data/OECD_Pisa_2018.xlsx", sheet = 1)

tb_reading <- read.xlsx("/Users/UCD/danielcapistrano@gmail.com/Academia/6_Public/Tweets/data/OECD_Pisa_2018.xlsx", sheet = 2)

# Merging tables ####

df_pisa <- tb_expenditure %>% left_join(tb_reading, by = "Country")

# Point per dollar ####

df_pisa <- 
  df_pisa %>%
  mutate(Point = C1_1_ISCED2_Public)


df_pisa %>% 
  filter(!is.na(C1_1_ISCED2_Public)) %>% 
  ggplot(aes(reorder(Country, C1_1_ISCED2_Public), C1_1_ISCED2_Public)) +
  geom_bar(stat = "identity") +
  labs(title = "Gasto por aluno em escolas públicas (2016)", 
       x = "", y = "Gasto por aluno em dólares PPP",
       caption = "Fonte: OECD/EAG2019")+
  coord_flip()
