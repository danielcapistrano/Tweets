library(tidyverse)
library(haven)
library(countrycode)
library(WDI)



# Load TALIS 2018 dataset
load("/home/dan/Insync/gdrive/Academia/0_Resources/data/TALIS18_Teach.RData")

# Import World Bank GINI

tb_gini <- WDI(country = "all", indicator = "3.0.Gini", start = 2010,end = 2018)

tb_gini_latest <- 
  tb_gini %>% 
  mutate(CNTRY = countrycode(iso2c, "iso2c", "iso3c")) %>% 
  filter(!is.na(SI.POV.GINI)) %>% 
  group_by(CNTRY) %>% 
  filter(SI.POV.GINI == max(SI.POV.GINI))

df_talis_teachers %>% 
  group_by(CNTRY) %>% 
  summarise(help = weighted.mean(TT3G34G, w = TCHWGT, na.rm = T),
            other = weighted.mean(TT3G53F, w = TCHWGT, na.rm = T)) %>% 
  ggplot(aes(x= help, y = other, label = CNTRY)) +
  geom_point() +
  geom_label()


df_talis_teachers %>% 
  ggplot(aes(x= CNTRY, y = TT3G42F)) +
  geom_jitter(alpha = 0.8, width = 0.1, height = 0.5)


df_talis_teachers %>% 
  group_by(CNTRY) %>% 
  summarise(pract = weighted.mean(TT3G42F, w = TCHWGT, na.rm = T)) %>% 
  ggplot(aes(x = reorder(CNTRY, - pract), y = pract)) +
  geom_bar(stat = "identity")

df_talis_teachers %>% 
  group_by(CNTRY) %>% 
  summarise(pract = weighted.mean(TT3G42F, w = TCHWGT, na.rm = T)) %>% 
  left_join(tb_gini_latest) %>% 
  ggplot(aes(x = pract, y = SI.POV.GINI)) +
  geom_point() +
  geom_label(aes(label = CNTRY))


df_talis_teachers %>% 
  select(TT3G34A:TT3G54A) %>% 
  select(TT3G34G, everything()) %>% 
  cor(use = "complete.obs") %>% 
  view()



library(codebook)

cb <- codebook_table(df_talis_teachers)
