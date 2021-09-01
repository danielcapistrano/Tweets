### Timeline of Brazilian government anti-vaccine policy
### Based on data organised by Bruno H Mioto @BrunoHMioto data


# Required #########################################################
library(tidyverse)
library(lubridate)


## Import data ########################################################
bmtable_raw <- read_csv("data/data-W1fvq.csv")


# Transform variables ##################################################

## Date
bmtable_raw$data_oferta <- gsub('\\.', '', bmtable_raw$`Data da oferta`)
bmtable_raw$date_offer <- lubridate::dmy(bmtable_raw$data_oferta, locale = "Portuguese")


## Offer
names(bmtable_raw)[3:7] <- c('T20201231', 'T20210331', 'T20210630', 'T20210930', 'T20211231')
bmtable <- bmtable_raw %>% pivot_longer(T20201231:T20211231, names_to = "Trimestre", values_to = "Entrega" )
bmtable$Trimestre <- lubridate::as_date(gsub('T','', bmtable$Trimestre))
bmtable$Entrega <- as.numeric(ifelse(bmtable$Entrega == "-",0, bmtable$Entrega))
bmtable <- bmtable %>% group_by(date_offer, Empresa, TOTAL) %>% 
  mutate(offer = cur_group_id())

bmtable$data_empresa <- paste0(bmtable$offer, " - ", bmtable$`Data da oferta`, 
                               "\n(", bmtable$Empresa, ")")


## Plot ###############################################################

bmtable %>%
  ggplot(aes(x = (Trimestre-10), y = reorder(data_empresa, -offer), size = Entrega, 
             color = `Resposta do Governo Federal`))+
  geom_point()+ 
  geom_text(aes(label = round(Entrega/1000000, 1)), color = "white", fontface = "bold",
            size = ifelse(bmtable$Entrega > 10000000, 5.5, 
                          ifelse(bmtable$Entrega > 4000000, 3.5, 2)))+
  scale_color_manual(values = c("darkgreen", "#d73027")) +
  scale_size (range = c(0,30), guide = "none") +
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_vline(xintercept = as.Date("2021-06-01"), linetype = "dashed") +
  annotate("text", x = as.Date("2021-05-26"), y = 2.1, label = "Junho de 2021", angle = 90) +
  theme(legend.position = "top", 
        panel.background = element_rect(fill = "grey95"),
        panel.grid.major.y = element_line(colour = "grey80"),
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        plot.title = element_text(hjust = 1, size = 18, face = "bold"),
        plot.caption = element_text(hjust = -7, vjust = -0.4, size = 12, color = "grey50"),
        axis.title.y = element_text(face = "bold", color = "#176aa6", size = 16),
        axis.title.x = element_text(face = "bold", color = "#176aa6", size = 16))+
    labs(x = "\nTrimestre de Entrega", y = "# Oferta / Data da oferta / Empresa\n",
         caption = "\nFonte: Butantan e Pfizer. Tabela preparada por Bruno H. Mioto @BrunoHMioto e gr?fico de @DanielCapis",
         title = " Ofertas de vacinas (COVID-19) apresentadas pelo Butantan e Pfizer ao Governo Federal\n(em milh?es de vacinas)")
  ggsave(filename = "vacinas.png", dpi = 400, height = 10.5, width = 10.5)




