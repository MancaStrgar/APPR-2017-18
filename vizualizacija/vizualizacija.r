# 3. faza: Vizualizacija podatkov

#Graf 10 držav z največ uvrstitvami med top 4 na evropskih prvenstvih
top10 <- ggplot(ep.rezultati %>% group_by(DRZAVA ) %>% summarise(stevilo = n()) %>%
         arrange(desc(stevilo)) %>% top_n(10),
       aes(x = reorder(DRZAVA, -stevilo), y = stevilo)) + geom_col() +
  xlab("Država") + ylab("Število uvrstitev") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle(" 10 najuspešnejših držav na EP")

#Graf povprečnjega števila metov na igro najboljših strelceu po letih
tocke <- ggplot(najboljsi.strelec, aes(x = LETO, y = TOCKE)) + geom_line() +
  xlab("Leto") + ylab("Točke") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Povprečno število metov na igro najboljših strelcev po letih")

#Graf najuspešnejših držav, glede na število MVP-jeu skozi vse EP-je
MVP.drzave <- ggplot(MVP.slo %>% group_by(DRZAVA) %>% summarise(stevilo = n()),
                aes(x = reorder(DRZAVA, -stevilo), y = stevilo)) + geom_col() +
  xlab("Država") + ylab("Število MVP-jev") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle(" Število MVP-jev na državo")

library(ggplot2)
library(dplyr)
library(readr)
library(tibble)

#Zemljevid zmagovalcev EP
evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                          "ne_50m_admin_0_countries", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(REGION_WB %in% c("Europe & Central Asia","Middle East & North Africa"))

prva.mesta <- ep.rezultati %>% filter(UVRSTITEV ==1) %>% group_by(DRZAVA) %>% 
  summarise(stevilo = n()) %>% arrange(desc(stevilo)) 

drzave.eng <- c("Sovjetska zveza" = "Russia",
                "Rusija" = "Russia",
                "Jugoslavija" = "Republic of Serbia",
                "Srbija" = "Republic of Serbia",
                "Litva" = "Lithuania",
                "Španija" = "Spain" ,
                "Italija" = "Italy",
                "Francija" ="France" ,
                "Madžarska" ="Hungary" ,
                "Nemčija" ="Germany",
                "Slovenija" ="Slovenia",
                "Latvija" ="Latvia",
                "Greece" = "Grčija",
                "Češkoslovaška" = "Czechia",
                "Egipt" = "Egypt"
)

zemljevid.zmagovalci <- ggplot() +
  geom_polygon(data = prva.mesta %>%
                 mutate(SOVEREIGNT = parse_factor(drzave.eng[DRZAVA], levels(evropa$SOVEREIGNT))) %>%
                 group_by(SOVEREIGNT) %>% summarise(stevilo = sum(stevilo)) %>%
                 right_join(evropa),
               aes(x = long, y = lat, group = group, fill = stevilo)) +
  coord_cartesian(xlim = c(-22, 40), ylim = c(30, 70))

