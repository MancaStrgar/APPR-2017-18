# 3. faza: Vizualizacija podatkov

#Graf 10 držav z največ uvrstitvami med top 4 na evropskih prvenstvih
top10 <- ggplot(ep.rezultati %>% group_by(DRZAVA) %>% summarise(stevilo = n()) %>%
         arrange(desc(stevilo)) %>% top_n(10),
       aes(x = reorder(DRZAVA, -stevilo), y = stevilo)) + geom_col() +
  xlab("Država") + ylab("Število uvrstitev") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Graf povprešnjega števila metov na igro najboljših strelceu po letih
tocke <- ggplot(najboljsi.strelec, aes(x = LETO, y = TOCKE)) + geom_line() +
  xlab("Leto") + ylab("Povprečje števila pik na tekmo") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

library(ggplot2)
library(dplyr)
library(readr)
library(tibble)


evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                          "ne_50m_admin_0_countries", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(CONTINENT == "Europe" | SOVEREIGNT %in% c("Russian Federation", "Egypt"),
                                  long > -30)
evropa1 <- ggplot() + geom_polygon(data = evropa, aes(x = long, y = lat, group = group))


prva.mesta <- ep.rezultati %>% filter(UVRSTITEV ==1) %>% group_by(DRZAVA) %>% 
  summarise(stevilo = n()) %>% arrange(desc(stevilo)) 



# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip",
                             "OB/OB", encoding = "Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels = levels(obcine$obcina))
zemljevid <- pretvori.zemljevid(zemljevid)



# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
