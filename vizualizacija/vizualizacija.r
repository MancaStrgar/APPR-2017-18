# 3. faza: Vizualizacija podatkov

#Graf 10 držav z največ uvrstitvami med top 4 na evropskih prvenstvih
ggplot(ep.rezultati %>% group_by(DRZAVA) %>% summarise(stevilo = n()) %>%
         arrange(desc(stevilo)) %>% top_n(10),
       aes(x = reorder(DRZAVA, -stevilo), y = stevilo)) + geom_col() +
  xlab("Država") + ylab("Število uvrstitev") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

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
