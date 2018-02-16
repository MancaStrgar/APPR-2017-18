# 2. faza: Uvoz podatkov


# Funkcija, ki uvozi vodilne države na evropskem prvenstvu
uvozi.ep.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela1 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela1 <- tabela1[-1,-c(2, 4, 7)]
  colnames(tabela1) <- c("LETO", 1:4)
  tabela1 <- melt(tabela1, id.vars = "LETO", variable.name = "UVRSTITEV", value.name = "DRZAVA") %>%
    mutate(LETO = LETO %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer(),
           UVRSTITEV = UVRSTITEV %>% parse_number()) %>% arrange(LETO, UVRSTITEV)
  Encoding(tabela1$DRZAVA) <- "UTF-8"
  tabela1$DRZAVA <-gsub("ZRJ", "Srbija", tabela1$DRZAVA)
  return(tabela1)
}
ep.rezultati <- uvozi.ep.rezultati()



# Funkcija, ki uvozi najkoristnejšege igralce EP
uvozi.MVP <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_ko%C5%A1arki"
  stran <- html_session(link) %>% read_html()
  html_tabela3 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>% .[[3]]
  tabela3 <- html_tabela3 %>% html_table(fill = TRUE)
  drzave <- html_tabela3 %>% html_nodes(xpath=".//tr") %>% .[-1] %>%
    sapply(. %>% html_nodes(xpath="./td") %>%
             lapply(. %>% html_nodes(xpath="./a[@class='image']") %>% html_attr("href") %>%
                      sapply(. %>% { gsub("US_.*_Flag", "Flag_of_the_United_States", .) } %>%
                               strapplyc("Flag_of_(.*)\\.svg") %>%
                               { gsub("_", " ", gsub("_\\(.*", "", gsub("^the_", "", .))) }) %>%
                      .[1]) %>% { ifelse(sapply(., is.list),NA, .) } %>%
             unlist()) %>% t() %>% data.frame()
  tabela3$DRZAVA <- drzave[[2]]
  tabela3$DRZAVA <-gsub("Second Spanish Republic", "Spain", tabela3$DRZAVA)
  tabela3$DRZAVA <-gsub("SFR Yugoslavia", "Yugoslavia", tabela3$DRZAVA)
  tabela3$DRZAVA <-gsub("FR Yugoslavia", "Srbia", tabela3$DRZAVA)
  colnames(tabela3) <- c("LETO", "MVP","DRZAVA")
  return(tabela3)
}
MVP <- uvozi.MVP()

drzave.slo <- c(
  "Spain" = "Španija",
  "Italy" = "Italija",
  "France" = "Francija",
  "Srbia" = "Srbija",
  "Lithuania" = "Litva",
  "Hungary" = "Madžarska",
  "Soviet Union" = "Sovjetska zveza",
  "Turkey" = "Turčija",
  "Czech Republic" = "Češka",
  "Croatia" = "Hrvaška",
  "Germany" = "Nemčija",
  "Yugoslavia" = "Jugoslavija",
  "Slovenia" = "Slovenija",
  "Israel" = "Izrael",
  "Croatia" = "Hrvaška",
  "Latvia" = "Latvija",
  "Estonia" = "Estonija",
  "Poland" = "Poljska",
  "Lebanon" = "Libanon",
  "Belgium" = "Belgija",
  "Netherlands" = "Nizozemska",
  "Bosnia and Herzegovina" = "Bosna in Hercegovina",
  "Greece" = "Grčija",
  "Bulgaria" = "Bolgarija",
  "Russia" = "Rusija"
)
MVP.slo <- MVP %>% mutate(DRZAVA = drzave.slo[DRZAVA])
  


# Funkcija, ki uvozi najboljše strelce na EP
uvozi.najboljsi.strelec <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_ko%C5%A1arki"
  stran <- html_session(link) %>% read_html()
  html_tabela4 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>% .[[4]] 
  tabela4 <- html_tabela4 %>% html_table(dec = ",", fill = TRUE)
  drzave2 <- html_tabela4 %>% html_nodes(xpath=".//tr") %>% .[-1] %>%
    sapply(. %>% html_nodes(xpath="./td") %>%
             lapply(. %>% html_nodes(xpath="./a[@class='image']") %>% html_attr("href") %>%
                      sapply(. %>% { gsub("US_.*_Flag", "Flag_of_the_United_States", .) } %>%
                               strapplyc("Flag_of_(.*)\\.svg") %>%
                               { gsub("_", " ", gsub("_\\(.*", "", gsub("^the_", "", .))) }) %>%
                      .[1]) %>% { ifelse(sapply(., is.list),NA, .) } %>%
             unlist()) %>% t() %>% data.frame()
  tabela4$DRZAVA <- drzave2[[2]]
  tabela4$DRZAVA <-gsub("SFR Yugoslavia", "Yugoslavia", tabela4$DRZAVA)
  tabela4$DRZAVA <-gsub("FR Yugoslavia", "Srbia", tabela4$DRZAVA)
  
  colnames(tabela4) <- c("LETO", "STRELEC", "TOCKE", "DRZAVA")
  return(tabela4)
}
najboljsi.strelec <- uvozi.najboljsi.strelec()  
najboljsi.strelec.slo <- najboljsi.strelec %>% mutate(DRZAVA = drzave.slo[DRZAVA])



# Funkcija, ki uvozi podatke slovenskih reprezentančnih igralcev
uvozi.slo.reprezentanca <- function() {
  link <- "http://www.kzs.si/tekmovanja-in-projekti/reprezentance/clani/zgodovina/2017/"
  stran <- html_session(link) %>% read_html()
  tabela2 <- stran %>% html_nodes(xpath="//table[@class='tabela_podatki']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela2 <- tabela2[,-1]
  colnames(tabela2) <- c("IGRALEC", "POZICIJA", "VISINA", "ROJEN", "KLUB")
  tabela2 <- tabela2 %>% mutate(VISINA = parse_number(VISINA),
                                ROJEN = gsub("[^0-9.]", "", ROJEN) %>% parse_date("%d.%m.%Y"))
  tabela2$KLUB <-gsub("Krka", "Krka (SLO)", tabela2$KLUB)
  return(tabela2)
}
slo.reprezentanca <- uvozi.slo.reprezentanca()

  