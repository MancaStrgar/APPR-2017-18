# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")

# Funkcija, ki uvozi vodilne države na evropskem prvenstvu
uvozi.ep.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela1 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela1 <- tabela1[-1,-c(2, 4, 7)]
  colnames(tabela1) <- c("LETO","ZMAGOVALEC","DRUGI", "TRETJI","ČETRTI")
  tabela1$LETO <- tabela1$LETO %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer()
  tabela1$ZMAGOVALEC <-gsub("ZRJ", "Srbija", tabela1$ZMAGOVALEC)
  tabela1$TRETJI <-gsub("ZRJ", "Srbija", tabela1$TRETJI)
<<<<<<< HEAD
  return(tabela1)
}
uvozi.ep.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela1 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela1 <- tabela1[-1,-c(2, 4, 7)]
  colnames(tabela1) <- c("LETO", 1:4)
  tabela1 <- melt(tabela1, id.vars = "LETO", variable.name = "Uvrstitev", value.name = "Drzava") %>%
    mutate(LETO = LETO %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer(),
           Uvrstitev = Uvrstitev %>% parse_number())
  Encoding(tabela1$Drzava) <- "UTF-8"
  return(tabela1)
}
=======
  
}
uvozi.ep.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela1 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela1 <- tabela1[-1,-c(2, 4, 7)]
  colnames(tabela1) <- c("LETO", 1:4)
  tabela1 <- melt(tabela1, id.vars = "LETO", variable.name = "Uvrstitev", value.name = "Drzava") %>%
    mutate(LETO = LETO %>% strapplyc("([0-9]+)") %>% unlist() %>% parse_integer(),
           Uvrstitev = Uvrstitev %>% parse_number())
  Encoding(tabela1$Drzava) <- "UTF-8"
  return(tabela1)
}


# Funkcija, ki uvozi rezultate držav na EP
uvozi.drzave.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela2 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[2]] %>% html_table(dec = ",", fill = TRUE)
  tabela2 <- tabela2[-1,-1]
  colnames(tabela2) <- c("DRŽAVA", "ZLATO", "SREBRO", "BRON", "SKUPAJ")

  
}




# Funkcija, ki uvozi podatke slovenskih reprezentančnih igralcev
uvozi.slo.reprezentanca <- function() {
  link <- "http://www.kzs.si/tekmovanja-in-projekti/reprezentance/clani/zgodovina/2017/"
  stran <- html_session(link) %>% read_html()
  tabela3 <- stran %>% html_nodes(xpath="//table[@class='tabela_podatki']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela3 <- tabela3[c("IME IN PRIIMEK", "IGRALNO MESTO", "VIŠINA", "ROJEN", "KLUB")]
  tabela3 <- tabela3 %>% mutate(VIŠINA = parse_number(VIŠINA),
                               ROJEN = gsub("[^0-9.]", "", ROJEN) %>% parse_date("%d.%m.%Y"))
<<<<<<< HEAD
 return(tabela3)
=======
 
>>>>>>> 26294b75dc2e2967074904f4cbad5c89fdadf161
}




# Funkcija, ki uvozi najkoristnejšege igralce EP
uvozi.MVP <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_ko%C5%A1arki"
  stran <- html_session(link) %>% read_html()
  tabela4 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[3]] %>% html_table(dec = ",", fill = TRUE)
  colnames(tabela4) <- c("LETO", "MVP")
  
  html_tabela4 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>% .[[3]]
  tabela4 <- html_tabela4 %>% html_table(fill = TRUE)
  drzave <- html_tabela4 %>% html_nodes(xpath=".//tr") %>% .[-1] %>%
    sapply(. %>% html_nodes(xpath="./td") %>%
             lapply(. %>% html_nodes(xpath="./a[@class='image']") %>% html_attr("href") %>%
                      sapply(. %>% { gsub("US_.*_Flag", "Flag_of_the_United_States", .) } %>%
                               strapplyc("Flag_of_(.*)\\.svg") %>%
                               { gsub("_", " ", gsub("_\\(.*", "", gsub("^the_", "", .))) }) %>%
                      .[1]) %>% { ifelse(sapply(., is.list),NA, .) } %>%
             unlist()) %>% t() %>% data.frame()
  tabela4$DRŽAVA <- drzave[[2]]
  tabela4$DRŽAVA <-gsub("Second Spanish Republic", "Spain", tabela4$DRŽAVA)
  tabela4$DRŽAVA <-gsub("SFR Yugoslavia", "Yugoslavia", tabela4$DRŽAVA)
  tabela4$DRŽAVA <-gsub("FR Yugoslavia", "Srbia", tabela4$DRŽAVA)
  
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
)
  tabela4 <- tabela4 %>% mutate(DRŽAVA = drzave.slo[DRŽAVA])
  
  



# Funkcija, ki uvozi najboljše strelce na EP
uvozi.najboljsi.strelec <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_ko%C5%A1arki"
  stran <- html_session(link) %>% read_html()
  tabela5 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[4]] %>% html_table(dec = ",", fill = TRUE)
  html_tabela5 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>% .[[4]]
  tabela5 <- html_tabela5 %>% html_table(fill = TRUE)
  drzave2 <- html_tabela5 %>% html_nodes(xpath=".//tr") %>% .[-1] %>%
    sapply(. %>% html_nodes(xpath="./td") %>%
             lapply(. %>% html_nodes(xpath="./a[@class='image']") %>% html_attr("href") %>%
                      sapply(. %>% { gsub("US_.*_Flag", "Flag_of_the_United_States", .) } %>%
                               strapplyc("Flag_of_(.*)\\.svg") %>%
                               { gsub("_", " ", gsub("_\\(.*", "", gsub("^the_", "", .))) }) %>%
                      .[1]) %>% { ifelse(sapply(., is.list),NA, .) } %>%
             unlist()) %>% t() %>% data.frame()
  tabela5$DRŽAVA <- drzave2[[2]]
  tabela5$DRŽAVA <-gsub("SFR Yugoslavia", "Yugoslavia", tabela5$DRŽAVA)
  tabela5$DRŽAVA <-gsub("FR Yugoslavia", "Srbia", tabela5$DRŽAVA)
  
  
  drzave2.slo <- c(
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
  
  tabela5 <- tabela5 %>% mutate(DRŽAVA = drzave2.slo[DRŽAVA])
  
  
  
  
  
  for (i in 1:ncol(tabela5)) {
    if (is.character(tabela5[[i]])) {
      Encoding(tabela5[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}






# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names = c("obcina", 1:4),
                    locale = locale(encoding = "Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse = " ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% melt(id.vars = "obcina", variable.name = "velikost.druzine",
                        value.name = "stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- factor(data$obcina, levels = obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.

# Funkcija, ki uvozi občine iz Wikipedije
uvozi.ep.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    tabela[[col]] <- parse_number(tabela[[col]], na = "-", locale = sl)
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names = c("obcina", 1:4),
                    locale = locale(encoding = "Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse = " ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% melt(id.vars = "obcina", variable.name = "velikost.druzine",
                        value.name = "stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- factor(data$obcina, levels = obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
