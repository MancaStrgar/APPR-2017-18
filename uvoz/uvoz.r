# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark = ",", grouping_mark = ".")

# Funkcija, ki uvozi vodilne države na evropskem prvenstvu
uvozi.ep.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela1 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  tabela1 <- tabela1[-1,-2]
  tabela1 <- tabela1[,-3]
  tabela1 <- tabela1[,-5]
  colnames(tabela1) <- c("LETO","ZMAGOVALEC","DRUGI", "TRETJI","ČETRTI")
  for (i in 1:ncol(tabela1)) {
    if (is.character(tabela1[[i]])) {
      Encoding(tabela1[[i]]) <- "UTF-8"
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





# Funkcija, ki uvozi rezultate držav na EP
uvozi.drzave.rezultati <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki"
  stran <- html_session(link) %>% read_html()
  tabela2 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[2]] %>% html_table(dec = ",", fill = TRUE)
  tabela2 <- tabela2[-1,-1]
  colnames(tabela2) <- c("DRŽAVA", "ZLATO", "SREBRO", "BRON", "SKUPAJ")
  for (i in 1:ncol(tabela2)) {
    if (is.character(tabela2[[i]])) {
      Encoding(tabela2[[i]]) <- "UTF-8"
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




# Funkcija, ki uvozi podatke slovenskih reprezentančnih igralcev
uvozi.slo.reprezentanca <- function() {
  link <- "http://www.kzs.si/tekmovanja-in-projekti/reprezentance/clani/zgodovina/2017/"
  stran <- html_session(link) %>% read_html()
  tabela3 <- stran %>% html_nodes(xpath="//table[@class='tabela_podatki']") %>%
    .[[1]] %>% html_table(dec = ",", fill = TRUE)
  colnames(tabela3) <- c("ŠT.", "IME IN PRIIMEK", "IGRALNO MESTO", "VIŠINA", "ROJEN", "KLUB")
  tabela3 <- tabela3[c("IME IN PRIIMEK", "IGRALNO MESTO", "VIŠINA", "ROJEN", "KLUB")]
  for (i in 1:ncol(tabela3)) {
    if (is.character(tabela3[[i]])) {
      Encoding(tabela3[[i]]) <- "UTF-8"
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




# Funkcija, ki uvozi najkoristnejšege igralce EP
uvozi.MVP <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_ko%C5%A1arki"
  stran <- html_session(link) %>% read_html()
  tabela4 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[3]] %>% html_table(dec = ",", fill = TRUE)
  for (i in 1:ncol(tabela4)) {
    if (is.character(tabela4[[i]])) {
      Encoding(tabela4[[i]]) <- "UTF-8"
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



# Funkcija, ki uvozi najboljše strelce na EP
uvozi.najboljsi.strelec <- function() {
  link <- "https://sl.wikipedia.org/wiki/Evropsko_prvenstvo_v_ko%C5%A1arki"
  stran <- html_session(link) %>% read_html()
  tabela5 <- stran %>% html_nodes(xpath="//table[@class='wikitable']") %>%
    .[[4]] %>% html_table(dec = ",", fill = TRUE)
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
