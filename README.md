# Analiza podatkov s programom R, 2017/18

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2017/18

## Tematika
Odločila sem se, da bom analizirala slovenske košarkaše na letošnjem evropskem prvenstvu in evropska košarkaška prvenstva. Za to temo sem se odločila, saj me košarka že od vedno zanima, letos pa smo postali še evropski prvaki. Podatke sem dobila na:

- http://www.kzs.si/tekmovanja-in-projekti/reprezentance/clani/zgodovina/2017/
- https://sl.m.wikipedia.org/wiki/Evropsko_prvenstvo_v_košarki

Moj projekt bo vsebova 5 tabel. Podatki, ki jih tabele vsebujejo po stolpcih so:

1. TABELA - analiza EP od 1935 do 2017:

- LETO
- ZMAGOVALEC
- DRUGI
- TRETJI
- ČETRTI


2. TABELA - analiza uspeha posamznih držav na EP:

- DRŽAVA
- ZLATO
- SREBRO
- BRON
- SKUPAJ


3. TABELA- analiza slovenskih košarkašov na EP 2017:

- IME IN PRIIMEK
- IGRALNO MESTO
- VIŠINA
- ROJEN
- KLUB


4. TABELA - analiza najkoristnejših igralcov (MVP) posameznih EP:

- LETO
- NAJKORISTNEJŠI IGRALEC (MVP)


5. TABELA - analiza igralceu, ki so na posameznem EP v povprečju dosegli največ pik:

- LETO
- IGRALEC
- POVPREČNO ŠTEVILO PIK NA TEKMO


S to nalogo poskusam ugotoviti, katere države so bile najbolj uspešne na evropskih prvenstvih, teh kateri so njboljši košarkaši evrope in od kod prihajajo.
=======

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
