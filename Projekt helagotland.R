#########################
###Projekt Helagotland###
#########################

library(readr)
library(tidyverse)
nuvarande_ledamoter <- read_csv2("nuvarande_ledamoter.csv", skip = 1)

# I filen ”Nuvarande ledamöter” ingår valtyp, geografi, parti, namn på ledamot, tillträdesdatum, kön, invalsordning, om de blivit personvalda, födelseår, vilken lista de blivit valda ifrån och platsen på den listan samt valtillfälle.

# Skapa Gotland data och filtrera nationell valdata till endast kommunval
nuvarande_ledamoter_Gotland <- nuvarande_ledamoter %>%
  filter(Kommun == "Gotland") %>%
  rename(Ålder_valdag = `Ålder valdag`)

nuvarande_ledamoter <- nuvarande_ledamoter %>%
  filter(Valtyp == "KF") %>%
  rename(Ålder_valdag = `Ålder valdag`)

# Medelålder på kommunpolitiker, Gotland och nationellt
nuvarande_ledamoter_Gotland %>%
  summarize(medelålder = mean(Ålder_valdag))

nuvarande_ledamoter %>%
  summarize(medelålder = mean(Ålder_valdag, na.rm = TRUE))

# Andel politiker under 30 i procent, Gotland och nationellt
nuvarande_ledamoter_Gotland %>%
  summarize(procent_under_30 = mean(Ålder_valdag < 30, na.rm = TRUE) * 100)

nuvarande_ledamoter %>%
  summarize(procent_under_30 = mean(Ålder_valdag < 30, na.rm = TRUE) * 100)

# Hur ser det ut i kommunerna?
kommunöversikt <- nuvarande_ledamoter %>%
  group_by(Kommun) %>%
  summarize(medelålder = mean(Ålder_valdag, na.rm = TRUE), 
            procent_under_30 = mean(Ålder_valdag < 30, na.rm = TRUE) * 100) %>%
  arrange(desc(procent_under_30))

# Hur många poliitker var under 30 på valdagen?
nuvarande_ledamoter_Gotland %>%
  count(Ålder_valdag < 30)
