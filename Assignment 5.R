# Har hakket koden til Øystein fra forelesningen

#rm(list=ls())




###############################################################
rm(list = ls())

#install.packages("PxWebApiData")
library(PxWebApiData)
library(data.table)
library(tidyverse)

?ApiData

county <- ApiData("http://data.ssb.no/api/v0/dataset/95274.json?lang=no",
                  getDataByGET = TRUE)

whole_country <- ApiData("http://data.ssb.no/api/v0/dataset/95276.json?lang=no",
                         getDataByGET = TRUE)

# two similar lists, different labels and coding
head(county[[1]])
head(county[[2]])

head(whole_country[[1]])

# Use first list, rowbind both data
dframe <- bind_rows(county[[1]], whole_country[[1]])


# new names, could have used dplyr::rename()
names(dframe)
names(dframe) <- c("region", "date", "variable", "value")
str(dframe)

# Split date
dframe <- dframe %>% separate(date, 
                              into = c("year", "month"), 
                              sep = "M")
head(dframe)

# Make a new proper date variable
library(lubridate)
dframe <- dframe %>%  mutate(date = ymd(paste(year, month, 1)))
str(dframe)

# And how many levels has the variable?
dframe %>% select(variable) %>% unique()

# dplyr::recode()
dframe <- dframe %>% mutate(variable = dplyr::recode(variable,
                                                      "Utleigde rom"="rentedrooms",
                                                      "Pris per rom (kr)"="roomprice",
                                                      "Kapasitetsutnytting av rom (prosent)"="roomcap",
                                                      "Kapasitetsutnytting av senger (prosent)"="bedcap",
                                                      "Losjiomsetning (1 000 kr)"="revenue",
                                                      "Losjiomsetning per tilgjengeleg rom (kr)"="revperroom",
                                                      "Losjiomsetning, hittil i år (1 000 kr)"="revsofar",
                                                      "Losjiomsetning per tilgjengeleg rom, hittil i år (kr)"="revroomsofar",
                                                      "Pris per rom hittil i år (kr)"="roompricesofar",
                                                      "Kapasitetsutnytting av rom hittil i år (prosent)"="roomcapsofar",
                                                      "Kapasitetsutnytting av senger, hittil i år (prosent)"="bedcapsofar"))

dframe %>% select(variable2) %>% unique()
with(dframe, table(variable))

#

mosaic::tally(~region, data = dframe)

# we now have the data in long format ready for data wrangling

dframe <- dframe %>% mutate(region = dplyr::recode(region,
                                                  
                                                      "Trøndelag - Trööndelage"="Trøndelag",
                                                      "Troms og Finnmark - Romsa ja Finnmárku"="Troms & Finmark",
                                                      "Heile landet"="Whole Country"))

dframe %>% select(region) %>% unique()

## Lager graf og filterer for roomcap

dframe %>% filter(variable == "roomcap")%>%
ggplot(aes(x=date,y=value, group=region))+
  geom_line(aes(color=region))+
  ylab("Rom utleid i %")+xlab("Måned")+ 
  labs(title = "Kapasitets utnyttelse av hotell rommer",subtitle="i 2020")+ 
  theme_linedraw()

