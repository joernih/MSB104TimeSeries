## code to prepare `DATASET` dataset goes here
###########################################################################################################################################################3
library(MSB104TimeSeries)
library(COVID19)
library(dplyr)
library(lubridate)
library(zoo)
library(PxWebApiData)
library(ggplot2)
###########################################################################################################################################################3
sel_cou <- c('NOR','ITA','SWE','GBR','ISR','FIN','CZE','ESP','USA','CAN','SVK','IND','JPN')[c(1:2)]
all_data <- COVID19::covid19(country=sel_cou, level=1,verbose = T)
unique(all_data$id)
###
COVID19 <- all_data %>% dplyr::filter(id%in%sel_cou) %>%
dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
dplyr::mutate(dayofyear=lubridate::yday(date)) %>%
dplyr::select(id,date,confirmed,deaths,hosp,dayofyear,year,population) %>%
dplyr::mutate(c_deaths=deaths-dplyr::lag(deaths)) %>%
dplyr::mutate(ma_deaths=round(rollmean(c_deaths,k=7, fill=NA)),digits=4) %>%
dplyr::mutate(ma_deaths_perc=(ma_deaths/population)*100000)
usethis::use_data(COVID19, overwrite = TRUE)
View(all_data)
###########################################################################################################################################################3
covidts <- COVID19
sel_cou <- c('NOR','ITA','SWE','GBR','ISR','FIN','CZE','ESP','USA','CAN','SVK','JPN')
covhos <- plot19ts(sel_cou=sel_cou,covid19df=covidts,yvar='hosp')
covhos <- plot19ts(sel_cou=sel_cou,covid19df=covidts,yvar='ma_deaths')
covhos <- plot19ts(sel_cou=sel_cou,covid19df=covidts,yvar='ma_deaths_perc')
gridExtra::grid.arrange(grobs=covhos)
##covhos
###########################################################################################################################################################3
library(dplyr)
library(PxWebApiData)
library(ggplot2)
ssbdoede <- read.delim("ssbdoede.txt", header=FALSE)
names(ssbdoede) <- c("Kjonn","Alder","Uke","Doede","Aar","Antall")

ssbdoedec <- ssbdoede %>%
	dplyr::mutate(Doede=gsub("D\xf8de","Doede",Doede)) %>%
	dplyr::mutate(Kjonn=gsub("Begge kj\xf8nn","Begge",Kjonn)) %>%
	dplyr::mutate(Alder=gsub("Alle aldre","-1",Alder)) %>%
	dplyr::mutate(Alder=gsub(" \xe5r","",Alder)) %>%
	dplyr::mutate(Alder=gsub(" \xe5r eller aldre","",Alder)) %>%
	dplyr::mutate(Uke=gsub("Uke ","",Uke)) %>%
	dplyr::mutate(Alder=as.numeric(Alder)) %>%
	dplyr::mutate(Uke=as.numeric(Uke)) %>%
	dplyr::mutate(Antall=as.numeric(Antall))

alle_df <- ssbdoedec %>% dplyr::filter(Alder==-1) %>%
  dplyr::mutate(Aar=as.factor(Aar)) %>%
  dplyr::filter(Aar%in%c("2020","2021","2019","2018")) %>% dplyr::arrange(Aar)

ggplot(dplyr::filter(alle_df,Kjonn=='Begge'), aes(x=Uke,y=Antall, color=Aar)) + geom_smooth() + geom_vline(xintercept=22) + labs(x='ukenr',y='antall døde')
ggplot(dplyr::filter(alle_df,Kjonn=='Kvinner'), aes(x=Uke,y=Antall, color=Aar)) + geom_smooth() + geom_vline(xintercept=22) + labs(x='ukenr',y='antall døde')

aldr_df2 <- ssbdoedec %>%
  # Filter
  dplyr::filter(Alder!=-1,Kjonn=='Begge') %>%
  dplyr::filter(Alder<100) %>%
  dplyr::filter(Alder>0) %>%
  dplyr::filter(Aar%in%c("2020","2021")) %>%
  # Intervall
  dplyr::mutate(Interv=base::cut(Alder,breaks=seq(0,100, by=20)))  %>%
  # Prepreation and mutate
  dplyr::arrange(Kjonn,Aar,Alder,Uke) %>%
  dplyr::group_by(Kjonn,Aar,Interv,Uke) %>%
  #dplyr::mutate(Antall=tidyr::replace_na(Antall,0)) %>%
  dplyr::mutate(Antallinter=sum(Antall,na.rm=T)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(Uke<44)
###########################################################################################################################################################3
df1 <- dplyr::filter(aldr_df2,Alder>60)
g1 <- ggplot(df1, aes(x=Uke,y=Antallinter, color=interaction(Interv,Aar))) + geom_point() + geom_smooth() + labs(x='ukenr',y='antall døde')
df2 <- dplyr::filter(aldr_df2,Alder<60,Alder>40)
g2 <- ggplot(df2, aes(x=Uke,y=Antallinter, color=interaction(Interv,Aar))) + geom_point() + geom_smooth() + labs(x='ukenr',y='antall døde')
df3 <- dplyr::filter(aldr_df2,Alder<40)
g3 <- ggplot(df3, aes(x=Uke,y=Antallinter, color=interaction(Interv,Aar))) + geom_point() + geom_smooth() + labs(x='ukenr',y='antall døde')
gridExtra::grid.arrange(g1,g2,g3)
###########################################################################################################################################################3

