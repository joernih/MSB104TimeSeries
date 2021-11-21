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
all_data <- COVID19::covid19(verbose = F)
unique(all_data$id)
#   [1] "ABW" "AFG" "AGO" "AIA" "ALB" "AND" "ARE"
#   [8] "ARG" "ARM" "ASM" "ATG" "AUS" "AUT" "AZE"
#  [15] "BDI" "BEL" "BEN" "BES" "BFA" "BGD" "BGR"
#  [22] "BHR" "BHS" "BIH" "BLR" "BLZ" "BMU" "BOL"
#  [29] "BRA" "BRB" "BRN" "BTN" "BWA" "CAC" "CAF"
#  [36] "CAN" "CHE" "CHL" "CHN" "CIV" "CMR" "COD"
#  [43] "COG" "COK" "COL" "COM" "CPV" "CRI" "CUB"
#  [50] "CUW" "CYM" "CYP" "CZE" "DEU" "DJI" "DMA"
#  [57] "DNK" "DOM" "DPC" "DZA" "ECU" "EGY" "ERI"
#  [64] "ESP" "EST" "ETH" "FIN" "FJI" "FLK" "FRA"
#  [71] "FRO" "FSM" "GAB" "GBR" "GEO" "GHA" "GIB"
#  [78] "GIN" "GLP" "GMB" "GNB" "GNQ" "GPC" "GRC"
#  [85] "GRD" "GRL" "GTM" "GUF" "GUM" "GUY" "HND"
#  [92] "HRV" "HTI" "HUN" "IDN" "IMN" "IND" "IRL"
#  [99] "IRN" "IRQ" "ISL" "ISR" "ITA" "JAM" "JOR"
# [106] "JPN" "KAZ" "KEN" "KGZ" "KHM" "KIR" "KNA"
# [113] "KOR" "KWT" "LAO" "LBN" "LBR" "LBY" "LCA"
# [120] "LIE" "LKA" "LSO" "LTU" "LUX" "LVA" "MAR"
# [127] "MCO" "MDA" "MDG" "MDV" "MEX" "MHL" "MKD"
# [134] "MLI" "MLT" "MMR" "MNE" "MNG" "MNP" "MOZ"
# [141] "MRT" "MSR" "MSZ" "MTQ" "MUS" "MWI" "MYS"
# [148] "MYT" "NAM" "NCL" "NER" "NGA" "NIC" "NLD"
# [155] "NOR" "NPL" "NZL" "OMN" "PAK" "PAN" "PER"
# [162] "PHL" "PLW" "PNG" "POL" "PRI" "PRT" "PRY"
# [169] "PSE" "PYF" "QAT" "REU" "RKS" "ROU" "RUS"
# [176] "RWA" "SAU" "SDN" "SEN" "SGP" "SHN" "SLB"
# [183] "SLE" "SLV" "SMR" "SOM" "SRB" "SSD" "STP"
# [190] "SUR" "SVK" "SVN" "SWE" "SWZ" "SXM" "SYC"
# [197] "SYR" "TCA" "TCD" "TGO" "THA" "TJK" "TLS"
# [204] "TON" "TTO" "TUN" "TUR" "TWN" "TZA" "UGA"
# [211] "UKR" "URY" "USA" "UZB" "VAT" "VCT" "VEN"
# [218] "VGB" "VIR" "VNM" "VUT" "WLF" "WSM" "YEM"
# [225] "ZAF" "ZMB" "ZWE"
sel_cou <- c('NOR','ITA','SWE','GBR','ISR','FIN','CZE','ESP','USA','CAN','SVK','IND','JPN')
COVID19 <- all_data %>% dplyr::filter(id%in%sel_cou) %>%
# Datering
dplyr::mutate(year=as.factor(lubridate::year(date))) %>%
dplyr::mutate(dayofyear=lubridate::yday(date)) %>%
dplyr::select(id,date,confirmed,deaths,hosp,dayofyear,year,population) %>%
dplyr::mutate(c_deaths=deaths-dplyr::lag(deaths)) %>%
dplyr::mutate(ma_deaths=round(rollmean(c_deaths,k=7, fill=NA)),digits=4) %>%
dplyr::mutate(ma_deaths_perc=(ma_deaths/population)*100000)
usethis::use_data(COVID19, overwrite = TRUE)
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

