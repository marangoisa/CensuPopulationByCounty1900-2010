#set path
setwd('')

##################################################################################
#Import data and maps
##################################################################################

#Population censu 2010 by csa https://data.census.gov/cedsci/table?q=population%20csa&g=0100000US.330000&tid=DECENNIALSF12010.P1&d=DEC%20Summary%20File%201&hidePreview=false&cid=P001001&vintage=2010
Dec2010Count<-read.csv('./Data/Dec2010Count.csv')
Dec2000Count<-read.csv('./Data/Dec2000Count.csv')
#Population census 1900-1990 by county http://data.nber.org/data/census-decennial-population.html
cencounts<-read.csv('./Data/cencounts.csv')
#state fips
StateFIPS<-read.csv(paste0('./StateFIPS.csv'))
#County info
Countydf<-read.csv('./Data/CountyInfo.csv')

##################################################################################
#CSA by county
##################################################################################
#Drop counties with no CSA
Countydf<-Countydf[!is.na(Countydf$CSA),]
#drop counties outisde main land USA
Countydf<-Countydf[!Countydf$STATEFP%in%c('60','66','69','72','78'),]

##################################################################################
#More cleaning
##################################################################################

#as character
Countydf$STATEFP<-as.character(Countydf$STATEFP)
Countydf$CSA<-as.character(Countydf$CSA)
Dec2010Count$GEO_ID<-as.character(Dec2010Count$GEO_ID)
Dec2000Count$GEO_ID<-as.character(Dec2000Count$GEO_ID)
cencounts$fips<-as.character(cencounts$fips)
Countydf$GEOID<-as.character(Countydf$GEOID)
Countydf$ALAND<-as.numeric(as.character(Countydf$ALAND))
#names
names(Dec2010Count)[3]<-'pop2010'
names(Dec2000Count)[3]<-'pop2000'


##################################################################################
#Get poulation
##################################################################################

Dec2010Count$GEOID<-NA
Dec2000Count$GEOID<-NA
for(i in 1:nrow(Dec2010Count)){Dec2010Count$GEOID[i]<-strsplit(Dec2010Count$GEO_ID[i],'US')[[1]][2]}
for(i in 1:nrow(Dec2000Count)){Dec2000Count$GEOID[i]<-strsplit(Dec2000Count$GEO_ID[i],'US')[[1]][2]}
for(i in c("pop1900","pop1910","pop1920","pop1930","pop1940","pop1950","pop1960","pop1970","pop1980","pop1990","pop2000","pop2010")){
  cencounts[,names(cencounts)==i]<-as.numeric(as.character(cencounts[,names(cencounts)==i]))
  Countydf$NEW<-NA
  names(Countydf)[names(Countydf)=='NEW']<-i}
for(i in 1:nrow(cencounts)){
  if(as.numeric(cencounts$fips[i])<10000){cencounts$fips[i]<-paste0('0',cencounts$fips[i])}}
Countydf$state<-NA
for(i in 1:nrow(Countydf)){
  if(as.character(Countydf$STATEFP[i])=='11'){sti<-'DC'}else{sti<-as.character(StateFIPS$Postal.Code[StateFIPS$FIPS==as.numeric(as.character(Countydf$STATEFP[i]))])}
  Countydf$state[i]<-sti
  GEOID<-Countydf$GEOID[i]
  GEOIDnb<-GEOID
  GEOID00<-GEOID
  GEOID10<-GEOID
  Countydf$NAME[i]
  Countydf$state[i]
  if(GEOID=='46102'){
    GEOIDnb<-'46113'
    GEOID10<-'46113'
    GEOID00<-'46113'}#change name in 2014
  if(GEOID=='08014'){
    GEOIDnb<-'08013'
    GEOID00<-'08013'}#became county in 2000s
  if(GEOIDnb=='12086'){GEOIDnb<-'12025'}#spelling nber
  Countydf$pop2010[i]<-Dec2010Count$pop2010[Dec2010Count$GEOID==GEOID10]
  Countydf$pop2000[i]<-Dec2000Count$pop2000[Dec2000Count$GEOID==GEOID00]
  Countydf[i,c("pop1900","pop1910","pop1920","pop1930","pop1940","pop1950",
               "pop1960","pop1970","pop1980","pop1990")]<-
    cencounts[cencounts$fips==GEOIDnb,c("pop1900","pop1910","pop1920",
                                        "pop1930","pop1940","pop1950","pop1960",
                                        "pop1970","pop1980","pop1990")]
}

PopDEC2010<-Countydf%>%
  group_by(CSA) %>%
  summarize(pop1900=sum(pop1900,na.rm=TRUE),pop1910=sum(pop1910,na.rm=TRUE),
            pop1920=sum(pop1920,na.rm=TRUE),pop1930=sum(pop1930,na.rm=TRUE),
            pop1940=sum(pop1940,na.rm=TRUE),pop1950=sum(pop1950,na.rm=TRUE),
            pop1960=sum(pop1960,na.rm=TRUE),pop1970=sum(pop1970,na.rm=TRUE),
            pop1980=sum(pop1980,na.rm=TRUE),pop1990=sum(pop1990,na.rm=TRUE),
            pop2010=sum(pop2010,na.rm=TRUE),areaT=sum(ALAND,na.rm=TRUE)*3.86102e-7)
rm(i,sti,GEOID,GEOID00,GEOID10,GEOIDnb,StateFIPS,cencounts,County,Countydf,CSA,Dec2000Count,Dec2010Count)
