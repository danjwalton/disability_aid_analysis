#### Setup ####

#danjwalton 2019
required.packages <- c("data.table", "RJSONIO", "WDI", "openxlsx","ggplot2","Cairo")
lapply(required.packages, require, character.only = T)

if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/disability_aid_analysis/"
}else if(Sys.info()[["user"]] %in% c("dean-b") | Sys.info()[["user"]] %in% c("deanb")){
  wd <- "C:/git/disability_aid_analysis/"
}
setwd(wd)

load_crs <- function(dataname="crs", path="project_data"){
  require("data.table")
  files.bz <- list.files(path, pattern=paste0(dataname, "_part.+[.]bz"))
  files.csv <- list.files(path, pattern=paste0(dataname, "_part.+[.]csv"))
  if(length(files.bz) > 0 & length(files.csv) > 0){
    files <- files.csv
    read.crs <- function(x){return(fread(x, encoding = 'UTF-8'))}
  } else {
    if(length(files.bz) > 0){
      files <- files.bz
      read.crs <- function(x){return(read.csv(x))}
    } else {
      files <- files.csv
      read.crs <- function(x){return(fread(x))}
    }
  }
  crs <- list()
  for(i in 1:length(files)){
    print(paste0("Loading part ", i, " of ", length(files)))
    filepath <- paste0(path, "/", files[i])
    crs[[i]] <- read.crs(filepath)
  }
  crs <- rbindlist(crs)
  return(crs)
}



#source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/load_and_join.R")
crs <- load_crs(path="output")
dpos <- read.csv("project_data/dpos.csv", header=F, encoding = 'UTF-8')
i <- 0
while(i < 10){
  try({
    rm(pop)
    pop <- as.data.table(WDI(indicator = c("SP.POP.TOTL"), extra=T))
  }, silent=T)
  if(exists("pop")){
    if(all(c("SP.POP.TOTL") %in% names(pop))){
      fwrite(pop, "project_data/pop.csv")
      break
    }
  }
  i <- i + 1
  print(paste0("Error. Retrying... ",i,"/10"))
}
pop <- fread("project_data/pop.csv")

pop[country=="China"]$country <- "China (People's Republic of)"
pop[country=="Congo, Rep."]$country <- "Congo"
pop[country=="Cote d'Ivoire"]$country <- "Côte d'Ivoire"
pop[country=="Korea, Dem. Peopleâ€™s Rep."]$country <- "Democratic People's Republic of Korea"
pop[country=="Congo, Dem. Rep."]$country <- "Democratic Republic of the Congo"
pop[country=="Egypt, Arab Rep."]$country <- "Egypt"
pop[country=="Kyrgyz Republic"]$country <- "Kyrgyzstan"
pop[country=="Lao PDR"]$country <- "Lao People's Democratic Republic"
pop[country=="Micronesia, Fed. Sts."]$country <- "Micronesia"
pop[country=="Venezuela, RB"]$country <- "Venezuela"
pop[country=="Vietnam"]$country <- "Viet Nam"
pop[country=="West Bank and Gaza"]$country <- "West Bank and Gaza Strip"
pop[country=="Yemen, Rep."]$country <- "Yemen"

channel.codes <- read.xlsx("project_data/DAC-CRS-CODES.xlsx", sheet="Channel codes", startRow=7)
top.channel.codes <- channel.codes[!duplicated(channel.codes$`Channel.Parent.Category`),][,c("Channel.ID", "Full.Name.(English)")]
names(top.channel.codes) <- c("Channel ID", "ParentChannelName")

ffwrite <- function(x, path="output/"){
  if(!is.data.table(x))stop("Data is not a data.table object")
  dataname <- deparse(substitute(x))
  fwrite(x, paste0(path, dataname, ".csv"))
}

DI_colours <- c("#f8c1b2","#f0826d","#e84439","#bc2629","#8f1b13")

#### Analysis and chart production ####

# Disability-relevant Aid
split.years <- dcast(crs, Year ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
split.years[, (paste0(names(split.years)[names(split.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(split.years)[names(split.years) != "Year"]), by=Year]
ffwrite(split.years)

split.years <- melt(split.years,id.vars=c("Year"))
split.years$variable <- factor(split.years$variable, levels = c("Significant.share","Principal.share","None.share","Significant","Principal","None"))
split.chart <- ggplot(subset(split.years,variable %in% c("Significant.share","Principal.share")),aes(Year,value,group=variable,color=NULL,fill=variable)) +
  geom_bar(position="stack", stat="identity",show.legend=T) +
  scale_fill_manual(values=DI_colours[c(1,3)],drop = TRUE,labels=c("Significant","Principal"))+
  scale_y_continuous(labels = scales::percent_format(accuracy=0.5))+
  theme(
  legend.title=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  legend.position="top"
  ,legend.text = element_text(size=15,family="Averta Regular")
  ,legend.justification=c(0,0)
  ,legend.direction="vertical"
  ,axis.title.x=element_blank()
  ,axis.title.y=element_text(size=15, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
  ,axis.ticks=element_blank()
  ,axis.line.y = element_line(size = 1.1)
  ,axis.line.x = element_line(size = 1.1)
  ,axis.text.y = element_text(size=15)
  ,axis.text.x = element_text(size=20,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
  ,legend.background = element_rect(fill = "transparent", colour = "transparent")
  ,legend.key = element_blank()
) + labs(y="Percentage of total ODA")


#P/S SPLIT
split.years <- dcast(crs, Year ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
split.years[, (paste0(names(split.years)[names(split.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(split.years)[names(split.years) != "Year"]), by=Year]
ffwrite(split.years)

#Marker comparison
marker.split <- setnames(dcast(crs[Year==2018 & Disability %in% c("Principal disability component", "Significant disability component")], Disability ~ ., value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T)), c("relevance", "marker"))
di.split <- setnames(dcast(crs[Year==2018 & relevance != "None"], relevance ~ ., value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T)), c("relevance", "DI"))
comparison <- cbind(marker.split, di.split[,2])
ffwrite(comparison)

#Overlap
overlap <- dcast(crs[Year == 2018, .(dac=ifelse(Disability %in% c("Principal disability component", "Significant disability component"), "DAC-marked", "Not DAC-marked"), di=ifelse(relevance != "None", "DI-marked", "Not DI-marked"), USD_Disbursement_Defl=USD_Disbursement_Defl)], di ~ dac, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(overlap)

overlap.inclusion <- dcast(crs[Year == 2018 & inclusion == "inclusion", .(dac=ifelse(Disability %in% c("Principal disability component", "Significant disability component"), "DAC-marked", "Not DAC-marked"), di=ifelse(relevance != "None", "DI-marked", "Not DI-marked"), USD_Disbursement_Defl=USD_Disbursement_Defl)], di ~ dac, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(overlap.inclusion)

#Inclusion
inclusion.years <- dcast(crs[relevance != "None"], Year ~ inclusion, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(inclusion.years)

marker.inclusion <- setnames(dcast(crs[Year==2018 & Disability %in% c("Principal disability component", "Significant disability component")], inclusion ~ ., value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T)), c("inclusion", "marker"))
di.inclusion <- setnames(dcast(crs[Year == 2018 & relevance != "None"], inclusion ~., value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T)), c("inclusion", "DI"))
inclusion.compare <- cbind(marker.inclusion, di.inclusion[,2])
ffwrite(inclusion.compare)

#SECTORS
sectors.years <- dcast(crs, SectorName + relevance ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
sectors.years[, c("SectorID", "SectorName") := tstrsplit(sectors.years$SectorName, "[.] ")]
sectors.years[is.na(SectorName)]$SectorName <- sectors.years[is.na(SectorName)]$SectorID
sectors.years$SectorTopLevel <- sectors.years$SectorName
sectors.years[grepl("^I[.]1[.]", SectorID)]$SectorTopLevel <- "Education"
sectors.years[grepl("^I[.]2[.]", SectorID)]$SectorTopLevel <- "Health"
sectors.years[grepl("^I[.]3[.]", SectorID)]$SectorTopLevel <- "Population Policies/Programmes & Reproductive Health"
sectors.years[grepl("^I[.]4[.]", SectorID)]$SectorTopLevel <- "Water Supply & Sanitation"
sectors.years[grepl("^I[.]5[.]", SectorID)]$SectorTopLevel <- "Government & Civil Society"
sectors.years[grepl("^I[.]6[.]", SectorID)]$SectorTopLevel <- "Other Social Infrastructure & Services"
ffwrite(sectors.years)

#SECTORS INCLUSION
sectors.inclusion <- dcast(crs[inclusion == "inclusion"], SectorName + relevance ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
sectors.inclusion[, c("SectorID", "SectorName") := tstrsplit(sectors.inclusion$SectorName, "[.] ")]
sectors.inclusion[is.na(SectorName)]$SectorName <- sectors.inclusion[is.na(SectorName)]$SectorID
sectors.inclusion$SectorTopLevel <- sectors.inclusion$SectorName
sectors.inclusion[grepl("^I[.]1[.]", SectorID)]$SectorTopLevel <- "Education"
sectors.inclusion[grepl("^I[.]2[.]", SectorID)]$SectorTopLevel <- "Health"
sectors.inclusion[grepl("^I[.]3[.]", SectorID)]$SectorTopLevel <- "Population Policies/Programmes & Reproductive Health"
sectors.inclusion[grepl("^I[.]4[.]", SectorID)]$SectorTopLevel <- "Water Supply & Sanitation"
sectors.inclusion[grepl("^I[.]5[.]", SectorID)]$SectorTopLevel <- "Government & Civil Society"
sectors.inclusion[grepl("^I[.]6[.]", SectorID)]$SectorTopLevel <- "Other Social Infrastructure & Services"
ffwrite(sectors.inclusion)

#DONORS
donors.years <- dcast(crs, Year + DonorName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
donors.years[, (paste0(names(donors.years)[!(names(donors.years) %in% c("Year", "DonorName"))], ".share")) := .SD/sum(.SD), .SDcols = (names(donors.years)[!(names(donors.years) %in% c("Year", "DonorName"))]), by=.(Year,DonorName)]
donors.years[,min.maj.share := sum(.SD), .SDcols = c("Significant.share","Principal.share"), by=.(Year,DonorName)]
ffwrite(donors.years)

#DONORS INCLUSION
donors.inclusion <- dcast(crs, Year + DonorName ~ relevance + inclusion, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
donors.inclusion[, (paste0(names(donors.inclusion)[!(names(donors.inclusion) %in% c("Year", "DonorName"))], ".share")) := .SD/sum(.SD), .SDcols = (names(donors.inclusion)[!(names(donors.inclusion) %in% c("Year", "DonorName"))]), by=.(Year,DonorName)]
donors.inclusion[,inclusion.share := sum(.SD), .SDcols = c("Significant_inclusion.share","Principal_inclusion.share"), by=.(Year,DonorName)]
ffwrite(donors.inclusion)

#DONOR COUNTRIES INCLUSION
donorcountries.inclusion <- dcast(crs, Year + DonorName ~ relevance + inclusion, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
donorcountries.inclusion[, (paste0(names(donorcountries.inclusion)[!(names(donorcountries.inclusion) %in% c("Year", "DonorName"))], ".share")) := .SD/sum(.SD), .SDcols = (names(donorcountries.inclusion)[!(names(donorcountries.inclusion) %in% c("Year", "DonorName"))]), by=.(Year,DonorName)]
donorcountries.inclusion[,inclusion.share := sum(.SD), .SDcols = c("Significant_inclusion.share","Principal_inclusion.share"), by=.(Year,DonorName)]
ffwrite(donorcountries.inclusion)

top.donors <- c("United Kingdom","United States","Germany","Sweden")
donors.years <- subset(donors.years,DonorName %in% top.donors)
donor.chart.significant <- ggplot(donors.years[,c("Year","DonorName","Significant")],aes(Year,Significant,group=DonorName,color=DonorName,fill=DonorName)) +
  geom_line(stat="identity",show.legend=T,size=2) +
  scale_color_manual(values=DI_colours[c(1:4)],drop = TRUE)+
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top"
    ,legend.text = element_text(size=15,family="Averta Regular")
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_blank()
    ,axis.title.y=element_text(size=15, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
    ,axis.ticks=element_blank()
    ,axis.line.y = element_line(size = 1.1)
    ,axis.line.x = element_line(size = 1.1)
    ,axis.text.y = element_text(size=15)
    ,axis.text.x = element_text(size=20,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_blank()
  ) + labs(y="Significant disability-relevant ODA (USDm)")
  
#RECIPIENTS
recipients.years <- dcast(crs, Year + RecipientName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
recipients.years <- merge(recipients.years, pop[,c("country", "SP.POP.TOTL", "year")], by.x=c("Year", "RecipientName"), by.y=c("year", "country"))
recipients.years <- recipients.years[, `:=` (per.cap = (Principal + Significant)/(SP.POP.TOTL/1000000)), by=.(Year, RecipientName)]
ffwrite(recipients.years)

#RECIPIENTS INCLUSION
recipients.inclusion <- dcast(crs, Year + RecipientName ~ relevance + inclusion, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
recipients.inclusion[, (paste0(names(recipients.inclusion)[!(names(recipients.inclusion) %in% c("Year", "RecipientName"))], ".share")) := .SD/sum(.SD), .SDcols = (names(recipients.inclusion)[!(names(recipients.inclusion) %in% c("Year", "RecipientName"))]), by=.(Year, RecipientName)]
recipients.inclusion[,inclusion.share := sum(.SD), .SDcols = c("Significant_inclusion.share","Principal_inclusion.share"), by=.(Year,RecipientName)]
recipients.inclusion <- merge(recipients.inclusion, pop[,c("country", "SP.POP.TOTL", "year")], by.x=c("Year", "RecipientName"), by.y=c("year", "country"))
recipients.inclusion <- recipients.inclusion[, `:=` (per.cap = (Principal_inclusion + Significant_inclusion)/(SP.POP.TOTL/1000000)), by=.(Year, RecipientName)]
ffwrite(recipients.inclusion)

top.recipients=c("Tuvalu","Tonga","South Sudan","Vanuatu","Nauru")
recipients.years <- subset(recipients.years,RecipientName %in% top.recipients)

recip.chart.significant <- ggplot(recipients.years[,c("Year","RecipientName","significant.per.cap")],aes(Year,significant.per.cap,group=RecipientName,color=RecipientName,fill=RecipientName)) +
  geom_line(stat="identity",show.legend=T,size=2) +
  scale_color_manual(values=DI_colours,drop = TRUE)+
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top"
    ,legend.text = element_text(size=15,family="Averta Regular")
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_blank()
    ,axis.title.y=element_text(size=15, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
    ,axis.ticks=element_blank()
    ,axis.line.y = element_line(size = 1.1)
    ,axis.line.x = element_line(size = 1.1)
    ,axis.text.y = element_text(size=15)
    ,axis.text.x = element_text(size=20,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_blank()
  ) + labs(y="Significant disability-relevant ODA receieved per capita (USDm)")

#SUBPURPOSE
crs$disability.subpurpose <- "Other"
crs[(employment == "employment" | education == "education" | family == "family" | advocacy == "advocacy")]$disability.subpurpose <- "Mixed"
crs[employment == "employment" & education != "education" & family != "family" & advocacy != "advocacy"]$disability.subpurpose <- "Employment"
crs[employment != "employment" & education == "education" & family != "family" & advocacy != "advocacy"]$disability.subpurpose <- "Education"
crs[employment != "employment" & education != "education" & family == "family" & advocacy != "advocacy"]$disability.subpurpose <- "Family"
crs[employment != "employment" & education != "education" & family != "family" & advocacy == "advocacy"]$disability.subpurpose <- "Self-advocacy"
subpurpose.years <- dcast(crs[relevance != "None"], relevance + disability.subpurpose ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(subpurpose.years)

#SUBPURPOSE INCLUSION
crs$disability.subpurpose <- "Other"
crs[(employment == "employment" | education == "education" | family == "family" | advocacy == "advocacy")]$disability.subpurpose <- "Mixed"
crs[employment == "employment" & education != "education" & family != "family" & advocacy != "advocacy"]$disability.subpurpose <- "Employment"
crs[employment != "employment" & education == "education" & family != "family" & advocacy != "advocacy"]$disability.subpurpose <- "Education"
crs[employment != "employment" & education != "education" & family == "family" & advocacy != "advocacy"]$disability.subpurpose <- "Family"
crs[employment != "employment" & education != "education" & family != "family" & advocacy == "advocacy"]$disability.subpurpose <- "Self-advocacy"
subpurpose.inclusion <- dcast(crs[relevance != "None" & inclusion == "inclusion"], relevance + disability.subpurpose ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(subpurpose.inclusion)

crs$disability.concsub <- gsub("^, |, $", "", gsub("(, )+",", ",paste(gsub("Not.*", "", crs$employment), gsub("Not.*", "", crs$education), gsub("Not.*", "", crs$family), gsub("Not.*", "", crs$advocacy), sep=", ")))
concsub.inclusion <- dcast(crs[relevance != "None" & inclusion == "inclusion"], disability.concsub ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(concsub.inclusion)

subpurpose.years <- melt(subpurpose.years,id.vars=c("relevance","disability.subpurpose"))
subpurpose.years <- subpurpose.years[,.(min.maj = sum(value)), by=.(variable,disability.subpurpose)]
subpurpose.chart <- ggplot(subpurpose.years,aes(variable,min.maj,group=disability.subpurpose,color=NULL,fill=disability.subpurpose)) +
  geom_bar(position="stack", stat="identity",show.legend=T) +
  scale_fill_manual(values=c(DI_colours,"#6b120a"),drop = TRUE)+
  scale_y_continuous(labels = scales::number_format(accuracy=100))+
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top"
    ,legend.text = element_text(size=15,family="Averta Regular")
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_blank()
    ,axis.title.y=element_text(size=15, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
    ,axis.ticks=element_blank()
    ,axis.line.y = element_line(size = 1.1)
    ,axis.line.x = element_line(size = 1.1)
    ,axis.text.y = element_text(size=15)
    ,axis.text.x = element_text(size=20,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_blank()
  ) + labs(y="Disability-inclusive ODA (USD millions)")

#GENDER
gender.years <- dcast(crs[relevance %in% c("Significant","Principal")], Year ~ Gender, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
gender.years[, (paste0(names(gender.years)[names(gender.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(gender.years)[names(gender.years) != "Year"]), by=Year]
ffwrite(gender.years)

#GENDER INCLUSION
gender.inclusion <- dcast(crs[relevance %in% c("Significant","Principal") & inclusion == "inclusion"], Year ~ Gender, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
gender.inclusion[, (paste0(names(gender.inclusion)[names(gender.inclusion) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(gender.inclusion)[names(gender.inclusion) != "Year"]), by=Year]
ffwrite(gender.inclusion)

gender.years <- melt(gender.years,id.vars=c("Year"))
gender.years$variable <- factor(gender.years$variable, levels = c("Not screened.share","No gender component.share","Significant gender component.share","Principal gender component.share","Not screened","No gender component","Significant gender component","Principal gender component"))
gender.chart <- ggplot(subset(gender.years,variable %in% c("Principal gender component.share","Significant gender component.share","No gender component.share","Not screened.share")),aes(Year,value,group=variable,color=NULL,fill=variable)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=DI_colours[c(1:4)],drop = TRUE,labels=c("Not screened","No gender component","Significant gender component","Principal gender component"))+
scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top"
    ,legend.text = element_text(size=15,family="Averta Regular")
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_blank()
    ,axis.title.y=element_text(size=15, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
    ,axis.ticks=element_blank()
    ,axis.line.y = element_line(size = 1.1)
    ,axis.line.x = element_line(size = 1.1)
    ,axis.text.y = element_text(size=15)
    ,axis.text.x = element_text(size=20,margin=margin(t=20,r=0,b=0,l=0),family="Averta Regular")
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_blank()
  ) + labs(y="Percentage of disability spending")

#CHANNELS
firstCap <- function(y) {
  sapply(y, function(x) {
    x <- as.character(x)
    s <- strsplit(x, " ")[[1]]
    paste(sapply(s, function(t) {
      ifelse(toupper(t) == t, t, paste0(toupper(substring(tolower(t), 1,1)), substring(tolower(t), 2)))
    }
    ), collapse=" ")
  }
  )
}
crs$channel <- firstCap(crs$ChannelReportedName)
crs$dpo <- "Other channel"
crs[channel %in% dpos$V1]$dpo <- "DPO"
crs$ParentChannelCode <- floor(crs$ParentChannelCode/1000)*1000
crs$TopChannelCode <- floor(crs$ParentChannelCode/10000)*10000
crs <- merge(crs, top.channel.codes, by.x="ParentChannelCode", by.y="Channel ID")

channels.inclusion <- dcast(crs, ParentChannelName ~ inclusion, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(channels.inclusion)

dpos.years <- dcast(crs, dpo + ParentChannelName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
dpos.years <- melt(dpos.years[,!("None")],id.vars=c("dpo","ParentChannelName"))
dpos.years <- dpos.years[,.(min.maj = sum(value)), by=.(dpo,ParentChannelName)]
dpos.years$pie.class <- "Other channel"
dpos.years$pie.class[which(dpos.years$ParentChannelName=="Developing country-based NGO ")] <- "Developing country-based NGO"
dpos.years$pie.class[which(dpos.years$ParentChannelName=="Donor country-based NGO")] <- "Donor country-based NGO"  
dpos.years$pie.class[which(dpos.years$ParentChannelName=="INTERNATIONAL NGO")] <- "International NGO"
dpos.years$pie.class[which(dpos.years$dpo=="DPO")] <- "DPO"

all.channels.graphing <- dpos.years[,.(total = sum(min.maj)), by=.(pie.class)]
all.channels.graphing$percentages <- paste0(round(all.channels.graphing$total / sum(all.channels.graphing$total) * 100, 1), "%")

all.channels.chart <- ggplot(all.channels.graphing,aes(x="",total,group=pie.class,color=NULL,fill=pie.class)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(data=all.channels.graphing,aes(x="",total,label=percentages),size = 5,position = position_stack(vjust=0.5))+
  scale_fill_manual(values=DI_colours[c(1:5)],drop = TRUE)+
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top"
    ,legend.text = element_text(size=15,family="Averta Regular")
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_blank()
    ,axis.title.y=element_text(size=30, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
    ,axis.ticks=element_blank()
    ,axis.text.x = element_blank()
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_blank()
  )+coord_polar("y")

dpos.graphing <- subset(dpos.years,dpo=="DPO")[,.(total = sum(min.maj)), by=.(ParentChannelName)]
dpos.graphing <- dpos.graphing[c(1:3),]
dpos.graphing$percentages <- paste0(round(dpos.graphing$total / sum(all.channels.graphing$total) *100 , 2), "%")
dpos.graphing$ParentChannelName[which(dpos.graphing$ParentChannelName=="INTERNATIONAL NGO")] <- "International NGO"

dpos.chart <- ggplot(dpos.graphing,aes(x="",total,group=ParentChannelName,color=NULL,fill=ParentChannelName)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(data=dpos.graphing,aes(x="",total,label=percentages),size = 5,position = position_stack(vjust=0.5))+
  scale_fill_manual(values=c("#ebcfe5","#cb98c4","#994d98"),drop = TRUE)+
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position="top"
    ,legend.text = element_text(size=15,family="Averta Regular")
    ,legend.justification=c(0,0)
    ,legend.direction="vertical"
    ,axis.title.x=element_blank()
    ,axis.title.y=element_text(size=30, angle=90, margin=margin(t=0,r=10,b=0,l=0), hjust=1,family="Averta Regular")
    ,axis.ticks=element_blank()
    ,axis.text.x = element_blank()
    ,legend.background = element_rect(fill = "transparent", colour = "transparent")
    ,legend.key = element_blank()
  )+coord_polar("y")

#### Chart printing ####

chart.list = c("split.chart",
  "all.channels.chart",
  "dpos.chart",
  "subpurpose.chart",
  "donor.chart.minor",
  "recip.chart.minor",
  "gender.chart"
)

res_def = c(1000,500,500,1000,1000,1000,1000)

setwd("C:/git/disability_aid_analysis/output/charts")

for(i in 1:length(chart.list)){
  chart.name = chart.list[i]
  chart.width = res_def[i]
    chart = get(chart.name)
    if(!is.null(chart)){
      Cairo(family="Averta Regular",file=paste0(chart.name,".png"),width=chart.width,height=700,units="px",bg="transparent")
      tryCatch({print(chart)},error=function(e){})
      dev.off()
    }
}