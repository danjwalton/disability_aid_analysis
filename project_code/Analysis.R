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
pop <- as.data.table(WDI("all","SP.POP.TOTL", start=2014, end=2018))

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
split.years$variable <- factor(split.years$variable, levels = c("Minor.share","Major.share","None.share","Minor","Major","None"))
split.chart <- ggplot(subset(split.years,variable %in% c("Minor.share","Major.share")),aes(Year,value,group=variable,color=NULL,fill=variable)) +
  geom_bar(position="stack", stat="identity",show.legend=T) +
  scale_fill_manual(values=DI_colours[c(1,3)],drop = TRUE,labels=c("Minor","Major"))+
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

#ID
total.id.years <- crs[, .(ID = sum(USD_Disbursement_Defl[relevance != "None" & intellectual == "intellectual"], na.rm=T), Other = sum(USD_Disbursement_Defl[relevance != "None" & intellectual != "intellectual"],na.rm=T), None = sum(USD_Disbursement_Defl[relevance == "None"], na.rm=T)), by=Year]
total.id.years[, (paste0(names(total.id.years)[names(total.id.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(total.id.years)[names(total.id.years) != "Year"]), by=Year]
total.id.years <- total.id.years[order(Year)]
ffwrite(total.id.years)

#P/S SPLIT ID
split.id.years <- dcast(crs[intellectual == "intellectual"], Year ~ relevance + intellectual, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
split.id.years[, (paste0(names(split.id.years)[names(split.id.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(split.id.years)[names(split.id.years) != "Year"]), by=Year]
ffwrite(split.id.years)

#SECTORS ID
sectors.id.years <- dcast(crs[intellectual == "intellectual"], SectorName + relevance ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
sectors.id.years[, c("SectorID", "SectorName") := tstrsplit(sectors.id.years$SectorName, "[.] ")]
sectors.id.years[is.na(SectorName)]$SectorName <- sectors.id.years[is.na(SectorName)]$SectorID
sectors.id.years$SectorTopLevel <- sectors.id.years$SectorName
sectors.id.years[grepl("^I[.]1[.]", SectorID)]$SectorTopLevel <- "Education"
sectors.id.years[grepl("^I[.]2[.]", SectorID)]$SectorTopLevel <- "Health"
sectors.id.years[grepl("^I[.]3[.]", SectorID)]$SectorTopLevel <- "Population Policies/Programmes & Reproductive Health"
sectors.id.years[grepl("^I[.]4[.]", SectorID)]$SectorTopLevel <- "Water Supply & Sanitation"
sectors.id.years[grepl("^I[.]5[.]", SectorID)]$SectorTopLevel <- "Government & Civil Society"
sectors.id.years[grepl("^I[.]6[.]", SectorID)]$SectorTopLevel <- "Other Social Infrastructure & Services"
ffwrite(sectors.id.years)

#DONORS ID
donors.id.years <- dcast(crs, Year + DonorName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
donors.id.years[, (paste0(names(donors.id.years)[!(names(donors.id.years) %in% c("Year", "DonorName"))], ".share")) := .SD/sum(.SD), .SDcols = (names(donors.id.years)[!(names(donors.id.years) %in% c("Year", "DonorName"))]), by=.(Year,DonorName)]
donors.id.years[,min.maj.share := sum(.SD), .SDcols = c("Minor.share","Major.share"), by=.(Year,DonorName)]
ffwrite(donors.id.years)

top.donors <- c("United Kingdom","United States","Germany","Sweden")
donors.id.years <- subset(donors.id.years,DonorName %in% top.donors)
donor.chart.minor <- ggplot(donors.id.years[,c("Year","DonorName","Minor")],aes(Year,Minor,group=DonorName,color=DonorName,fill=DonorName)) +
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
  ) + labs(y="Minor disability-relevant ODA (USDm)")
  
#RECIPIENTS ID
recipients.id.years <- dcast(crs, Year + RecipientName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
recipients.id.years <- merge(recipients.id.years, pop[,c("country", "SP.POP.TOTL", "year")], by.x=c("Year", "RecipientName"), by.y=c("year", "country"))
recipients.id.years <- recipients.id.years[, `:=` (minor.per.cap = Minor/SP.POP.TOTL), by=.(Year, RecipientName)]
ffwrite(recipients.id.years)

top.recipients=c("Tuvalu","Tonga","South Sudan","Vanuatu","Nauru")
recipients.id.years <- subset(recipients.id.years,RecipientName %in% top.recipients)

recip.chart.minor <- ggplot(recipients.id.years[,c("Year","RecipientName","minor.per.cap")],aes(Year,minor.per.cap,group=RecipientName,color=RecipientName,fill=RecipientName)) +
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
  ) + labs(y="Minor disability-relevant ODA receieved per capita (USDm)")



#ID SUBPURPOSE
crs$disability.subpurpose <- "Other"
crs[(employment == "employment" | education == "education" | family == "family" | advocacy == "advocacy")]$disability.subpurpose <- "Mixed"
crs[employment == "employment" & education != "education" & family != "family" & advocacy != "advocacy"]$disability.subpurpose <- "Employment"
crs[employment != "employment" & education == "education" & family != "family" & advocacy != "advocacy"]$disability.subpurpose <- "Education"
crs[employment != "employment" & education != "education" & family == "family" & advocacy != "advocacy"]$disability.subpurpose <- "Family"
crs[employment != "employment" & education != "education" & family != "family" & advocacy == "advocacy"]$disability.subpurpose <- "Self-advocacy"
subpurpose.id.years <- dcast(crs[relevance != "None" | Disability == "Principal disability component"], relevance + disability.subpurpose ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(subpurpose.id.years)

subpurpose.id.years <- melt(subpurpose.id.years,id.vars=c("relevance","disability.subpurpose"))
subpurpose.id.years <- subpurpose.id.years[,.(min.maj = sum(value)), by=.(variable,disability.subpurpose)]
subpurpose.id.chart <- ggplot(subpurpose.id.years,aes(variable,min.maj,group=disability.subpurpose,color=NULL,fill=disability.subpurpose)) +
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
  ) + labs(y="Disability-relevant ODA (USD millions)")

#ID GENDER
gender.id.years <- dcast(crs[relevance %in% c("Minor","Major")], Year ~ Gender, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
gender.id.years[, (paste0(names(gender.id.years)[names(gender.id.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(gender.id.years)[names(gender.id.years) != "Year"]), by=Year]
ffwrite(gender.id.years)
gender.id.years <- melt(gender.id.years,id.vars=c("Year"))
gender.id.years$variable <- factor(gender.id.years$variable, levels = c("Not screened.share","No gender component.share","Significant gender component.share","Principal gender component.share","Not screened","No gender component","Significant gender component","Principal gender component"))
gender.chart <- ggplot(subset(gender.id.years,variable %in% c("Principal gender component.share","Significant gender component.share","No gender component.share","Not screened.share")),aes(Year,value,group=variable,color=NULL,fill=variable)) +
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

#CHANNELS ID
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

dpos.id.years <- dcast(crs, dpo + ParentChannelName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
dpos.id.years <- melt(dpos.id.years[,!("None")],id.vars=c("dpo","ParentChannelName"))
dpos.id.years <- dpos.id.years[,.(min.maj = sum(value)), by=.(dpo,ParentChannelName)]
dpos.id.years$pie.class <- "Other channel"
dpos.id.years$pie.class[which(dpos.id.years$ParentChannelName=="Developing country-based NGO ")] <- "Developing country-based NGO"
dpos.id.years$pie.class[which(dpos.id.years$ParentChannelName=="Donor country-based NGO")] <- "Donor country-based NGO"  
dpos.id.years$pie.class[which(dpos.id.years$ParentChannelName=="INTERNATIONAL NGO")] <- "International NGO"
dpos.id.years$pie.class[which(dpos.id.years$dpo=="DPO")] <- "DPO"

all.channels.graphing <- dpos.id.years[,.(total = sum(min.maj)), by=.(pie.class)]
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

dpos.graphing <- subset(dpos.id.years,dpo=="DPO")[,.(total = sum(min.maj)), by=.(ParentChannelName)]
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
  "subpurpose.id.chart",
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