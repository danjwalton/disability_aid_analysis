#danjwalton 2019
required.packages <- c("data.table", "RJSONIO", "WDI", "readxl")
lapply(required.packages, require, character.only = T)

if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/disability_aid_analysis/"
}else if(Sys.info()[["user"]] %in% c("dean-b") | Sys.info()[["user"]] %in% c("deanb")){
  wd <- "C:/git/disability_aid_analysis/"
}
setwd(wd)

source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/load_and_join.R")
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

channel.codes <- read_excel("project_data/DAC-CRS-CODES.xls", sheet="Channel codes", skip=6)
top.channel.codes <- channel.codes[!duplicated(channel.codes$`Channel Parent Category`),][,c("Channel ID", "Full Name (English)")]
names(top.channel.codes) <- c("Channel ID", "ParentChannelName")

ffwrite <- function(x, path="output/"){
  if(!is.data.table(x))stop("Data is not a data.table object")
  dataname <- deparse(substitute(x))
  fwrite(x, paste0(path, dataname, ".csv"))
}

#P/S TOTAL DISABILITY
split.years <- dcast(crs, Year ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
split.years[, (paste0(names(split.years)[names(split.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(split.years)[names(split.years) != "Year"]), by=Year]
ffwrite(split.years)

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
donors.id.years <- dcast(crs, Year + DonorName ~ intellectual, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
donors.id.years[, (paste0(names(donors.id.years)[!(names(donors.id.years) %in% c("Year", "DonorName"))], ".share")) := .SD/sum(.SD), .SDcols = (names(donors.id.years)[!(names(donors.id.years) %in% c("Year", "DonorName"))]), by=Year]
ffwrite(donors.id.years)

#RECIPIENTS ID
recipients.id.years <- dcast(crs[intellectual == "intellectual"], Year + RecipientName ~ ., value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
recipients.id.years <- merge(recipients.id.years, pop[,c("country", "SP.POP.TOTL", "year")], by.x=c("Year", "RecipientName"), by.y=c("year", "country"))
recipients.id.years <- recipients.id.years[, .(intellectual = `.`, per.capita = `.`/SP.POP.TOTL), by=.(Year, RecipientName)]
ffwrite(recipients.id.years)

#ID SUBPURPOSE
crs$intellectual.subpurpose <- "Other"
crs[(employment == "employment" | education == "education" | family == "family")]$intellectual.subpurpose <- "Mixed"
crs[employment == "employment" & education != "education" & family != "family"]$intellectual.subpurpose <- "Employment"
crs[employment != "employment" & education == "education" & family != "family"]$intellectual.subpurpose <- "Education"
crs[employment != "employment" & education != "education" & family == "family"]$intellectual.subpurpose <- "Family"
subpurpose.id.years <- dcast(crs[relevance != "None" | intellectual == "intellectual"], intellectual + intellectual.subpurpose ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(subpurpose.id.years)

#ID GENDER
gender.id.years <- dcast(crs[intellectual == "intellectual"], Year ~ Gender, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
gender.id.years[, (paste0(names(gender.id.years)[names(gender.id.years) != "Year"], ".share")) := .SD/sum(.SD), .SDcols = (names(gender.id.years)[names(gender.id.years) != "Year"]), by=Year]
ffwrite(gender.id.years)

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

dpos.id.years <- dcast(crs[intellectual == "intellectual"], dpo + ParentChannelName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
channels.id.years <- dcast(crs[intellectual == "intellectual"], channel + ChannelName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))


