#### Setup ####

#danjwalton 2019 - UPDATED deanbreed 2021
required.packages <- c("data.table", "RJSONIO", "WDI", "openxlsx","ggplot2","Cairo")
lapply(required.packages, require, character.only = T)

if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/disability_aid_analysis/"
}else if(Sys.info()[["user"]] %in% c("dean-b") | Sys.info()[["user"]] %in% c("deanb")){
  wd <- "C:/git/disability_aid_analysis/"
}
setwd(wd)

# load_crs <- function(dataname="crs", path="project_data"){
#   require("data.table")
#   files.bz <- list.files(path, pattern=paste0(dataname, "_part.+[.]bz"))
#   files.csv <- list.files(path, pattern=paste0(dataname, "_part.+[.]csv"))
#   if(length(files.bz) > 0 & length(files.csv) > 0){
#     files <- files.csv
#     read.crs <- function(x){return(fread(x, encoding = 'UTF-8'))}
#   } else {
#     if(length(files.bz) > 0){
#       files <- files.bz
#       read.crs <- function(x){return(read.csv(x))}
#     } else {
#       files <- files.csv
#       read.crs <- function(x){return(fread(x))}
#     }
#   }
#   crs <- list()
#   for(i in 1:length(files)){
#     print(paste0("Loading part ", i, " of ", length(files)))
#     filepath <- paste0(path, "/", files[i])
#     crs[[i]] <- read.crs(filepath)
#   }
#   crs <- rbindlist(crs)
#   return(crs)
# }
# 
# crs <- load_crs(path="output")

crs <- read.csv("output/crs.csv")

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
# pop <- fread("project_data/pop.csv")

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

crs <- merge(crs,top.channel.codes,by.x="ParentChannelCode",by.y="Channel ID",all.x=T)
crs$ParentChannelName[which(crs$ParentChannelCode %in% c(61008:61009))] <- "Private sector in provider country"
crs$ParentChannelName[which(crs$ParentChannelCode %in% c(62009))] <- "Private sector in recipient country"
crs$ParentChannelName[which(crs$ParentChannelCode %in% c(63002,63009))] <- "Private sector in third country"
crs <- data.table(crs)

#### Overall Aid ####

# Overall

overall.years <- dcast(crs, RecipientName  ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(overall.years)

# Donor

donors.recip.years <- dcast(crs, DonorName + RecipientName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(donors.recip.years)

# Channel

channels.recip.years <- dcast(crs, ParentChannelName + RecipientName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(channels.recip.years)

#### Disability Aid ####

crs_sub_disability <- subset(crs,relevance %in% c("Principal","Significant"))

# Overall

disab.overall.years <- dcast(crs_sub_disability, RecipientName + relevance  ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(disab.overall.years)

# Donor

disab.donors.recip.years <- dcast(crs_sub_disability, DonorName + RecipientName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(disab.donors.recip.years)

# Channel

disab.channels.recip.years <- dcast(crs_sub_disability, ParentChannelName + RecipientName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(disab.channels.recip.years)

#### Psychosocial ####

crs_sub_psychosocial <- subset(crs,crs$psychosocial == "psychosocial")

# Overall

psychosocial.overall.years <- dcast(crs_sub_psychosocial, RecipientName  ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(psychosocial.overall.years)

# Donor

psychosocial.donors.recip.years <- dcast(crs_sub_psychosocial, DonorName + RecipientName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(psychosocial.donors.recip.years)

# Channel

psychosocial.channels.recip.years <- dcast(crs_sub_psychosocial, ParentChannelName + RecipientName ~ Year, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
ffwrite(psychosocial.channels.recip.years)

#### END ###