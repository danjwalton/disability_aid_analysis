#deanbreed 2021

required.packages <- c("data.table", "anytime", "ggplot2", "scales", "bsts", "dplyr", "plyr","Hmisc","reshape2","splitstackshape","Cairo","svglite","extrafont","jsonlite","countrycode","openxlsx","english","stringr","tidyr", "RJSONIO")
lapply(required.packages, require, character.only = T)

if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/disability_aid_analysis/"
  wd2 <- "G:/My Drive/Work/GitHub/covid-tracking/input"
}else if(Sys.info()[["user"]] %in% c("dean-b") | Sys.info()[["user"]] %in% c("deanb")){
  wd <- "C:/git/disability_aid_analysis/"
  wd2 <- "C:/git/covid-tracking/input"
}
setwd(wd2)

all <- read.csv("donors_selected.csv")[,c("country","code","org_type","disbursements","commitments")]

choice <- "disbursements"
current_month <- 3 # Note: This is the less than or equal to so if you want to include up to November, for example, this must say 11.
current_year <- 2021
current_yyyymm <- 202102

if (choice == "commitments"){
  dat <- read.csv("Trends in IATI - Commitments May 24.csv")
}
if (choice == "disbursements"){
  dat <- read.csv("Trends in IATI - Disbursements May 24.csv")
}

meta_columns <- read.csv("meta_columns.csv")

names(dat) <- gsub("...", " - ", names(dat), fixed = TRUE)
names(dat) <- gsub(".", " ", names(dat), fixed = TRUE)
dictionary <- merge(data.frame(names(dat)),meta_columns[,c("col_alias","col_name")],by.x="names.dat.",by.y="col_alias",all.x=T)
dictionary$col_name[which(is.na(dictionary$col_name))] <- dictionary$names.dat.[which(is.na(dictionary$col_name))]
names(dat) <- mapvalues(names(dat), from=dictionary$names.dat.,to=dictionary$col_name)

filtered_iati <- dat
rm(dat)

agg_oda_filtered <- subset(filtered_iati, reporting_org_ref %in% all$code)
rm(filtered_iati)

assign(choice,agg_oda_filtered)
rm(agg_oda_filtered)

#### Further filter of dataset ####

t = get(choice)
t$year <- t$x_transaction_year
t$month <- as.numeric(substr(t$x_yyyymm,5,6))
t <- subset(t,x_yyyymm <= current_yyyymm)
t <- merge(t,unique(all),by.x="reporting_org_ref",by.y="code")

t <- subset(t,t[[choice]]==1)

t <- subset(t,t$x_finance_type != "GNI: Gross National Income")
t <- subset(t,t$x_finance_type != 1)
t <- subset(t,t$x_finance_type != "Guarantees/insurance")
t <- subset(t,t$x_finance_type != 1100)

# Note: GNI is a DAC artefact so needs removal and Guarantees/insurance is conditional.

# Flow and Finance type sorting

t$x_flow_type_code[is.na(t$x_flow_type_code)] <- "Not specified"
t$x_flow_type_code[t$x_flow_type_code=="10"] <- "ODA"
t$x_flow_type_code[t$x_flow_type_code=="20"] <- "OOF"
t$x_flow_type_code[t$x_flow_type_code=="21"] <- "Non-export credit OOF"
t$x_flow_type_code[t$x_flow_type_code=="30"] <- "Private Development Finance"
t$x_flow_type_code[t$x_flow_type_code=="35"] <- "Private Market"
t$x_flow_type_code[t$x_flow_type_code=="40"] <- "Non flow"
t$x_flow_type_code[t$x_flow_type_code=="50"] <- "Other flows"

t$flow_type <- t$x_flow_type_code
t$flow_type[which(t$x_flow_type_code %in% c("OOF","Non-export credit OOF"))] <- "OOF"
t$flow_type[which(t$x_flow_type_code %in% c("Private Development Finance","Private Market","Non flow","Other flows"))] <- "Other flows"

t <- t[as.character(t$x_transaction_year) >= 2015,]

memory.limit(1000000000)
markers <- t
names(markers) = gsub("_",".",names(markers))
original_names = names(markers)
markers.split <- cSplit(markers,c("DAC Policy Marker Code","DAC Policy Marker Significance"),sep="|",type.convert="as.character")
markers <- NULL
t <- NULL
hold <- grep("DAC Policy Marker Code",names(markers.split))
test <- data.table(markers.split)[,..hold]
test <- data.table(test==1)
names(test) <- paste0("gender_search_",c(1:ncol(test)))
markers.split <- data.table(cbind(markers.split,test))
for (i in c(1:24)){
  column_name_1 <- paste0("gender_search_",i)
  if(i<10){
  column_name_2 <- paste0("DAC Policy Marker Significance_0",i)
  } else {column_name_2 <- paste0("DAC Policy Marker Significance_",i)}
  markers.split[,..column_name_2] <- as.numeric(unlist(as.vector(data.frame(markers.split[,..column_name_2]))))
  markers.split[,..column_name_1] <- markers.split[,..column_name_1]*markers.split[,..column_name_2]
}

# new_names = setdiff(names(markers.split),original_names)
markers.split.wide = reshape(markers.split, varying=new_names, direction="wide", sep="_")
markers.split.long = markers.split.long[!is.na(`DAC Policy Marker Code`),]
markers.split.long = markers.split.long[!duplicated(markers.split.long[,c("DAC Policy Marker Code","id")]),]

id_hold <- dcast(markers.split.long, id~`DAC Policy Marker Code`,value.var="DAC Policy Marker Significance")

markers$id = 1:nrow(markers)
t = merge(markers,id_hold,by="id")
names(t) = gsub(".","_",names(t),fixed=T)

t <- t[which(!is.na(t$org_type)),]
t.hold <- t
t <- data.table(t)[x_recipient_number==1 & x_sector_number==1]

t$x_original_transaction_value_usd <- t$x_original_transaction_value_usd/1000000
t.hold$x_transaction_value_usd <- t.hold$x_transaction_value_usd/1000000
t.hold$x_recipient_transaction_value_usd <- t.hold$x_recipient_transaction_value_usd/1000000

#### Potential filtering ####

keep <- c(
  "CrsID"
  ,
  "ProjectNumber"
  ,
  "Year"
  ,
  "Aid_t"
  ,
  "FlowName"
  ,
  "DonorName"
  ,
  "RecipientName"
  ,
  "USD_Commitment_Defl"
  ,
  "USD_Disbursement_Defl"
  ,
  "SectorName"
  ,
  "PurposeName"
  ,
  "ProjectTitle"
  ,
  "ShortDescription"
  ,
  "LongDescription"
  ,
  "Gender"
  ,
  "Disability"
  ,
  "ChannelReportedName"
  ,
  "ChannelName"
  ,
  "ChannelCode"
  ,
  "ParentChannelCode"
)

keep_iati <- c(
  "ï  IATI Identifier"
  ,
  #"ProjectNumber"
  #,
  "x_transaction_year"
  ,
  #"Aid_t"
  #,
  #"FlowName"
  #,
  "reporting_org_narrative"
  ,
  "recipient_name"
  ,
  #"USD_Commitment_Defl"
  #,
  "x_original_transaction_value_usd"
  ,
  "x_recipient_transaction_value_usd"
  ,
  "x_transaction_value_usd"
  ,
  "x_dac3_sector"
  ,
  #PurposeName"
  #,
  "title_narrative"
  ,
  "description_narrative"
  ,
  "transaction_description_narrative"
  ,
  #"Gender"
  #,
  #"Disability"
  #,
  "ChannelReportedName"
  ,
  "ChannelName"
  ,
  "ChannelCode"
  ,
  "ParentChannelCode"
)

crs <- crs[, ..keep]
crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
]

#### Keywords ####

principal.keywords <- c(
  "disab", "discapaci", "incapaci", "minusvÃ¡lido", "invalidit", "infirmitÃ©", "d-isab"
  #,
  #"disorder"
  ,
  "handicap"
  ,
  "impairment", "impaired"
  ,
  "pwd", "gwd", "cwd"
  ,
  "chronic health", "chronic ill", "maladie chronique", "enfermedad crÃ³nica"
  ,
  "deaf", "sordo", "sourd"
  ,
  "blind", "ciego", "aveugle", "eye health"
  ,
  "with special needs", "con necesidades especiales", "besoins spÃ©ciau", "besoins spÃ©cifiques", "special needs education", "disabilities and special needs"
  ,
  "autistic", "autism", "autist"
  ,
  "mental health", "santÃ© mentale", "salud mental"
  ,
  "prosthe", "prosthÃ¨", "prÃ³tesi"
  ,
  "mobility device", "dispositivo de movilidad", "dispositif de mobilitÃ©"
  ,
  "wheelchair", "fauteuil roulant", "silla de ruedas"
  ,
  "plegia", "paralys"
  ,
  "hearing aid", "audÃ­fono", "dispositif d'Ã©coute pour malentendant"
  ,
  "amputation", "amputee", "amputÃ©", "amputa"
  ,
  "schizophreni", "esquizofrenia", "schizophrÃ©nie"
  ,
  "bipolar"
  ,
  "leprosy"
  ,
  "sign language", "langage des signes", "lenguaje de seÃ±as"
  ,
  "arthriti", "artritis", "arthrite"
  ,
  "rheumat", "rhumat", "reumat"
  ,
  "dementia", "dÃ©mence", "demencia"
  ,
  "spina "
  ,
  "hydrocephalus", "hidrocefalia", "l'hydrocÃ©phalie"
  ,
  "diabetes", "diabÃ¨te"
  ,
  "atlas alliance", "atlas allinance", "abilis foundation", "zapdd"
  #,
  #"dpos ", "dpo ", "dpo's", "dpos[.]", "dpo[.]", "dpo's[.]"
  ,
  "special education", "educaciÃ³n especial", "Ã©ducation spÃ©ciale", "special needs education", "special need education"
  ,
  "learning difficult", "learning disa", "difficultÃ©s d'apprentissage", "dificultades de aprendizaje", "discapacidad de aprendizaje", "trouble d'apprentissage", "learning problem"
  ,
  "trisomy.{0,1}21", "trisomie.{0,1}21", "trisomÃ­a.{0,1}21"
  ,
  "down syndrom", "syndrome de down", "sÃ­ndrome de down"
  ,
  "cerebral",	"cÃ©rÃ©brale"
  ,
  "crpd"
  ,
  "psycho.{0,1}social disab", "disability and psycho.{0,1}social", "discapacidad psicosocial", "handicap psychosocial", "discapacidad y psicosocial", "handicap et psychosocial"
  ,
  "cognitive dis", "discapacidad cognitiva", "dÃ©ficience cognitive", "cognitive defici", "cognitive delay", "delayed cognitive"
  ,
  "fetal alcohol syndrome"
  ,
  "developmental delay"
  ,
  "pmld"
  ,
  "neuro.{0,1}development"
  ,
  "neuro.{0,1}diverse"
  ,
  "sclerosis", "sclÃ©rose"
  ,
  "albinism", "albino"
  ,
  "assistive technology", "assistive devices", "tecnologÃ­a de asistencia", "la technologie d'assistance", "dispositifs d'assistance", "dispositivos de ayuda"
  ,
  "reasonable accommodation", "acomodaciÃ³n razonable", "acomodaciones razonables", "amÃ©nagements raisonnables", "accommodement raisonnable"
  ,
  "inclusive education","Ã©ducation inclusive","educaciÃ³n inclusive","accessibility","accesibilidad","accessibilitÃ©"
  ,
  "workplace accommodations", "amÃ©nagements en milieu de travail", "alojamiento en el lugar de trabajo"
  ,
  "psychosocial di$"
  ,
  "people with limited capacit", "personnes avec une capacitÃ©.{0,1} limitÃ©e", "personas con capacidad.{0,2} limitada"
)


significant.keywords <- c(
  "vulnerable group", "vulnerable people", "vulnerable population", "vulnerable individual", "vulnerable girl", "vulnerable women", "vulnerable boy", "vulnerable men", "vulnerable refugee", "who are vulnerable", "which are vulnerable", "vulnerable child"
  ,
  #"significantity group", "significantity population", "significantities"
  #,
  "marginali.ed group", "marginali.ed people", "marginali.ed population", "marginali.ed individual", "marginali.ed girl", "marginali.ed women", "marginali.ed boy", "marginali.ed men", "marginali.ed refugee", "who are marginali.ed", "which are marginali.ed", "marginali.ed child", "marginali.ed and young"
  ,
  "war victim", "victimas de guerra", "victimes de guerre", "victim. of war"
  ,
  #"conflict affected", "conflict-affected", "conflict victim", "victim. of conflict"
  #,
  "landmine victim", "victime de mine", "vÃ­ctima de minas terrestres", "landmine survivor", "sobreviviente de minas terrestres", "survivant d'une mine",
  #,
  #"wounded"
  #,
  #"injured", "injuries"
  #,
  #"physiotherapy", "fisioterapia"
  "inclusive education","Ã©ducation inclusive", "educaciÃ³n inclusiva"
  ,
  "inclusive employment", "empleo inclusivo", "emploi inclusif"
)

disqualifying.keywords <- c(
  "ciego de avila"
  ,
  "chronic malnutrition"
  ,
  "mole rat"
  ,
  "cgpwd"
  ,
  "cpwd"
  ,
  "rvcwda"
  ,
  "pwdtc"
  ,
  " road"
  ,
  "highway"
  ,
  "environmental health"
  ,
  "environmental condition"
  ,
  "rehydration therapy"
  #,
  #"-dpo", "cidpo", "hdpo", "dpo series", "financial sector dpo", "dpo (ri)", "management dpo", "poverty dpo", "growth dpo", "support dpo", "system dpo", "programmatic dpo"
  ,
  "fiscal"
  ,
  "growth and compet"
  ,
  "combination therap"
  ,
  "emergency dpo"
  ,
  "conventional weapons", "weapons destruction"
  ,
  "fairtradeafrica"
  ,
  "blindness prevention", "avoidable blindness"
  ,
  "bautista"
  ,
  "unyago"
  ,
  "neurotoxin"
)

significant.disqualifying.keywords <- c(
  "HIV"
  ,
  "AIDS"
)

disqualifying.sectors <- c(
  "15111"
  ,
  "15114"
  #,
  #"32210"
  ,
  "12181"
  ,
  "12182"
)

channel.keywords <- c(
  "abilis foundation"
  ,
  "disab"
  ,
  "atlas alliance", "atlas-alliansen"
  ,
  "handicap international"
)

inclusion.keywords <- c(
  "inclus"
  ,
  "empower", "habiliter", "autorizar"
  ,
  "rights", "droits", "derechos"
  ,
  "advocacy", "plaidoyer", "abogacÃ­a"
  ,
  "self-representative", "auto-reprÃ©sentant",	"auto-representante"
  ,
  "autonomy", "autonomie", "autonomÃ­a"
  ,
  #"community", "communatÃ©"
  #,
  "integration", "intÃ©gration", "integraciÃ³n"
  ,
  "autogestores"
  ,
  "accessiblity", "accessibilitÃ©", "accesibilidad"
  ,
  "assistive technology", "assistive devices", "tecnologÃ­a de asistencia", "la technologie d'assistance", "dispositifs d'assistance", "dispositivos de ayuda"
  ,
  "reasonable accommodation", "acomodaciÃ³n razonable", "acomodaciones razonables", "amÃ©nagements raisonnables", "accommodement raisonnable"
  ,
  "rehabilitation", "rÃ©habilitation", "rehabilitaciÃ³n"
  ,
  "crpd"
  ,
  "workplace accommodations", "amÃ©nagements en milieu de travail", "alojamiento en el lugar de trabajo"
)

advocacy.keywords <- c(
  "empower", "habiliter", "autorizar"
  ,
  "rights", "droits", "derechos"
  ,
  "self.advoca", "autogestores"
  ,
  "self.representative", "auto.reprÃ©sentant",	"auto.representante"
  ,
  "autonomy", "autonomie", "autonomÃ­a"
)

employment.keywords <- c(
  "employ", "emplear", "empleo", "emploi"
  ,
  "travail", "trabajo"
  ,
  "job"
  ,
  "labour", "labor[.]", "labor "
  ,
  "cash for work"
  ,
  "vocation", "vocaciÃ³n"
  ,
  "profession", "profesiÃ³n"
  ,
  "skills", "compÃ©tences",	"habilidades"
  ,
  "livelihood", "moyens de subsistance", "sustento"
  ,
  "earning", "revenus",	"ganador"
  ,
  "microcredit", "microcrÃ©dit"
  ,
  "article.{0,1}27"
  ,
  "workshop", "atelier"
  ,
  "business", "affaires",	"negocio"
  ,
  "workplace"
  ,
  "social protection", "social security", "sÃ©curitÃ© sociale", "protection sociale",	"protecciÃ³n social", "seguridad social"
  ,
  "assistive devices", "tecnologÃ­a de asistencia", "la technologie d'assistance", "dispositifs d'assistance", "dispositivos de ayuda"
  ,
  "reasonable accommodation", "acomodaciÃ³n razonable", "acomodaciones razonables", "amÃ©nagements raisonnables", "accommodement raisonnable"
  ,
  "workplace accommodations", "amÃ©nagements en milieu de travail", "alojamiento en el lugar de trabajo"
)

intellectual.keywords <- c(
  "intellect", "intelect"
  ,
  "cognitive dis", "discapacidad cognitiva", "dÃ©ficience cognitive", "cognitive defici", "cognitive delay", "delayed cognitive"
  ,
  "autistic", "austism", "autist"
  ,
  "with special needs", "con necesidades especiales", "avec des besoins spÃ©ciau", "avec des besoins spÃ©cifiques", "special needs education", "disabilities and special needs", "with disabilities or special needs", "con discapacidad o necesidades especiales", "with disabilities and special needs", "con discapacidad y necesidades especiales"
  ,
  "special education", "educaciÃ³n especial", "Ã©ducation spÃ©ciale", "special school", "special needs education", "special need education"
  ,
  "learning diff", "learning disa", "difficultÃ©s d'apprentissage", "dificultades de aprendizaje", "discapacidad de aprendizaje", "trouble d'apprentissage"
  ,
  "developmental disab", "developmental disorder", "development disab", "development disorder", "trouble du dÃ©veloppement", "discapacidad de desarrollo", "discapacidades del desarrollo", "trastorno del desarrollo"
  ,
  "with developmental delay", "with delayed development", "avec retard de dÃ©veloppement", "con retraso en el desarrollo", "avec dÃ©veloppement retardÃ©"
  ,
  "trisomy.{0,1}21", "trisomie.{0,1}21", "trisomÃ­a.{0,1}21"
  ,
  "down syndrom", "syndrome de down", "sÃ­ndrome de down"
  ,
  "complex support", "soutien complexe", "apoyo compleja"
  ,
  "psycho.{0,1}social disab", "disability and psycho.{0,1}social", "discapacidad psicosocial", "handicap psychosocial", "discapacidad y psicosocial", "handicap et psychosocial"
  ,
  "fetal alcohol syndrome"
  ,
  "pmld"
  ,
  "neuro.{0,1}development"
  ,
  "neuro.{0,1}diverse"
  ,
  "mental disab", "maladie mentale", "discapacidad mental"
  ,
  "people with limited capacit", "personnes avec une capacitÃ©.{0,1} limitÃ©e", "personas con capacidad.{0,2} limitada"
)

intellectual.disqualifying <- c(
  "^(?!.*intellectual).*deaf", "^(?!.*intelectual).*discapacidad auditiva"
  ,
  "^(?!.*intellectual).*blind"
  ,
  "^(?!.*intellectual).*visual impairment", "^(?!.*intelectual).*discapacidad visual"
  ,
  "intellectual property"
  ,
  "intellectual information"
  ,
  "intellectuals"
  ,
  "intellectual space"
  ,
  "intellectual boundaries"
  ,
  "micronutrient"
  ,
  "intellectual investment"
  ,
  "microbiome"
  ,
  "scholarships"
  ,
  "biological mechanisms"
  ,
  "cancer"
  ,
  "assessment tool"
  ,
  "fighting maternal and child undernutrition"
  ,
  "improve mental health"
  ,
  "research enrichment"
  ,
  "biomedical"
)

education.keywords <- c(
  "educat", "Ã©ducation", "educa", "Ã©duquer"
  ,
  "learning", "apprentissage", "aprendizaje"
  ,
  "article.{0,1}24"
  ,
  "class", "classe",	"clase"
  ,
  #"study", "estudiar", "Ã©tude"
  #,
  "school", "college", "university", "escula", "colegio", "universidad", "Ã©cole", "collÃ¨ge", "universitÃ©"
  ,
  "teach", "enseÃ±ar", "enseigner"
)

family.keywords <- c(
  "parent", "padre"
  ,
  "sibling", "fratrie", "hermano"
  ,
  "family", "families", "famille", "familia"
  ,
  "article.{0,1}23"
)

##################

crs$Gender <- as.character(crs$Gender)
crs[is.na(Gender)]$Gender <- "Not screened"
crs[Gender == "0"]$Gender <- "No gender component"
crs[Gender == "1"]$Gender <- "Significant gender component"
crs[Gender == "2"]$Gender <- "Principal gender component"

crs$Disability <- as.character(crs$Disability)
crs[is.na(Disability)]$Disability <- "Not screened"
crs[Disability == "0"]$Disability <- "No disability component"
crs[Disability == "1"]$Disability <- "Significant disability component"
crs[Disability == "2"]$Disability <- "Principal disability component"

t$relevance <- "None"
t[grepl(paste(significant.keywords, collapse = "|"), tolower(paste(t$title_narrative, t$description_narrative, t$transaction_description_narrative)))]$relevance <- "Significant"
t[grepl(paste(principal.keywords, collapse = "|"), tolower(paste(t$description_narrative,t$transaction_description_narrative)))]$relevance <- "Significant"
t[grepl(paste(principal.keywords, collapse = "|"), tolower(t$title_narrative))]$relevance <- "Principal"
t[grepl(paste(channel.keywords, collapse = "|"), tolower(paste(t$funding_orgs,t$accountable_orgs,t$extending_orgs,t$implementing_orgs)))]$relevance <- "Principal"
t$principal <- "No"
t[relevance != "None"][grepl(paste(principal.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$principal <- "Yes"

t$check <- "No"
t[relevance == "Significant"]$check <- "potential false positive"
t[relevance != "None"][x_sector_code %in% disqualifying.sectors]$check <- "potential false negative" # Needs changing to reflect IATI
t[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$check <- "potential false negative"

t[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$relevance <- "None"
t[principal != "Yes"][grepl(paste(significant.disqualifying.keywords, collapse = "|"), (paste(t[principal != "Yes"]$title_narrative, t[principal != "Yes"]$description_narrative, t[principal != "Yes"]$transaction_description_narrative)))]$relevance <- "None"
t[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

t$inclusion <- "Not inclusion"
t[relevance != "None"][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$inclusion <- "inclusion"

t$employment <- "Not employment"
t[relevance != "None"][grepl(paste(employment.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$employment <- "employment"

t$intellectual <- "Not intellectual"
t[relevance != "None"][grepl(paste(intellectual.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$intellectual <- "intellectual"
t[intellectual == "intellectual"][grepl(paste(intellectual.disqualifying, collapse = "|"), tolower(paste(t[intellectual == "intellectual"]$title_narrative, t[intellectual == "intellectual"]$description_narrative,t[intellectual == "intellectual"]$transaction_description_narrative)), perl=T)]$intellectual <- "Not intellectual"

t$education <- "Not education"
t[relevance != "None"][grepl(paste(education.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$education <- "education"

t$family <- "Not family"
t[relevance != "None"][grepl(paste(family.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$family <- "family"

t$advocacy <- "Not advocacy"
t[relevance != "None"][grepl(paste(advocacy.keywords, collapse = "|"), tolower(paste(t[relevance != "None"]$title_narrative, t[relevance != "None"]$description_narrative, t[relevance != "None"]$transaction_description_narrative)))]$advocacy <- "advocacy"

setwd(wd)

source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/split_and_save.R")
split_and_save(t, "output", 0)