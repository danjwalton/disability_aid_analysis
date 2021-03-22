#danjwalton 2019 - UPDATED deanbreed 2021

required.packages <- c("data.table", "RJSONIO","stringr")
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
crs <- load_crs()

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

crs <- crs[, ..keep]
crs <- crs[
    FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
   FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
  ] # Removes unspecified flows and OOFs

crs <- crs[as.character(Year) >= 2015]

crs <- crs[RecipientName %in% c("Zambia","Kenya","Uganda")]

#### Keywords ####

principal.keywords <- c("disab","discapaci","incapaci","minusválido","invalidit","infirmité","d-isab","disorder","handicap"

,"impairment","impaired","pwd","gwd","cwd","chronic health","chronic ill","maladie chronique","enfermedad crónica","deaf"

,"sordo","sourd","blind","ciego","aveugle","eye health","with special needs","con necesidades especiales","besoins spéciau"

,"besoins spécifiques","special needs education","disabilities and special needs","autistic","autism","autist","mental health"

,"santé mentale","salud mental","prosthe","prosthè"," prótesi","mobility device"," dispositivo de movilidad"," dispositif de mobilité"

,"wheelchair","fauteuil roulant"," silla de ruedas","plegia","paralys","hearing aid","audífono","dispositif d'écoute pour malentendant"

,"amputation","amputee","amputé","amputa","schizophreni","esquizofrenia","schizophrénie","bipolar","leprosy","sign language"

,"langage des signes","lenguaje de señas","arthriti","artritis","arthrite","rheumat","rhumat","reumat","dementia","démence"

,"demencia","spina","hydrocephalus","hidrocefalia","l'hydrocéphalie,diabetes","diabète","atlas alliance","atlas allinance"

,"abilis foundation","zapdd","special education", "educación especial","éducation spéciale","special needs education"

,"special need education","learning difficult","difficultés d'apprentissage","dificultades de aprendizaje","learning disa"

,"discapacidad de aprendizaje","trouble d'apprentissage","learning problem","trisomy.{0,1}21","trisomie.{0,1}21"," trisomía.{0,1}21"

,"down syndrom","syndrome de down","síndrome de down","cerebral","cérébrale","crpd","cognitive dis","discapacidad cognitiva"

,"déficience cognitive","cognitive defici","cognitive delay","delayed cognitive","fetal alcohol syndrome","developmental delay"

,"pmld","neuro.{0,1}development","neuro.{0,1}diverse","sclerosis","sclérose","albinism","albino","assistive technology"

,"tecnología de asistencia","la technologie d'assistance","assistive devices","dispositifs d'assistance","dispositivos de ayuda"

,"reasonable accommodation","acomodación razonable","acomodaciones razonables","aménagements raisonnables","accommodement raisonnable"

,"inclusive education","éducation inclusive","educación inclusive","accessibility","accesibilidad","accessibilité"

,"workplace accommodations","aménagements en milieu de travail","alojamiento en el lugar de trabajo,people with limited capacit"

,"personnes avec une capacité.{0,1} limitée","personas con capacidad.{0,2} limitada"

)


significant.keywords <- c("vulnerable group","vulnerable people","vulnerable population","vulnerable individual","vulnerable girl","vulnerable women"
  
  ,"vulnerable boy","vulnerable men","vulnerable refugee","who are vulnerable","which are vulnerable","vulnerable child","marginali.ed group","marginali.ed people"
  
  ,"marginali.ed population","marginali.ed individual","marginali.ed girl","marginali.ed women","marginali.ed boy","marginali.ed men"
  
  ,"marginali.ed refugee","who are marginali.ed","which are marginali.ed","marginali.ed child","marginali.ed and young","war victim"
  
  ,"victimas de guerra","victimes de guerre","victim. of war","landmine victim","victime de mine","víctima de minas terrestres"
  
  ,"landmine survivor","sobreviviente de minas terrestres","survivant d'une mine","inclusive education","éducation inclusive"
  
  ,"educación inclusiva","inclusive employment","empleo inclusivo","emploi inclusif"
  
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

psychosocial.keywords <- c(
  "incapaci","minusválido","invalidit","infirmité","disorder"
  
  #,"impaired"
  
  ,"pwd","gwd","cwd","chronic health","chronic ill"
  
  ,"enfermedad crónica","with special needs","con necesidades especiales","besoins spéciau","besoins spécifiques","special needs education"
  
  ,"autistic","autism","autist"
  
  ,"mental health","santé mentale","salud mental","prosthe", "prosthè", "prótesi", "paralys"
  
  ,"schizophreni","esquizofrenia","schizophrénie","bipolar"
  
  #,"cognitive dis","discapacidad cognitiva","déficience cognitive","cognitive defici","cognitive delay","delayed cognitive"
  
  ,"dementia", "démence", "demencia"
  
  ,"learning difficult", "difficultés d'apprentissage", "dificultades de aprendizaje","learning disa", "discapacidad de aprendizaje"
  
  ,"trouble d'apprentissage", "learning problem"
  
  ,"down syndrom", "syndrome de down", "síndrome de down","trisomy.{0,1}21","trisomie.{0,1}21","trisomía.{0,1}21"
)

disqualifying.sectors <- c(
  "Public finance management (PFM)"
  ,
  "Domestic revenue mobilisation"
  ,
  "Medical education/training"
  ,
  "Medical research"
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

#### Checks for additional keywords ####

# crs$relevance <- "None"
# crs[grepl(paste(c(significant.keywords,principal.keywords,validity.words), collapse = "|"), tolower(paste(crs$LongDescription, crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Principal"
# initial <- crs[relevance == "Principal"]
# 
# extra.words <- c("trauma","well.{0,1}being")
# 
# for (additional.word in extra.words){
# print(additional.word)
# 
# crs$relevance <- "None"
# crs[grepl(additional.word, tolower(paste(crs$LongDescription,crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Principal"
# after <- crs[relevance == "Principal"]
# 
# difference <- after[which(after$CrsID %in% setdiff(after$CrsID,initial$CrsID))]
# 
# if (nrow(difference)>0){
#   write.csv(difference,paste0("output/additional words incl long zku second/",str_replace_all(additional.word,"\\\\b",""),".csv"))
# }
# 
# if (nrow(difference) == 0 & nrow(after)>0){
#   write.csv(difference,paste0("output/additional words incl long zku second/",str_replace_all(additional.word,"\\\\b",""),"_full.csv"))
# }
# 
# }

#### End ####

crs$relevance <- "None"
crs[grepl(paste(c(principal.keywords,significant.keywords), collapse = "|"), tolower(paste(crs$ProjectTitle, crs$ShortDescription, crs$LongDescription)))]$relevance <- "Significant"
crs[grepl(paste(principal.keywords, collapse = "|"), tolower(crs$LongDescription))]$relevance <- "Significant"
crs[grepl(paste(principal.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Principal"
crs[grepl(paste(channel.keywords, collapse = "|"), tolower(crs$ChannelReportedName))]$relevance <- "Principal"
# crs <- subset(crs,relevance %in% c("Principal","Significant"))

# write.csv(crs,"crs.csv")

crs$check <- "No"
crs <- data.table(crs)

crs[relevance == "None" & Disability == "Principal disability component"]$relevance <- "Principal"
crs[relevance == "None" & Disability == "Significant disability component"]$relevance <- "Significant"

crs$psychosocial <- "Not psychosocial"
crs[relevance != "None"][grepl(paste(psychosocial.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$psychosocial <- "psychosocial"

# crs[relevance == "Significant"]$check <- "potential false positive"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative" # If in the disqualifying sectors, it should be checked.
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative" # If contains the disqualifying keywords, it should be checked.
crs[relevance != "None"][grepl(paste(significant.disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative" # If contains the disqualifying keywords, it should be checked

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
crs[relevance != "None"][grepl(paste(significant.disqualifying.keywords, collapse = "|"), (paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

crs[relevance == "Principal" & Disability == "No disability component"]$check <- "potential false positive"
crs[relevance == "Significant" & Disability == "No disability component"]$check <- "potential false positive"

write.csv(crs,"output/crs.csv")
# source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/split_and_save.R")
# split_and_save(crs, "output", 0)
