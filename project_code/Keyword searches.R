#danjwalton 2019

required.packages <- c("data.table", "RJSONIO")
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
  ]

crs <- crs[as.character(Year) >= 2014]

principal.keywords <- c(
  "disab", "discapaci", "incapaci", "minusválido", "invalidit", "infirmité", "d-isab"
  #,
  #"disorder"
  ,
  "handicap"
  ,
  "impairment", "impaired"
  ,
  "pwd", "gwd", "cwd"
  ,
  "chronic health", "chronic ill", "maladie chronique", "enfermedad crónica"
  ,
  "deaf", "sordo", "sourd"
  ,
  "blind", "ciego", "aveugle", "eye health"
  ,
  "with special needs", "con necesidades especiales", "besoins spéciau", "besoins spécifiques", "special needs education", "disabilities and special needs"
  ,
  "autistic", "autism", "autist"
  ,
  "mental health", "santé mentale", "salud mental"
  ,
  "prosthe", "prosthè", "prótesi"
  ,
  "mobility device", "dispositivo de movilidad", "dispositif de mobilité"
  ,
  "wheelchair", "fauteuil roulant", "silla de ruedas"
  ,
  "plegia", "paralys"
  ,
  "hearing aid", "audífono", "dispositif d'écoute pour malentendant"
  ,
  "amputation", "amputee", "amputé", "amputa"
  ,
  "schizophreni", "esquizofrenia", "schizophrénie"
  ,
  "bipolar"
  ,
  "leprosy"
  ,
  "sign language", "langage des signes", "lenguaje de señas"
  ,
  "arthriti", "artritis", "arthrite"
  ,
  "rheumat", "rhumat", "reumat"
  ,
  "dementia", "démence", "demencia"
  ,
  "spina "
  ,
  "hydrocephalus", "hidrocefalia", "l'hydrocéphalie"
  ,
  "diabetes", "diabète"
  ,
  "atlas alliance", "atlas allinance", "abilis foundation", "zapdd"
  #,
  #"dpos ", "dpo ", "dpo's", "dpos[.]", "dpo[.]", "dpo's[.]"
  ,
  "special education", "educación especial", "éducation spéciale", "special needs education", "special need education"
  ,
  "learning difficult", "learning disa", "difficultés d'apprentissage", "dificultades de aprendizaje", "discapacidad de aprendizaje", "trouble d'apprentissage", "learning problem"
  ,
  "trisomy.{0,1}21", "trisomie.{0,1}21", "trisomía.{0,1}21"
  ,
  "down syndrom", "syndrome de down", "síndrome de down"
  ,
  "cerebral",	"cérébrale"
  ,
  "crpd"
  ,
  "psycho.{0,1}social disab", "disability and psycho.{0,1}social", "discapacidad psicosocial", "handicap psychosocial", "discapacidad y psicosocial", "handicap et psychosocial"
  ,
  "cognitive dis", "discapacidad cognitiva", "déficience cognitive", "cognitive defici", "cognitive delay", "delayed cognitive"
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
  "sclerosis", "sclérose"
  ,
  "albinism", "albino"
  ,
  "assistive technology", "assistive devices", "tecnología de asistencia", "la technologie d'assistance", "dispositifs d'assistance", "dispositivos de ayuda"
  ,
  "reasonable accommodation", "acomodación razonable", "acomodaciones razonables", "aménagements raisonnables", "accommodement raisonnable"
  ,
  "inclusive education","éducation inclusive","educación inclusive","accessibility","accesibilidad","accessibilité"
  ,
  "workplace accommodations", "aménagements en milieu de travail", "alojamiento en el lugar de trabajo"
  ,
  "psychosocial di$"
  ,
  "people with limited capacit", "personnes avec une capacité.{0,1} limitée", "personas con capacidad.{0,2} limitada"
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
  "landmine victim", "victime de mine", "víctima de minas terrestres", "landmine survivor", "sobreviviente de minas terrestres", "survivant d'une mine",
  #,
  #"wounded"
  #,
  #"injured", "injuries"
  #,
  #"physiotherapy", "fisioterapia"
  "inclusive education","éducation inclusive", "educación inclusiva"
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
  "Public finance management (PFM)"
  ,
  "Domestic revenue mobilisation"
  #,
  #"Mineral/mining policy and administrative management"
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

inclusion.keywords <- c(
  "inclus"
  ,
  "empower", "habiliter", "autorizar"
  ,
  "rights", "droits", "derechos"
  ,
  "advocacy", "plaidoyer", "abogacía"
  ,
  "self-representative", "auto-représentant",	"auto-representante"
  ,
  "autonomy", "autonomie", "autonomía"
  ,
  #"community", "communaté"
  #,
  "integration", "intégration", "integración"
  ,
  "autogestores"
  ,
  "accessiblity", "accessibilité", "accesibilidad"
  ,
  "assistive technology", "assistive devices", "tecnología de asistencia", "la technologie d'assistance", "dispositifs d'assistance", "dispositivos de ayuda"
  ,
  "reasonable accommodation", "acomodación razonable", "acomodaciones razonables", "aménagements raisonnables", "accommodement raisonnable"
  ,
  "rehabilitation", "réhabilitation", "rehabilitación"
  ,
  "crpd"
  ,
  "workplace accommodations", "aménagements en milieu de travail", "alojamiento en el lugar de trabajo"
)

advocacy.keywords <- c(
  "empower", "habiliter", "autorizar"
  ,
  "rights", "droits", "derechos"
  ,
  "self.advoca", "autogestores"
  ,
  "self.representative", "auto.représentant",	"auto.representante"
  ,
  "autonomy", "autonomie", "autonomía"
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
  "vocation", "vocación"
  ,
  "profession", "profesión"
  ,
  "skills", "compétences",	"habilidades"
  ,
  "livelihood", "moyens de subsistance", "sustento"
  ,
  "earning", "revenus",	"ganador"
  ,
  "microcredit", "microcrédit"
  ,
  "article.{0,1}27"
  ,
  "workshop", "atelier"
  ,
  "business", "affaires",	"negocio"
  ,
  "workplace"
  ,
  "social protection", "social security", "sécurité sociale", "protection sociale",	"protección social", "seguridad social"
  ,
  "assistive devices", "tecnología de asistencia", "la technologie d'assistance", "dispositifs d'assistance", "dispositivos de ayuda"
  ,
  "reasonable accommodation", "acomodación razonable", "acomodaciones razonables", "aménagements raisonnables", "accommodement raisonnable"
  ,
  "workplace accommodations", "aménagements en milieu de travail", "alojamiento en el lugar de trabajo"
)

intellectual.keywords <- c(
  "intellect", "intelect"
  ,
  "cognitive dis", "discapacidad cognitiva", "déficience cognitive", "cognitive defici", "cognitive delay", "delayed cognitive"
  ,
  "autistic", "austism", "autist"
  ,
  "with special needs", "con necesidades especiales", "avec des besoins spéciau", "avec des besoins spécifiques", "special needs education", "disabilities and special needs", "with disabilities or special needs", "con discapacidad o necesidades especiales", "with disabilities and special needs", "con discapacidad y necesidades especiales"
  ,
  "special education", "educación especial", "éducation spéciale", "special school", "special needs education", "special need education"
  ,
  "learning diff", "learning disa", "difficultés d'apprentissage", "dificultades de aprendizaje", "discapacidad de aprendizaje", "trouble d'apprentissage"
  ,
  "developmental disab", "developmental disorder", "development disab", "development disorder", "trouble du développement", "discapacidad de desarrollo", "discapacidades del desarrollo", "trastorno del desarrollo"
  ,
  "with developmental delay", "with delayed development", "avec retard de développement", "con retraso en el desarrollo", "avec développement retardé"
  ,
  "trisomy.{0,1}21", "trisomie.{0,1}21", "trisomía.{0,1}21"
  ,
  "down syndrom", "syndrome de down", "síndrome de down"
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
  "people with limited capacit", "personnes avec une capacité.{0,1} limitée", "personas con capacidad.{0,2} limitada"
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
  "educat", "éducation", "educa", "éduquer"
  ,
  "learning", "apprentissage", "aprendizaje"
  ,
  "article.{0,1}24"
  ,
  "class", "classe",	"clase"
  ,
  #"study", "estudiar", "étude"
  #,
  "school", "college", "university", "escula", "colegio", "universidad", "école", "collège", "université"
  ,
  "teach", "enseñar", "enseigner"
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

crs$relevance <- "None"
crs[grepl(paste(significant.keywords, collapse = "|"), tolower(paste(crs$ProjectTitle, crs$ShortDescription, crs$LongDescription)))]$relevance <- "Significant"
crs[grepl(paste(principal.keywords, collapse = "|"), tolower(crs$LongDescription))]$relevance <- "Significant"
crs[grepl(paste(principal.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Principal"
crs[grepl(paste(channel.keywords, collapse = "|"), tolower(crs$ChannelReportedName))]$relevance <- "Principal"
crs$principal <- "No"
crs[relevance != "None"][grepl(paste(principal.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$principal <- "Yes"

crs$check <- "No"
crs[relevance == "Significant"]$check <- "potential false positive"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
crs[principal != "Yes"][grepl(paste(significant.disqualifying.keywords, collapse = "|"), (paste(crs[principal != "Yes"]$ProjectTitle, crs[principal != "Yes"]$ShortDescription, crs[principal != "Yes"]$LongDescription)))]$relevance <- "None"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

crs$inclusion <- "Not inclusion"
crs[relevance != "None"][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$inclusion <- "inclusion"

crs$employment <- "Not employment"
crs[relevance != "None"][grepl(paste(employment.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$employment <- "employment"

crs$intellectual <- "Not intellectual"
crs[relevance != "None"][grepl(paste(intellectual.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$intellectual <- "intellectual"
crs[intellectual == "intellectual"][grepl(paste(intellectual.disqualifying, collapse = "|"), tolower(paste(crs[intellectual == "intellectual"]$ProjectTitle, crs[intellectual == "intellectual"]$ShortDescription,crs[intellectual == "intellectual"]$LongDescription)), perl=T)]$intellectual <- "Not intellectual"

crs$education <- "Not education"
crs[relevance != "None"][grepl(paste(education.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$education <- "education"

crs$family <- "Not family"
crs[relevance != "None"][grepl(paste(family.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$family <- "family"

crs$advocacy <- "Not advocacy"
crs[relevance != "None"][grepl(paste(advocacy.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$advocacy <- "advocacy"

source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/split_and_save.R")
split_and_save(crs, "output", 0)
