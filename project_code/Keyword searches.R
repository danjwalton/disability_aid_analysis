#danjwalton 2019

required.packages <- c("data.table", "RJSONIO")
lapply(required.packages, require, character.only = T)

if(Sys.info()[["user"]]=="dan-w" | Sys.info()[["user"]]=="danw"){
  wd <- "G:/My Drive/Work/GitHub/disability_aid_analysis/"
}else if(Sys.info()[["user"]] %in% c("dean-b") | Sys.info()[["user"]] %in% c("deanb")){
  wd <- "C:/git/disability_aid_analysis/"
}
setwd(wd)

source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/load_and_join.R")
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

major.keywords <- c(
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
  "special needs", "necesidades especiales", "besoins spéciau"
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
  "hearing aid", "audífono", "dispositif d'écoute pour malentendant"
  ,
  "amputation", "amputee", "amputé", "amputa"
  ,
  "schizophreni", "esquizofrenia", "schizophrénie"
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
  "special education", "educación especial", "éducation spéciale"
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
  "psycho.{0,1}social"
)


minor.keywords <- c(
  "vulnerable group", "vulnerable people", "vulnerable population", "vulnerable individual", "vulnerable girl", "vulnerable women", "vulnerable boy", "vulnerable men", "vulnerable refugee", "who are vulnerable", "which are vulnerable", "vulnerable child"
  ,
  #"minority group", "minority population", "minorities"
  #,
  "marginali.ed group", "marginali.ed people", "marginali.ed population", "marginali.ed individual", "marginali.ed girl", "marginali.ed women", "marginali.ed boy", "marginali.ed men", "marginali.ed refugee", "who are marginali.ed", "which are marginali.ed", "marginali.ed child", "marginali.ed and young"
  ,
  "war victim", "victimas de guerra", "victimes de guerre", "victim. of war"
  ,
  #"conflict affected", "conflict-affected", "conflict victim", "victim. of conflict"
  #,
  "landmine victim", "victime de mine", "víctima de minas terrestres"
  #,
  #"wounded"
  #,
  #"injured", "injuries"
  #,
  #"physiotherapy", "fisioterapia"
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
)

disqualifying.sectors <- c(
  "Public finance management (PFM)"
  ,
  "Domestic revenue mobilisation"
  #,
  #"Mineral/mining policy and administrative management"
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
  "integration", "intégration", "integración"
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
)

intellectual.keywords <- c(
  "intellect", "intelect"
  ,
  "cognitive", "cognitiva"
  ,
  "autistic", "austism", "autist"
  ,
  "special needs", "necesidades especiales", "besoins spéciau", "besoins spécifiques"
  ,
  "special education", "educación especial", "éducation spéciale"
  ,
  "learning diff", "learning disa", "difficultés d'apprentissage", "dificultades de aprendizaje", "discapacidad de aprendizaje", "trouble d'apprentissage"
  ,
  "developmental disab", "developmental disorder", "development disab", "development disorder", "trouble du développement", "discapacidad de desarrollo", "discapacidades del desarrollo", "trastorno del desarrollo"
  ,
  "trisomy.{0,1}21", "trisomie.{0,1}21", "trisomía.{0,1}21"
  ,
  "down syndrom", "syndrome de down", "síndrome de down"
  ,
  "complex support", "soutien complexe", "apoyo compleja"
  ,
  "cerebral",	"cérébrale"
  ,
  "psycho.{0,1}social"
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
crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$ProjectTitle, crs$ShortDescription, crs$LongDescription)))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$LongDescription))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Major"
crs[grepl(paste(channel.keywords, collapse = "|"), tolower(crs$ChannelReportedName))]$relevance <- "Major"

crs$check <- "No"
crs[relevance == "Minor"]$check <- "potential false positive"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"

crs$inclusion <- "Not inclusion"
crs[relevance != "None"][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$inclusion <- "inclusion"

crs$employment <- "Not employment"
crs[relevance != "None"][grepl(paste(employment.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$employment <- "employment"

crs$intellectual <- "Not intellectual"
crs[relevance != "None" | Disability %in% c("Principal disability component", "Significant disability component")][grepl(paste(intellectual.keywords, collapse = "|"), tolower(paste(crs[relevance != "None" | Disability %in% c("Principal disability component", "Significant disability component")]$ProjectTitle, crs[relevance != "None" | Disability %in% c("Principal disability component", "Significant disability component")]$ShortDescription, crs[relevance != "None" | Disability %in% c("Principal disability component", "Significant disability component")]$LongDescription)))]$intellectual <- "intellectual"

crs$education <- "Not education"
crs[relevance != "None"][grepl(paste(education.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$education <- "education"

crs$family <- "Not family"
crs[relevance != "None"][grepl(paste(family.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$family <- "family"

source("https://raw.githubusercontent.com/danjwalton/crs_keyword_searching/master/project_code/split_and_save.R")
split_and_save(crs, "output", 0)
