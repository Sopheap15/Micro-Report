# Load Library----
library(tidyverse,warn.conflicts = F)
library(janitor)
library(kableExtra)
library(AMR)
library(readxl)
library(plotly)

# Month -----
month <- data.frame(collection_date_in_month = factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), levels = month.abb)) 

# list of organism in notifiable----
org_in_hos <- c("Klebsiella pneumoniae",
                "Acinetobacter", 
                "Burkholderia cepacia",
                "Stenotrophomonas maltophilia",
                "Pseudomonas aeruginosa",
                "Pseudomonas",
                "Non-fermenting gram negative rods",
                "Staphylococcus aureus"
)


# Contamination organism ----
cont_org_list <- c("Coagulase Negative Staphylococcus",
                   "Corynebacterium",
                   "Streptococcus viridans, alpha-hem.",
                   "Micrococcus",
                   "Bacillus")

# Load dictionary ----
dic <- list.files(path = "data", pattern = "^[Dd]ic.*(\\s)?.xls(x)?", full.names = T) %>% 
  read_excel(sheet = "hospital", col_names = T, trim_ws = T)

dic <- dic %>% 
  select(parameter = Parameter, full_name = `Full name`) %>%
  pivot_wider(names_from = parameter,values_from = full_name) %>% 
  clean_names() %>%
  mutate(short_name = case_when(
    hospital_name == "Siem Reap Provincial Referral Hospital" ~ "SRH",
    hospital_name == "Kampong Cham Provincial Referral Hospital" ~ "KCM",
    hospital_name == "Battambang Provincial Referral Hospital" ~ "BTB",
    hospital_name == "Takeo Provincial Referral Hospital" ~ "TKV",
    hospital_name == "All laboratories" ~ "All"),
    hospital_name = recode(hospital_name,
                           "All laboratories" = "Siem Reap, Takeo, Battambang and Kampong Cham Provincial Referral Hospital")) %>% 
  mutate_at(vars(contains("date")), as.Date, format = "%d-%m-%Y")

# Load data ----
# Read all file in folder data
read_file <- function(path){
  path %>% 
  read_excel(na = c("","NA"), trim_ws = T) %>% 
  clean_names() %>%
  select(-any_of(c("ampi_peni","cephalosporins","gentamicin_synergy","novobiocin",
            "metronidazole","oral_cephalosporins" )))
}
 
data <- list.files(path = "data", 
                   pattern = "[Bb]ac(.*)?port(.+)?.xls(x)?", 
                   full.names = T) %>% 
  purrr::discard(file_name,.p = ~stringr::str_detect(.,"~")) %>% 
  purrr::map(~read_file(.)) %>% 
  reduce(., bind_rows) %>% 
  mutate(sex = factor(sex))

# Convert date
data <- data %>% 
  mutate_at(vars("dob","collection_date", "admission_date"), convert_to_datetime)

# Recode specimen and wards
data <- data %>% 
  mutate(collection_date_in_month = factor(format(collection_date,"%b"), 
                                           levels = month.abb),
         sample = recode(sample, "Pus aspirate" = "Pus",
                         "Pus swab"     = "Pus",
                         "Swab‐genital" = "Genital swab"))

# Filter lab name
data <- data %>% 
  filter(lab_name == ifelse(dic$short_name == "All", lab_name, dic$short_name))

# Filter date
data <- data %>% 
  arrange(collection_date) %>% 
  filter(collection_date >= dic$start_date,
         collection_date <= ifelse(dic$end_date > max(collection_date), 
                                   max(collection_date), dic$end_date))


# Deduplicate data
data <- data %>% 
  distinct(patient_id, lab_id, collection_date, sample, result, .keep_all = T)

# reject specimen
reject_spe <- data %>% 
  select(patient_id, lab_id, collection_date ,sample, result, comment, rejected_comment) %>% 
  distinct(patient_id, lab_id, collection_date, sample, result, .keep_all = T)


# Age group
data <- data %>% 
  mutate(unit = str_extract(age, pattern = "\\D+"),
         age = str_extract(age, pattern = "\\d+"),
         age = case_when(unit == "y" ~ as.numeric(age)*365,
                         unit == "m" ~ as.numeric(age)*30,
                         TRUE ~ as.numeric(age))) %>% 
  relocate(unit, .after = age)


# Recode contamination organism
data <- data %>%
  filter(!is.na(result)) %>% 
  mutate(result = case_when(str_detect(result, "^Micrococcus") == TRUE ~ "Micrococcus",
                            str_detect(result, "^Baci(\\w+)(?!(.+)?anth(\\w+))") == TRUE ~ "Bacillus",
                            str_detect(result, "^Cory(\\w+)(?!(.+)?diph(\\w+))") == TRUE ~ "Corynebacterium",
                            # str_detect(result, "viridans") == TRUE ~ "Streptococcus viridans, alpha-hem.",
                            TRUE ~ result))

# Remove unusal string and convert to standard name
data <- data %>% 
  mutate(
         result = gsub("(\\s)?sp(p|.)?$", "", result), # remove sp. or spp.
         result = gsub("(\\s)?\\d$","", result), # remove number
         result = gsub("^[Pp]resumptive","", result), # remove word presumptive
         result = gsub("(\\s)?.not albicans$","", result), # remove word .not albicans
         result = gsub("(\\s)?\\(rods\\)$","", result), # remove word rods
         result = gsub("non-[tT]yphi$|non-[tT]yphi/non-[pP]aratyphi$","", result), 
         result = gsub(",(\\s)?$","", result), 
         result = trimws(result,"both")
         #mo = as.mo(result, info = F) # convert to standard name
         )  

# Convert antibiotic name to standard
data <- data %>% 
  rename_with(as.ab, amikacin:vancomycin) %>% 
  mutate(across(where(is.rsi.eligible), as.rsi))


# Finding last month
last_month <- data %>% 
  distinct(collection_date_in_month)

# Read and Joint ward data ----
ward <- list.files(path = "data", pattern = "^[Dd]ic.*(\\s)?.xls(x)?", full.names = T) %>%
  read_excel(sheet = "ward", col_names = T, trim_ws = T)

data <- left_join(data,ward, by = c("sample_source" = "ward_from_Camlis")) %>% 
  mutate(sample_source = coalesce(new_ward_in_english, sample_source)) %>%
  select(-new_ward_in_english,-Comment)

rm(ward)

# Deduplication----

# deduplicare by patient ID
dedup_by_pid <- data %>% 
  arrange(collection_date) %>% 
  distinct(patient_id,.keep_all = T)

# deduplicare by patient id, lab id and sample type
dedup_by_id_stype <- data %>% 
  arrange(collection_date) %>% 
  distinct(patient_id, lab_id, sample,.keep_all = T)


# Blood culture first isolate----
bc_first_isolate <- data %>%
  filter(sample == "Blood Culture", !result %in% c(cont_org_list,"No growth")) %>% 
  mutate(mo = as.mo(result, info = F)) %>% 
  filter_first_isolate(episode_day = 30,
                       col_patient_id = "patient_id",
                       col_date = "collection_date",
                       col_mo = "result", info = F) %>% 
  eucast_rules(col_mo = "mo", rules = "all", info = F)

bc_cont_deduplicate <- data %>% # need to modify on contamination organism
  filter(sample == "Blood Culture", result %in% c(cont_org_list)) %>% 
  distinct(lab_id, result,.keep_all = T)

# Possible HAI
HAI <- data %>% # from dataset select only notifiable organism
  select(patient_id, sample, collection_date, admission_date, result, collection_date_in_month) %>% 
  filter(sample == "Blood Culture", result %in% org_in_hos) %>% 
  mutate(HAI = as.numeric(collection_date - admission_date)) %>%
  arrange(collection_date_in_month) %>% 
  distinct(patient_id ,.keep_all = T)

# Clean comment----
comment <- data %>% 
  select(lab_name, sample, result, comment) %>% 
  filter(sample == "Blood Culture", !is.na(comment)) %>% 
  mutate(comment = str_replace_all(comment, c("\\bone" = "1", "\\btwo" = "2", "\\bthree" = "3", 
                                              "\\bfour" = "4", "\\bfive" = "5", "\\bsix" = "6",
                                              "\\bseven" = "7", "\\beight" = "8", "\\bnine" = "9",
                                              "\\bzero" = "0")),
         bottle_pos = str_extract(comment, 
                                  pattern = "[0-9] (of|Of) [0-9]"),
         day_growth = str_extract(tolower(comment), 
                                  pattern = "(day|Day).*?\\d|\\d.?(day|Day).?"),
         day_growth = str_extract(day_growth, 
                                  pattern = "[0-9]"))

# Critical result detection
critical_result <- data %>% 
  filter(sample == "Blood Culture", !is.na(comment), result != "No growth") %>% 
  distinct(patient_id, result, .keep_all = T) %>% 
  select(comment) %>% 
  mutate(call = str_detect(tolower(comment), "call[ed]?|phon[ed]?|dr(\\.+)?|clinician[s]?"), 
         total = n()) %>%
  filter(call == T) %>% 
  mutate(p = round_half_up(n()*100/total)) %>% 
  select(total, p) %>% 
  distinct()

# TAT
TAT <- data %>% 
  select(lab_name, patient_id, collection_date, admission_date, sample, result, comment) %>%
  filter(!is.na(result), sample == "Blood Culture", !is.na(comment), !result  %in% c("No growth")) %>%
  distinct(patient_id, result, .keep_all = T) %>% 
  mutate(comment_by = str_extract(str_to_lower(comment), 
                                  pattern = "(\\.*)?(call[ed]?|phon[ed]?)(.*)"),
         comment_by = str_replace(comment_by, 
                                  pattern = "(die[d]|dead) (on)? \\d+[/ ,.-]\\d+[/ ,.-]\\d+",""),
         date = str_extract(comment_by, pattern = "\\d+[/ ,.-]\\d+[/ ,.-]\\d+"),
         time = str_extract(comment_by, pattern = "\\d{1,}[: .;](\\s)?\\d{1,}(\\s)?([AaPp][Mm])?")
  ) %>% 
  #mutate_at(c("date", "time"), ~replace(., is.na(.), "0:0")) %>% 
  mutate(primary_report = lubridate::dmy_hm(paste(date, time), quiet = TRUE))



