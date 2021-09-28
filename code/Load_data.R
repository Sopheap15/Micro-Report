# Load Library----
library(here)
library(tidyverse,warn.conflicts = F)
library(janitor)
library(kableExtra)
library(AMR)
library(readxl)

# Load dictionary ----

dic <- read_excel(here("data","Dictionary.xlsx"),sheet = "hospital",col_names = T,trim_ws = T)

dic <- dic %>% 
  select(parameter = Parameter,full_name = `Full name`) %>%
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
data <- read_excel(here("data","Bacteriology Report.xlsx"), skip = as.numeric(dic$skip), na = c("","NA"),trim_ws = T) %>%
  clean_names() %>%
  mutate_at(vars(contains("date")), as.Date, format = "%d-%m-%Y") %>% 
  mutate(sex = factor(sex),comment = x51) %>% 
  select(-x51)

# Filter data base on dictionary
data <- data %>% 
  arrange(collection_date) %>% 
  filter(collection_date >= dic$start_date,
         collection_date <= ifelse(dic$end_date > max(collection_date), max(collection_date), dic$end_date), # filter date 
         lab_name == ifelse(dic$short_name == "All", lab_name, dic$short_name))

# Recode organism name
data <- data %>% 
  mutate(result = gsub(c(" sp.$|1$|2$|^Presumptive|,.not albicans$|, non-typhi$|,  non‐Typhi/non‐Paratyphi$"),"",result),
         result = trimws(result, which = "both"), # with remove sp., 1, 2 and presumptive and then remove leading and trailing space
         mo = as.mo(result))  # convert result to mo as organism according to AMR package

# Convert antibiotic name to standard
data <- data %>% 
  mutate(CRO = coalesce(ceftriaxone,ceftriaxone_30_gnb),
         CHL = coalesce(chloramphenicol,chloramphenicol_30),
         NOR = coalesce(norfloxacin,norfloxacin_10_gnb),
         SXT = coalesce(trimeth_sulfa,trimeth_sulfa_1_25)) %>% 
  select(-ceftriaxone, -ceftriaxone_30_gnb,-chloramphenicol,
         -chloramphenicol_30,-norfloxacin,-norfloxacin_10_gnb,
         -trimeth_sulfa,-trimeth_sulfa_1_25) %>% 
  relocate(mo,contaminant,comment,.after = last_col()) %>% 
  rename_with(as.ab,amoxi_clav:SXT) %>% 
  mutate(across(where(is.rsi.eligible),as.rsi))

# Recod specimen and wards
data <- data %>% 
  mutate(collection_date_in_month = factor(format(collection_date,"%b"), levels = month.abb),
         sample = recode(sample, "Pus aspirate" = "Pus",
                         "Pus swab"     = "Pus",
                         "Swab‐genital" = "Genital swab"))

# Finding last month
last_month <- data %>% 
  distinct(collection_date_in_month)

# Read and Joint ward data ----
ward <- read_excel(here("data","Dictionary.xlsx"),sheet = "ward",col_names = T,trim_ws = T)

data <- left_join(data,ward, by = c("sample_source" = "ward_from_Camlis")) %>% 
  mutate(sample_source = coalesce(new_ward_in_english,sample_source)) %>%
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

# Month -----
month <- data.frame(collection_date_in_month = factor(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), levels = month.abb)) 

# contamination organism ----
cont_org_list <- c("Coagulase Negative Staphylococcus",
                   "Corynebacterium",
                   "Streptococcus viridans, alpha-hem.",
                   "Micrococcus",
                   "Bacillus")


# Blood culture first isolate----
bc_first_isolate <- data %>%
  filter(sample == "Blood Culture") %>%  
  filter(!result %in% c(cont_org_list,"No growth")) %>% 
  filter_first_isolate(episode_day = 30, col_patient_id = "patient_id",col_date = "collection_date",col_mo = "result") %>% 
  eucast_rules(col_mo = "mo",rules = "all",info = F)

bc_cont_first_isolate <- data %>% # need to modify on contamination organism
  filter(sample == "Blood Culture") %>% 
  filter(result %in% c(cont_org_list)) %>% 
  filter_first_isolate(episode_days = 30, col_patient_id = "patient_id",col_date = "collection_date",col_mo = "result")








