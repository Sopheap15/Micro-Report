# read file ----
read_file <- function(path) {
  path %>%
    read_excel(na = c("", "NA"), trim_ws = T) %>%
    clean_names() %>%
    select(-any_of(c(
      "no", "dob", "test_date", "ampi_peni", "cephalosporins", "fluoroquinolone",
      "gentamicin_synergy", "novobiocin", "metronidazole", "oral_cephalosporins"
    )))
}

# format date----
format_date <- function(d) {
  if (is.character(d$collection_date) == TRUE) {
    d %>%
      mutate_at(
        vars(contains("date")),
        ~ as.POSIXct(.,
                     format = "%d-%B-%Y %H:%M",
                     tz = "GMT"
        )
      )
  } else if (is.numeric(d$collection_date) == TRUE) {
    d %>%
      mutate_at(
        vars(contains("date")),
        ~ as.POSIXct(. * (60 * 60 * 24),
                     origin = "1899-12-30",
                     tz = "GMT"
        )
      )
  } else {
    "Datetime is in correct format"
  }
}

# clean organisms---- 
clean_org <- function(data = data, col_org = results){
  data %>% 
    filter(!is.na(results)) %>% 
    mutate(
      results = case_when(
        str_detect(results, "(?i)Micrococcus") ~ "Micrococcus",
        str_detect(results, "(?i)Baci(\\w+)(?!(.+)?anth(\\w+))") ~ "Bacillus",
        str_detect(results, "(?i)Cory(\\w+)(?!(.+)?diph(\\w+))") ~ "Corynebacterium",
        str_detect(results, "(?i)Ent\\w+ales.*sp") ~ "Enterobacter",
        str_detect(results, "(?i)Ent\\w+ales.*acae") ~ "Enterobacter cloacae",
        str_detect(results, "(?i)Ent\\w+ales.*rans") ~ "Pantoea agglomerans",
        str_detect(results, "(?i)Ent\\w+ales.*genes") ~ "Klebsiella aerogenes",
        str_detect(results, "(?i)Ent\\w+ales.*genes") ~ "Klebsiella aerogenes",
        str_detect(results, "(?i)Ent\\w+ales.*zakii") ~ "Cronobacter sakazakii",
        TRUE ~ results
      ),
      results = str_remove(results, "(\\s)?sp\\.*$"),
      # remove sp. or spp.
      results = str_remove(results, "(\\s)?\\d$"),
      # remove number
      results = str_remove(results, "^[Pp]resumptive"),
      # remove word presumptive
      results = str_remove(results, "(\\s)?.not albicans$"),
      # remove word .not albicans
      results = str_remove(results, "(\\s)?\\(rods\\)$"),
      # remove word rods
      results = str_remove(results, "non-[tT]yphi$|non-[tT]yphi/non-[pP]aratyphi$"),
      results = str_remove(results, ",(\\s)?$"),
      results = str_trim(results, "both")
    
    ) 
}




