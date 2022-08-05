

read_excel("data/Kampong cham Bacteriology_report 14.07.22.xlsx") %>% 
  mutate_at(vars(contains("date")), ~ as.POSIXct(.* (60*60*24)
                                               , origin="1900-01-01"
                                           , tz="GMT")) %>% view()


kc <- read_excel("data/Kampong cham Bacteriology_report 14.07.22.xlsx") %>% clean_names()
            
data %>% 
  select(contains("date")) %>% 
  map_at(vars(contains("date")), ~ as.POSIXct(.* (60*60*24)
                                              , origin="1900-01-01"
                                              , tz="GMT"))
da <- read_excel("Bacteriology_report(1).xlsx")

format_date <- function(d){
  if(is.character(d$collection_date) == TRUE) {
    d %>% 
      mutate_at(vars(contains("date")), lubridate::ymd_hms)
  }else if (is.numeric(d$collection_date)==TRUE){
    d %>%
      mutate_at(vars(contains("date")), ~ as.POSIXct(.* (60*60*24)
                                                     , origin="1900-01-01"
                                                     , tz="GMT"))
  }else{
    "Datetime is in correct format"
  }
  
}

format_date(data) %>% str()

