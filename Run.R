# Load data----
source("code/Load_data.R",knitr::knit_global())

# Render report
rmarkdown::render("code/Microbiology Report V1 06.08.21.Rmd",
                    output_file = paste0("Outputs/Microbiology_Report_",{Sys.Date()},".html"), quiet = T)



