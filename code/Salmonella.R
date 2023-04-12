
d <- data %>% 
  filter(str_detect(results, "(?i).*salmonella.*")) %>% 
  group_by(collection_date_in_month, sample) %>% 
  summarise(n = n(),.groups = "keep")

blank_data <- blank_data %>% 
  filter(!collection_date_in_month %in% d$collection_date_in_month) %>% 
  filter(collection_date_in_month %in% last_month$collection_date_in_month) 

da <- rbind(blank_data, d) %>%
  group_by(collection_date_in_month,sample) %>%
  summarise(n = sum(n),.groups = "keep") %>% 
  mutate(sample = ifelse(sample == "", "Blood Culture", sample)) 


p <- da %>%
  rename(month = collection_date_in_month) %>% 
  ggplot() +
  geom_bar(aes(factor(month, levels = month.abb), n, fill = sample, 
               text = paste0(sample,": ", n)),
           stat = "identity", width = 0.5) +
  theme_bw() +
  labs(x = "Month", y = "Salmonella cases") +
  scale_fill_manual(na.translate = F,
                    breaks = "Blood Culture",
                    values = specimen_color) +
  scale_y_continuous(limits = c(0, max(da$n) + 2))


p <- ggplotly(p, tooltip = "text" ) %>% 
  layout(
    legend = list(title = list(text = ""),
                  x = 1, y = 0.5   
    )) %>% 
  config(displayModeBar = F)