#################################
## Summarize flight height from reflection calculation

X <- readxl::read_xlsx("Data/Kittiwakes with reflection combined_Refl.xlsx")

X %>% dplyr::select(`Flying Height (m)`,Species) %>%
  mutate(
    corr.height = `Flying Height (m)`*2
  ) %>% group_by(Species) %>%
  summarise(meanval = mean(corr.height), s.size = n()) %>%
  dplyr::filter(!is.na(meanval))
