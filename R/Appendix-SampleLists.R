# This script is used to create the lists of the samples for the appendix
here::i_am("R/Appendix-SampleLists.R")
library(here)
library(dplyr)
library(stringr)
library(writexl)
source(here("R/setup_data.R"))
here::i_am("R/Appendix-SampleLists.R")

core_papers <- eval_core %>%
  select(all_of(c(
    "Authors", "Publication Year", "Article Title", "Source Title", "Sample"))
    ) %>%
  rename(
    Year = `Publication Year`,
    Title =`Article Title`,
    Outlet = `Source Title`,
    Source = Sample
  ) %>%
  mutate(
    Title = str_to_sentence(Title),
    Outlet = str_to_title(Outlet),
    Source = ifelse(
      Source == "InformalSearch", "Informal sample",
        ifelse(Source == "CitedReferences", "Citation sample",
               ifelse(Source == "TimotheeParrique", "Degrowth sample",
                      ifelse(Source == "WoS", "WoS sample", "XXX"))))
  ) %>%
  arrange(-desc(Authors))

write_xlsx(
  x = core_papers,
  path = here("output/Appendix-CorePapers.xlsx"))

relevant_papers <- eval_relevant %>%
  filter(Assessment=="Relevant") %>%
  select(all_of(c(
    "Authors", "Publication Year", "Article Title", "Source Title", "Sample"))
    ) %>%
  rename(
    Year = `Publication Year`,
    Title =`Article Title`,
    Outlet = `Source Title`,
    Source = Sample
  ) %>%
  mutate(
    Title = str_to_sentence(Title),
    Outlet = str_to_title(Outlet),
    Source = ifelse(
      Source == "InformalSearch", "Informal sample",
      ifelse(Source == "CitedReferences", "Citation sample",
             ifelse(Source == "TimotheeParrique", "Degrowth sample",
                    ifelse(Source == "WoS", "WoS sample", "XXX"))))
  ) %>%
  arrange(-desc(Authors))

write_xlsx(
  x = relevant_papers,
  path = here("output/Appendix-RelevantPapers.xlsx"))
