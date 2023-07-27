# This file gets usually called by other files. It takes the result of
#  `setup_cited_references.R` (which takes much longer) and creates a
#  tibble with the cited references.
here::i_am("R/build_cited_refs.R")
library(stringr)
library(dplyr)
library(readxl)

cited_articles <- readRDS(here("data/tidy/cited_articles.Rds")) %>%
  mutate(
    citing_pub_title = str_squish(str_replace_all(
      citing_pub_title, regex("\\W+"), " ")))

# TODO: qualitative_evaluation.xlsx
sample_info <- read_xlsx(here("data/manual/Qual-review-Rev.xlsx")) %>%
  select(all_of(c("Article Title", "Sample", "Assessment"))) %>%
  filter(Sample=="WoS") %>%
  mutate(
    `Article Title` = str_squish(str_replace_all(
      `Article Title`, regex("\\W+"), " ")))

cited_articles_v2 <- left_join(
  cited_articles, sample_info, by=c("citing_pub_title"="Article Title")) %>%
  filter(Assessment %in% c("Core", "Relevant"))

citations_from_core <- cited_articles_v2 %>%
  filter(Assessment %in% c("Core")) %>%
  summarise(times_cited = n(), .by = c("pub_id", "first_author", "Year")) %>%
  arrange(-times_cited) %>%
  filter(times_cited >= 5)

citations_from_relevant <- cited_articles_v2 %>%
  filter(Assessment %in% c("Relevant")) %>%
  summarise(times_cited = n(), .by = c("pub_id", "first_author", "Year")) %>%
  arrange(-times_cited) %>%
  filter(times_cited >= 10)

citatation_sample <- bind_rows(citations_from_core, citations_from_relevant) %>%
  select(-times_cited) %>%
  unique()
