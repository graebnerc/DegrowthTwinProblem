here::i_am("R/PostDevelopment-Links.R")
library(data.table)
library(here)
library(dplyr)

# Requires one to call 'python/' first
full_text_data <- fread(here("data/tidy/PostDevelopmentSearch.csv")) %>%
  dplyr::mutate(
    mentions_postdevel = ifelse(postdevelopment>0, 1, 0),
    mentions_decol = ifelse(decolonial>0, 1, 0),
    post_devel_paper = ifelse(mentions_postdevel+mentions_decol>0, 1, 0)
    )

cat("Share of papers with a reference to ",
    "post-development and/or decoloniality: ",
    round(sum(full_text_data$post_devel_paper) /
            nrow(full_text_data), 2)*100, "%", sep = "")

