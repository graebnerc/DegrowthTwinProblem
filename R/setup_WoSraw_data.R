# This script creates the excel to be used for the qualitative assessment of
#  the papers, and merges the WoS searches in different languages into one
#  consistent data set. Thus, it does not create the the data set considering
#  also the non-WoS data sets; for this see the script 'setup_data.R'
here::i_am("R/setup_WoSraw_data.R")
library(here)
library(readxl)
library(dplyr)
library(data.table)

languages <- c("en", "dt", "es", "fr", "pt")
raw_data <- list()
for (i in seq_along(languages)){
  current_language <- languages[i]
  language_file <- read_xls(
    path = here(paste0("data/raw/WoS_", current_language, "/savedrecs.xls"))
    ) %>% dplyr::mutate(search_language = current_language)
  raw_data[[current_language]] <- language_file
}

raw_data_full <- rbindlist(raw_data)
raw_data_unique_v1 <- raw_data_full %>%
  unique(by = "UT (Unique WOS ID)") %>%
  dplyr::mutate(# Fix typo from WoS
    Authors = ifelse(
      test = Authors=="Anguelovski, I; Alier, JM",
      yes = "Anguelovski, I; Martinez-Alier, J",
      no = Authors))


# Remove corrections not filtered by WoS-------------------
raw_data_unique_v2 <- raw_data_unique_v1 %>%
  dplyr::filter(
    !stringr::str_detect(`Document Type`, "Correction") # Remove the two corrections
  )

fwrite(x = raw_data_unique_v2, here("data/tidy/WoS-RawSampleUnique.csv"))

# Summary for screening figure-------------------
raw_results_en <- nrow(raw_data[["en"]])
raw_results_dt <- nrow(raw_data[["dt"]])
raw_results_es <- nrow(raw_data[["es"]])
raw_results_fr <- nrow(raw_data[["fr"]])
raw_results_pt <- nrow(raw_data[["pt"]])
raw_sample_size <- nrow(raw_data_full)
raw_unique_sample_size <-  nrow(raw_data_unique_v1)
duplicates_removed <- raw_sample_size - raw_unique_sample_size
initial_sample <- nrow(raw_data_unique_v2)
corrections_removed <- raw_unique_sample_size - initial_sample

if ((raw_results_en+raw_results_dt+raw_results_es +
     raw_results_fr+raw_results_pt)!=raw_sample_size){
  warning("Raw sample not sum of language sub samples!")
}

save(list = c(
  "raw_results_en", "raw_results_dt", "raw_results_es",
  "raw_results_fr", "raw_results_pt", "corrections_removed",
  "duplicates_removed", "raw_sample_size", "initial_sample", "raw_unique_sample_size"),
  file = here("data/tidy/sample_summary_WoSraw.Rdata"))

# Create file for quali assessment of new articles---------
old_quali <- read_xlsx(
  path = here("data/manual/Qual-review-Original.xlsx"),
  sheet = "Paper assessment"
  ) %>%
  dplyr::filter(!is.na(`Article Title`))

# These articles do not show up in the second WoS search any more:
articles_not_in_new_sample <- old_quali %>%
  dplyr::filter(!`Article Title` %in% raw_data_unique_v2$`Article Title`)
paste0("Articles that do not show up anymore: ",
       nrow(articles_not_in_new_sample))

# Of these, those were considered relevant:
relevant_articles_removed <- articles_not_in_new_sample %>%
  dplyr::filter(
    !is.na(Core),
    `SecStage Assessment of 1st stage no-core`!="Drop" |
      is.na(`SecStage Assessment of 1st stage no-core`))

paste0("Core articles that do not show up anymore: ",
       nrow(relevant_articles_removed))
fwrite(
  x = articles_not_in_new_sample,
  file =  here("data/tidy/ArticlesRemovedInRevisionSearch.csv")
  )

# Create the templates for the new qualitative assessment:
blank_sheet_new_quali_assessment <- raw_data_unique_v2 %>%
  dplyr::filter(!`Article Title` %in% old_quali$`Article Title`) %>%
  dplyr::select(any_of(names(old_quali))) %>%
  dplyr::mutate(DOIlink = paste0("https://www.doi.org/", DOI), Sample="Revision1") %>%
  dplyr::mutate(DOIlink = ifelse(stringr::str_detect(DOIlink, "NA"), "", DOIlink))

openxlsx::write.xlsx(
  x = blank_sheet_new_quali_assessment,
  file = here("data/manual/QualReview-Revision-BLANK.xlsx")
  )
