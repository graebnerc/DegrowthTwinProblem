here::i_am("R/setup_data.R")
library(here)
library(readxl)
library(dplyr)
library(data.table)

# Import the WoS raw result data (excl. cited refs)--------
# File with information about what the relevant columns mean:
info_sheet <- read_xlsx(
  path = here("data/manual/WoS-VariablesOfInterest.xlsx"),
  col_names = TRUE)
rel_cols <-  info_sheet %>%
  dplyr::filter(!is.na(DescriptiveAnalysis)) %>%
  dplyr::pull(Column)
rel_cols <- c(rel_cols, "Authors", "Author Full Names", "Article Title")

# The file containing the result from the WoS search
wos_result <- as_tibble(
  x = fread(here("data/tidy/WoS-RawSampleUnique.csv"))
  ) %>%
  dplyr::select(all_of(rel_cols))

# Articles that were removed in the updated search
#  For each article there is reason, e.g. that in the first search it was
#  accidentally included because Commentaries were not properly sorted out
articles_removed_rev_search <- fread(
  file = here("data/tidy/ArticlesRemovedInRevisionSearch.csv"))

# The excel sheet containing our qualitative assessment
evaluation_data <- read_xlsx(here("data/manual/Qual-review-Rev.xlsx")) %>%
  filter(!`Article Title` %in% articles_removed_rev_search$`Article Title`)

# The papers obtained during the informal search
informal_additions <- read_xlsx(
  path = here("data/manual/QualReview-Rev-InformalSample.xlsx")
) %>%
  as_tibble() %>%
  filter(!is.na(`Article Title`)) %>%
  dplyr::mutate(Sample="InformalSearch")

# The publications from the cited references sample
evaluation_cited_refs <- read_xlsx(
  path = here("data/manual/Qual-review-CitationSample.xlsx"))

# Create final sample file-----------------------
wos_sample_full <- full_join(
  wos_result, evaluation_data, by=c("Article Title")) %>%
  dplyr::select(all_of(c(
    "Authors", "Publication Year", "Article Title",
    "SecStageCore Classification  (Narrative)",
    "SecStageCore Classification  (Methodology)",
    "Assessment", "Addresses", "Affiliations", "Source Title", "Sample"
  ))) %>%
  filter(!`Article Title` %in% articles_removed_rev_search$`Article Title`) %>%
  bind_rows(., informal_additions %>%
              dplyr::select(all_of(c(
                "Authors", "Publication Year", "Article Title",
                "SecStageCore Classification  (Narrative)",
                "SecStageCore Classification  (Methodology)",
                "Assessment", "Addresses", "Affiliations", "Source Title", "Sample"
              )))) %>%
  dplyr::filter(Assessment != "Already considered")

if (sum(is.na(wos_sample_full$Assessment)) > 0){
  papers_wo_assessment <- sum(is.na(wos_sample_full$Assessment))
  warning(paste0(
    "There are ", papers_wo_assessment, " papers without an assessment!"))
  dplyr::filter(wos_sample_full, is.na(Assessment))
}

cited_refs_core_papers <- evaluation_cited_refs %>%
  dplyr::filter(Assessment == "Core") %>%
  dplyr::select(all_of(names(wos_sample_full)))

cited_refs_relevant_papers <- evaluation_cited_refs %>%
  dplyr::filter(Assessment == "Relevant") %>%
  dplyr::select(all_of(names(wos_sample_full)))

# Create core and relevant sample--------------------------

## Create the core sample-------------------------
eval_core_base <- wos_sample_full %>%
  dplyr::filter(Assessment=="Core") %>%
  rbind(cited_refs_core_papers) %>%
  dplyr::mutate(across(.cols = everything(), .fns = factor))

core_w_missing_info <- eval_core_base %>%
  filter(is.na(`Source Title`)) %>%
  select(all_of(c("Article Title", "Sample")))

core_missing_info_added <- read_xlsx(
  path = here("data/manual/Additional-Info-CoreSample.xlsx")) %>%
  select(any_of(names(eval_core_base))) %>%
  left_join(., core_w_missing_info, by="Article Title")

eval_core <- eval_core_base %>%
  filter(!`Article Title` %in% core_w_missing_info$`Article Title`) %>%
  bind_rows(core_missing_info_added)

if (nrow(eval_core_base) != nrow(eval_core)){
  warning("Problem occured when adding missing info to core sample!")
}

## Create the relevant sample
eval_relevant_base <- wos_sample_full %>%
  dplyr::filter(Assessment%in%c("Relevant")) %>%
  rbind(cited_refs_relevant_papers) %>%
  dplyr::mutate(across(.cols = dplyr::everything(), .fns = factor))

relevant_w_missing_info <- eval_relevant_base %>%
  filter(is.na(Authors)) %>%
  select(all_of(c("Article Title", "Assessment", "Sample"))) %>%
  mutate(`Article Title` = as.character(`Article Title`))

relevant_missing_info_added <- read_xlsx(
  path = here("data/manual/Additional-Info-RelevantSample.xlsx")) %>%
  select(any_of(names(eval_relevant_base))) %>%
  mutate(`Article Title` = as.character(`Article Title`)) %>%
  full_join(., relevant_w_missing_info, by="Article Title") %>%
  mutate(`Publication Year` = factor(`Publication Year`))

eval_relevant <- eval_relevant_base %>%
  filter(!`Article Title` %in% relevant_w_missing_info$`Article Title`) %>%
  bind_rows(relevant_missing_info_added)

if (nrow(eval_relevant_base) != nrow(eval_relevant)){
  warning("Problem occured when adding missing info to relevant sample!")
}

eval_relevant <- eval_relevant %>% bind_rows(eval_core)


# Summary------------------------------
n_init_sample <- nrow(wos_result)

n_informal_additions <- nrow(informal_additions)
informal_additions_already_there <- dplyr::filter(
  informal_additions, Assessment == "Already considered") %>% nrow()

dropped_abstract_screening <- nrow(
  filter(evaluation_data, # TODO
         Sample=="WoS",
         Assessment=="Dropped after abstract screening"))
dropped_fulltext_screening <- nrow(
  filter(evaluation_data,
         Sample=="WoS",
         Assessment=="Dropped after full text screening"))
inaccessible_papers <- nrow(
  filter(evaluation_data,
         Sample=="WoS",
         Assessment=="No access"))

prelim_core_n <- nrow(
  filter(evaluation_data, Assessment == "Core", Sample=="WoS"))
prelim_relevant_n <- nrow(
  filter(evaluation_data, Assessment == "Relevant", Sample=="WoS"))

## Cited references-------------------------
cited_refs_considered <- nrow(evaluation_cited_refs)
cited_refs_already_there <- nrow(
  filter(evaluation_cited_refs, Assessment == "Already considered"))
cited_refs_relevant <- nrow(cited_refs_relevant_papers)
cited_refs_irrelevant <- nrow(
  filter(evaluation_cited_refs, Assessment == "Irrelevant"))
cited_refs_inaccessible <- nrow(
  filter(evaluation_cited_refs, Assessment == "No access"))
cited_refs_core <- nrow(cited_refs_core_papers)

## Informal search-------------------------------
informal_cores <- informal_additions %>%
  filter(Assessment != "Already considered", Assessment=="Core") %>%
  nrow()

informal_relevants <- informal_additions %>%
  filter(Assessment != "Already considered", Assessment=="Relevant") %>%
  nrow()

informal_refs_inaccessible <-  informal_additions %>%
  filter(Assessment != "Already considered", Assessment=="No access") %>%
  nrow()
## Papers from Tim's blog-------------------
timmothyparrique_screened <- evaluation_data %>%
  filter(Sample=="TimotheeParrique")

tm_abstract_screened <- nrow(timmothyparrique_screened)
tm_drop_after_abstract <- timmothyparrique_screened %>%
  filter(Assessment=="Dropped after abstract screening") %>%
  nrow()
tm_inaccessible_after_abstract <- timmothyparrique_screened %>%
  filter(Assessment=="No access") %>%
  nrow()
tm_relevant <- timmothyparrique_screened %>%
  filter(Assessment=="Relevant")%>%
  nrow()
tm_core <- timmothyparrique_screened %>%
  filter(Assessment=="Core")%>%
  nrow()
tm_drop_after_fulltext <- timmothyparrique_screened %>%
  filter(Assessment=="Dropped after full text screening") %>%
  nrow()
tm_full_text_screened <- tm_relevant+tm_core+tm_drop_after_fulltext

## Final core--------------------------

n_core_sample <- nrow(eval_core)
n_relevant_sample <- nrow(eval_relevant)

if (n_core_sample !=  (prelim_core_n + informal_cores + cited_refs_core + tm_core)){
  warning("Inconsistency with core papers!!!")
}

if ((n_relevant_sample - n_core_sample) != (
  prelim_relevant_n + informal_relevants + cited_refs_relevant + tm_relevant)){
  warning("Inconsistency with relevant papers!!!")
}

save(list = c(
  "n_init_sample", "n_informal_additions", "informal_additions_already_there",
  "dropped_abstract_screening", "dropped_fulltext_screening",
  "prelim_core_n", "prelim_relevant_n", "cited_refs_considered",
  "cited_refs_already_there", "cited_refs_relevant", "cited_refs_irrelevant",
  "cited_refs_core", "informal_cores", "informal_relevants", "n_core_sample",
  "n_relevant_sample", "cited_refs_inaccessible", "informal_refs_inaccessible",
  "tm_abstract_screened", "tm_drop_after_abstract", "tm_inaccessible_after_abstract",
  "tm_inaccessible_after_abstract", "tm_relevant", "tm_core", "tm_full_text_screened",
  "tm_drop_after_fulltext", "inaccessible_papers"),
  file = here("data/tidy/sample_summary.Rdata"))

rm(list = setdiff(ls(), c("eval_core", "eval_relevant")))
