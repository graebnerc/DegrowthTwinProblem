# Create a txt with all the cited references of the new sample
here::i_am("R/setup_cited_references.R")
library(here)
library(data.table)
library(tidyr)
library(tibble)
library(stringr)

# Import and merge tab-delimited data----------------------
languages <- c("en", "dt", "es", "fr", "pt")
raw_data <- list()
for (i in seq_along(languages)){
  current_language <- languages[i]
  language_file <- fread(file = here(paste0("data/raw/WoS_", current_language, "/savedrecs.txt")), quote = "")
  language_file_sub <- language_file[, .(AU, TI, SO, DI, UT, CR)]
  raw_data[[current_language]] <- language_file_sub
}

raw_data_full <- rbindlist(raw_data)
raw_data_full_unique <- unique(raw_data_full, by = "UT")

wos_sample <- fread(here("data/tidy/WoS-RawSampleUnique.csv"))

init_sample_1_sub <- raw_data_full_unique[UT %in% wos_sample$`UT (Unique WOS ID)`]

# Extract cited references---------------------------------

# Extract references

rows_considered <- 1:nrow(init_sample_1_sub)
reference_list <- list()
for (i in rows_considered){
  print(paste0(i, "/", max(rows_considered)))
  row_ <- init_sample_1_sub[i, ]

  # Extract the cited references
  refs_i <- str_split(row_$CR, pattern = "; ", simplify = FALSE)
  refs_i <- str_squish(refs_i[[1]])

  # Ignore cases with empty or no cited references:
  kill_contitions <- c(
    !length(refs_i) > 0
  )
  if (length(refs_i)==1){
    if(refs_i==""){
      kill_contitions <- c(kill_contitions, TRUE)
    }
  }

  if (sum(kill_contitions)==0){
    # Remove uncaptured titles (usually do not appear in the real ref list)
    refs_i <- refs_i[refs_i != "[No title captured]"]

    # Extract the surname of the first author
    first_authors_i <- tibble(
      # Problem here: Authors such has "Juntas de buen gobierno" not considered
      # "first_author"=tolower(str_remove(refs_i, " .*$"))
      # Alternative:
      "first_author"=tolower(str_remove(refs_i, ",.*$"))
    ) %>%
      dplyr::mutate(
        first_author=str_remove_all(first_author, "[[:punct:]]")
      )

    # Extract title/source
    pub_id_i <- tibble(
      "pub_id"=purrr::map_chr(
        .x = str_split(refs_i, ",", 3),
        .f = ~str_squish(str_remove(.x[3], ",.*$"))))

    # Extract the publication year
    year_matrix <- str_extract_all(
      string = refs_i, pattern = "[0-9]", simplify = TRUE)
    if (ncol(year_matrix)<4){
      pub_years_i <- tibble("Year" = rep(NA, nrow(pub_id_i)))
    } else {
      pub_years_i <- as_tibble(year_matrix[,1:4],
                               .name_repair = "unique") %>%
        unite(col = "Year", sep = "", remove = TRUE)
    }

    reference_frame <- cbind(first_authors_i, pub_years_i, pub_id_i) %>%
      dplyr::mutate(citing_pub_title=row_[["TI"]])

    implausibles <- reference_frame %>%
      dplyr::filter(
        is.na(pub_id) | nchar(Year)<4 | !str_detect(Year, "^[:digit:]+$") |
          str_detect(first_author, "\\d")
      )

    # Remove implausible entries:
    reference_frame_plausibles <- reference_frame %>%
      dplyr::filter(
        !is.na(pub_id), # There is a publication id
        nchar(Year)==4, # Column year has exactly four characters
        str_detect(Year, "^[:digit:]+$"), # Column year contains only digits
        !str_detect(first_author, "\\d") # Author does not contains digits
      )
    if ( (nrow(reference_frame_plausibles)<=nrow(reference_frame)) &
         (length(refs_i[is.na(pub_id_i$pub_id)]) > 0) ){
      # TODO: Shouldn't this be !is.()!? AND I ADDED SMALLER THAN EQUAL!
      # Add at least those with missing publication id
      # -----------------------------------------------
      # Get vector of missings:
      special_cases <- refs_i[is.na(pub_id_i$pub_id)]
      # Extract the years:
      special_cases_year <- purrr::map_chr(
        .x = str_extract_all(special_cases, "[:digit:]"),
        .f = ~paste(.x, collapse = ""))
      # Remove the years:
      special_cases <- str_remove_all(special_cases, "[:digit:]")

      # Split the remainders by commas:
      remainders <- str_split(special_cases, pattern = ", ", simplify = FALSE)
      # Detect patterns where there are lower letters
      # -> this needs to be authors since sources only consist of upper letters
      subset_author_ids <- purrr::map(.x = remainders, .f = ~str_detect(.x, "[:lower:]"))
      # Detect the patterns that do not have lower letters and are at least of
      #  length 2: this need to be sources
      subset_source_ids <- purrr::map(
        .x = remainders,
        .f = function(.x) !str_detect(.x, "[:lower:]") & nchar(.x)>2)

      # Put those back into the respective vectors; if in doubt, write NA
      remaining_authors <- rep(NA, length(remainders))
      remaining_ids <- rep(NA, length(remainders))
      for (i in seq_along(remainders)){
        rem_author <- remainders[[i]][subset_author_ids[[i]]]
        rem_id <- remainders[[i]][subset_source_ids[[i]]]
        remaining_authors[i] <- ifelse(test = length(rem_author)==1, yes = rem_author, no = NA)
        remaining_ids[i] <- ifelse(test = length(rem_id)==1, yes = rem_id, no = NA)
      }
      # Construct tibble:
      fixed_entries <- tibble(
        "first_author"=tolower(remaining_authors),
        "Year"=special_cases_year,
        "pub_id"=remaining_ids
      ) %>%
        dplyr::mutate(citing_pub_title=row_[["TI"]]) %>%
        dplyr::filter(
          !is.na(pub_id), !is.na(pub_id)
        )

      reference_frame <-rbind(reference_frame_plausibles, fixed_entries)
    }


    reference_list[[row_[["TI"]]]] <- reference_frame
  }
}

full_list <- rbindlist(reference_list)
saveRDS(object = full_list, file = here("data/tidy/cited_articles.Rds"))
