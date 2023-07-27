euf_blue <- "#00395B"

#' Save a table as latex file
save_table <- function(table_, file_name){
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  print(xtable(table_),
        floating = FALSE,
        include.rownames = FALSE,
        booktabs = TRUE,
        sanitize.colnames.function=bold,
        file = file_name)
}

#' Create an author ranking
#'
#' Returns a ranking of the authors: for each author the number of articles
#'  on which the author appeared as a co-author is counted
make_author_ranking <- function(x){
  x %>%
    select(Authors) %>%
    separate(col = "Authors", into = as.character(1:30), sep = ";", fill = "right") %>%
    pivot_longer(cols = all_of(as.character(1:30))) %>%
    filter(!is.na(value)) %>%
    select(-all_of("name")) %>%
    mutate(value = stringr::str_trim(value)) %>%
    count(value) %>%
    rename(Author=value, Articles=n) %>%
    dplyr::mutate(Author = str_to_title(Author)) %>%
    arrange(-Articles)
}

#' Create an journal ranking
#'
#' Returns a ranking of the journals: for each journal the number of articles
#'  published is counted
make_journal_ranking <- function(x){
  x %>%
    select(all_of("Source Title")) %>%
    dplyr::mutate(`Source Title` = str_to_title(`Source Title`)) %>%
    count(`Source Title`) %>%
    rename(Journal=`Source Title`, Articles=n) %>%
    arrange(-Articles)
}

#' Create an affiliation ranking
#'
#' Returns a ranking of the affiliations: for each affiliation the number of
#'  articles on which the affiliation appeared is counted
make_affiliation_ranking <- function(x){
  warning("Affiliations column seems to refer only to corresponding author; use address instead")
  x %>%
    select(Affiliations) %>%
    separate(col = "Affiliations", into = as.character(1:45), sep = ";", fill = "right") %>%
    pivot_longer(cols = all_of(as.character(1:45))) %>%
    filter(!is.na(value)) %>%
    select(-all_of("name")) %>%
    mutate(value = stringr::str_trim(value)) %>%
    count(value) %>%
    rename(Affiliation=value, Articles=n) %>%
    mutate(Articles=as.integer(Articles)) %>%
    dplyr::mutate(Affiliation = str_to_title(Affiliation)) %>%
    arrange(-Articles)
}

#' Create an affiliation ranking based on address column
#'
#' Returns a ranking of the affiliations: for each affiliation the number of
#'  articles on which the affiliation appeared is counted; in contrast to the
#'  function `make_affiliation_ranking`, the affiliation is extracted from the
#'  address column
make_affiliation_ranking2 <- function(x){
  warning("This does not work well yet, since affiliations in the address column are spelled in different ways")
  country_names <- unique(c(
    maps::iso3166$ISOname,
    maps::iso3166$mapname,
    maps::iso3166$sovereignty,
    "England"))

  countries_match <- str_c(country_names, collapse = "|")

  # First column: counts nations of the authors
  address_over <- x %>%
    select(all_of("Addresses")) %>%
    separate("Addresses", into = as.character(1:40), sep = "; \\[") %>%
    pivot_longer(cols = all_of(as.character(1:40))) %>%
    filter(!is.na(value)) %>%
    # dplyr::mutate(value=gsub("\\[", "", value), value=gsub("\\]", "", value)) %>%
    select(-all_of("name")) %>%
    mutate(
      value = stringr::str_trim(value),
      authors=str_replace(str_replace(value, "\\[", ""), "(?=\\]).*", ""),
      n_authors=str_count(authors, ";")+1,
      institution=str_extract(value, "(?<=\\]).*"),
      institution=str_to_title(stringr::str_trim(institution))
    ) %>%
    select(all_of(c("value", "n_authors", "institution"))) %>%
    group_by(institution) %>%
    summarise(
      n = sum(n_authors), .groups = "drop"
    ) %>%
    rename(Affiliation=institution, Articles=n) %>%
    mutate(Articles=as.integer(Articles))%>%
    arrange(-Articles)
}

#' Create an funder ranking
#'
#' Returns a ranking of the funders: for each funder the number of
#'  articles on which the funder appears is counted
#'  DOES NOT MAKE MUCH SENSE
make_funding_ranking <- function(x){
  x %>%
    select(all_of("Funding Orgs")) %>%
    separate(col = "Funding Orgs", into = as.character(1:15), sep = ";", fill = "right") %>%
    pivot_longer(cols = all_of(as.character(1:15))) %>%
    filter(!is.na(value)) %>%
    select(-all_of("name")) %>%
    mutate(value = stringr::str_trim(value)) %>%
    count(value) %>%
    rename(Funder=value, Articles=n) %>%
    dplyr::mutate(Funder = str_to_title(Funder)) %>%
    arrange(-Articles)
}

#' Create address ranking
#'
#' Returns two columns: in `Authors`, the home country of the affiliation of
#'  each author is counted. Thus, if two authors come from an institution from
#'  Germany, the counter of Germany is increased by two.
#'  Column `Articles` counts the number of articles for which the country was
#'   involved. Thus, in the case above, the counter for Germany is increased
#'   only by one.
make_address_ranking <- function(x){
  # Prepare country list:
  country_names <- unique(c(
    maps::iso3166$ISOname,
    maps::iso3166$mapname,
    maps::iso3166$sovereignty,
    "England"))

  countries_match <- str_c(country_names, collapse = "|")

  # First column: counts nations of the authors
  address_over <- x %>%
    select(all_of("Addresses")) %>%
    separate("Addresses", into = as.character(1:40), sep = "; \\[") %>%
    pivot_longer(cols = all_of(as.character(1:40))) %>%
    filter(!is.na(value)) %>%
    # dplyr::mutate(value=gsub("\\[", "", value), value=gsub("\\]", "", value)) %>%
    select(-all_of("name")) %>%
    mutate(
      value = stringr::str_trim(value),
      authors=str_replace(str_replace(value, "\\[", ""), "(?=\\]).*", ""),
      n_authors=str_count(authors, ";")+1,
      institution=str_extract(value, "(?<=\\]).*"),
      country = str_extract(institution, countries_match)
    ) %>%
    select(all_of(c("n_authors", "country"))) %>%
    dplyr::filter(!is.na(country))

  address_list <- list()
  for (i in unique(address_over$country)){
    address_list[[i]] <- 0
  }
  for (i in 1:nrow(address_over)){
    address_list[[address_over[i,][["country"]]]] <- address_list[[
      address_over[i,][["country"]]]] + address_over[i,][["n_authors"]]
  }

  address_table <- as_tibble(address_list) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Country",
      values_to = "Authors"
    ) %>%
    mutate(Authors=as.integer(Authors)) %>%
    arrange(-Authors)

  # Second column: country for each article the nb of countries involved:
  y <- select(x, all_of("Addresses"))
  country_count <- c()
  for (i in 1:nrow(x)){
    country_count <- c(country_count, unique(str_extract_all(y[i,][[1]], countries_match)[[1]]))
  }

  country_count <- as_tibble(table(country_count)) %>%
    rename(Articles=n) %>%
    mutate(Articles=as.integer(Articles))

  # Combine and return
  final_table <- full_join(
    address_table,
    country_count,
    by=c("Country"="country_count"))
  return(final_table)
}

#' Make a bar plot of author origins
#'
#' @param address_data_prep Address data as produced by `make_address_ranking`
make_address_plot <- function(address_data_prep){
  address_data_prep %>%
    # pivot_longer(
    #   cols = all_of(c("Authors", "Articles")),
    #   names_to = "Kind",
    #   values_to = "n") %>%
    # dplyr::filter(n>0) %>%
    dplyr::filter(Authors>0) %>%
    ggplot(
      data = .,
      mapping = aes(
        x=reorder(Country, -Authors), #-n
        y=Authors
        #y=n, color=Kind,fill=Kind
        )
    ) +
    geom_bar(
      stat = "identity",
      color=get_euf_colors("blue"),
      fill=get_euf_colors("blue")
      ) + # , position = position_dodge2()
    #coord_flip() +
    #scale_color_brewer(palette = "Set1", aesthetics = c("color", "fill")) +
    scale_y_continuous(expand = expansion(add = c(0, 2))) +
    labs(
      title = "Institutional origins (core sample)",
      y = "Count") +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      #legend.position = "bottom",
      legend.position = "none"
      )
}

#' Create ranking of of authors according to their citations
make_author_citation_ranking <- function(x){
  author_abbrevation <- x %>%
    select(all_of("Authors")) %>%
    separate(col = "Authors", into = as.character(1:30),
             sep = ";", fill = "right") %>%
    mutate(
      across(.cols = everything(), .fns = ~str_extract(.x, "^.*(?=(,))"))
      ) %>%
    unite(col = "Authors", sep = "; ") %>%
    mutate(Authors=str_remove_all(Authors, "; NA")) %>%
    mutate(Authors = ifelse(
      test = str_count(Authors, ";")==0,
      yes = Authors,
      no = ifelse(
        test = str_count(Authors, ";")==1,
        yes = str_replace_all(Authors, "; ", " & "),
        no = str_replace(Authors, ";.*", " et al.")))) %>%
    mutate(Authors=str_squish(Authors))

  author_ranking <- x %>%
    select(all_of(
      c("Times Cited, WoS Core",
        "Times Cited, All Databases", "Publication Year"))
    ) %>%
    rename(`Citations (WoS Core)`=`Times Cited, WoS Core`,
           `Citations`=`Times Cited, All Databases`)

  author_ranking <- cbind(author_abbrevation, author_ranking) %>%
    mutate(
      Authors=paste0(Authors, " (", `Publication Year`, ")"),
      `Citations (WoS Core)` = as.integer(`Citations (WoS Core)`),
      Citations = as.integer(Citations)
      ) %>%
    arrange(-`Citations (WoS Core)`) %>%
    select(-`Publication Year`)

  return(author_ranking)
}

#' Visualize the distribution of citations
#'
#' @param wos Should total citations or citations from WoS core be used?
visualize_citation_distribution <- function(x, wos=FALSE){
  citation_source <- ifelse(
    test = wos, yes = "Citations (WoS Core)", no = "Citations")

  dist_data <- x %>%
    select(all_of(
      c("Times Cited, WoS Core",
        "Times Cited, All Databases", "Publication Year"))
    ) %>%
    rename(`Citations (WoS Core)`=`Times Cited, WoS Core`,
           `Citations`=`Times Cited, All Databases`)

  theme_adjust <- function(p){
    p +
      scale_x_continuous(breaks = seq(0, 160, 20)) +
      theme_bw() +
      theme(
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5))
  }

  histo <- ggplot(data = dist_data, mapping = aes_string(x=citation_source)) +
    geom_histogram(binwidth=5, color=euf_blue, fill=euf_blue) +
    labs(title = "Citations per paper",
         y = "Number of articles") +
    scale_y_continuous(expand = expansion(add = c(0, 0.5)),
                       breaks = seq(0, 10, 2))
  histo <- theme_adjust(histo)

  ecdf <- ggplot(data = dist_data, mapping = aes_string(x=citation_source)) +
    stat_ecdf(geom = "step", pad = TRUE, color=euf_blue) +
    labs(title = "Empirical distribution",
         y = "Share of sample") +
    scale_y_continuous(
      expand = expansion(add = c(0, 0.05)),
      limits = c(0,1),
      labels = scales::percent_format(scale = 100))
  ecdf <- theme_adjust(ecdf)

  full_plot <- ggpubr::ggarrange(histo, ecdf, ncol = 2, labels = c("a)", "b)"))
  full_plot
}

