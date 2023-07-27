here::i_am("R/Figures-Descriptives.R")
library(here)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(readxl)
library(stringr)
library(data.table)
library(broom)
library(icaeDesign)
library(ggthemes)
source(here("R/setup_data.R"))
source(here("R/analysis_functions.R"))

here::i_am("R/Figures-Descriptives.R")

# # Set up relevant sample for below-------------------------
relevant_sample <- eval_relevant
core_sample <- eval_core

# Journal ranking - nb. of articles--------------
journal_ranking_core <- make_journal_ranking(core_sample) %>%
  dplyr::filter(Articles>1)
save_table(journal_ranking_core, here("output/journal_core_tab.tex"))

print(xtable(
  x = journal_ranking_core),
  type = "html",
  include.rownames = FALSE,
  file = here("output/journal_core_tab.html"))

# Origins of the articles------------------------
## Address/country ranking--------------
address_data <- make_address_ranking(core_sample)

address_data_plot <- address_data %>%
  #rename(Authors=Authors_core) %>%
  make_address_plot(.)
address_data_plot <- address_data_plot +
  scale_y_continuous(breaks = seq(0, 18, 2), expand = expansion()) +
  labs(title = "Affiliations of the authors of core papers") +
  labs(y = "Articles")
address_data_plot

## Map-----------------

address_data_core <- make_address_ranking(core_sample) %>%
  rename(Authors_core=Authors,
         Articles_core=Articles)

address_data_relevant <- make_address_ranking(relevant_sample) %>%
  rename(Authors_relevant=Authors,
         Articles_relevant=Articles)

address_data <- full_join(
  address_data_relevant, address_data_core, by = "Country") %>%
  as_tibble(.) %>%
  dplyr::mutate(
    Country = ifelse(Country == "USA", "United States of America", Country)
  )

address_data_match <- address_data %>%
  dplyr::mutate(
    Country = ifelse(Country == "United States of America", "USA", ifelse(
      Country == "England", "UK", Country
    ))
  )

world_map <- map_data("world") %>%
  left_join(., address_data_match, by = c("region"="Country"), relationship = "many-to-many") %>%
  dplyr::mutate(across(.cols = c(contains("Authors"), contains("Articles")),
                       .fns = ~ifelse(is.na(.x), 0, .x)))

# world_data_helper <- tidy(world_data_raw)
#
# country_ids <- tibble(
#   "geounit" = world_data_raw@data$geounit,
#   "id" = unique(world_data_helper$id)
# )
#
# world_data_fortified <- world_data_helper %>%
#   left_join(., country_ids, by="id") %>% # , region = "geounit"
#   left_join(., address_data, by = c("geounit"="Country")) %>% # "id"="Country"
#   dplyr::mutate(across(.cols = c(contains("Authors"), contains("Articles")),
#                        .fns = ~ifelse(is.na(.x), 0, .x)))

map_Authors_core <- world_map %>%
  ggplot(aes(fill = Articles_core, map_id = region)) +
  geom_map(map = world_map, color="grey", linewidth=0.1) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_sf() +
  labs(
    title = "Articles with at least one author from a given country") +
  scale_fill_gradient(
    low="white",
    high=get_euf_colors("blue"),
    breaks = seq(0, 18, 2)
  ) +
  guides(fill = guide_colorbar(
    title = "Articles",
    title.position = "top",
    barwidth = 0.75,
    barheight = 8)
  ) +
  theme_map() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size=13)
  )
map_Authors_core

## Full plot (section 2)----------------------------
origins_plot <- ggpubr::ggarrange(
  address_data_plot, map_Authors_core, # method_plot,
  nrow = 2, labels = c("A)", "B)"), heights = c(1, 1))

ggsave(
  plot = origins_plot,
  filename = here("output/origin-plot-core.pdf"),
  height = 4.75,
  width = 5.5)

# Plots with both data sets----------------------------------------------------

## Publication year--------------------

pub_year_full <- relevant_sample %>%
  select(`Publication Year`) %>%
  count(`Publication Year`) %>%
  filter(!is.na(`Publication Year`)) %>%
  mutate(Sample="Full sample")

pub_year_relevant <- relevant_sample %>%
  filter(Assessment == "Relevant") %>%
  select(`Publication Year`) %>%
  count(`Publication Year`) %>%
  filter(
    !is.na(`Publication Year`)) %>%
  mutate(Sample="Relevant publications")

pub_year_core <- relevant_sample %>%
  filter(Assessment == "Core") %>%
  select(`Publication Year`) %>%
  count(`Publication Year`) %>%
  filter(!is.na(`Publication Year`)) %>%
  mutate(Sample="Core publications")

plot_data <- rbind(pub_year_relevant, pub_year_core, pub_year_full)
pub_plot <- ggplot(
    data = plot_data,
    mapping = aes(x=`Publication Year`, y = n, color=Sample, group=Sample)
  ) +
  geom_line(alpha=0.75) + geom_point(alpha=0.75, aes(shape = Sample)) +
  scale_color_euf(palette = "mixed") +
  scale_y_continuous(limits = c(0, 25)) +
  labs(title = "Number of publications per year", y = "Nb. of publications") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave(plot = pub_plot,
       filename = here("output/pub_year_plot.pdf"),
       width = 7, height = 3)

ggsave(plot = pub_plot,
       filename = here("output/pub_year_plot.jpeg"),
       width = 7, height = 3)

# Distribution of the sub-arguments--------------
arg_dist <- read_excel(here("data/manual/Argument-Distribution.xlsx"))
head(arg_dist)

arg_dist_plot <- arg_dist %>%
  group_by(Argument, `Narrative of the argument`) %>%
  summarise(Instances=n(), .groups = "drop") %>%
  mutate(
    Argument = case_when(
      Argument == "Degrowth in the North as decolonization of the South" ~ "Degrowth in the North as\n decolonization of the South",
      Argument == "Growth dependence is problematic everywhere" ~ "Growth dependence is\n problematic everywhere",
      Argument == "South as inspiration, origin and ally" ~ "South as inspiration,\n origin and ally",
      Argument == "Material obstacles due to dependencies" ~ "Material obstacles\n due to dependencies",
      Argument == "Neocolonial agenda-setting" ~ "Neocolonial\n agenda-setting",
      .default = Argument
    )
  ) %>%
  ggplot(
    data = .,
    mapping = aes(
      x=reorder(Argument, Instances), y = Instances,
      fill=`Narrative of the argument`)
  ) +
  geom_bar(stat = "identity",  color="white") +
  coord_flip() +
  scale_fill_euf(palette = "mixed", discrete = TRUE) +
  scale_y_continuous(expand = expansion()) +
  labs(
    title = "Distribution of the sub-arguments in the core sample",
    y = "Nb. of publications that make the argument") +
  guides(fill = guide_legend(
    title = "Narrative of the argument", title.position = "top")) +
  theme_icae() +
  theme(
    legend.title = element_text(),
    legend.position = c(0.8, 0.13),
    legend.background = element_rect(fill = "white", color=NA),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=9),
    axis.text.x = element_text(size=7),
    axis.title.x = element_text(size=9))

ggsave(
  plot = arg_dist_plot,
  filename = here("output/DistributionArguments.pdf"),
  width = 6.25, height = 4)
