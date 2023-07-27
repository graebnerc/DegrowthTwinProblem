here::i_am("R/Figures-MethodologyNarratives.R")
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(icaeDesign)
source(here("R/analysis_functions.R"))
source(here("R/setup_data.R"))
here::i_am("R/Figures-MethodologyNarratives.R")

translation_vec <- c(
  "Empirical - quanti"="Quantitative",
  "Empirical - quali"="Qualitative",
  "Empirical - case study"="Case Study",
  "Empirical - MixedMethods"="Mixed Methods",
  "Theoretical"="Theoretical")
level_vec <- c(
  "Quantitative", "Mixed Methods", "Case Study", "Qualitative", "Theoretical")

# Main narratives (section 3)---------------
perc_color <- "white" # Font color within bars
translation_vec_narratives <- c(
  "Empirical - quanti"="Quantitative",
  "Empirical - quali"="Qualitative",
  "Empirical - case study"="Case Study",
  "Empirical - MixedMethods"="Mixed Methods",
  "Empirical - MixedMethods"="Case Study",
  "Theoretical"="Theoretical")

level_vec_narratives <- c(
  "Synergies", "Challenges")

perc_label_y <- 1.08 # height for percentage label
n_label_y <- 0.92 # height for number of papers label

## Data----------------------
narrative_plot_data <- eval_core %>%
  dplyr::rename(Narrative = `SecStageCore Classification  (Narrative)`) %>%
  dplyr::select(Narrative) %>%
  dplyr::filter(
    !is.na(Narrative)
  ) %>%
  dplyr::mutate(
    Narrative = as.character(Narrative),
    Narrative = ifelse(
      test = Narrative == "Commonalities",
      yes = "Synergies",
      no = "Challenges"
    ),
    Narrative = factor(Narrative)
  ) %>%
  dplyr::group_by(Narrative) %>%
  dplyr::tally() %>%
  dplyr::mutate(prop=n/sum(.$n)) %>%
  dplyr::mutate(lab=paste0(round(prop*100, 1), "%")) %>%
  dplyr::mutate(Main="Orientation")

## Plot----------------------
narrative_plot <- ggplot(
  data = narrative_plot_data, aes(
    x=Main, y=prop,
    fill=Narrative
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  annotate(
    geom = "text",
    y=filter(narrative_plot_data, Narrative=="Synergies")$prop / 2,
    x = perc_label_y, color = perc_color, hjust = "center",
    label=filter(narrative_plot_data, Narrative=="Synergies")$lab
  ) +
  annotate(
    geom = "text",
    y=filter(narrative_plot_data, Narrative=="Synergies")$prop / 2,
    x = n_label_y, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(narrative_plot_data, Narrative=="Synergies")$n,
      ")"
    )
  ) +
  annotate(
    geom = "text",
    y = filter(narrative_plot_data, Narrative=="Synergies")$prop +
      (filter(narrative_plot_data, Narrative=="Challenges")$prop / 2),
    x = perc_label_y, color = perc_color, hjust = "center",
    label=filter(narrative_plot_data, Narrative=="Challenges")$lab
  ) +
  annotate(
    geom = "text",
    y = filter(narrative_plot_data, Narrative=="Synergies")$prop +
      (filter(narrative_plot_data, Narrative=="Challenges")$prop / 2),
    x = n_label_y, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(narrative_plot_data, Narrative=="Challenges")$n,
      ")"
    )
  ) +
  labs(title = "", y = "Share") +
  scale_y_continuous(expand = expansion(), labels = scales::percent_format()) +
  scale_fill_euf(palette = "mixed", aesthetics = c("color", "fill"), drop=FALSE) +
  labs(title = "Main narratives of core papers") +
  coord_flip() +
  theme_icae() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size=8),
    axis.text.x = element_text(size=8),
    legend.position = "right"
  )
narrative_plot

ggsave(
  plot = narrative_plot,
  filename = here("output/narratives-core.pdf"),
  height = 1.8,
  width = 6.25)

# Narratives and their methodology-----------

## Data--------------------------------
narrative_methodology_reldata <- eval_core %>%
  dplyr::rename(Methodology = `SecStageCore Classification  (Methodology)`) %>%
  dplyr::rename(Narrative = `SecStageCore Classification  (Narrative)`) %>%
  dplyr::filter(Narrative %in% c("Challenges", "Commonalities", "Other")) %>%
  dplyr::mutate(
    Narrative = as.character(Narrative),
    Narrative = ifelse(
      test = Narrative == "Commonalities",
      yes = "Synergies",
      no = "Challenges"
    ),
    Narrative = factor(Narrative)
  ) %>%
  dplyr::filter(!is.na(Methodology), !is.na(Narrative)) %>%
  dplyr::mutate(Methodology = recode(Methodology, !!!translation_vec)
  ) %>%
  dplyr::mutate(Methodology = factor(
    Methodology, levels = level_vec)) %>%
  dplyr::select(all_of(c("Methodology", "Narrative"))) %>%
  dplyr::group_by(Methodology, Narrative) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Narrative) %>%
  dplyr::mutate(n_group=sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prop=n/n_group)%>%
  dplyr::mutate(lab=paste0(round(prop*100, 1), "%")) %>%
  dplyr::mutate(Main="Orientation")
narrative_methodology_reldata

## Upper plot: Main methodologies in the core sample---------------------
perc_color <- "white" # Font color within bars
perc_label_y_2 <- 1.1
n_label_y_2 <- 0.9
correction_last <- 0.2

method_plot_data <- eval_core %>%
  dplyr::rename(Methodology = `SecStageCore Classification  (Methodology)`) %>%
  dplyr::select(Methodology) %>%
  dplyr::filter(
    !is.na(Methodology)
  ) %>%
  dplyr::group_by(Methodology) %>%
  dplyr::tally() %>%
  dplyr::mutate(prop=n/sum(.$n)) %>%
  dplyr::mutate(lab=paste0(round(prop*100, 1), "%")) %>%
  dplyr::mutate(Methodology = recode(Methodology, !!!translation_vec)
  ) %>%
  dplyr::mutate(Methodology = factor(
    Methodology, levels = level_vec)) %>%
  dplyr::arrange(Methodology) %>%
  dplyr::mutate(Main="Orientation", cumprop = cumsum(prop))

method_plot <- ggplot(
  data = method_plot_data, aes(
    x=Main, y=prop,
    fill=Methodology
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop / 2,
    x = perc_label_y_2, color = perc_color, hjust = "center",
    label=filter(method_plot_data, Methodology=="Theoretical")$lab
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop / 2,
    x = n_label_y_2, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      label=filter(method_plot_data, Methodology=="Theoretical")$n,
      ")"
    )
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop / 2,
    x = perc_label_y_2, color = perc_color, hjust = "center",
    label=filter(method_plot_data, Methodology=="Qualitative")$lab
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop / 2,
    x = n_label_y_2, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      label=filter(method_plot_data, Methodology=="Qualitative")$n,
      ")"
    )
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop +
      filter(method_plot_data, Methodology=="Case Study")$prop / 2,
    x = perc_label_y_2, color = perc_color, hjust = "center",
    label=filter(method_plot_data, Methodology=="Case Study")$lab
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop +
      filter(method_plot_data, Methodology=="Case Study")$prop / 2,
    x = n_label_y_2, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      label=filter(method_plot_data, Methodology=="Case Study")$n,
      ")"
    )
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop +
      filter(method_plot_data, Methodology=="Case Study")$prop +
      filter(method_plot_data, Methodology=="Mixed Methods")$prop / 2 - 0.02,
    x = perc_label_y_2+correction_last, color = perc_color, hjust = "center",
    label=filter(method_plot_data, Methodology=="Mixed Methods")$lab
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop +
      filter(method_plot_data, Methodology=="Case Study")$prop +
      filter(method_plot_data, Methodology=="Mixed Methods")$prop / 2 - 0.02,
    x = n_label_y_2+correction_last, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      label=filter(method_plot_data, Methodology=="Mixed Methods")$n,
      ")"
    )
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop +
      filter(method_plot_data, Methodology=="Case Study")$prop +
      filter(method_plot_data, Methodology=="Mixed Methods")$prop +
      filter(method_plot_data, Methodology=="Quantitative")$prop / 2 - 0.02,
    x = perc_label_y_2-correction_last, color = perc_color, hjust = "center",
    label=filter(method_plot_data, Methodology=="Quantitative")$lab
  ) +
  annotate(
    geom = "text",
    y=filter(method_plot_data, Methodology=="Theoretical")$prop +
      filter(method_plot_data, Methodology=="Qualitative")$prop +
      filter(method_plot_data, Methodology=="Case Study")$prop +
      filter(method_plot_data, Methodology=="Mixed Methods")$prop +
      filter(method_plot_data, Methodology=="Quantitative")$prop / 2 - 0.02,
    x = n_label_y_2-correction_last, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      label=filter(method_plot_data, Methodology=="Quantitative")$n,
      ")"
    )
  ) +
  labs(title = "", y = "Share") +
  scale_y_continuous(expand = expansion(), labels = scales::percent_format()) +
  scale_fill_euf(palette = "mixed", aesthetics = c("color", "fill"), drop=FALSE) +
  labs(title = "Main methodologies of core papers") +
  coord_flip() +
  theme_icae() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size=8),
    axis.text.x = element_text(size=8),
    legend.position = "right"
  )
method_plot

## Lower plot: Narratives and their methodology------------
narrative_methodology <- ggplot(
  data = narrative_methodology_reldata, mapping = aes(fill=Methodology, x=Narrative, y=prop)) +
  labs(y = "Number of publications", title = "Narratives and their methodology") +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_euf(palette = "mixed", aesthetics = c("color", "fill"), drop=FALSE) +
  scale_y_continuous(expand = expansion(add = c(0, 0.025)), labels = scales::percent_format()) +
  scale_x_discrete(drop=FALSE) +
  annotate(# Challenges - Theoretical
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical"
      )$prop / 2,
    x = perc_label_y, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$lab
  ) +
  annotate(# Challenges - Theoretical
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop / 2,
    x = n_label_y, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Theoretical")$n,
      ")"
    )
  ) +
  annotate(# Synergies - Theoretical
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical"
    )$prop / 2,
    x = perc_label_y+1, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical")$lab
  ) +
  annotate(# Synergies - Theoretical
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical")$prop / 2,
    x = n_label_y+1, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Theoretical")$n,
      ")"
    )
  ) +
  annotate(# Challenges - Qualitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical"
    )$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative"
        )$prop / 2+0.17),
    x = perc_label_y, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Qualitative")$lab
  ) +
  annotate(# Challenges - Qualitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical"
    )$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop / 2+0.175),
    x = n_label_y, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$n,
      ")"
    )
  ) +
  annotate(# Synergies - Qualitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical")$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Qualitative"
        )$prop / 2) + 0.0,
    x = perc_label_y+1, color = perc_color, hjust = "center", #perc_color
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Qualitative")$lab
  ) +
  annotate(# Synergies - Qualitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Qualitative")$prop / 2+ 0.0,
    x = n_label_y+1, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Qualitative")$n,
      ")"
    )
  ) +
  annotate(# Synergies - Case study
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical")$prop +
      filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Qualitative")$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Case Study"
      )$prop / 2 + 0.0),
    x = perc_label_y+1, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Case Study")$lab
  ) +
  annotate(# Synergies - Case study
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Theoretical")$prop +
      filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Qualitative")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Case Study")$prop / 2 - 0.0,
    x = n_label_y+1, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Case Study")$n,
      ")"
    )
  ) +
  annotate(# Challenges - quantitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative"
      )$prop / 2+0.17),
    x = perc_label_y, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Quantitative")$lab
  ) +
  annotate(# Challenges - quantitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative")$prop / 2 + 0.17,
    x = n_label_y, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative")$n,
      ")"
    )
  ) +
  annotate(# Challenges - Case Study
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative"
      )$prop / 2-0.12),
    x = perc_label_y, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Case Study")$lab
  ) +
  annotate(# Challenges - Case Study
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative"
        )$prop / 2 - 0.12,
    x = n_label_y, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Case Study")$n,
      ")"
    )
  ) +
  annotate(# Synergies - quantitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative"
      )$prop / 2) + 0.165,
    x = perc_label_y+1.25, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Quantitative")$lab
  ) +
  annotate(# Synergies - quantitative
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative")$prop / 2 + 0.165,
    x = n_label_y+1.25, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Quantitative")$n,
      ")"
    )
  ) +
  annotate(# Synergies - Mixed Methods
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      (filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative"
      )$prop / 2) + 0.165,
    x = perc_label_y+0.75, color = perc_color, hjust = "center",
    label=filter(
      narrative_methodology_reldata,
      Narrative=="Synergies",
      Methodology=="Mixed Methods")$lab
  ) +
  annotate(# Synergies - Mixed Methods
    geom = "text",
    y=filter(
      narrative_methodology_reldata,
      Narrative=="Challenges",
      Methodology=="Theoretical")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Qualitative")$prop +
      filter(
        narrative_methodology_reldata,
        Narrative=="Challenges",
        Methodology=="Quantitative")$prop / 2 + 0.165,
    x = n_label_y+0.75, color = perc_color, hjust = "center",
    label=paste0(
      "(n=",
      filter(
        narrative_methodology_reldata,
        Narrative=="Synergies",
        Methodology=="Mixed Methods")$n,
      ")"
    )
  ) +
  coord_flip() +
  theme_icae() +
  theme(
    axis.title = element_blank(),
    legend.text = element_text(size=8),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=10)
  )
narrative_methodology

# Separated plots--------------

methodology_narrative_plot <- ggpubr::ggarrange(
  method_plot, narrative_methodology,
  nrow = 2, labels = c("A)", "B)"), heights = c(1.1, 2)
)

ggsave(
  plot = methodology_narrative_plot,
  filename = here("output/narratives-methodology-core.pdf"),
  height = 4.8,
  width = 6.25)

# TODO Really center the legend; to this end it seems to be necessary
# to extract it using get_legend() and then add it
