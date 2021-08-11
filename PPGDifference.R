library(nbastatR)
library(tidyverse)
library(gt)
library(psych)
NBAData <- bref_players_stats(tables = c("per_game"), seasons = 2020:2021)
NBAData <- NBAData %>% 
  select(yearSeason, namePlayer, countGames, ptsPerGame)

df <- NBAData %>%
  filter(countGames>25) %>%
  select(yearSeason, namePlayer, ptsPerGame) %>%
  pivot_wider(names_from = yearSeason, 
              values_from =  ptsPerGame,
              names_glue = "{yearSeason}") %>%
  mutate(difference =  `2021` - `2020`) %>%
  filter(!is.na(difference)) %>%
  arrange(desc(difference)) %>%
  headTail(top=10, bottom=10, ellipsis = FALSE) %>%
  `colnames<-` (c("Player", "2020", "2021", "Difference")) %>%
  gt %>%
    tab_header(
      title = md("Points Per Game Difference"),
      subtitle = md(
        "2020-2021 Regular Season"
      )
    ) %>%
  fmt_number(
    columns = c(`2020`:`2021`),
    decimals = 1,
    use_seps = T
  ) %>% 
  tab_options(
    table.background.color = "white",
    column_labels.font.size = 12,
    column_labels.font.weight = 'bold',
    row_group.font.weight = 'bold',
    row_group.background.color = "#E5E1D8",
    table.font.size = 10,
    heading.title.font.size = 22,
    heading.subtitle.font.size = 10,
    table.font.names = "Franklin Gothic Medium",
    data_row.padding = px(8),
    footnotes.padding = px(.5)
  )  %>%
  tab_source_note(source_note = md("Table: @egecinar")) %>%
  data_color(
    columns = Difference,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(palette = "Redmonder::dPBIRdGn",
                                       direction = 1) %>% as.character(),
      domain = NULL,
      na.color = "#000000"
    )
  ) %>% 
  gtsave("proje.png")
