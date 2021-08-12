library(nbastatR)
library(tidyverse)
library(psych)
library(gt)

NBAData <- bref_players_stats(seasons = 2020:2021, tables = c("per_game"))
NBAData <- NBAData %>%
  select(yearSeason, namePlayer, countGames, ptsPerGame, urlPlayerHeadshot)

df <- NBAData %>%
  filter(countGames>25) %>%
  select(urlPlayerHeadshot, yearSeason, namePlayer, ptsPerGame) %>%
  pivot_wider(names_from = yearSeason, 
              values_from =  ptsPerGame,
              names_glue = "{yearSeason}") %>%
  mutate(change =  `2021` - `2020`) %>%
  filter(!is.na(change)) %>%
  arrange(desc(change)) %>%
  headTail(top=10, bottom=10, ellipsis = FALSE) %>%
 `colnames<-` (c(" ", "Player", "2020", "2021", "Change")) %>%
  gt %>%
  text_transform(
    locations = cells_body(
      c(" ")
    ),
    fn = function(x) {
      web_image(
        url = x,
        height = 35
      )
    }
  ) %>%
    tab_header(
    title = md("**Change in Points Per Game**"),
    subtitle = md(
      "2020-2021 Regular Seasons"
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
  tab_source_note(source_note = md("Table: @egecinar3")) %>%
  data_color(
    columns = Change,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(palette = "Redmonder::dPBIRdGn",
                                       direction = 1) %>% as.character(),
      domain = NULL,
    )
  ) %>%
  gtsave("project.png")






