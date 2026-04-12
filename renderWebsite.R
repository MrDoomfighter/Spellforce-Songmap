# prepare data

## load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(quarto)
library(leaflet)

## read data
tracks = read.csv("./data/tracks.csv")
songs = read.csv("./data/songs.csv")
maps = read.csv("./data/maps.csv")
locations = read.csv("./data/locations.csv")

## merge data
tableData = bind_rows(
  maps |> select(map, mapSort, mapNameDE, campaign, openerId, loopId),
  locations |> select(map, openerId, loopId) |>
    left_join(
      maps |> select(map, mapSort, mapNameDE, campaign),
      by = 'map'
    )
) |>
  pivot_longer(
    cols = c(openerId, loopId),
    names_to = NULL,
    values_to = 'id'
  ) |>
  full_join(
    tracks |> select(id, file),
    by = 'id'
  ) |>
  left_join(
    songs,
    by = 'file',
    suffix = c('Map', 'Song') # suffix for campaign variable
  ) |>
  select(-id) |>
  distinct() |>
  filter(!is.na(file) & str_detect(file, 'silence|red_legion.mp3', negate = TRUE)) |>
  arrange(file, mapSort) |>
  group_by(campaignSong, file, youtubeCode) |>
  summarise(maps = ifelse(sum(!is.na(map) > 0), paste0('<a href = ', map |> na.omit(), '.html>', mapNameDE |> na.omit(), '</a>', collapse = '<br>'), '')) |>
  ungroup() |>
  mutate(
    campaignSong = campaignSong |> factor(labels = c('The Order of Dawn', 'The Breath of Winter', 'Shadow of the Phoenix')),
    youtubeLink = ifelse(youtubeCode == '', '', paste0('<a href="https://www.youtube.com/watch?', youtubeCode,'">Link</a>'))
  )

# save data (as the global environment is not accessible within a quarto render)
save(tableData, file = './data/data.Rdata')

# render index and map files
quarto_render(input = "index.qmd")
