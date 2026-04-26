# prepare data

## load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(quarto)

## read data
tracks = read.csv("./data/tracks.csv")
songs = read.csv("./data/songs.csv")
maps = read.csv("./data/maps.csv")
locations = read.csv("./data/locations.csv")

## recode Shal'Dun
maps = maps |>
  mutate(
    mapNameEN = ifelse(map == 'P110_Shaldun.map', 'Shal Dun', mapNameEN),
    mapNameEN = ifelse(map == '018_farlorns_hope.map', 'Farlorns Hope', mapNameEN),
    mapNameDE = ifelse(map == 'P110_Shaldun.map', 'Shal Dun', mapNameDE)
  )

## merge data
tableData = bind_rows(
  maps |> select(map, mapSort, mapNameEN, mapNameDE, campaign, openerId, loopId),
  locations |> select(map, openerId, loopId) |>
    left_join(
      maps |> select(map, mapSort, mapNameEN, mapNameDE, campaign),
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
  arrange(campaignSong, file, mapSort) |>
  summarise(
    mapsEN = ifelse(sum(!is.na(map) > 0), paste0('<a href = ', map |> na.omit(), '.html>', mapNameEN |> na.omit(), '</a>', collapse = '<br>'), ''),
    mapsDE = ifelse(sum(!is.na(map) > 0), paste0('<a href = ', map |> na.omit(), '.html>', mapNameDE |> na.omit(), '</a>', collapse = '<br>'), ''),
    .by = c(campaignSong, file, youtubeCode)
  ) |>
  mutate(
    campaignSong = campaignSong |> factor(labels = c('The Order of Dawn', 'The Breath of Winter', 'Shadow of the Phoenix')),
    youtubeLink = ifelse(youtubeCode == '', '', paste0('<a href="https://www.youtube.com/watch?v=', youtubeCode,'">Link</a>'))
  )

mapSongs = maps |>
  left_join(
    tracks |> left_join(songs) |> select(id, file, youtubeCode),
    by = join_by(openerId == id)
  ) |>
  left_join(
    tracks |> left_join(songs) |> select(id, file, youtubeCode),
    by = join_by(loopId == id),
    suffix = c('Opener', 'Loop')
  ) |>
  select(-mapSort, -campaign, -openerId, -loopId)

locationSongs = locations |>
  left_join(
    maps |> select(map, length),
    by = 'map'
  ) |> left_join(
    tracks |> left_join(songs) |> select(id, file, youtubeCode),
    by = join_by(openerId == id)
  ) |>
  left_join(
    tracks |> left_join(songs) |> select(id, file, youtubeCode),
    by = join_by(loopId == id),
    suffix = c('Opener', 'Loop')
  ) |>
  select(-openerId, -loopId) |>
  
  ## add circle polygons
  rowwise() |>
  mutate(
    geometry = lapply(
      0:359, function(d) { # d = degrees of circle
        
        point.x = sin(d / 360 * 2 * pi) * radius + x
        point.y = cos(d / 360 * 2 * pi) * radius + y
        
        st_point(
          c(
            case_when( # cut off circle at maps edge
              point.x < 1 ~ 1,
              point.x > length ~ length,
              .default = point.x
            ),
            case_when(
              point.y < 1 ~ 1,
              point.y > length ~ length,
              .default = point.y
            )
          )
        )
        
      }
    ) |>
      st_as_sfc() |>
      st_combine() |>
      st_cast('POLYGON')
  ) |>
  st_as_sf() |>
  ungroup() |>
  
  ## union circles with same songs
  summarise(
    geometry = st_union(geometry),
    .by = c(map, fileOpener, youtubeCodeOpener, fileLoop, youtubeCodeLoop)
  ) |>
  
  ## recode popups
  mutate(
    popup = paste0(
      ifelse(fileOpener == fileLoop, '', 'Opener: '), '<span style="font-weight: bold">', fileOpener, '</span><br>', ifelse(str_detect(fileOpener, 'silence|atmo'), '', paste0(
      '<iframe width="300" height="200" src="https://www.youtube.com/embed/', youtubeCodeOpener,
      '" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen>
      </iframe><br>')),
    ifelse(fileOpener == fileLoop, '', paste0('Loop: <span style = "font-weight: bold">', fileLoop, '</span><br>', ifelse(str_detect(fileLoop, 'silence|atmo'), '', paste0(
      '<iframe width="300" height="200" src="https://www.youtube.com/embed/', youtubeCodeLoop,
      '" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen>
        </iframe>'))))
    )
  )

# save data (as the global environment is not accessible within a quarto render)
save(tableData, mapSongs, locationSongs, file = './data/data.Rdata')

# render index and map files
## index
quarto_render(input = "index.qmd", profile = 'english')
quarto_render(input = "index.qmd", profile = 'german')

## loop through all maps
for (i in 1:nrow(maps)) {
  quarto_render(
    input = "mapTemplate.qmd",
    output_file = paste0(maps[i, 'map'], '.html'),
    execute_params = list(map = maps[i, 'map']),
    profile = 'english'
  )
  
  quarto_render(
    input = "mapTemplate.qmd",
    output_file = paste0(maps[i, 'map'], '.html'),
    execute_params = list(map = maps[i, 'map']),
    profile = 'german'
  )
}
