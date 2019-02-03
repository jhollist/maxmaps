library(rvest)
library(dplyr)

state_url <- "https://en.wikipedia.org/wiki/List_of_capitals_in_the_United_States"

states_caps <- state_url %>%
  read_html %>%
  html_nodes("table")

states_caps <- states_caps[[1]] %>%
  html_table( fill = TRUE) %>%
  slice(-1)

states_caps <- states_caps[,c(1,2,4)] %>%
  select(state = State, abr = Abr., capital = Capital)

northeast <- c("Maine","New Hampshire", "Vermont", "New York", "Massachusetts",
               "Connecticut", "Rhode Island", "Maryland", "New Jersey",
               "Delaware", "Pennsylvania")
southeast <- c("West Virginia", "Virginia","Kentucky","Tennessee", "North Carolina",
               "South Carolina", "Florida", "Georgia", "Alabama", "Mississippi",
               "Arkansas", "Louisiana")
midwest <- c("Ohio","Michigan","Indiana","Illinois","Wisconsin","Minnesota",
             "Iowa","Missouri","Kansas","Nebraska","South Dakota","North Dakota")
southwest <- c("Oklahoma", "Texas","New Mexico", "Arizona", "Alaska", "Hawaii")
west <- c("Colorado","Utah", "Montana","Idaho", "Wyoming", "Nevada","California",
          "Oregon", "Washington")

states_caps <- states_caps %>%
  mutate(brms_regions = case_when(state %in% northeast ~ "northeast",
                                  state %in% southeast ~ "southeast",
                                  state %in% midwest ~ "midwest",
                                  state %in% southwest ~ "southwest",
                                  state %in% west ~ "west",
                                  T ~ ""))

