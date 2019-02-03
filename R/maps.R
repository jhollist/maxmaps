library(USAboundaries)
library(sf)
library(dplyr)
library(ggplot2)

northeast <- c("Maine","New Hampshire", "Vermont", "New York", "Massachusetts",
               "Connecticut", "Rhode Island", "Maryland", "New Jersey", 
               "Delaware", "Pennsylvania")
southeast <- c("West Virginia", "Virginia","Kentucky","Tennessee", "North Carolina",
               "South Carolina", "Florida", "Georgia", "Alabama", "Mississippi",
               "Arkansas", "Louisiana")
midwest <- c("Ohio","Michigan","Indiana","Illinois","Wisconsin","Minnesota",
             "Iowa","Missouri","Kansas","Nebraska","South Dakota","North Dakota")

the_rest <-

# Get map of lower 48 states
usa_ne <- us_boundaries() %>%
  filter(name %in% northeast)
           
usa_se <- us_boundaries() %>%
  filter(name %in% southeast)

usa_mw <- us_boundaries() %>%
  filter(name %in% midwest)

usa_48 <- us_boundaries() %>%
  filter(!name %in% c("Alaska", "Hawaii", "Puerto Rico"))

states_gg <- ggplot(us_boundaries()) +
  #geom_sf(data = usa_ne, fill = "black",size = 2) +
  #geom_sf(data = usa_ne) +
  #geom_sf(data = usa_se, fill = "black", size = 2) +
  #geom_sf(data = usa_se) +
  #geom_sf(data = usa_mw, fill = "black", size = 2) +
  #geom_sf(data = usa_mw)
  geom_sf(data = usa_48)
states_gg

ggsave("states.pdf", states_gg, width = 11, height = 8.5)
