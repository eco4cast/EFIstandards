
sf::write_sf(USAboundaries::us_states(), "states.nc")
sf::st_read("states.nc")

library(tidync)
