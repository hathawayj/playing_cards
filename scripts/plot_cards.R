pacman::p_load(tidyverse, trelliscopejs, rsvg)

svg_paths <- list.files("data", ".svg", full.names = TRUE)

# where the display will be stored
display_path <- "trelliscopejs"
# where to write individual panels within this directory
panel_path <- file.path(display_path, "panels/")


image_paths <- str_replace_all(svg_paths, "data/", panel_path) %>%
  str_replace_all(".svg", ".png")

map2(svg_paths, image_paths, ~rsvg_png(.x, .y))



