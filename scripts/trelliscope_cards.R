devtools::install_github("hafen/trelliscopejs", ref = "new-features")
pacman::p_load(trelliscopejs, tidyverse)

# where the display will be stored
display_path <- "trelliscopejs"
# where to write individual panels within this directory
panel_path <- file.path(display_path, "panels/")


# the input to trelliscope will simply be a data frame with a column designated as the location of the image to display
# below there is just one column, but any additional column you add will be cognostics
# simply think of it as a data frame where each row points to an image to be displayed
# one nice aspect of this is that while generating the images may take some time, that is done independent of creating the display, and creating the display is very fast and therefore can be updated easily
d <- data.frame(
  panel = trelliscopejs::img_panel_local(file.path("panels", list.files(panel_path))),
  stringsAsFactors = FALSE
)

trelliscopejs::trelliscope(d, panel_col = "panel", name = "test")
