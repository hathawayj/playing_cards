devtools::install_github("hafen/trelliscopejs", ref = "new-features")
pacman::p_load(trelliscopejs, tidyverse)

# where the display will be stored
display_path <- "docs"
# where to write individual panels within this directory
panel_path <- file.path(display_path, "panels/")


# the input to trelliscope will simply be a data frame with a column designated as the location of the image to display
# below there is just one column, but any additional column you add will be cognostics
# simply think of it as a data frame where each row points to an image to be displayed
# one nice aspect of this is that while generating the images may take some time, that is done independent of creating the display, and creating the display is very fast and therefore can be updated easily


d <- tibble(
  paths = file.path("panels", list.files(panel_path))
) %>%
  mutate(name = str_remove(paths, "panels/") %>% str_remove(".png")) %>%
  separate(name , into = c("name", "suite"), sep = "_") %>%
  mutate(value = case_when(name == "ace" ~ 1,
                           name == "jack" ~ 11,
                           name == "queen" ~ 12,
                           name == "king" ~ 13,
                           TRUE ~ as.numeric(name)),
         color = case_when(suite %in% c("clubs", "spades") ~ "black",
                           suite %in% c("hearts", "diamonds") ~ "red"),
         rand_1 = sample(52, 52),
         rand_2 = sample(52, 52),
         rand_3 = sample(52, 52)
  ) %>%
  arrange(rand_1) %>%
  mutate(hand6_1 = rep(c(sample(8, 8), 9), each = 6)[1:52],
         hand7_1 = rep(c(sample(7, 7),8), each = 7)[1:52]) %>%
  arrange(rand_2) %>%
  mutate(hand6_2 = rep(c(sample(8, 8), 9), each = 6)[1:52],
         hand7_2 = rep(c(sample(7, 7),8), each = 7)[1:52]) %>%
  arrange(rand_3) %>%
  mutate(hand6_3 = rep(c(sample(8, 8), 9), each = 6)[1:52],
         hand7_3 = rep(c(sample(7, 7),8), each = 7)[1:52]) %>%
  arrange(hand7_3) %>%
  mutate(panel = trelliscopejs::img_panel_local(paths))


         

trelliscope(d[order(d$suite,d$value),], name = "one_deck", path = "docs", desc = "One deck of cards", nrow = 4, ncol = 13, 
            thumb = TRUE)

trelliscope(d, name = "hand_7", path = "docs", desc = "Set to show random hand of 7", 
            nrow = 3, ncol = 7, thumb = TRUE)

trelliscope(d[order(d$hand6_1), ], name = "hand_6", path = "docs", 
            desc = "Set to show random hand of 6", 
            nrow = 3, ncol = 6, thumb = TRUE)



