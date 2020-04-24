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
  paths = file.path("panels", list.files(panel_path) %>% str_subset("_[c|d|h|s]"))
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

# Can switch to the views idea once I figure out how to do it.



paths_all <- list.files(path = "docs/panels", full.names = TRUE ) %>%
  str_remove_all("docs/")

d_mean7 <- d %>%
  group_by(hand7_1, value, name) %>%
  summarize(n = n(), suites = str_c(sort(suite), collapse = "-")) %>%
  ungroup() %>%
  complete(hand7_1, value = 1:13, fill = list(n = 0, suites = "blank", name = "blank")) %>%
  arrange(hand7_1) %>%
  mutate(rows = 1:n()) %>%
  group_by(rows)

mean_trell7 <- bind_rows(
  d_mean7 %>% 
    filter(n == 0) %>% 
    mutate(path = "panels/blank.png"),
  
  d_mean7 %>% 
    filter(n ==1) %>% 
    mutate(path = str_subset(paths_all, str_c("_", suites)) %>% str_subset(name)),
  
  
  d_mean7 %>% 
    filter(n %in% 2:4) %>% 
    mutate(path = str_subset(paths_all, suites) %>% 
             str_subset(str_c(name, "_")) %>% 
             str_subset(str_c("-", n, "-")))

) %>% ungroup() %>%
  arrange(hand7_1, value) %>%
  mutate(panel = trelliscopejs::img_panel_local(path))



d_mean6 <- d %>%
  group_by(hand6_1, value, name) %>%
  summarize(n = n(), suites = str_c(sort(suite), collapse = "-")) %>%
  ungroup() %>%
  complete(hand6_1, value = 1:13, fill = list(n = 0, suites = "blank", name = "blank")) %>%
  arrange(hand6_1) %>%
  mutate(rows = 1:n()) %>%
  group_by(rows)

mean_trell6 <- bind_rows(
  d_mean6 %>% 
    filter(n == 0) %>% 
    mutate(path = "panels/blank.png"),
  
  d_mean6 %>% 
    filter(n ==1) %>% 
    mutate(path = str_subset(paths_all, str_c("_", suites)) %>% str_subset(name)),
  
  d_mean6 %>% 
    filter(n %in% 2:4) %>% 
    mutate(path = str_subset(paths_all, suites) %>% 
             str_subset(str_c(name, "_")) %>% 
             str_subset(str_c("-", n, "-")))
  
) %>% ungroup() %>%
  arrange(hand6_1, value) %>%
  mutate(panel = trelliscopejs::img_panel_local(path))


trelliscope(mean_trell6, name = "Show_mean_hand_6", path = "docs", desc = "Set to show random hand of 6 with blank values", 
            nrow = 6, ncol = 13, thumb = TRUE, order = 5)


trelliscope(mean_trell7, name = "Show_mean_hand_7", path = "docs", desc = "Set to show random hand of 7 with blank values", 
            nrow = 7, ncol = 13, thumb = TRUE, order = 4)

trelliscope(d[order(d$hand6_1), ], name = "Hand_of_6", path = "docs", 
            desc = "Set to show random hand of 6", 
            nrow = 3, ncol = 6, thumb = TRUE, order = 3)

trelliscope(d, name = "Hand_of_7", path = "docs", desc = "Set to show random hand of 7", 
            nrow = 3, ncol = 7, thumb = TRUE, order = 2)

trelliscope(d[order(d$suite,d$value),], name = "one_deck", path = "docs", desc = "One deck of cards", nrow = 4, ncol = 13, thumb = TRUE, order = 1)

thumbnails <- list.files("thumbnails", full.names = TRUE)

fs::file_copy(thumbnails[1], "docs/appfiles/displays/common/Hand_of_6/thumb.png", overwrite = TRUE)
fs::file_copy(thumbnails[2], "docs/appfiles/displays/common/Hand_of_7/thumb.png", overwrite = TRUE)
fs::file_copy(thumbnails[3], "docs/appfiles/displays/common/one_deck/thumb.png", overwrite = TRUE)
fs::file_copy(thumbnails[4], "docs/appfiles/displays/common/Show_mean_hand_6/thumb.png", overwrite = TRUE)
fs::file_copy(thumbnails[5], "docs/appfiles/displays/common/Show_mean_hand_7/thumb.png", overwrite = TRUE)
