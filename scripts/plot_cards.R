pacman::p_load(tidyverse, trelliscopejs, rsvg, magick)

svg_paths <- list.files("data", ".svg", full.names = TRUE)

## One card
image_paths <- str_replace_all(svg_paths, "data/", "card_images/one/") %>%
  str_replace_all(".svg", ".png") %>% 
  str_remove_all("English_pattern_") %>%
  str_remove_all("_of")

map2(svg_paths, image_paths, ~rsvg_png(.x, .y))

one_card_paths <- list.files("card_images/one", full.names = TRUE)
one_card <- svg_paths %>% map(~image_read_svg(.x))
names(one_card) <- one_card_paths %>% fs::path_file() %>% fs::path_ext_remove()

##### functions ####

write_cards <- function(x, y, path_o) image_write(x, path = str_c(path_o, y))

names_cards <- function(values_n = c(1,2,3), cardn = card, ilist = image_list, divide = "--"){
  names(ilist)[values_n] %>%
    str_remove_all(str_c(cardn, "_")) %>%
    str_c(collapse = "-") %>%
    str_c(divide, .) %>%
    str_c(".png") %>%
    str_c(cardn, "_", .)
}

plot_4 <- function(cards = "10", image_list = one_card, path_out = "card_images/four/"){
  
  inames <- names(image_list)
  ilocations <- str_which(inames, cards)
  
  cards4 <- image_list[ilocations][[1]] %>%
    image_composite(image_list[ilocations][[2]], offset = "+70+30") %>%
    image_composite(image_list[ilocations][[3]], offset = "+140+60") %>%
    image_composite(image_list[ilocations][[4]], offset = "+210+90") %>%
    image_crop("300x450+05") %>%
    image_border(color = "black", "2x2")
  
  names4 <- names_cards(values_n = 1:4, cardn = cards, 
                        ilist = image_list[ilocations], divide = "-4-")
  
  image_write(cards4, path = str_c("card_images/four/",names4))
  
}

plot_3 <- function(cards, image_list = one_card, path_out = "card_images/three/"){
  
  inames <- names(image_list)
  ilocations <- str_which(inames, cards)
  
  combos_n <- combn(4,3, simplify = FALSE)
  
  cards_3 <- function(values_n = c(1,2,3)){
    image_list[ilocations][[values_n[1]]] %>%
      image_composite(image_list[ilocations][[values_n[2]]], offset = "+70+30") %>%
      image_composite(image_list[ilocations][[values_n[3]]], offset = "+140+60") %>%
      image_crop("300x450+05") %>%
      image_border(color = "black", "2x2")
  }
  
  file_names <- map_chr(combos_n, ~names_cards(.x, cardn = cards, 
                                               ilist = image_list[ilocations], divide = "-3-"))    
  image_out <- map(combos_n, ~cards_3(.x))    
      
  map2(image_out, file_names, ~write_cards(.x, .y, path_o = path_out))
  
}

plot_2 <- function(card, image_list = one_card, path_out = "card_images/two/"){
  
  inames <- names(image_list)
  ilocations <- str_which(inames, card)
  
  combos_n <- combn(4,2, simplify = FALSE)
  
  cards_2 <- function(values_n = c(1,2)){
    image_list[ilocations][[values_n[1]]] %>%
      image_composite(image_list[ilocations][[values_n[2]]], offset = "+70+30") %>%
      image_crop("300x450+05") %>%
      image_border(color = "black", "2x2")
  }
  
  
  file_names <- map_chr(combos_n, ~names_cards(.x, cardn = card, 
                                               ilist = image_list[ilocations], divide = "-2-"))    
  image_out <- map(combos_n, ~cards_2(.x))    
  
  map2(image_out, file_names, ~write_cards(.x, .y, path_o = path_out))
  
}


c(2:10, "ace", "jack", "queen", "king") %>% map(~plot_4(card = .x, image_list = one_card))
c(2:10, "ace", "jack", "queen", "king") %>% map(~plot_3(card = .x, image_list = one_card))
c(2:10, "ace", "jack", "queen", "king") %>% map(~plot_2(card = .x, image_list = one_card))

### Move all 2,3,4 cards to panels folder

file_paths <- list.files("card_images", recursive = TRUE, full.names = TRUE)
 
panel_paths <- str_c("docs/panels/", fs::path_file(file_paths))

map2(file_paths, panel_paths, ~fs::file_copy(.x,.y))

