pacman::p_load(tidyverse, rvest, purrr, fs)
# https://freakonometrics.hypotheses.org/6491
# https://medium.com/@kyleake/wikipedia-data-scraping-with-r-rvest-in-action-3c419db9af2d
# https://stackoverflow.com/questions/35247033/using-rvest-to-extract-links


svg_links <- function(x){
    read_html(x) %>%
    html_node("body #content #bodyContent #file a")  %>%
    html_attr("href")
}


url <- "https://commons.wikimedia.org/wiki/Category:SVG_English_pattern_playing_cards"

url_refs <- url %>%
  read_html() %>%
  html_node("body #content #bodyContent #mw-category-media") %>%
  html_nodes("ul li a") %>%
  html_attr("href") %>%
  str_c("https://commons.wikimedia.org", .) %>%
  unique(.) %>%
  str_subset("cards_deck", negate = TRUE)
  

url_svg <- map(url_refs, ~svg_links(.x)) %>% unlist()
write_lines(url_svg, "data/svg_paths.txt")

path_svg <- url_svg %>% 
  fs::path_file()  %>%
  str_c("data/", .)


map2(url_svg, path_svg, ~download.file(.x,.y))




