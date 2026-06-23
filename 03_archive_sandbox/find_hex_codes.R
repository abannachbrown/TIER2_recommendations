library(rvest)
library(stringr)
library(purrr)

get_hex_colors <- function(url) {
  page <- read_html(url)
  
  # Extract all style/script content
  styles <- page %>%
    html_elements("style, link[rel='stylesheet']") %>%
    html_text()
  
  # Regex for hex colors
  hex_matches <- str_extract_all(styles, "#[A-Fa-f0-9]{3,6}") %>%
    unlist() %>%
    unique()
  
  return(hex_matches)
}

# Example
colors <- get_hex_colors("https://tier2-project.eu/")
print(colors)

#FFFFFF
#0D2557
#32dbce
#ebecf0
