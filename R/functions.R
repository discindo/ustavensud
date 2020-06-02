# Define some functions
# roxygen2 included, maybe we'll need that for later

#' page_count 
#' count the number of pages in every category
#'
#' @param first_page an url form the category pages of ustavensud.mk
#' @param bow a bow defined with polite::bow()
#' 
#' @return a number vector with category count
#' @import rvest, xml2, tidyverse
#' @export


page_count <- function(first_page, bow) {
  
  session <- nod(
    bow = ustaven_bow,
    path = first_page
  )
  
  html_page <- scrape(session)
  
  page_numbers <- html_page %>% 
    html_nodes(css = "a.page-numbers") %>% 
    html_attr("href") %>% 
    stringr::str_extract(., pattern = "(\\d)+$") %>% 
    as.numeric()
  
  return(max(page_numbers))
}

#' construct_category_urls
#' make all urls given a list
#'
#' @param page_number a vector from category_page_count
#' @param list_url the url from first_page
#'
#' @return a character vector with the verditct
#' @import rvest, xml2, tidyverse
#' @export

construct_category_urls <- function(page_number, list_url) {
  
  url <- paste0(list_url, "&paged=", page_number)
  
  return(url)
}


#' get_judgement_urls
#' get the urls for the pages holding the judgements
#'
#' @param url an url form the category pages of ustavensud.mk
#' @param bow a bow defined with polite::bow()
#'
#' @return a list containing urls
#' @import rvest, xml2, tidyverse
#' @export

get_judgement_urls <- function(url, bow) {
  
  session <- nod(
    bow = bow,
    path = url
  )

  html_page <- scrape(session)
  
  page_urls <- html_page %>% 
    html_nodes(css = "div:nth-child(1) > header:nth-child(1) > h3:nth-child(1) > a:nth-child(1)") %>% 
    html_attr("href") %>% 
    stringr::str_replace(., pattern = "http://ustavensud.mk/", replacement = "")
  
  return(page_urls)
}

#' get_judgment_text
#' get the text of the judgment
#'
#' @param url an url for the page containing the judgment
#' @param bow a bow defined with polite::bow()
#'
#' @return a list with the date and the text of the document
#' @import rvest, xml2, tidyverse
#' @export


get_judgment_text <- function(url, bow){
  
  session <- nod(
    bow = bow,
    path = url
  )
  
  html_page <- scrape(session)
  
  write_html(html_page, str_remove(url, "\\?p="))
  
  date <- html_page %>% 
    html_node("div.blog-info:nth-child(1) > a:nth-child(2)") %>% 
    html_text(trim = TRUE) 
  
  text <- html_page %>% 
    html_nodes(css = "p") %>% 
    html_text(trim = TRUE) 
  
  content <- list(date, text)
  
  return(content) 
}
```

#' transform_court
#' make a series of transformations to the original data
#'
#' @param tibble a tibble containing the original data
#'
#' @return a tibble with 4 variables
#' @import tidiverse
#' @export

transform_court <- function(tibble) {
  tibble %>% 
    #unnest(url) %>% 
    unnest_longer(text) %>% 
    group_by(url) %>% 
    mutate(p_number = row_number()) %>% 
    mutate(date = text[str_detect(text, "С к о п ј е")]) %>% 
    mutate(date = text[str_detect(text, "С к о п ј е")]) %>% 
    mutate(date = str_split(date, "\n")[[1]][2]) %>% 
    mutate(date = str_remove(date, "година")) %>% 
    mutate(date = str_replace(date, "јануари", "January")) %>%
    mutate(date = str_replace(date, "февруари", "February")) %>%
    mutate(date = str_replace(date, "март", "March")) %>%
    mutate(date = str_replace(date, "април", "April")) %>%
    mutate(date = str_replace(date, "мај", "May")) %>%
    mutate(date = str_replace(date, "јуни", "June")) %>%
    mutate(date = str_replace(date, "јули", "July")) %>%
    mutate(date = str_replace(date, "август", "August")) %>%
    mutate(date = str_replace(date, "септември", "September")) %>%
    mutate(date = str_replace(date, "октомври", "October")) %>%
    mutate(date = str_replace(date, "ноември", "November")) %>%
    mutate(date = str_replace(date, "декември", "December")) %>% 
    mutate(date = dmy(date)) %>% 
    ungroup()
  
  return(tibble)
}

