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


#' transform_court
#' make a series of transformations to the original data
#'
#' @param tibble a tibble containing the original data
#'
#' @return a tibble with 4 variables
#' @import tidiverse
#' @export

transform_court <- function(tibble) {
  tibble_long <- tibble %>% 
    unnest_wider(text) %>% 
    rename(date = ...1, text = ...2) %>% 
    unnest_longer(text) %>% 
    group_by(url) %>%
    mutate(p_number = row_number()) %>%
    mutate(date = str_replace(date, "Јан", "January")) %>%
    mutate(date = str_replace(date, "Фев", "February")) %>%
    mutate(date = str_replace(date, "Мар", "March")) %>%
    mutate(date = str_replace(date, "Апр", "April")) %>%
    mutate(date = str_replace(date, "Мај", "May")) %>%
    mutate(date = str_replace(date, "Јун", "June")) %>%
    mutate(date = str_replace(date, "Јул", "July")) %>%
    mutate(date = str_replace(date, "Авг", "August")) %>%
    mutate(date = str_replace(date, "Сеп", "September")) %>%
    mutate(date = str_replace(date, "Окт", "October")) %>%
    mutate(date = str_replace(date, "Ное", "November")) %>%
    mutate(date = str_replace(date, "Дек", "December")) %>%
    mutate(date = dmy(date)) %>%
    ungroup()
  
  return(tibble_long)
}

