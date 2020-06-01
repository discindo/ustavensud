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

#' get_judgement_text
#' get the text of the judgement
#'
#' @param url an url for the page containing the judgement
#' @param bow a bow defined with polite::bow()
#'
#' @return a character vector with the verditct
#' @import rvest, xml2, tidyverse
#' @export


get_judgement_text <- function(url, bow){
  
  session <- nod(
    bow = bow,
    path = url
  )
  
  html_page <- scrape(session)
  
  text <- html_page %>% 
    html_nodes(css = "p") %>% 
    html_text(trim = TRUE) 
  
  return(text) #[3:4] not sure abut subsetting here. maybe keep the whole text.
}