source("R/functions.R")

all_rds <- list.files(path = "data/", pattern = "*.rds", full.names = TRUE)

urls <- readRDS("data/urls_odluki.rds")
length(urls)

texts <- readRDS("data/text_odluki.rds")
length(texts)

#everything is of class 'character'. ok.
classes_txt <- lapply(texts, class)
sum(classes_txt=="character")

# everything is a character vector of some length. ok
sum(lapply(texts, length)==0)

# glue the <p> per odluka in one string
library(glue)
glued <- lapply(texts, glue_collapse, sep = " ")
glued[[1]]

length(glued)

url_glued <- tibble::tibble("URL"=unlist(urls), "ODLUKA"=unlist(glued))
nrow(url_glued)
url_glued
