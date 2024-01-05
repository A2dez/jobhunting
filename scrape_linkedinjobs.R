library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(purrr)
library(magrittr)


source("functions.R")
source("config.R")

CURRENCY <- "€"



#later versions will need this to be broken down fruther
url_unnumbered = paste0('https://www.linkedin.com/jobs-guest/jobs/api/seeMoreJobPostings/search?',
                        'keywords=', search_terms[["keywords"]], 
                        "&location=", search_terms[["location"]],
                        "&geoId=", search_terms[["geoID"]], 
                        "&trk=",search_terms[["trk"]],
                        "public_jobs_jobs-search-bar_search-submit&start=")

pages_to_grab <- seq(search_terms[["start"]],search_terms[["end"]], search_terms[["steps"]])

#speed up:
#insert section to get rid of unwanted urls (i.e. likely no jobs there )

# # https://stackoverflow.com/questions/56067780/r-using-purrrsafely-to-handle-webscraping-failed-urls
urls <- paste0(url_unnumbered, pages_to_grab)
h <- urls[1:2] %>%
  map(~{
    Sys.sleep(sample(seq(1, 3, by=0.001), 1))
    safe_html(.x)})
jobs_raw <- discard(map(h, 'result'), ~ is.null(.x)) 



jobs_data <- scrape_jobs_by_keyword(jobs_raw, output = getwd(), location2 = 'dublin')
glimpse(jobs_data)


# urls_existence = sapply(urls[1:2],valid_url)
# urls_existence[1][[1]]
# 
# 

# f <- scrape_jobs_by_keyword(jobs_raw, output = getwd(), location2 = 'dublin')
# f2 <- scrape_jobs_by_keyword(h[[1]]$result %>% list, output = getwd(), location2 = 'dublin')
# 
# 
# discard(h, ~ is.null(.x$result)) 
# pluck
# 
# # [[9]]
# # [[9]]$result
# # {html_document}
# # <html>
# #   [1] <body>\n<li>\n        \n\n    \n\n    \n    \n    \n      <div class="base-card relative w-full hover:no-underlin ...
# # 
# # [[9]]$error
# # NULL
# 
# # https://stackoverflow.com/questions/41094157/handling-error-response-to-empty-webpage-from-read-html
# c('http://tweg.com', 'http://wikipedia.org') %>% 
#   map(safely(read_html, 'empty page'))
# # paste0(url_unnumbered, pages_to_grab[1:2])%>% 
# #   map(~ safely(read_html, .x) %>% list)
# 
# url = urls[1]
# 
# x <-map_dbl(urls,valid_url)
# x
# i = 1
# x[[2]]
# i
# while(x[[i]] == 1){print(x); i = i + 1}
# 
# x[3][[1]] == T
# ?pluck
# while(x == T){print(1)}
# 
# valid_url <- function(url_in,t=2){
#   con <- url(url_in)
#   check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
#   suppressWarnings(try(close.connection(con),silent=T))
#   ifelse(is.null(check),TRUE,FALSE)
# }
# 
# 
# 
# 
# for (url in urls){
#   if (valid_url(url, t =2)) {
#     data = read_html(url)
#     jobs_raw = c(output, list(data)) #read_html output is in the wrong format 
#   } else {
#     break
#   }
# 
#   #https://stackoverflow.com/questions/74312869/slow-down-the-pace-of-read-html-in-r
#   Sys.sleep(20) #slows down the call to LinkedIn, removing the 429 error. 10 secs too short. Optimal might be <20
# }
# 
# 
# # Check URLs exists
# #https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
# urls <-   c("http://www.amazon.com",
#             "http://this.isafakelink.biz",
#             "https://stackoverflow.com")
# 
# urls = paste0(url_unnumbered, pages_to_grab)
# 
# 
# 
# 
# 
# # read_html_slowly(0, 10)
# 
# # 
# # search_terms <- list("keywords" = 'Data%2BScientist', 
# #                      "location" = "ireland",
# #                      "geoId" = "",
# #                      'trk' = "", 
# #                      "start" = 0, #LinkedIn is zero-indexed
# #                      "end" = 500,  #likely to be a variable, unlike start and steps
# #                      "steps" = 25)
# # 
# # search_terms <- list("keywords" = 'nlp', 
# #                      "location" = "ireland",
# #                      "geoId" = "",
# #                      'trk' = "", 
# #                      "start" = 0, #LinkedIn is zero-indexed
# #                      "end" = 0,  #likely to be a variable, unlike start and steps
# #                      "steps" = 25)
# # 
# # search_terms <- list("keywords" = 'machine%2Blearning', 
# #                      "location" = "ireland",
# #                      "geoId" = "",
# #                      'trk' = "", 
# #                      "start" = 0, #LinkedIn is zero-indexed
# #                      "end" = 225,  #likely to be a variable, unlike start and steps
# #                      "steps" = 25)
# # search_terms <- list("keywords" = 'lab%20technician', 
# #                      "location" = "ireland",
# #                      "geoId" = "",
# #                      'trk' = "", 
# #                      "start" = 0, #LinkedIn is zero-indexed
# #                      "end" = 50,  #likely to be a variable, unlike start and steps
# #                      "steps" = 25)
# # 
# # search_terms <- list("keywords" = 'microbiologist', 
# #                      "location" = "ireland",
# #                      "geoId" = "",
# #                      'trk' = "", 
# #                      "start" = 0, #LinkedIn is zero-indexed
# #                      "end" = 150,  #likely to be a variable, unlike start and steps
# #                      "steps" = 25)
# # 
# # search_terms <- list("keywords" = 'quality%2Bcontrol', 
# #                      "location" = "ireland",
# #                      "geoId" = "",
# #                      'trk' = "", 
# #                      "start" = 0, #LinkedIn is zero-indexed
# #                      "end" = 150,  #likely to be a variable, unlike start and steps
# #                      "steps" = 25)
# 
# 
# # View(jobs_dfrm)
# # 
# # 
# # 
# # # Dig out time data -------------------------------------------------------------------------
# # 
# # jobs_raw[[1]] %>% html_text2()
# # 
# # j <- readRDS('machine%2Blearning')
# # jobs_raw  %>%
# #   map_dfr( ~.x %>% 
# #              html_nodes('li')   %>% html_text2() %>% as_tibble() 
# #   )  %>% 
# #   separate(col = value, into = letters[1:5], sep = "\\n") 
# # 
# # d  %>%
# #   map_dfr( ~.x %>% html_nodes('li') %>%html_nodes("div") %>% html_text2 %>% as_tibble) %>%
# #   mutate(count = str_count(value, '\\n'), 
# #          empty_next = !str_detect(lead(value), '[[:graph:]]')) %>% 
# #   filter(empty_next) %>%
# #   separate(col = value, into = letters[1:5], sep = "\\n")    %>% 
# #   mutate(e = if_else(is.na(e), str_extract(d, '\\d+.*?$'), e), 
# #          salary = if_else(str_detect(d, "€"), str_extract(d, "€.*"), NA_character_)) %>%
# #   View
# # 
# # # View(jobs_dfrm2)
# # 
# # 
# # # testing login page ------------------------------------------------------
# # 
# # url <- "https://www.linkedin.com/jobs/search/?currentJobId=3727085797&geoId=105178154&keywords=nlp&location=Dublin%2C%20County%20Dublin%2C%20Ireland&origin=JOB_SEARCH_PAGE_SEARCH_BUTTON&refresh=true"
# # d <- read_html(url)
# # d %>% 
# #   html_nodes('li') %>%html_nodes("div") %>% html_text2 %>% as_tibble%>%
# # mutate(count = str_count(value, '\\n'), 
# #        empty_next = !str_detect(lead(value), '[[:graph:]]')) %>% 
# #   filter(empty_next) %>%
# #   separate(col = value, into = letters[1:5], sep = "\\n")    %>% 
# #   mutate(e = if_else(is.na(e), str_extract(d, '\\d+.*?$'), e), 
# #          salary = if_else(str_detect(d, "€"), str_extract(d, "€.*"), NA_character_)) %>%
# #   View
# # 
# # ?html_nodes
# # # -------------------------------------------------------------------------
# # data_science <- map(pages_to_grab, ~ read_html(paste0(url_unnumbered, .x)))
# # 
# # jobs_dfrm <- data_science %>%
# #   map_dfr( ~.x %>% 
# #              html_nodes('li')   %>%  html_text2() %>% as_tibble() 
# #   )  %>% 
# #   separate(col = value, into = letters[1:5], sep = "\\n") 
# # 
# #   # arrange(count %>% desc) %>%
# #   # View
# # 
# # links <- data_science  %>% map (~.x %>% html_nodes('li') %>% html_nodes('a') %>% html_attr('href')) #%>% unlist
# # 
# # # bind_rows(links)
# # #make sure each group has exactly fifty (or max length) links
# # for (i in which(lengths(links) < 50)){links[[i]] <- c(links[[i]], rep(NA, max(lengths(links)) - length(links[[i]])) )}
# # 
# # links <- unlist(links)
# # jobs_dfrm$link1 <- links[seq_along(links) %% 2 != 0]
# # jobs_dfrm$link2 <- links[seq_along(links) %% 2 == 0]
# # 
# # 
# # View(jobs_dfrm)
# # # -------------------------------------------------------------------------
# # 
# # 
# # View(jobs_dfrm)
# # 
# # # rep(NA, 7)
# # max(lengths(links))
# # 
# # View(data_science)
# # dmanage my record
# # 
# # data_science <- data_science[1:40]
# # # str_count(output_all$value, "\\n") %>% table
# # names(data_science)
# # data_science$link1
# # 
# # 
# # ddddata200
# # 
# # data400 <-  read_html(paste0(url_unnumbered, 425))
# # data400 %>%
# #   html_nodes('li')   %>% html_text2() %>% as_tibble() %>%
# #   separate(col = value, into = letters[1:5], sep = "\\n") 
# # 
# #   
# # View(output_all)
# # data_science %>%
# #   map_dfr( ~.x %>% 
# #              html_nodes('li')   %>% html_text2() %>% as_tibble() %>%
# #              separate(col = value, into = letters[1:5], sep = "\\n")
# #   ) -> output_all
# # 
# # names(output)[2] <- "job"
# # 
# # output %>% 
# #   group_by(job) %>% 
# #   summarise(count = n()) %>%
# #   arrange(count %>% desc) %>% 
# #   View
# # 
# # dfr <- data[[1]]
# # 
# # data <- 
# #   #pages to grab
# #   seq(search_terms[["start"]],search_terms[["end"]], search_terms[["steps"]]) %>% 
# #   #create url and read it directly
# #   map( ~ paste0(url_unnumbered, .x) %>% 
# #          read_html )
# # # 
# # # data[[1]] %>% html_nodes('li') %>% html_nodes('a') %>%  html_text() %>% 
# # #   str_replace_all("\\n\\s+", '') #this gets job and company
# # 
# # dfrm <- data %>% 
# #   html_nodes('li')   %>% html_text2() %>% as_tibble() %>%
# #   separate(col = value, into = letters[1:5], sep = "\\n") 
# # View(dfrm)
# # 
# # dd
# # 
# # View(dfrm)
# # 
# # 
# # lapply(links, function(x){ if (length(x) == 50){ x <- x} else{x <- c(x, rep(NA, max(lengths(links)) - length(x)))} })
# # lapply(links, function(x){ if (length(x) < 50){ print(x)}})
# # 
# # 
# # x <- links[[35]]
# # links <- unlist(links)
# # 
# # 
# # 
# # ?separate
# #   read_delim(delim = "\\n", col_names = F)
# #   str_split("\\n") #html_nodes('a')
# # 
# # 
# # ?html_text
# #   
# #   data <- read_html(x = "https://www.linkedin.com/jobs/search?keywords=Data&location=Ireland&locationId=&geoId=104738515&f_TPR=&f_WT=2")
# #   
# #   'https://www.linkedin.com/jobs-guest/jobs/api/seeMoreJobPostings/search?keywords=data&location=ireland&geoId=&trk=public_jobs_jobs-search-bar_search-submit&start=50')
# # 
# # str #sapply(unlist) %>% as.data.frame  %>% View
# # page = read_html(x = "https://www.deutsche-biographie.de/search?_csrf=45b6ee54-385e-4777-90bf-9067923e6a00&name=meier")
# # name = page %>% html_nodes(".media-heading a") #%>% html_text()
# # information = page %>% html_nodes("#secondColumn p") %>% html_text()
# # result = data.frame(name, information, stringsAsFactors = FALSE)
# # 
# # #manipulate data in columns
# # result$yearofbirth = sub("(^[^-]+)-.*", "\\1", result$information) #extract characters before dash
# # result$yearofdeath = sub(',.*$','', result$information)
# # result$yearofdeath = sub('.*-','', result$yearofdeath) #extract characters after dash
# # result$profession = sub("^.*?,", "", result$information) #extract characters after comma
# # result$profession = trimws(result$profession, whitespace = "[ \t\r\n]") #trim leading and trailing white space
# # result$information = NULL
# # ```
# 
# 
# obj1 <- list("a", list(1, elt = "foo"))
# obj2 <- list("b", list(2, elt = "bar"))
# x <- list(obj1, obj2)
# 
# pluck(h ,1, 1)
# map(h, 1)
# 
