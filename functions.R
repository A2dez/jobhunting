
safe_html <- safely(read_html, otherwise = NULL)

scrape_jobs_by_keyword <- function(jobs_raw, output, location2) {
  
  jobs_dfrm <-  jobs_raw  %>%
    #grab the 'li' nodes then 'div' and extract text. Convert to tibble for ease of manipulation
    #id lets us match each batch of scraped data with its equivalent set of links below
    map_dfr( ~.x %>% html_nodes('li') %>%html_nodes("div") %>% html_text2 %>% as_tibble, .id = 'step') %>% 
    #how many \ns are there? 
    #we only want the lines with an empty row following it
    mutate(empty_next = !str_detect(lead(value), '[[:graph:]]')) %>% 
    filter(empty_next == T) %>%
    #brutal column names for now. should be five bits of data separated by \n
    separate(col = value, into = letters[1:5], sep = "\\n")    %>% 
    #extract age of ad e.g. 1 month ago, 10 months ago
    #then extract salary data in Euros. 
    mutate(e = if_else(is.na(e), str_extract(d, '\\d+.*?$'), e), 
           salary = if_else(str_detect(d, CURRENCY), str_extract(d, paste0(CURRENCY, ".*")), NA_character_))
  
  
  links <- jobs_raw  %>% map_dfr(~.x %>% html_nodes('li') %>% html_nodes('a') %>% html_attr('href') %>% as_tibble, .id = 'step')
  
  links$aorb <- as.logical(seq_along(links$value) %% 2)
  links <- links %>% 
    group_by(step, aorb) %>%
    mutate(id = seq_along(value)) %>% 
    pivot_wider(names_from = aorb, values_from = value ) %>%
    ungroup %>%
    select(link1 = "TRUE", link2 = "FALSE")
  
  
  jobs_dfrm$location2 <- str_detect(jobs_dfrm$d, regex(location2, ignore_case = T))
  jobs_dfrm$id <- seq_along(jobs_dfrm$a)
  jobs_dfrm <- jobs_dfrm %>% group_by(b) %>% mutate(count1 = n()) %>% ungroup %>% group_by(c) %>% mutate(count2 = n()) %>% ungroup
  jobs_dfrm <- bind_cols(jobs_dfrm, links) 
  jobs_dfrm <- jobs_dfrm %>% select( id, count1, b, c,count2, location2, e, salary, link1, link2, d)
  
  write_csv(jobs_dfrm, file.path(output, paste0(search_terms[["keywords"]], '.csv')))
  return(jobs_dfrm)
}