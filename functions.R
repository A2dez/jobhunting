
safe_html <- safely(read_html, otherwise = NULL)

read_html_safe <- function(urls) { 
  jobs_raw <- urls %>% map(~safe_html(.x)) 
  lapply(jobs_raw, function(safe_result){ safe_result %>% pluck( 'result')})
}

scrape_repeatedly <- function(urls) {
  output <- tibble(url = urls,  
                   scraped_data = read_html_safe(urls) ) 
  failure_lgl <-  sapply(output$scraped_data, is.null)
  no_failures <- failure_lgl %>% sum
  prop_failures <- no_failures/nrow(output)
  if (prop_failures > 0.1) {
    output <- output %>%
      mutate(scraped_data = if_else(failure_lgl, 
                                    read_html_safe(url), 
                                    scraped_data)) 
  }
  return(output)
}

pull_job_data <- function(page) {
  text <- page %>% html_elements("div") %>% html_text %>% unique %>% sapply(str_trim)
  applicants <- str_extract(text, '\\d+(?=\\s+applicants)') %>% .[is.na(.) == F] %>% unique %>% as.numeric
  # output = list("url" = job_url, 
  #               # "data" = list(text), 
  #               "no_applicants" = applicants)
  return(applicants)
}

  
  
scrape_jobs_by_keyword <- function(jobs_raw, output, location2, CURRENCY = "€") {
  
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
  links$listing_id <- str_extract(links$value, '\\d{10}')
  links <-  links %>% 
    mutate(link_type = if_else(is.na(listing_id), 'company', 'job')) %>% 
    group_by(step, link_type) %>%
    mutate(id = seq_along(value)) %>% 
    pivot_wider(names_from = link_type, values_from = c(value, listing_id) ) %>% 
    ungroup %>% 
    select(job_link = "value_job", company_link = "value_company", listing_id = 'listing_id_job')

  jobs_dfrm$location2 <- str_detect(jobs_dfrm$d, regex(location2, ignore_case = T))
  jobs_dfrm$id <- seq_along(jobs_dfrm$a)
  jobs_dfrm <- jobs_dfrm %>% group_by(b) %>% mutate(count1 = n()) %>% ungroup %>% group_by(c) %>% mutate(count2 = n()) %>% ungroup
  jobs_dfrm <- bind_cols(jobs_dfrm, links) 
  jobs_dfrm <- jobs_dfrm %>% 
    select(job_link, id, count1, b, c,count2, location2, e, salary, listing_id, company_link, d) %>% 
    #remove duplicates
    group_by(listing_id) %>% mutate(count = (id)) %>% ungroup %>% distinct(listing_id, .keep_all = T) 
  
  return(jobs_dfrm)
}
