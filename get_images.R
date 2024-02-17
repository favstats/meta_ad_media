# 
# library(metatargetr)
# 
# get_ad_snapshots("561403598962843", download = T, hashing = T, mediadir = "data/media")
# 
# 
# 
# 
# geddem <- httr::GET("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=DE&view_all_page_id=190784743602&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all")
# 
# 



library(playwrightr)
# library(tidyverse)
options(timeout=300)

source("utils.R")
# Sys.setenv("piggyback_cache_duration"=3600)

# source("utils.R")

options(python_init = TRUE)

# cntry_str <- "NL"
time_preset <- commandArgs(trailingOnly = TRUE)
time_preset <- "last_30_days"

# install.packages("pacman")
pacman::p_load(
  reticulate,
  vroom,
  progress,
  janitor,
  fs,
  tidyr,
  # appendornot,
  countrycode,
  dplyr,
  stringr,
  lubridate,
  purrr,
  glue,
  rvest,
  cli,
  digest,
  readr,
  piggyback
)


# options(googledrive_quiet = TRUE)
# 
# drive_auth(path = Sys.getenv("GOOGLE_APPLICATION_KEY"))

# conda_install(packages = "fcntl", pip = T)
if(Sys.info()[["sysname"]]=="Windows"){
  
  pw_init(use_xvfb = F)
} else{
  
  conda_install(packages = "xvfbwrapper", pip = T)
  
  print("installed xvfbwrapper")
  conda_install(packages = "playwright", pip = T)
  print("installed playwright")
  
  pw_init(use_xvfb = T)
  system("playwright install")
}



browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)

print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


# page_df %>%
#   goto("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=BE&view_all_page_id=56605856504&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all&start_date[min]=2024-02-01&start_date[max]=")
# 
# page_df %>% 
#   goto("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=DE&view_all_page_id=190784743602&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all")


page_df %>% 
  goto("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country=US&view_all_page_id=7860876103&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all&start_date[min]=2024-02-07&start_date[max]=")



try({
  page_df %>%
    get_by_test_id("cookie-policy-manage-dialog-accept-button") %>%
    slice(1) %>%
    click() %>%
    screenshot("/data/res/facebook_add_reports/test.png")
})



get_one <- page_df %>% 
  get_by_text("Library ID") 
  # playwrightr::get_content()

get_one %>% 
  slice(n()) %>% 
  click()

page_df %>% 
  get_content()

# [1] "Library ID: 785925946712303​InactiveFeb 8, 2024 - Feb 9, 2024Platforms​​Categories​Estimated audience size:>1MAmount spent (EUR):<€100Impressions:1K - 2KEU transparency​Open Dropdown​See ad detailsVlaams BelangSponsored • Paid for by Vlaams BelangWillen we een zekere toekomst voor Vlaanderen? Dan moeten we beginnen bij een degelijk, kwalitatief onderwijs. De kennis van onze Nederlandse taal, vanaf de kleuterschool, is hierbij cruciaal. Daarnaast moeten leerkrachten zich gewaardeerd voelen, zodat onderwijzen opnieuw op de eerste plaats komt. Administratieve lasten moeten tot een minimum beperkt worden. Een ander en beter onderwijsbeleid is mogelijk en komt er met het Vlaams Belang!"
# /html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]/div/div[3]
# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[{index}]
# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]
# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]
retrieve_ads <- function(paage  , index) {
  fin <- NULL
  try({
  fin <- paage %>% 
      html_elements(xpath = glue::glue('//*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[{index}]')) %>% 
      html_children() %>% 
      html_children() %>% 
      html_children() %>% 
      html_children() %>% 
      html_children() %>% 
      html_text() %>% set_names(paste0(1:length(.))) %>% 
      as.list() %>% as_tibble() %>% 
      janitor::clean_names()    
  })

  return(fin)
}

the_content <- page_df %>% 
  playwrightr::get_content()


nads <- the_content %>% 
  html_elements(xpath = "/html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[6]/div[1]/div/div/div/div/div/div[1]/div") %>% 
  html_text() %>% 
  parse_number()


the_content %>% retrieve_ads(1)

first_attempt <- 1:nrow(get_one) %>% 
  map_dfr_progress(~{
    the_content %>% retrieve_ads(.x)
  }) %>%
  arrange(x3) 

thismonth <- lubridate::today() %>% 
  format("%b")
  
# oomf <- table(str_detect(first_attempt$x3, paste0(thismonth, " 1")))
# oomf[[TRUE]]
# oomf %>% as.data.frame() %>% 
#   mutate(perc = Var1)

click_howmany_times <- nads/nrow(first_attempt)

fulldat <- 1:round(click_howmany_times) %>% 
  map_dfr_progress(~{
    
    get_two <- page_df %>% 
      get_by_text("Library ID") 
    
    print("one")
    
    try({
      get_two %>% 
        slice(n()) %>% 
        click()      
    })

    
    print("two")
    
    second_attempt <- tibble()
    for (jj in 1:nrow(get_two)) {
      thas <- the_content %>% retrieve_ads(jj)
      if(is.null(thas)) {
        print("errooor reached")
        break
      }
      second_attempt <- second_attempt %>% bind_rows(thas)
    }
    
    return(second_attempt)
    
  }) %>% 
  bind_rows(first_attempt) %>% distinct() 


# fulldat %>% View()
 # %>% View()

# library(metatargetr)
metadat <- fulldat %>% 
  distinct() %>% 
  mutate(id = str_remove(x1, "Library ID: ")) %>% 
  pull(id) %>% 
  map_dfr_progress(~{
    get_ad_snapshots(.x, download = T, hashing = T, mediadir = "data/media")
  })

saveRDS(metadat, "data/us.rds")

# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[16]
# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[3]/div[3]/div[1]/div[1]


  
  