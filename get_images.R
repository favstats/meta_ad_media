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
  piggyback, 
  remotes
)

if(!("playwrightr" %in% installed.packages())){
  remotes::install_github("benjaminguinaudeau/playwrightr")
}



library(playwrightr)
# library(tidyverse)
options(timeout=300)

source("utils.R")
# Sys.setenv("piggyback_cache_duration"=3600)

# source("utils.R")

# the_cntry <- "BE"

options(python_init = TRUE)

# cntry_str <- "NL"
full_cntry_list <- read_rds("https://github.com/favstats/meta_ad_reports/raw/main/cntry_list.rds") %>% 
  rename(iso2c = iso2,
         country = cntry) 


cntries <- full_cntry_list$iso2c


retrieve_ads <- function(paage  , index) {
  fin <- NULL
  try({
    # fin <- paage %>% 
    #   html_elements(xpath = glue::glue('//*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[{index}]')) %>% 
    #   html_children() %>% 
    #   html_children() %>% 
    #   html_children() %>% 
    #   html_children() %>% 
    #   html_children() %>% 
    #   html_text() %>% set_names(paste0(1:length(.))) %>% 
    #   as.list() %>% as_tibble() %>% 
    #   janitor::clean_names()    
    
    # index <- 20
    fin <- the_content %>% 
      # html_elements(xpath = glue::glue('//*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]')) %>% 
      html_elements(xpath = glue::glue('/html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[7]/div[2]/div[2]/div[4]/div[1]/div[{index}]/div/div[1]/div/div[1]')) %>% 
      
      # html_elements(xpath = glue::glue('//*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]')) %>% 
      # html_children() %>% 
      # html_children() %>% 
      # html_children() %>% 
      html_children() %>% 
      html_children() %>% 
      html_text() %>% set_names(paste0(1:length(.))) %>% 
      as.list() %>% as_tibble() %>% 
      janitor::clean_names() 
  })
  
  return(fin)
}



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


retrieve_em_all <- function(the_cntry) {
  
  # the_cntry <- "CH"
  
  out <- the_cntry %>% 
    map(~{
      .x %>% 
        paste0(c("-yesterday", "-last_7_days", "-last_30_days", 
                 "-last_90_days"))
    }) %>% 
    unlist() %>% 
    .[str_detect(., "last_30_days")] %>% 
    # .[100:120] %>% 
    map_dfr_progress(~{
      the_assets <- httr::GET(paste0("https://github.com/favstats/meta_ad_reports/releases/expanded_assets/", .x))
      
      the_assets %>% httr::content() %>% 
        html_elements(".Box-row") %>% 
        html_text()  %>%
        tibble(raw = .)   %>%
        # Split the raw column into separate lines
        mutate(raw = strsplit(as.character(raw), "\n")) %>%
        # Extract the relevant lines for filename, file size, and timestamp
        transmute(
          filename = sapply(raw, function(x) trimws(x[3])),
          file_size = sapply(raw, function(x) trimws(x[6])),
          timestamp = sapply(raw, function(x) trimws(x[7]))
        ) %>% 
        filter(filename != "Source code") %>% 
        mutate(release = .x) %>% 
        mutate_all(as.character)
    })
  
  latest <- out  %>% 
    rename(tag = release,
           file_name = filename) %>% 
    arrange(desc(tag)) %>% 
    separate(tag, into = c("country", "timeframe"), remove = F, sep = "-") %>% 
    filter(str_detect(file_name, "rds")) %>% 
    mutate(day  = str_remove(file_name, "\\.rds|\\.zip") %>% lubridate::ymd()) %>% 
    arrange(desc(day)) %>% 
    group_by(country) %>% 
    slice(1) %>% 
    ungroup() 
  
  
  
  download.file(paste0("https://github.com/favstats/meta_ad_reports/releases/download/", the_cntry,"-last_30_days/", latest$file_name), 
                destfile = "report.rds"
  )
  
  report <- readRDS("report.rds")
  
  
  thismonth <- lubridate::today() %>% - 7
  
  report$page_id %>% 
    map_dfr_progress(~{
      
      page_id <- .x
      # page_id <- report$page_id[1]
      
      page_df %>% 
        goto(glue::glue("https://www.facebook.com/ads/library/?active_status=all&ad_type=political_and_issue_ads&country={the_cntry}&view_all_page_id={page_id}&sort_data[direction]=desc&sort_data[mode]=relevancy_monthly_grouped&search_type=page&media_type=all&start_date[min]={as.character(thismonth)}&start_date[max]="))
      
      
      
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
      
      the_content <- page_df %>% 
        playwrightr::get_content()
      
      
      nads <- the_content %>% 
        html_elements(xpath = "/html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[7]/div[1]/div/div/div/div/div/div[1]/div") %>% 
        
        # html_elements(xpath = "/html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[6]/div[1]/div/div/div/div/div/div[1]/div") %>% 
        html_text() %>% 
        parse_number()
      
      
      # the_content %>% retrieve_ads(3)
      
      # the_content %>% 
      #   html_elements(xpath = "/html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[7]/div[2]/div[2]/div[4]/div[1]/div[1]/div/div[1]/div/div[1]")
      # /html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]
      # /html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[7]/div[2]/div[2]/div[4]/div[1]/div[1]/div/div[1]/div/div[1]
                  # index <- 1
      # the_content %>% 
      #   # html_elements(xpath = glue::glue('//*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]')) %>% 
      #   html_elements(xpath = glue::glue('/html/body/div[1]/div[1]/div[1]/div/div[1]/div/div[7]/div[2]/div[2]/div[4]/div[1]/div[1]/div/div[1]/div/div[{index}]')) %>% 
      # 
      #   # html_elements(xpath = glue::glue('//*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[1]')) %>% 
      #   # html_children() %>% 
      #   # html_children() %>% 
      #   # html_children() %>% 
      #   html_children() %>% 
      #   html_children() %>% 
      #   html_text() %>% set_names(paste0(1:length(.))) %>% 
      #   as.list() %>% as_tibble() %>% 
      #   janitor::clean_names()   # %>% View()
      
      first_attempt <- 1:nrow(get_one) %>% 
        map_dfr_progress(~{
          the_content %>% retrieve_ads(.x)
        }) %>%
        arrange(x3) 
      
      
      
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
      get_ad_snapshots_sf <- possibly(get_ad_snapshots, quiet = F, otherwise = NULL)
      # library(metatargetr)
      metadat <- fulldat %>% 
        distinct() %>% 
        mutate(id = str_remove(x1, "Library ID: ")) %>% 
        pull(id) %>% 
        map_dfr_progress(~{
          get_ad_snapshots_sf(.x, download = T, hashing = T, mediadir = "data/media")
        })
      
      
      # saveRDS(metadat, "data/us.rds")
      the_tag <- paste0(the_cntry, "-", "media")
      
      save_csv(metadat, glue::glue("data/media/{the_cntry}.csv"))
      save_csv(fulldat, glue::glue("data/media/{the_cntry}-pw.csv"))
      
      try({
        download.file(paste0("https://github.com/favstats/meta_ad_media/releases/download/", the_tag, glue::glue("/{the_cntry}.csv")), 
                      destfile = glue::glue("data/media/{the_cntry}.csv")
        )
        
        if(exists( glue::glue("data/media/{the_cntry}.csv"))){
          read_csv( glue::glue("data/media/{the_cntry}.csv")) %>% 
            distinct() %>% 
            mutate(cntry = the_cntry) %>% 
            save_csv( glue::glue("data/media/{the_cntry}.csv"))         
        } 
      })
  
      try({
        download.file(paste0("https://github.com/favstats/meta_ad_media/releases/download/", the_tag, glue::glue("/{the_cntry}-pw.csv")), 
                      destfile = glue::glue("data/media/{the_cntry}-pw.csv")
        )
        
        if(exists( glue::glue("data/media/{the_cntry}-pw.csv"))){
          read_csv( glue::glue("data/media/{the_cntry}-pw.csv")) %>% 
            distinct() %>% 
            mutate(cntry = the_cntry) %>% 
            save_csv( glue::glue("data/media/{the_cntry}-pw.csv"))         
        } 
      })
      
      

      
      
      full_repos <- read_rds("https://github.com/favstats/meta_ad_reports/releases/download/ReleaseInfo/full_repos.rds")
      
      
      # cntry_name
      
      ## TODO: fix it
      # if(!(the_tag %in% release_names)){
      try({
        pb_release_create_fr(repo = "favstats/meta_ad_media", 
                             tag = the_tag,
                             body = paste0("This release includes ", the_cntry, " Meta ad media."), 
                             releases = full_repos)    # Sys.sleep(5)    
      })
      
      # }
      
      # file.copy(report_path, paste0(the_date, ".zip"), overwrite = T)
      
      try({
        # print(paste0(the_date, ".rds"))
        # print(the_tag)
        # debugonce(pb_upload_file_fr)
        # debugonce(pb_upload_file_fr)
        
        dir("data/media", recursive = T, full.names = T) %>% 
          keep(~str_detect(.x, "waiting_room")) %>%
          walk(file.remove)
        
        # try({
          download.file(paste0("https://github.com/favstats/meta_ad_media/releases/download/", the_tag, "/hash_table.csv"), 
                        destfile = "data/media/hash_table.csv"
          )
        # })
        
        
        if(exists("hash_table.csv")){
          read_csv("data/media/hash_table.csv") %>% 
            distinct() %>% 
            mutate(cntry = the_cntry) %>% 
            save_csv("data/media/hash_table.csv")         
        } 

        
        hash_table <- read_csv("data/media/hash_table.csv")
        

        
        dir("data/media", recursive = T, full.names = T) %>% 
          walk(pb_upload_file_fr(repo = "favstats/meta_ad_media", tag = the_tag, releases = full_repos))
        
        
        
        # dir("data/media", recursive = T, full.names = T) %>% 
        #   # discard(~str_detect(.x, "hash_table")) %>% 
        #   walk(file.remove)
        
        
        
      })
      
    })
  

  
  
  
  

  
}
# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[2]/div[4]/div[1]/div[16]
# //*[@id="content"]/div/div[1]/div/div[6]/div[2]/div[3]/div[3]/div[1]/div[1]


sf_retrieve_em_all <- possibly(retrieve_em_all, otherwise = NULL, quiet = F)

cntries %>% 
  sample(length(.)) %>% 
  keep(~str_detect(.x, "CH")) %>% 
  # .[1] %>% 
  walk_progress(sf_retrieve_em_all)
  
# })



# gc()

# # .[1:7] %>% 
# walk_progress( ~ {
# 
#   
# })

# unzip("report/TN/2023-11-28.zip", exdir = "extracted", overwrite = T)

# unzip(dir(paste0("report/",cntry_str), full.names = T, recursive = T), exdir = "extracted")

print("NL UNZIPPED")



unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)

print("################6")

dir() %>%
  keep( ~ str_detect(.x, ".txt")) %>%
  discard( ~ str_detect(.x, "n_advertisers.txt")) %>%
  walk(file.remove)

# all_reports_old <- readRDS("logs/all_reports.rds")

print("################9")

# all_reports <- dir("report", full.names = T, recursive = T)

print("################10")

# all_reports <- all_reports_old %>% 
#   c(all_reports) %>% 
#   unique()
# print("################11")
# 
# saveRDS(all_reports, file = paste0("logs/all_reports_", time_preset, ".rds"))

print("################12")

unlink("report", recursive = T, force = T)
unlink("extracted", recursive = T, force = T)
  