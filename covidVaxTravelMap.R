library(httr)
library(purrr)
library(stringr)
library(dplyr)
library(readr)
library(janitor)
library(magrittr)
library(rjson)
library(stringi)
library(rlang)

DW_API <- Sys.getenv("DW_API_KEY")

safe_extract <- function(l, wut) {
  res <- l[wut]
  null_here <- map_lgl(res, is.null)
  res[null_here] <- NA
  res
}

republishChart <- function(API_KEY, chartID, data, subtitle = NULL, notes) {
  
  dataRefresh <- PUT(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                  chartID, "/data"),
                     add_headers(authorization = paste("Bearer", 
                                                       API_KEY, 
                                                       sep = " ")),
                     body = format_csv(data))
  
  call_back <- list(metadata = list())
  
  if (!is.null(subtitle)) {
    call_back$metadata$describe$intro <- subtitle   
  }
  
  call_back$metadata$annotate$notes <- notes
  
  notesRes <- PATCH(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                 chartID),
                    add_headers(authorization = paste("Bearer", API_KEY, 
                                                      sep = " ")),
                    body = call_back,
                    encode = "json")
  
  publishRes <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", 
                 chartID, "/publish"),
    add_headers(authorization = paste("Bearer", 
                                      API_KEY, 
                                      sep = " "))
  )
  
  list(dataRefresh, notesRes, publishRes) -> resList
  
  if (any(map_lgl(resList, http_error))) {
    which(map_lgl(resList, http_error))[1] -> errorIdx
    
    stop_for_status(resList[[errorIdx]], task = paste0("update step ",
                                                       errorIdx, 
                                                       " of chart ", 
                                                       chartID))
    
  } else {
    message(paste0("Chart ", chartID, " updated successfully"))
  }
  
}

getGHTree <- function(url) {
  req <- GET(url = url)
  stop_for_status(req)
  return(req)
}

statePops <- read_csv(file = "statePops.csv", col_names = T, 
                      col_types = "cci")

kffFetch <- tryCatch(
  {
    kffTree <- getGHTree("https://api.github.com/repos/KFFData/COVID-19-Data/git/trees/kff_master?recursive=1")
  }, error = function(cond) {
    condFull <- error_cnd(class = "kffGithubError", message = paste("An error occured with the update:", 
                                                                        cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
)


map_chr(content(kffTree)$tree, 'path') %>% 
  str_subset('State Policy Actions/State COVID-19 Vaccine Mandates/Detailed Table Categories/(.*)') %>% 
  str_match("\\d{4}-\\d{2}-\\d{2}.*") %>% 
  sort(decreasing = T) %>% 
  nth(1) -> latestCovidVaxMandateFileSuffix

latestCovidVaxMandateFile <- paste0("https://raw.githubusercontent.com/KFFData/COVID-19-Data/kff_master/State%20Policy%20Actions/State%20COVID-19%20Vaccine%20Mandates/Detailed%20Table%20Categories/",
                                    latestCovidVaxMandateFileSuffix)
  

stateConversion <- tibble(
  state_full_name = c(state.name, "District of Columbia"),
  state_abbv = c(state.abb, "DC")
)


kffVaxDwnld <- tryCatch(
  {
    read_csv(file = latestCovidVaxMandateFile, col_names = c(
      "state_full_name", "Any Mandate in Place?", "Description of Mandate"
    ), col_types = "ccc") %>% 
      rename_with(make_clean_names, .cols = everything()) %>% 
      filter(state_full_name != "United States") -> vaxMandates
  }, error = function(cond) {
    condFull <- error_cnd(class = "kffVaxUrlError", message = paste("An error occured with the update:", 
                                                                    cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
)


kffMaskDwnld <- tryCatch(
  {
    read_csv(file = "https://raw.githubusercontent.com/KFFData/COVID-19-Data/kff_master/State%20Policy%20Actions/State%20Social%20Distancing%20Actions/Master%20File_Social%20Distancing.csv",
             col_names = c(
               "state_full_name", "statewide_face_mask_requirement", "emergency_declaration", "status_of_reopening"
             ), col_types = "cccc") %>% 
      rename_with(make_clean_names, .cols = everything()) %>% 
      filter(state_full_name != "United States") -> maskSocialDistancingMandates
  }, error = function(cond) {
    condFull <- error_cnd(class = "kffMaskUrlError", message = paste("An error occured with the update:", 
                                                                    cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
)

vaxMandates %>% 
  inner_join(maskSocialDistancingMandates, by = "state_full_name") %>% 
  inner_join(stateConversion, by = "state_full_name") %>% 
  mutate(description_of_mandate = iconv(description_of_mandate, 
                                        "latin1", "ASCII", sub = "")) -> allRestrictions


## COVID-19 cases
covidCases <- tryCatch({
  cdcCasesJSON <- fromJSON(file = "https://raw.githubusercontent.com/Dotdash-Data-Journalism/covidVaxTravelMap/main/cdcCovidCases.json")
  
  cdcCasesTable <- cdcCasesJSON %>% 
    extract2("US_MAP_DATA") %>%
    map_df(`[`)
  
  cdcUpdateDate <- cdcCasesJSON %>% 
    extract2("CSVInfo") %>% 
    extract2("update") %>% 
    str_extract("\\w{3}\\s+\\d{1,2}\\s+\\d{4}") %>% 
    as.Date(format = "%b %d %Y")
  
  cdcCasesTable[which(cdcCasesTable$abbr == "NY"), "new_cases07"] <- sum(
    pull(cdcCasesTable[which(cdcCasesTable$abbr == "NY"), "new_cases07"]), 
    pull(cdcCasesTable[which(cdcCasesTable$abbr == "NYC"), "new_cases07"])
  )
  
  cdcCasesTable[which(cdcCasesTable$abbr == "NY"), "Seven_day_cum_new_cases_per_100k"] <- round((pull(cdcCasesTable[which(cdcCasesTable$abbr == "NY"), "new_cases07"]) / filter(statePops, Location == "NY")$Census2019) * 100000, 1)
    

  
  cdcCases <- cdcCasesTable %>% 
    select(abbr, Seven_day_cum_new_cases_per_100k) %>% 
    inner_join(statePops, by = c("abbr" = "Location")) %>%
    mutate(case_date = cdcUpdateDate) %>% 
    rename(cases_per_100K = Seven_day_cum_new_cases_per_100k) %>% 
    select(abbr, LongName, cases_per_100K, case_date)
  
}, error = function(cond) {
  condFull <- error_cnd(class = "covidCasesError", message = paste("An error occured with the update:", 
                                                                   cond, "on", Sys.Date(), "\n"
  ))
  
  write(condFull[["message"]], "./errorLog.txt", append = T)
  
  return(condFull)
})



## COVID-19 vax rates
covidVax <- tryCatch(
  {
    cdcVax <- fromJSON(file = "https://raw.githubusercontent.com/Dotdash-Data-Journalism/covidVaxTravelMap/main/cdcVaccines.json") %>% 
      extract2(2) %>%
      map_df(`[`) %>% 
      mutate(vax_date = base::as.Date(Date)) %>% 
      mutate(LongName = if_else(
        LongName == "New York State", "New York", 
        LongName)) %>% 
      select(vax_date, Location, LongName, Census2019, Series_Complete_Pop_Pct)
  }, error = function(cond) {
    condFull <- error_cnd(class = "covidVaxError", message = paste("An error occured with the update:", 
                                                                     cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
)

## Joining
cdcCases %>%
  inner_join(cdcVax, by = c("abbr" = "Location", "LongName" = "LongName")) %>% 
  inner_join(allRestrictions, by = c("LongName" = "state_full_name")) -> cdcFull

## Writing
write_csv(cdcFull, "cdcFull.csv")

### Updating DWs
republishChart(API_KEY = DW_API, chartID = "2rzuj", data = cdcFull, notes = paste0(
  "COVID-19 vaccination and mandate/restrictions data as of ", format(unique(cdcVax$vax_date), "%m/%d/%Y")
))

republishChart(API_KEY = DW_API, chartID = "1CpAe", data = cdcFull, notes = paste0(
  "COVID-19 case and mandate/restrictions data as of ", format(cdcUpdateDate, "%m/%d/%Y")
))
