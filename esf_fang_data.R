rm(list = ls());gc()
library(data.table)
library(dplyr)
library(stringr)
library(RCurl)
library(rvest)
library(jsonlite)

# ### esf.fang.city, city match
# esffang_city_url <- function(){
#   esf_fang_url <- "https://sh.esf.fang.com/newsecond/esfcities.aspx"
#   esf_fang_url <- read_html(esf_fang_url, encoding = "GBK")
#   esffang_city <<- data.frame(city = esf_fang_url %>% html_nodes(".onCont a") %>% html_text(),
#                             city_url = esf_fang_url %>% html_nodes(".onCont a") %>% html_attr("href")
#   )
#   return("finished")
# }
# 
# esffang_city_url()
# esffang_city$city_url <- paste0("https:", esffang_city$city_url)
# write.csv(esffang_city, "esffang_city_url.csv", row.names = F, quote = F)

esffang_city <- fread("esffang_city_url.csv")



esffang_data_get <- function(city_get = "深圳"){
  city_url <- esffang_city$city_url[esffang_city$city == city_get]
  if(length(city_url) == 0) print("WRONG CITY NAME")
  
  esffang_city_district_url <- read_html(paste0(city_url, "/housing/"), encoding = "GBK")
  esffang_city_district <<- data.frame(district = esffang_city_district_url %>% html_nodes(".qxName>a") %>% html_text(),
                                       district_url = esffang_city_district_url %>% html_nodes(".qxName>a") %>% html_attr("href"))
  esffang_city_district <- esffang_city_district[-1, ]
  esffang_city_district$district_url <- paste0(city_url, esffang_city_district$district_url)
  
  print("GET CITY DISTRICT URL")
  
  
  district_business_area <- list()
  tmp <-  lapply(1:nrow(esffang_city_district), function(i) {
    district_tmp <- read_html(esffang_city_district$district_url[i], encoding = "GBK")
    business_area <- district_tmp %>% html_nodes("#shangQuancontain a") %>% html_text()
    business_area_url <- district_tmp %>% html_nodes("#shangQuancontain a") %>% html_attr("href")
    business_area_reuslt <- data.frame(business_area, business_area_url)
    business_area_reuslt$district <- esffang_city_district$district[i]
    district_business_area[[i]] <<- business_area_reuslt
    district_page <- district_tmp %>% html_nodes(".fanye span") %>% html_text()
    return(district_page)
  })
  
  esffang_city_district$page_num <- unlist(tmp)
  esffang_city_district$page_num <- as.numeric(str_replace_all(esffang_city_district$page_num, "共|页", ""))
  print("GET CITY DISTRICT BUSINESS AREA URL")
  
  district_business_area <- rbindlist(district_business_area)
  district_business_area <- district_business_area[district_business_area$business_area != "不限",]
  district_business_area <- district_business_area[district_business_area$district %in% esffang_city_district$district[esffang_city_district$page_num >= 100]]
  district_business_area$business_area_url <- paste0(city_url, district_business_area$business_area_url)
  
  district_business_area$page_num <- unlist(lapply(district_business_area$business_area_url, function(x){
    read_html(x, encoding = "GBK") %>% html_nodes(".fanye span") %>% html_text()
  }))
  district_business_area$page_num <- as.numeric(str_replace_all(district_business_area$page_num, "共|页", ""))
  
  
  
  url_tmp <- esffang_city_district[esffang_city_district$page_num < 100,] %>% data.table()
  url_tmp2 <- select(district_business_area, district, district_url = business_area_url, page_num) %>% data.table()
  url_tmp <- rbind(url_tmp, url_tmp2)
  url_tmp$district_url <- str_replace(url_tmp$district_url, "_0_0_0_0_1_0_0_0/", "")
  
  
  print("Final ROUND:BEGIN TO GET DATA")
  cat("BUSINESS AREA NUM: ", nrow(url_tmp))
  
  #### get url
  house_out <- list()
  for(i in 1:nrow(url_tmp)){
    cat('district:', i)
    url_scrapy <- paste0(url_tmp$district_url[i], "_0_1_0_0_", 1:url_tmp$page_num[i], "_0_0_0/")

    house_out[[i]] <- lapply(1:length(url_scrapy), function(j) {
      aa <- read_html(url_scrapy[j], encoding = "GBK") 
      Sys.sleep(runif(1, max = 2))
      print(j)
      # house
      aa %>% html_nodes('.houseList')
    })
    Sys.sleep(2)
  }
  
  ### process data to dataframe 
  result_out <- list()
  for(i in 1:length(house_out)){
    tmp <- house_out[[i]] %>% purrr::flatten() %>% lapply(function(x) x %>% html_nodes('.list'))
    name <- unlist(lapply(tmp, function(x) x %>% html_nodes('.plotTit') %>% html_text()))
    name_href <- unlist(lapply(tmp, function(x) x %>% html_nodes('.plotTit') %>% html_attr("href")))
    name_price <- unlist(lapply(tmp, function(x) x %>% html_nodes('.listRiconwrap') %>% html_text() %>% str_trim() %>% str_extract('.*元')))
    data_out <- data.frame(name, name_href, name_price)
    data_out$region <- url_tmp$district[i]
    result_out[[i]] <- data_out
  }
  
  
  result_out <- rbindlist(result_out)
  
  return(result_out)
}


result_out <- esffang_data_get("成都")
