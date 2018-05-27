rm(list = ls());gc()
library(data.table)
library(dplyr)
library(stringr)
library(rvest)
library(jsonlite)


######## ys brand company
url_search <- paste0('http://bizsearch.winshangdata.com/pinpai/y0-t0-m0-q0-p0-d0-z0-n0-c0-l0-x0-b0-pn',1:708,'.html')

url_content <- list()
i = 1
t1 <- Sys.time()
for(i in 1:708){
  url_tmp <- url_search[i]
  url_content_tmp <- read_html(url_tmp, encoding = "UTF-8")
  url_content[[i]] <- url_content_tmp
  if(i %% 10 == 0){
    print(i)
    Sys.sleep(runif(1,min = 2, max = 5))
  }
}
Sys.time() - t1


get_brand_href <-  function(x){
  aa_tmp <- x %>% html_nodes(".l-list .l-attr a") %>% html_attrs()
  aa_tmp <- data.table(matrix(unlist(aa_tmp), ncol = 4 ,byrow = T))
  aa_tmp
}

brand_href <- lapply(url_content, get_brand_href)
brand_href <- rbindlist(brand_href)
brand_href <- select(brand_href, title = V4, href = V1)
# save(brand_href, file = "F://scrapy_data/R/brand/brand_href.rdata")
load("F://scrapy_data//R//brand//brand_href.rdata")


brand_company <- c()
t1 <- Sys.time()
for(i in 1:nrow(brand_href)){
  try(brand_company[i] <- read_html(brand_href$href[i], encoding = "UTF-8") %>% html_nodes(".d-des-txt") %>% html_text())
  if(i %% 50 == 0){
    print(i)
    Sys.sleep(runif(1,min = 2, max = 5))
  }
}
Sys.time() - t1


index <- which(is.na(brand_company))
t1 <- Sys.time()
for(i in index){
  try(brand_company[i] <- read_html(brand_href$href[i], encoding = "UTF-8") %>% html_nodes(".d-des-txt") %>% html_text())
  if(i %% 50 == 0){
    print(i)
    Sys.sleep(runif(1,min = 2, max = 5))
  }
}
Sys.time() - t1


brand_href[index,]
brand_href$company <- brand_company
brand_href <- select(brand_href, brand = title, company, href)
write.csv(brand_href, "F://scrapy_data//R//brand//brand_company.csv", row.names = F)
