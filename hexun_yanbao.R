#################################
### 抓取和讯网的研报信息
#################################

## 和讯网研报网址：http://yanbao.stock.hexun.com/listnews1_1.shtml
library(rvest)
library(stringr)
library(dplyr)
library(magrittr)
yanbao_spyder <- function(page) {
  base_url <- "http://yanbao.stock.hexun.com/listnews1_"
  page_content <- paste0("http://yanbao.stock.hexun.com/listnews1_", page, ".shtml") %>% 
    read_html(encoding = "GBK") %>% 
    html_table(fill = TRUE) %>% 
    .[[1]] %>% 
    .[, -6]
  Sys.sleep(0.5)  # 让爬虫休息0.5秒
  page_content
}

# 抓取第1页的网址，判断爬虫函数yanbao_spyder是否存在问题
yanbao_spyder(1)

# 抓取前20页
yanbao1_20 <- lapply(1:20, yanbao_spyder)
yanbao1_20

bind_rows(yanbao1_20) %>% 
  DT::datatable()
  











































