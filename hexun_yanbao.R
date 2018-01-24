##%######################################################%##
#                                                          #
####                 抓取和讯网的研报信息               ####
#                                                          #
##%######################################################%##

# Info: 抓取和讯网的研报信息
# Author: Leo Lee
# Last update: 2018-01-02 23:09:23


rm(list = ls())
gc()
## 和讯网研报网址：http://yanbao.stock.hexun.com/listnews1_1.shtml
library(rvest)
library(stringr)
library(dplyr)
library(magrittr)

html_session("http://yanbao.stock.hexun.com/listnews1_1.shtml")

yanbao_spyder <- function(page) {
  base_url <- "http://yanbao.stock.hexun.com/listnews1_"
  page_content <- paste0("http://yanbao.stock.hexun.com/listnews1_", page, ".shtml") %>% 
    read_html(encoding = "GBK") %>%  ## 注意：这里字符集的设定只能是`GBK`
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

# bind_rows()函数来自dplyr包，可以直接把一个由数据框组成的列表按行合并为一个更大的数据框
# 这里也可以用`do.call(yanbao1_20, rbind)`
bind_rows(yanbao1_20) %>% 
  DT::datatable()

## 注：这里的代码并不完整，并没有考虑到抓取失败的问题，有时间再重新写
  
