
##%######################################################%##
#                                                          #
####                         前程无忧爬虫               ####
#                                                          #
##%######################################################%##

# Date: 2017年12月31日下午
# Info: 抓取前程无忧上包含“数据挖掘”关键词的职位信息，首先打开`前程无忧`官方网站，然后键入“数据挖掘”关键词进行搜索，记录下首页地址
# Author：Leo Lee
# Last update: 2018-01-02 22:57:04


## `数据挖掘`职位信息抓取
rm(list = ls())
gc()
library(rvest)
library(stringr)

# 测试抓取第一页
url0 <- "http://search.51job.com/jobsearch/search_result.php?fromJs=1&keyword=%E6%95%B0%E6%8D%AE%E6%8C%96%E6%8E%98&keywordtype=2&lang=c&stype=2&postchannel=0000&fromType=1&confirmdate=9"
html_session(url0)

web0 <- read_html(url0, encoding = "gbk")
web0

web0 %>% 
  html_nodes(css = "div.el p.t1 span a") %>% 
  html_attr(name = "title") ->
  job_title1
job_title1

web0 %>% 
  html_nodes(css = "div.el>span.t2>a") %>%  # >表示父子关系，下一级，而非多级
  html_attr(name = "title") ->
  job_company

web0 %>% 
  html_nodes(css = "div.el>span.t3") %>% 
  html_text() %>% 
  .[-1] ->
  job_location

web0 %>% 
  html_nodes(css = "div.el>span.t4") %>% 
  html_text() %>% 
  .[-1] ->
  job_salary
job_salary

web0 %>% 
  html_nodes(css = "div.el>span.t5") %>% 
  html_text() %>% 
  .[-1] -> 
  job_pub_date

tibble::tibble(job_title = job_title1,
               job_company,
               job_location,
               job_salary,
               job_pub_date) ->
  dm_job_page1
DT::datatable(dm_job_page1)



## 构造抓取函数
dm_spyder <- function(website) {
  web0 <- read_html(website, encoding = "gbk")
  web0
  
  web0 %>% 
    html_nodes(css = "div.el p.t1 span a") %>% 
    html_attr(name = "title") ->
    job_title
  
  web0 %>% 
    html_nodes(css = "div.el>span.t2>a") %>%  # >表示父子关系，下一级，而非多级
    html_attr(name = "title") ->
    job_company
  
  web0 %>% 
    html_nodes(css = "div.el>span.t3") %>% 
    html_text() %>% 
    .[-1] ->
    job_location
  
  web0 %>% 
    html_nodes(css = "div.el>span.t4") %>% 
    html_text() %>% 
    .[-1] ->
    job_salary
  job_salary
  
  web0 %>% 
    html_nodes(css = "div.el>span.t5") %>% 
    html_text() %>% 
    .[-1] -> 
    job_pub_date
  
  tibble::tibble(job_title,
                 job_company,
                 job_location,
                 job_salary,
                 job_pub_date) 
}



## 抓取所有页面
## 由于存在250页的搜索结果，因此需要获取第1页至第250页的网址
library(stringr)
library(rvest)
web0 %>% 
  html_nodes(css = "div.p_in li a") %>% 
  html_attr(name = "href") %>% 
  .[1] ->
  links_page2

## 测试规律
# links_page[2] <- read_html(links_page[1], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name = "href") %>% 
#   .[1]
# links_page[3] <- read_html(links_page[2], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name= "href") %>% 
#   .[3]
# links_page[4] <- read_html(links_page[3], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name = "href") %>% 
#   .[4] 
# links_page[5] <- read_html(links_page[4], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name = "href") %>% 
#   .[5] 
# links_page[6] <- read_html(links_page[5], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name = "href") %>% 
#   .[6] 
# 
# 
# links_page[7] <- read_html(links_page[6], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name = "href") %>% 
#   .[7] 
# links_page[8] <- read_html(links_page[7], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_attr(name = "href") %>% 
#   .[7] 
# links_page[9] <- read_html(links_page[8], encoding = "gbk") %>% 
#   html_nodes(css = "div.p_in li a") %>% 
#   html_text()
#   html_attr(name = "href") %>% 
#   .[7] 

## 获取所有网址
links_page <- character(length = 250L)
links_page[1] <- url0

for (i in 2:250) {
  if (i == 2) {
    links_page[i] <- read_html(links_page[i - 1], encoding = "gbk") %>% 
      html_nodes(css = "div.p_in li a") %>% 
      html_attr(name = "href") %>% 
      .[1]
  } else if (i <= 6) {
    links_page[i] <- read_html(links_page[i - 1], encoding = "gbk") %>% 
      html_nodes(css = "div.p_in li a") %>% 
      html_attr(name = "href") %>% 
      .[i]
  } else {
    links_page[i] <- read_html(links_page[i - 1], encoding = "gbk") %>% 
      html_nodes(css = "div.p_in li a") %>% 
      html_attr(name = "href") %>% 
      .[7]
  }
  cat(paste0("Page ", i, " finished.", "\n", "The site is ", links_page[[i]]))
  Sys.sleep(rnorm(1, mean = 0.5))
}

# 测试网址是否唯一
unique(links_page) %>% length()


## 遍历抓取所有职位
links_content <- vector(mode = "list", length = 250L)
for (i in seq_along(links_page)) {
  tryCatch(expr = {
    links_content[[i]] <- dm_spyder(links_page[i])
  },
  error = function(e) {cat("Error: ", conditionMessage(e), "\n")}
  )
  cat(str_c("第", i, "页抓取成功！", "\n"))
  Sys.sleep(0.3)
}

## 看看那些页码抓取失败了
sapply(links_content, is.null) %>% which(isTRUE(.)) # 151，152
## 明显第151页和152页抓取失败

links_content[[151]] <- dm_spyder(links_page[151])
links_content[[152]] <- dm_spyder(links_page[152])

dm_jobs <- do.call(rbind, links_content)  
DT::datatable(dm_jobs)  

writexl::write_xlsx(x = dm_jobs, path = "F:\\R_scripts_data\\前程无忧网站数据挖掘职位爬虫数据.xlsx")
shell.exec("F:\\R_scripts_data\\前程无忧网站数据挖掘职位爬虫数据.xlsx")
saveRDS(object = ls(), file = "F:\\R_scripts_data\\前程无忧网站数据挖掘职位爬虫数据.rds")


