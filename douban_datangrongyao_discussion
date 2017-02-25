##---------大唐荣耀-----------------
###### 抓取豆瓣大唐荣耀讨论帖   ####
##---------大唐荣耀-----------------

# 原网址：https://movie.douban.com/subject/24827545/discussion/?start=0&sort_by=time
# 对豆瓣网上网友关于《大唐荣耀》这部电视剧的发帖信息

library(rvest)
library(stringr)
library(dplyr)

# 对第2页进行抓取
webs <- "https://movie.douban.com/subject/24827545/discussion/?start=20&sort_by=time"
web_content <- read_html(webs, encoding = "UTF-8")

titles <- web_content %>%
  # 这里css = "td:nth-child(1)"表明帖子名称存在于表格之中，td表示table data(表格的列)
  # nth-child(1)表示第一列，tr:nth-child(9)表示第9行
  html_nodes(css = "td:nth-child(1)") %>%
  html_text() %>%
  str_replace_all(pattern = "[[:space:]]", replacement = "")

# 对所有帖子进行抓取
webSpyder_datang <- function(page) {
  # 网址
  baseUrl <- "https://movie.douban.com/subject/24827545/discussion/?start="
  web <- str_c(baseUrl, 20*(page - 1), "&sort_by=time")
  
  # 抓取贴名
  titles <- web_content %>%
    html_nodes(css = "td:nth-child(1)") %>%
    html_text() %>%
    str_replace_all(pattern = "[[:space:]]", replacement = "")
  titles <- titles[!(titles %in% c("一刻", "豆瓣摄影", "最近更新/热门话题"))]
  
  # 抓取发帖人
  authors <- web_content %>%
    html_nodes(css = "td:nth-child(2)") %>%
    html_text() %>%
    str_replace_all(pattern = "[[:space:]]", replacement = "")
  authors <- authors[authors != "作者"]
  
  # 抓取回应数
  responses <- web_content %>%
    html_nodes(css = "td:nth-child(3)") %>%
    html_text() %>%
    str_replace_all(pattern = "[[:space:]]", replacement = "")
  responses <- responses[responses != "回应" ]
  
  # 抓取更新时间
  update_time <- web_content %>%
    html_nodes(css = "td:nth-child(4)") %>%
    html_text()
  update_time <- update_time[update_time != "更新时间"]
  
  data.frame(titles = titles, authors = authors, 
             responses = responses, update_time = update_time,
             stringsAsFactors = FALSE)
}

webSpyder_datangm <- compiler::cmpfun(webSpyder_datang)

# 循环抓取
pb <- progress_bar$new(
  format = "  progress [:bar] :percent in :elapsed",
  total = 10, clear = FALSE)

datang <- vector(mode = "list", length = 17L)
for (i in seq_along(datang)) {
  tryCatch(
    {datang[[i]] <- webSpyder_datangm(i)},
    error = function(e){cat("ERROR :",conditionMessage(e),"\n")})
  pb$tick()
  Sys.sleep(0.5) #增加了Sys.sleep(seconds)函数，让每一步循环都暂停一段时间。
}
str(datang, max.level = 1)

datang_douban <- purrr::map_df(datang, rbind)
DT::datatable(tibble::as_tibble(datang_douban))
