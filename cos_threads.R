## 抓取统计之都（https://cos.name/cn/）论坛发帖信息
# 基础网址
baseUrl <- "https://cos.name/cn/"
# 看规律
page1 <- "https://cos.name/cn/"
page2 <- "https://cos.name/cn/page/2/"
page3 <- "https://cos.name/cn/page/3/"

webSpyder_cos <- function(page, baseUrl = "https://cos.name/cn/") {
  if (is.numeric(page)) {
    if (page == 1) {
      website <- baseUrl
    } else {
      website <- str_c(baseUrl, "page/", page, "/")
    } 
  } else warning("page must be a numerical scalar")
  
  web_content <- read_html(website)
  
  # 提取帖子的主题
  topic <- web_content %>%
    html_nodes(css = "a.bbp-topic-permalink") %>%
    html_text()
  
  # 提取发帖人
  author <- web_content %>%
    html_nodes(css = "span.bbp-topic-started-by") %>%
    html_nodes(css = ".bbp-author-name")%>%
    html_text()
    
    
  # 提取帖子参与人数
  participant <- web_content %>%
    html_nodes(css = "li.bbp-topic-voice-count") %>%
    html_text() %>%
    .[-1]
  
  # 提取帖子回应人数
  reply <- web_content %>%
    html_nodes(css = "li.bbp-topic-reply-count") %>%
    html_text() %>%
    .[-1]
  
  # 提取帖子最后更新的时间
  update <- web_content %>%
    html_nodes(css = "li.bbp-topic-freshness") %>%
    html_text() %>%
    .[-1] %>%
    str_extract(pattern = "\\d.*前")
  
  # 提取帖子对应的网址
  link <- web_content %>%
    html_nodes(css = "a.bbp-topic-permalink") %>%
    str_extract('https.*\\d/')
  
  
  data.frame(topic = topic, author = author, 
             participant = participant, 
             reply = reply, 
             update = update,
             link = link,
             stringsAsFactors = FALSE)
}
# 对爬虫函数进行编译，提高速度
webSpyder_cosm <- compiler::cmpfun(webSpyder_cos)

# 新建一个进度条
pb <- progress_bar$new(
  format = "  抓取进度 [:bar] :percent in :elapsed",
  total = 1170, clear = TRUE)

cos_threads <- vector(mode = "list", length = 1170)
for (i in seq_along(cos_threads)) {
  tryCatch(
    {cos_threads[[i]] <- webSpyder_cosm(i)},
    error = function(e) {cat("ERROR :", conditionMessage(e),"\n")})
  pb$tick()
  Sys.sleep(0.5) # 增加了Sys.sleep(seconds)函数，让每一步循环都暂停一段时间。
}

cos_thread_final <- purrr::map_df(cos_threads, rbind)
DT::datatable(cos_thread_final)
dim(cos_thread_final)

library(readr)
write_csv(cos_thread_final, path = "统计之都发帖统计.csv")

## 参考：大数据分析之——数据提取(http://blog.sciencenet.cn/blog-556556-855593.html)
