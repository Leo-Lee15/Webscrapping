##%######################################################%##
#                                                          #
####                       慕课网爬虫                   ####
#                                                          #
##%######################################################%##

## Author: Leo
## Last update: 2018-01-10 0:26

library(rvest)
library(stringr)
library(dplyr)
library(progress)

web_url <- "https://www.imooc.com/"
html_session(web_url)

page_content <- read_html(web_url, encoding = "utf-8")
page_content


## 抓取大类课程名称
page_content %>% 
  html_nodes(css = "div.item a") %>% 
  html_text() %>%
  # \\s匹配空格
  str_replace_all(pattern = "[\n\r\t\\s]", replacement = "") ->
  course_categories

## 抓取大类课程链接
page_content %>% 
  html_nodes(css = "div.item a") %>% 
  html_attr(name = "href") %>% 
  str_c("https://www.imooc.com", .) ->
  course_detailed_urls
course_detailed_urls

df1 <- tibble::tibble(course_categories, course_detailed_urls)
df1

## 前端开发
course_detailed_urls %>% 
  .[[1]] %>% 
  read_html ->
  content_page1
## 前端开发课程名称
content_page1 %>% 
  html_nodes(css = "div.course-card-content h3") %>% 
  html_text() ->
  course_page1_name
# 前端开发课程难度和学习人数
content_page1 %>% 
  html_nodes(css = "div.course-card-info span") %>% 
  html_text() %>% 
  .[c(TRUE, FALSE)] ->
  course_level
content_page1 %>% 
  html_nodes(css = "div.course-card-info span") %>% 
  html_text() %>% 
  .[c(FALSE, TRUE)] %>% 
  as.integer() ->
  course_learn_num
# 前端开发课程描述
content_page1 %>% 
  html_nodes(css = "p.course-card-desc") %>% 
  html_text() ->
  course_desc
# 前端课程网址
content_page1 %>% 
  html_nodes(css = "div.course-card-container a") %>% 
  html_attr(name = "href") %>% 
  str_c("https://www.imooc.com", .) ->
  course_website


tibble(course_page1_name, course_level, course_learn_num, course_desc, course_website) %>% 
  DT::datatable()



### 抓取整个<<前端开发>>课程的所有课程
# 第一页的网址
fe_base_page <- course_detailed_urls[1]
fe_content1 <- read_html(fe_base_page)
# 查看最后一页的网址，看看一共有多少页
fe_content1 %>% 
  html_nodes(css = "div.page a") %>% 
  html_attr(name = "href") %>% 
  .[length(.)] %>% 
  str_extract(pattern = 'page=[0-9]+') %>% 
  str_extract(pattern = '[0-9]+') %>% 
  as.integer() ->
  fe_pages_num



imooc_spyder <- function(base_url, page, category, more_pages = TRUE) {
  if (more_pages) {
    web_cont <- str_c(base_url, "&page=", page) %>% 
      read_html()
  } else {
    web_cont <- base_url %>% 
      read_html()
  }
  
  # 课程名称
  course_name <- web_cont %>% 
    html_nodes(css = "div.course-card-content h3") %>% 
    html_text()
  # 课程难度和学习人数
  course_level <- web_cont %>% 
    html_nodes(css = "div.course-card-info span") %>% 
    html_text() %>% 
    .[c(TRUE, FALSE)]
  
  course_num <- web_cont %>% 
    html_nodes(css = "div.course-card-info span") %>% 
    html_text() %>% 
    .[c(FALSE, TRUE)] %>% 
    as.integer()
  
  # 课程描述
  course_desc <- web_cont %>% 
    html_nodes(css = "p.course-card-desc") %>% 
    html_text()
  # 课程网址
  course_urls <-   web_cont %>% 
    html_nodes(css = "div.course-card-container a") %>% 
    html_attr(name = "href") %>% 
    str_c("https://www.imooc.com", .) 
  
  tibble::tibble(category, course_name, course_level, course_num, course_desc, course_urls)
  
}
# 测试脚本是否存在问题
imooc_spyder(base_url = fe_base_page, page = 1, category = "前端")

## 抓取所有的前端课程
# 创建进度条
pb <- progress_bar$new(
  format = "  progress [:bar] :percent in :elapsed",
  total = fe_pages_num, clear = FALSE)
# 循环抓取
fe_course <- vector(mode = "list", length = fe_pages_num)
for (i in seq_along(fe_course)) {
  tryCatch(
    {fe_course[[i]] <- imooc_spyder(base_url = fe_base_page, page = i, category = "前端开发")},
    error = function(e) {cat("ERROR :", conditionMessage(e),"\n")})
  pb$tick()
  Sys.sleep(0.5) #增加了Sys.sleep(seconds)函数，让每一步循环都暂停一段时间。
}
fe_course

do.call(rbind, fe_course) %>% 
  DT::datatable()


#### 抓取所有的课程  ####
## 首先查看每一个大类课程的页数
pages_per_course <- integer(length = length(course_detailed_urls))
names(pages_per_course) <- course_detailed_urls
for (i in course_detailed_urls) {
  i  %>% 
    read_html(encoding = "utf-8") %>% 
    html_nodes(css = "div.page a") ->
    aa
  if (length(as.character(aa)) != 0) {
    aa %>% 
      html_attr(name = "href") %>% 
      .[length(.)] %>% 
      str_extract(pattern = 'page=[0-9]+') %>% 
      str_extract(pattern = '[0-9]+') %>% 
      as.integer() ->
      pages_per_course[i]
  } else {
    pages_per_course[i] <- 1
  }
}
pages_per_course  # 每一个大类课程的页面数




# 抓取所有的课程
all_courses <- vector(mode = "list", length = length(course_detailed_urls))
names(all_courses) <- course_detailed_urls
all_courses


for (i in seq_along(course_detailed_urls)) {
  for (j in 1:pages_per_course[i]) {
    tryCatch(
      expr = {
        if (unname(pages_per_course[i]) != 1) 
          all_courses[[i]][[j]] <- imooc_spyder(base_url = course_detailed_urls[i],
                                                page = j, category = course_detailed_urls[i], 
                                                more_pages = TRUE)
        else 
          all_courses[[i]][[j]] <- imooc_spyder(base_url = course_detailed_urls[i], 
                                                page = j, category =course_detailed_urls[i], 
                                                more_pages = FALSE)
      },
      error = function(e) {cat("Error: ", conditionMessage(e), "\n")}
    )
    cat(str_c("第", i, "类课程抓取成功！", "  ", names(all_courses)[i], "&page=", j, "\n"))
    Sys.sleep(0.8)
  }
}
str(all_courses, max.level = 2)

all_courses_df1 <- lapply(all_courses, function(x) do.call(rbind, x))
all_courses_df <- do.call(rbind, all_courses_df1)
rownames(all_courses_df) <- NULL

all_courses_df %>% 
  mutate(categories = case_when(
    category == course_detailed_urls[1] ~ "前端开发",
    category == course_detailed_urls[2] ~ "后端开发",
    category == course_detailed_urls[3] ~ "移动开发",
    category == course_detailed_urls[4] ~ "数据库",
    category == course_detailed_urls[5] ~ "云计算&大数据",
    category == course_detailed_urls[6] ~ "运维&测试",
    category == course_detailed_urls[7] ~ "UI设计"
  )) %>% 
  select(-category) %>% 
  select(categories, everything()) ->
  all_courses_tidy_df
all_courses_tidy_df %>%   
  DT::datatable(all_courses_df)

writexl::write_xlsx(x = all_courses_tidy_df, path = "F:\\R_scripts_data\\慕课网所有课程.xlsx")
# shell.exec("F:\\R_scripts_data\\慕课网所有课程.xlsx")


## 绘图
library(ggplot2)
library(viridis)
windowsFonts(
  yahei = windowsFont(family = "Microsoft YaHei")
)
# 查看每一类课程的免费课程数目
all_courses_tidy_df %>% 
  count(categories) %>%
  ggplot(aes(x = reorder(factor(categories), X = n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato2") +
  geom_text(aes(x = categories, y = n + 5, label = n)) +
  theme_minimal() +
  theme(axis.text.y = element_text(family = "yahei", size = 10)) +
  labs(x = "", y = "", title = "慕课网每大类课程的免费课程数目") +
  coord_flip()
# 查看最受欢迎的前10门课程
all_courses_tidy_df %>% 
  arrange(desc(course_num)) %>% 
  slice(1:10) %>% 
  select(categories, course_name, course_num) %>% 
  ggplot(aes(x = reorder(factor(course_name), X = course_num), y = course_num, fill = categories)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = course_name, y = course_num + 40000, label = course_num)) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(axis.text.y = element_text(family = "yahei", size = 10)) +
  labs(x = "", y = "", fill = "课程") + 
  coord_flip()
