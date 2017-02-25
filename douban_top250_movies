## R爬虫
## 抓取所有的250部电影

library(rvest)
library(stringr)
library(dplyr)
library(progress)

# 1. 观察网页规律
baseUrl <- "https://movie.douban.com/top250" # 第1页
url2 <- "https://movie.douban.com/top250?start=25&filter=" # 第2页
url3 <- "https://movie.douban.com/top250?start=50&filter=" # 第3页

# 2. 爬虫函数
webSpider_douban_top250 <- function(page) {
  if (is.numeric(page)) {
    real_page <- 25*(page - 1)
    webs <- paste0(baseUrl, "?start=", real_page, "&filter=")
  } else {
    warning("page must be a numerical scalar")
  }
  top250_content <- read_html(webs, encoding = "utf-8")
  titles <- top250_content %>%
    html_nodes(css = "span.title") %>%
    html_text()
  titles
  
  clean_titles <- titles[-str_which(titles, pattern = "/")]
  clean_titles
  
  ## 获取电影评分
  ratings <- top250_content %>%
    html_nodes(css = "span.rating_num") %>%
    html_text()
  ratings
  
  ## 获取详细信息
  movie_info <- top250_content %>%
    html_nodes(css = "div.bd > p:nth-child(1)") %>%
    html_text(trim = TRUE)
  movie_info
  
  ## 导演
  clean_directors1 <- str_extract(movie_info, pattern = "^导演.*主演?")
  clean_directors1
  
  clean_directors2 <- str_split(clean_directors1, pattern = "\\s{3}", simplify = TRUE)
  clean_directors2
  
  clean_directors <- clean_directors2[, 1]
  clean_directors
  
  ## 国家，年份，电影类型
  country_info <- top250_content %>%
    html_nodes(css = "div.bd > p:nth-child(1)") %>%
    str_extract(pattern = "\\d{4}.*")
  
  country_info_d <- str_split(country_info, pattern = "\\s/\\s") 
  country_info_d
  
  ## 年份
  year <- as.integer(str_extract(country_info, pattern = "\\d{4}"))
  year
  
  ## 国家
  country <- sapply(country_info_d, function(x) x[2])
  country
  
  ## 电影类型
  type <- sapply(country_info_d, function(x) x[3])
  type
  
  ## 一句话点评
  comment <- top250_content %>%
    html_nodes(css = "span.inq") %>%
    html_text()
  comment
  
  data.frame(
    name = clean_titles,
    year = year,
    director = clean_directors,
    country = country,
    type = type,
    rating = ratings
  )
}

webSpider_douban_top250m <- compiler::cmpfun(webSpider_douban_top250)

## 3. 循环抓取各个页面
# 用progress_bar函数加入进度条
pb <- progress_bar$new(
  format = "  progress [:bar] :percent in :elapsed",
  total = 10, clear = FALSE)

movies_mess <- vector(mode = "list", length = 10L)
for (i in seq_along(movies_mess)) {
  tryCatch(
    {movies_mess[[i]] <- webSpider_douban_top250m(i)},
    error = function(e){cat("ERROR :",conditionMessage(e),"\n")})
  pb$tick()
  Sys.sleep(0.5) #增加了Sys.sleep(seconds)函数，让每一步循环都暂停一段时间。
}
movies_mess

top250Movies <- movies_mess %>%
  purrr::map_df(rbind)
str(top250Movies)
top250Movies$year <- as.integer(top250Movies$year)
top250Movies$rating <- as.numeric(top250Movies$rating)

DT::datatable(top250Movies)


str(top250Movies)

# 第83部出了问题
top250Movies$country[top250Movies$country == "1964(下集)"] <- "中国大陆"

library(readr)
write_csv(top250Movies, path = "douban_top250Novies.csv")

library(ggthemes)
# 每一年最佳电影部数
top250Movies %>% 
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_line(aes(x = year, y = count), size = 2, color = "steelblue") +
  scale_x_continuous(breaks = seq(1930, 2015, by = 5)) +
  scale_y_continuous(breaks = seq(0, 20, by = 4)) +
  theme_economist() +
  labs(y = "部/年", x = "年份")
# 2010年最多，有13部  
top250Movies %>%
  filter(year == 2010) %>%
  print()

# 分地区的最佳电影数
countries <- top250Movies$country %>%
  str_split(pattern = "\\s{1}") %>%
  unlist()
tibble::tibble(country = countries) %>%
  ggplot() +
    geom_bar(aes(x = country, fill = country),
             show.legend = FALSE,
             width = 1) +
    coord_flip() +
    theme_minimal() +
    scale_y_continuous(breaks = c(seq(0, 150, by = 30), 150))

## 这里如何根据样条的长度递减的排列此图
tibble::tibble(country = countries) %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  ggplot() +
  geom_bar(aes(x = country, y = count, fill = country), 
           stat = "identity", 
           show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 150, by = 15)) +
  labs(x = NULL, y = NULL)


  
# 每年电影平均评分
top250Movies %>%
  group_by(year) %>%
  summarise(ave_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = year, y = ave_rating), size = 2, color = "steelblue") +
  scale_x_continuous(breaks = seq(1930, 2015, by = 5)) +
  theme_economist() +
  labs(y = "评分", x = "年份")
