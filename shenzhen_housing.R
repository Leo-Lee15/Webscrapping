##---------------------------------------------
######## 抓取深圳Q房网的小区房价信息  #########         
##--------------------------------------------

# 深圳Q房网：http://shenzhen.qfang.com/garden

library(rvest)
library(stringr)
library(dplyr)
library(progress)

base_url <- "http://shenzhen.qfang.com/garden"
webSpider <- function(page) {
  if (is.numeric(page)) {
    if (page == 1) {
      website <- base_url
    } else {
      website <- str_c(base_url, "/n", page)
    }
  } else {
    warnings("page must be a integer")
  }
  
  ## 抓取网页
  web_content <- read_html(website, encoding = "UTF8")
  houses <- web_content %>%
    html_nodes(css = "p.house-title > a") %>%
    html_text()
  cons_time <- web_content %>%
    html_nodes(css = "div.show-detail > p:nth-child(3) > span:nth-child(4)") %>%
    html_text()
  rent <- web_content %>% 
    html_nodes(css = "div.show-price > p:nth-child(1) > span.sale-price") %>%
    html_text()
  rent <- str_c(rent, "元/平米")
  address <- web_content %>%
    html_nodes(css = "div.show-detail > p.garden-address.text.clearfix > span") %>%
    html_text()
  
  messager <- web_content %>%
    html_nodes(css = "div.show-person > p:nth-child(1) > span.show-person-name") %>%
    html_text()
  
  phone_number <- web_content %>%
    html_nodes(css = "div.show-person > p:nth-child(1) > span.show-person-phone") %>%
    str_extract(pattern = "\\d{11}")
  
  tibble::tibble(houses, cons_time, address, rent, messager, phone_number)  
}
webSpiderm <- compiler::cmpfun(webSpider)


webSpiderm(23)

# 用progress_bar函数加入进度条
# 关于progress_bar函数的详细信息可以看函数的例子
pb <- progress_bar$new(
  format = "  progress [:bar] :percent in :elapsed",
  total = 10, clear = FALSE)

results <- vector(mode = "list", length = 10L)
for (page in seq_along(results)) {
  tryCatch({
   results[[page]] <- webSpiderm(page) 
  },
  
  # 加入报错信息是为了更好的理解抓取过程哪里出了问题。ERROR前面加入
  # \n是为了避免进度条和错误在同一行，导致输出结果不美观
  error = function(e) {cat("\nERROR :",conditionMessage(e),"\n")})
  pb$tick()
  Sys.sleep(0.5)
}

str(results, max.level = 1)

str(results[[1]], max.level = 1)

house <- purrr::map_df(results, rbind)
str(house)
DT::datatable(house)

hs <- house %>%
  mutate(rent1 = str_split(house$rent, pattern = "元", simplify = TRUE)[, 1]) %>%
  select(houses, cons_time, rent1, address, messager, phone_number) %>%
  rename(rent = rent1)
hs

hs$cons_time <- as.integer(str_extract(hs$cons_time, pattern = "\\d{4}"))

hs$rent <- as.numeric(hs$rent)

library(ggplot2)
library(dplyr)

## 深圳小区房价分布图
windowsFonts(xinwei = windowsFont("华文新魏"))
arrow_closed <- arrow(length = unit(0.3, "cm"), type = "closed") # 箭头封闭更好看
hs %>%
  ggplot() + 
  geom_histogram(aes(x = as.numeric(rent)), color = "white", fill = "steelblue", binwidth = 6000) + 
  theme_minimal() + 
  labs(x = "房价", y = "频数") +  
  scale_x_continuous(breaks = seq(10000, 150000, by = 20000)) + 
  labs(title = "深圳小区房价分布图  单位：元/平米",
          caption = "数据来源：http://shenzhen.qfang.com/garden\n制图：Leo Lee") +
  theme(
    # 控制主标题的位置和字号，hjust = 0.5表示居中
    plot.title = element_text(
      size = 20,
      # family = "xinwei",
      hjust = 0.5
    ),
    # 控制副标题的位置
    plot.caption = element_text(size = 14), 
    # 控制所有文字的字体和字号
    text = element_text(size = 18, family = "xinwei"),
    # 控制x轴刻度标签的颜色
    axis.text.x = element_text(colour = "black")  
  ) + 
  # 控制图形主题：坐标轴
  theme(
    axis.line.x = element_line(size = 0.5, arrow = arrow_closed), # 设置x坐标轴线条，arrow表示加上箭头
    axis.line.y = element_line(size = 0.5, arrow = arrow_closed)  # 设置y坐标轴线条
  ) + 
  geom_freqpoly(aes(x = rent), color = "red", size = 1, binwidth = 5000)
  
# 价格走势图
library(tibble)
hs_trend <- as_tibble(hs)  %>%
  group_by(cons_time) %>%
  dplyr::summarize(ave_price = mean(rent, na.rm = TRUE))

ggplot(hs_trend) + 
  geom_line(aes(x = cons_time, y = ave_price), 
            color = "steelblue", size = 2) +
  theme_minimal() + 
  labs(x = "年份", y = "住房平均价格") +  
  scale_x_continuous(breaks = seq(1980, 2016, by = 6),
                     labels = paste0(seq(1980, 2016, by = 6), "年建")) + 
  labs(title = "深圳小区住房均价图（按住房所建年份）  单位：元/平米",
       caption = "数据来源：http://shenzhen.qfang.com/garden\n制图：Leo Lee") +
  theme(
    # 控制主标题的位置和字号，hjust = 0.5表示居中
    plot.title = element_text(
      size = 20,
      # family = "xinwei",
      hjust = 0.5
    ),
    # 控制副标题的位置
    plot.caption = element_text(size = 14), 
    # 控制所有文字的字体和字号
    text = element_text(size = 18, family = "xinwei"),
    # 控制x轴刻度标签的颜色
    axis.text.x = element_text(colour = "black")  
  ) + 
  # 控制图形主题：坐标轴
  theme(
    axis.line.x = element_line(size = 0.5, arrow = arrow_closed), # 设置x坐标轴线条，arrow表示加上箭头
    axis.line.y = element_line(size = 0.5, arrow = arrow_closed)  # 设置y坐标轴线条
  )
