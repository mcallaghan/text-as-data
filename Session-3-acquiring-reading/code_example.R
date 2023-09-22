library(httr)
r <- GET("https://www.iea.org/policies")
r

library(rvest)
html <- read_html("https://www.iea.org/policies")
links <- html %>% html_elements("a.m-policy-listing-item__link")


library(tibble)
df <- tibble(text=character())
for (link in html_attr(links,"href")) {
  link_html <- read_html(paste0("https://iea.org",link))
  text <- link_html %>% html_element("div.m-block p") %>% html_text()
  df <- df %>% add_row(text=text)
  break
}

library(jsonlite)
library(dplyr)
library(dotenv)
load_dot_env(".env")
r <- GET(
  "https://api.openalex.org/works?filter=authorships.institutions.id:I24830596",
  add_headers(email=Sys.getenv("email"))
)
data <- fromJSON(content(r, "text"))
df <- cbind(
  select(data$results, where(is.character)), 
  select(data$results, where(is.numeric))
)
head(df)

cursor <- "*"
base_url <- "https://api.openalex.org/works?filter=authorships.institutions.id:I24830596"
df <- NULL
while (!is.null(cursor)) {
  r <- GET(paste0(
    base_url, "&per-page=200",
    "&cursor=",cursor
  ), add_headers(email=Sys.getenv("email")))
  data <- fromJSON(content(r, "text", encoding="utf-8"), simplifyDataFrame = TRUE)
  if (length(data$results) == 0) { break }
  page_df <- cbind(
    select(data$results, where(is.character)), 
    select(data$results, where(is.numeric))
  )
  df <- rbind(df, page_df)
  cursor <- data$meta$next_cursor
}
nrow(df)


library(rvest)
data <- read_html("https://www.theyworkforyou.com/pwdata/scrapedxml/debates/debates2023-09-19b.xml")
speeches <- data %>% html_elements("speech") 
df <- as_tibble(do.call(rbind, html_attrs(speeches)))
df$text <- speeches %>% html_text()