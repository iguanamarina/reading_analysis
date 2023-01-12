### GOODREADS ANALYSIS WITH R ###


library(tidyverse)
library(lubridate)

goodreads_data <- read_csv("goodreads_library.csv")

goodreads_data <- goodreads_data %>%
    select(`Title`, `Author`, `My Rating`, `Average Rating`, `Publisher`,
           `Number of Pages`, `Original Publication Year`, `Date Read`,
           `Date Added`)

## ANÁLISIS DE 2022 ###

books_2022 <- goodreads_data %>%
  filter(year(`Date Read`) == 2022) %>%
  # clean some column names
  rename(date_read = `Date Read`,
         page_count = `Number of Pages`,
         avg_rating = `Average Rating`,
         my_rating = `My Rating`)


# timing of books
books_2022 %>%
  count(date_read) %>%
  mutate(cumulative_books = cumsum(n)) %>%
  ggplot(aes(date_read, cumulative_books))+
  geom_line()+
  geom_point()


# cumulative pages read
books_2022 %>%
  arrange(date_read) %>%
  mutate(cumulative_pages = cumsum(page_count)) %>%
  ggplot(aes(date_read, cumulative_pages))+
  geom_line()+
  geom_point()


# my rating vs average rating
books_2022 %>%
  ggplot(aes(reorder(Title, rating_diff), rating_diff))+
  geom_col()+
  coord_flip()



books_2022 %>%
  ggplot(aes(date_read, avg_rating))+geom_point()


books_2022 %>%
  ggplot(aes(date_read, rating_diff))+
  geom_point()


books_2022 %>%
  ggplot(aes(date_read, page_count))+
  geom_point()


summary_by_date <- books_2020 %>%
  group_by(date_read, Title) %>%
  summarise(pages = sum(page_count)) %>%
  ungroup() %>%
  bind_rows(tibble(date_read = as.Date("2020-01-01"),
                   Title = NA_character_,
                   pages = 0)) %>%
  arrange(date_read) %>%
  mutate(previous_date = lag(date_read)) %>%
  mutate(days_since_last_book = as.numeric(difftime(
    date_read, previous_date, units = "days"
  )))