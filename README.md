# Data Harvesting API Project
Data harvesting assignment repository

## Libraries that we have used to get the REST API from Youtube and Reddit
```{r}
library(httr)
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tibble)
```

## Libraries that we have used to do the text mining
```{r}
library(tidytext)
library(ggplot2)
```

## env. file and the method we can call it to R
First of all, we saved API KEY and access token in the .env file.
Next, we saved it in .gitignore file.
Then, we erased the .env file and pushed it into github repository.

Code below enables us to call the API KEY and access token from .gitignore file.

```{r}
library(dotenv)
dotenv::load_dot_env(file = ".env")
access_token <- Sys.getenv("access_token")
API_KEY <- Sys.getenv("API_KEY")
```


## Reddit REST API  data extraction process

### Authentification Process and httr::GET request

Step 1. Set the **url** with endpoint and save some **query parameters** into a variable for GET request.

```{r}
search_url <- "https://api.reddit.com/r/politics/comments"

search_params <- list(
  q = "Donald Trump",
  type = "comments",
  sort = "best",
  limit = 100,
  "Authorization" = paste("Bearer", access_token),
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
)
```
Step 2. Run **GET request** using url and query parameters.

```{r}
req <- GET(
  url = search_url,
  query = search_params
)

print(req)

status_code(req)
```
We can check HTTP status code using code below.

```{R}
status_code(req)
```

Step 3. Parse the JSON content from the "req"

```{r}
response_json <- content(req, as = "text", encoding = "UTF-8")

# Convert the JSON content to a list
response_list <- fromJSON(response_json)

# Extract comments from the search results
link_comments <- data.frame(response_list$data$children$data$body)
print(link_comments)

# change the variable name
link_comments <- link_comments |> rename("text" = "response_list.data.children.data.body")
```
We repeated this 5 times in total to get 500 comments (it is limited to request 100 comments per one GET request).
Not to make comments duplicated a lot, we changed sort parameter in query parameters as "best", "relevance", "top", "hot", and "new".

Step 4. Merge all the dataset.

```{r}
trump = rbind(link_comments, link_comments_2, link_comments_3, link_comments_4, link_comments_5)

print(trump)
```

### Text mining

### NRC analysis

We have used **"nrc"** sentimental analysis tool.

```{r}
nrc = get_sentiments("nrc")
```
Step 1. Segement the sentiments: **positive, negative, anger, trust**

```{r}
# positive
nrc_positive <- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")

# negative
nrc_negative <- get_sentiments("nrc") |> 
  filter(sentiment == "negative")

# anger
nrc_anger <- get_sentiments("nrc") |> 
  filter(sentiment == "anger")

# trust
nrc_trust <- get_sentiments("nrc") |> 
  filter(sentiment == "trust")
```

Step 2. Tokenized the comments

```{r}
word = trump |> unnest_tokens(word, text)
print(word)
```

Step 3. Combine nrc and count each word to find the most frequent words from each sentiment

```{r}
# trump_positive
trump_positive <- word |> 
    #we combine both NRC and comments words
    inner_join(nrc_positive) |> 
    #we count the mentions of each word to find the most frequent
    count(word, sort = TRUE)

trump_positive$type <- c("positive")
print(trump_positive)

# trump_negative
trump_negative <- word |> 
  inner_join(nrc_negative) |> 
  count(word, sort = TRUE)

trump_negative$type <- c("negative")
print(trump_negative)

# trump_anger
trump_anger <- word |> 
  inner_join(nrc_anger) |> 
  count(word, sort = TRUE)

trump_anger$type <- c("anger")
print(trump_anger)

# trump_trust
trump_trust <- word |> 
  inner_join(nrc_trust) |> 
  count(word, sort = TRUE)

trump_trust$type = c("trust")
print(trump_trust)
```

Step 4. Merge all words with count

```{r}
trump_nrc = rbind(trump_positive, trump_negative, trump_anger, trump_trust)
print(trump_nrc)

# we have filtered word "vote" because it looks like having netural meaning and is repeated in various sentiment 
trump_nrc = trump_nrc |> filter(word != "vote")
print(trump_nrc)
```

Step 5. Visualize the result

```{r}
sentiment_nrc <- trump_nrc |> filter(n > 11) |> 
ggplot(aes(word, n, fill = type)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~type, nrow = 4, scales = "free_x") +
  ylab(NULL) + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(size = 5.8, face = 'bold'),
        plot.margin = unit(c(1, 1, 1, 0.1), "cm"),
        legend.position = "none")


print(sentiment_nrc)
```

### Term frequency analysis

Step 1. Calculate **total number of comments in each sentimental types**

We grouped by types to sum all the totals in the n column of trump_nrc and create total column with the total of words by type

```{r}
total_comment_words <- trump_nrc |> 
  #we group by types to sum all the totals in the n column of trump_nrc
  group_by(type) |> 
  #we create a column called total with the total of words by type
  summarize(total = sum(n))

total_comment_words
```
Step 2. Left_join the total_comment_words into trump_nrc to caluclate the percdentage of each words from the type

```{r}
comment_words <- left_join(trump_nrc, total_comment_words)
comment_words

comment_words <- comment_words %>%
  #we add a column for term_frequency in each type
  mutate(term_frequency = n/total)

comment_words
```

Step 3. Visualization

```{r}
library(ggplot2)

ggplot(comment_words, aes(term_frequency, fill = type)) +
  #we create the bars histogram
  geom_histogram(show.legend = TRUE) +
  #we set the limit for the term frequency in the x axis
  xlim(NA, 0.04) +
  #plot settings
  facet_wrap(~type, ncol = 2, scales = "free_y")
```
Step 4. Rank each comment_words by type

```{r}
freq_by_rank <- comment_words %>% 
  group_by(type) %>% 
  #we create the column for the rank with row_number by type
  mutate(rank = row_number()) %>%
  ungroup()

freq_by_rank

freq_by_rank |> group_by(type) |> filter(rank == 1 | rank == 2)
```

Step 5. Visualization

```{r}
freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = type)) + 
  #plot settings
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = TRUE) +
  theme_minimal()
```







