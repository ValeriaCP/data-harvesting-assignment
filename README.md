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

## YouTube API KEY request process

To be able to scrap information from the YouTube API we have to have an API KEY, this is sensitive information that we can not share with everyone, the steps to get a YouTube API are the following:

**1.** First create a Google account to access the Google API console and be able to request an API key.

**2.** Second step is to create a project on the Google Developers Console by clicking on the project dropdown that is next to the Google cloud logo.

**3.** After creating a new project, go to the left side bar and select "APIs & Services" and then on then clic on the "Enable APIS and Services" and search for the "YouTube Data API v3", select it and enable it.

**4.** After enabling the API, click on the "Create Credentials" button and choose the API key option.

**5.** Finally, once created, the API key will be displayed on the screen, ensure to copy the API key and keep it secure.

Once we have the API KEY, we can use it on R but as mentioned above when having a API KEY we should be careful to handle it due to several security and privacy concerns API keys are credentials to access specific resources, therefore we should avoid to give this access to malicious actors to manipulate or retrieve sensitive information.

## YouTube REST API data extraction process

We define a variable called "CNN" since this is the channel_username that we want to retrive information from.

```{r}
channel_username <- "CNN"
```

Create the API request URL

```{r}
url_channel <- paste0('https://www.googleapis.com/youtube/v3/channels?part=contentDetails&forUsername=', channel_username, '&key=', API_KEY)
```

Make the API request URL

```{r}
response_channel <- GET(url_channel)
```

Parsing API response, we use "text" to specify that we want this type of content to be extracted from the HTTP response.

```{r}
channel_info <- fromJSON(content(response_channel, "text"))
str(channel_info)
```

We see if on the "items" element there is the comment section

```{r}
content_details <- channel_info$items$contentDetails
```

We can see a data frame containing information about related playlists.

```{r}
related_playlists <- content_details$relatedPlaylists
str(related_playlists)
```

#### Get the video IDs

Function to get video IDs from the "uploads" playlist

```{r}

get_video_ids <- function() {
  url_channel <- paste0('https://www.googleapis.com/youtube/v3/channels?part=contentDetails&forUsername=', channel_username, '&key=', API_KEY)
  response_channel <- GET(url_channel)
  channel_info <- fromJSON(content(response_channel, "text"))

  # Extract the "uploads" playlist ID for CNN's channel
  uploads_playlist_id <- channel_info$items$contentDetails$relatedPlaylists$uploads

  # Construct the URL for retrieving videos in the "uploads" playlist
  url_videos <- paste0('https://www.googleapis.com/youtube/v3/playlistItems?part=contentDetails&playlistId=', uploads_playlist_id, '&maxResults=100&key=', API_KEY)

  # Make the API request to get the videos
  response_videos <- GET(url_videos)
  videos_info <- fromJSON(content(response_videos, "text"))

  # Extract the video IDs
  video_ids <- videos_info$items$contentDetails$videoId

  return(video_ids)
}
```

Retrieve video IDs from the "uploads" playlist

```{r}
video_ids <- get_video_ids()
```

Create a function to get comments on a tibble for the videos ids retrieved above and filered by "Trump" related comments 

```{r}
get_comments_for_video <- function(video_id) {
  max_results <- 100 #This is the maximun number of results per API request, becasue if we do not put it we will have a lot of them and R will collapse
  keyword <- "Trump" #this is the keyword we will use to filter comments
  next_page_token <- NULL #indicate that we don't have a page token yet
  all_comments <- character(0) # Initialize an empty character vector to store all filtered comments

  repeat {
#URL for the YouTube Data API request
    url_comments <- paste0('https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=', video_id, '&maxResults=', max_results, '&key=', API_KEY)

#Append the page token to the URL if it is not NULL
    if (!is.null(next_page_token)) {
      url_comments <- paste0(url_comments, '&pageToken=', next_page_token)
    }
#Make the GET request 
    response_comments <- GET(url_comments)
#Parse the JSON response
    comments_info <- fromJSON(content(response_comments, "text"))
#Extract the text (comments) needed 
    current_comments <- comments_info$items$snippet$topLevelComment$snippet$textDisplay
#Filter the comments with the keyword previously selected
    filtered_comments <- grep(keyword, current_comments, ignore.case = TRUE, value = TRUE)

 # Append the filtered comments to the vector of all comments
    all_comments <- c(all_comments, filtered_comments)

# Check if there is a next page token in the API response
    if (is.null(comments_info$nextPageToken)) {
      break
    } else {
      next_page_token <- comments_info$nextPageToken
    }
  }

  #List of filtered comments
  return(all_comments)
}

```

Retrieve and filter comments for each video in the playlist

```{r}
all_video_comments <- list()

for (video_id in video_ids) {
  comments_for_video <- get_comments_for_video(video_id)
  all_video_comments[[video_id]] <- comments_for_video
}
```
Tibble for all comments

```{r}
comments_tibble <- tibble(comments = unlist(all_video_comments))
print(comments_tibble)
```
### Text mining

Libraries used for text mining analysis

```{r}
library(tidytext)
library(dplyr)
```

### NRC analysis

We have used **"nrc"** sentimental analysis tool.

```{r}
get_sentiments("nrc")
```

Segment the sentiments: **positive, negative, anger, trust**

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

We use stop words to  be able to exclude them from the analysis, these are common words that have a little value.

```{r}
stop_words
```

Unnesting the words from the `comments_df`

```{r}
yt_word <- comments_tibble |> 
  unnest_tokens(word, comments) |> 
  anti_join(stop_words) 
```
### Sentiment analysis
Combine nrc and count each word to find the most frequent words from each sentiment

```{r}
# trump_positive
yt_trump_positive <- yt_word |> 
    inner_join(nrc_positive) |> 
    count(word, sort = TRUE)

yt_trump_positive$type <- c("positive")
print(yt_trump_positive)

# trump_negative
yt_trump_negative <- yt_word |> 
  inner_join(nrc_negative) |> 
  count(word, sort = TRUE)

yt_trump_negative$type <- c("negative")
print(yt_trump_negative)

# trump_anger
yt_trump_anger <- yt_word |> 
  inner_join(nrc_anger) |> 
  count(word, sort = TRUE)

yt_trump_anger$type <- c("anger")
print(yt_trump_anger)

# trump_trust
yt_trump_trust <- yt_word |> 
  inner_join(nrc_trust) |> 
  count(word, sort = TRUE)
yt_trump_trust$type = c("trust")
print(yt_trump_trust)

```

Merge all words with count

```{r}
yt_trump <- rbind(yt_trump_positive, yt_trump_negative,yt_trump_anger, yt_trump_trust)
```

Visualize the result

```{r}
library(ggplot2)

yt_trump_plot <- yt_trump |> 
  group_by(type) |> 
  top_n(8, wt = n) |>  #selected the top 8 more repeated words for each sentiment 
  ggplot(aes(word, n, fill = type)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~type, nrow = 3, scales = "free_x") +
  theme(axis.text.x = element_text(size = 5.5))


print(yt_trump_plot)
```

### Term frequency analysis

```{r}
yt_words <- yt_trump |> 
  count(type, word, sort = TRUE) #we count word frequencies keeping a column for the user where the word comes from
yt_words
```

Calculate total number of comments in each sentimental types

Grouped by type to sum all the totals in the n column of yt_words and create a column called total with the total of words by user

```{r}
total_words <- yt_words |> 
  group_by(type)|> 
  summarize(total = sum(n))
total_words
```

Add a column with this total number to the dataframe yt_words, we use left join because we need the join to keep all rows in yt_words, regardless of repeating rows

```{r}
words <- yt_words %>%
  left_join(total_words, by = "type")
  
words <- words |> 
  mutate(frequency = n / total)
words
```

Visualization

```{r}
library(ggplot2)

#Calculate the distribution and put it in the x axis, filling by type
ggplot(words, aes(frequency)) +
  geom_histogram(show.legend = TRUE) +
  xlim(NA, 0.03)
```
```{r}
ggplot(words, aes(frequency, fill = type)) +
  #we create the bars histogram
  geom_histogram(show.legend = TRUE) +
  #we set the limit for the term frequency in the x axis
  xlim(NA, 0.04) +
  #plot settings
  facet_wrap(~type, ncol = 2, scales = "free_y")
```
Rank each comment_words by type

```{r}
freq_rank <- words %>% 
  group_by(type) %>% 
  #we create the column for the rank with row_number by type
  mutate(rank = row_number()) %>%
  ungroup()

freq_rank

freq_rank |> group_by(type) |> filter(rank == 1 | rank == 2)
```
Plot the ranking

```{r}
freq_rank %>% 
  ggplot(aes(rank, frequency, color = type)) + 
  #plot settings
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = TRUE) +
  theme_minimal()
```

Sentiment plot of all comments related to Trump on YouTube CNN channel

```{r}

sentiments <- yt_trump %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  rename(count = n)
sentiments

```

Visualization

```{r}
library(ggplot2)

sentiments %>%
  #we weight by the count column: a term and its sentiment associated multiplied by count
  count(sentiment, word, wt = count) %>%
  ungroup() %>%
  #we filter by words appearing more than 90 times in the comments
  filter(n >= 90) %>%
  #create a new column called n that is equal to the count, but with the sign flipped
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  #reorder for descending order of bars
  mutate(term = reorder(word, n)) %>%
  #plot settings
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Number of mentions (contribution to sentiment)", y = NULL)
```

As visualization above, it becomes evident that the lexicon associated with Trump leans more towards negative sentiments than positive ones. The prevalence of words expressing unfavorable sentiments surpasses those conveying a positive tone in discussions related to Trump on the YouTube comments for the CNN channel and playlist about "Politics".

## Reddit REST API  data extraction process

### Get a token from Reddit API

You need to make reddit app to get the client_id and client_secret. <br/>
You can access website to make the reddit app here: [Reddit_APP](https://www.reddit.com/prefs/apps)

```{r}
client_id = "your client id"
client_secret = "your client secret"

response <- POST(
  url = "https://www.reddit.com/api/v1/access_token",
  body = list(
    grant_type = "client_credentials"),
  encode = "form",
  authenticate(client_id, client_secret)
  )

access_token <- content(response)$access_token
print(access_token)
```


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







