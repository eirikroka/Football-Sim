# load necessary package
library(httr)

# specify the date
date <- "2025-06-11"

# build the URL
url <- paste0("http://api.clubelo.com/", date)

# send a GET request to the API
response <- GET(url)

# check the status of the response
if (http_status(response)$category == "Success") {
  # parse the content of the response as text
  text <- content(response, "text", encoding = "UTF-8")
  
  # convert the text to a character vector
  text_vector <- textConnection(text)
  
  # read the CSV data
  data <- read.csv(text_vector, header = TRUE)
  
  # print the data
  print(data)
} else {
  # print the status of the response
  print(http_status(response)$message)
}

write.csv(data, file = "data/data1.csv", row.names = FALSE)
