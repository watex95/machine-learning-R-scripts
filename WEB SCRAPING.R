#loading the package:

library(xml2)
library(rvest)
library(stringr)

#Specifying the url for desired website to be scrapped
url <- 'https://www.amazon.in/OnePlus-Mirror-Black-64GB-Memory/dp/B0756Z43QS?tag=googinhydr18418-21&tag=googinkenshoo-21&ascsubtag=aee9a916-6acd-4409-92ca-3bdbeb549f80'
#Reading the html content from Amazon
webpage <- read_html(url)


#Now, as the next step, we will extract the following information from the website:
  
#Title: The title of the product.
#Price: The price of the product.
#Description: The description of the product.
#Rating: The user rating of the product.
#Size: The size of the product.
#Color: The color of the product.

#scrape title of the product
title_html <- html_nodes(webpage, 'h1#title')
title <- html_text(title_html)
head(title)


#The next step would be to remove spaces and new line with the help of the str_replace_all() function in the stringr library.
# remove all space and new lines
str_replace_all(title, '[\r\n]', "")

#Now we will need to extract the other related information of the product following the same process.

#Price of the product:
#scrape the price of the product
price_html <- html_nodes(webpage, 'span#priceblock_ourprice')
price <- html_text(price_html)
                       
# remove spaces and new line
str_replace_all(title, "[\r\n]", "")
                             
# print price value
head(price)


#Product description:
  
#scrape product description
desc_html <- html_nodes(webpage, 'div#productDescription')
desc <- html_text(desc_html)
                            
# replace new lines and spaces
desc <- str_replace_all(desc, "[\r\n\t]" , "")
desc <- str_trim(desc)
head(desc)


#Rating of the product:
  
# scrape product rating 
rate_html <- html_nodes(webpage, 'span#acrPopover')
rate <- html_text(rate_html)
                            
# remove spaces and newlines and tabs 
rate <- str_replace_all(rate, "[\r\n]", "")
rate <- str_trim(rate)
                            
# print rating of the product
head(rate)


#Size of the product:
#Scrape size of the product
size_html <- html_nodes(webpage, 'div#variation_size_name')
size_html <- html_nodes(size_html, 'span.selection')
size <- html_text(size_html)
                            
# remove tab from text
size <- str_trim(size)
                            
# Print product size
head(size)


#Color of the product:
# Scrape product color
color_html <- html_nodes(webpage, 'div#variation_color_name')
color_html <- html_nodes(color_html, 'span.selection')
color <- html_text(color_html)
                             
# remove tabs from text
color <- str_trim(color)
                             
# print product color
head(color)




#Step 4: We have successfully extracted data from all the fields which can be used to compare the product information from another site.

#Let's compile and combine them to work out a dataframe and inspect its structure.

#Combining all the lists to form a data frame
product_data<-data.frame(Title = title, Price = price,Description = desc,
                           Rating = rate, Size = size, Color = color)

#Structure of the data frame
str(product_data)



#Step 5: Store data in JSON format:
  
#As the data is collected, we can carry out different tasks on it such as compare, analyze, and arrive at business insights about it. Based on this data, we can think of training machine learning models over this.
#Data would be stored in JSON format for further process.
#Follow the given code and get the JSON result.
# Include 'jsonlite' library to convert in JSON form.
library(jsonlite)

# convert dataframe into JSON format
json_data <- toJSON(product_data)

# print output
cat(json_data)


#A word of caution for you: certain websites have anti-scraping
#policies. If you overdo it, you will be blocked and you will
#begin to see captchas instead of product details. Of course, 
#you can also learn to work your way around the captchas using
#different services available. However, you do need to understand
#the legality of scraping data and whatever you are doing with
#the scraped data.


















