## Explore food ratings by cuisine in Los Angeles restaurants
## Author: Jaymie Park

pacman::p_load(data.table, tidyverse, yelpr, rio, parallel, pbapply)

## Load restaurants
df <- fread("data/Restaurant_and_Market_Health_Inspections.csv")
df <- df %>% 
        ## Subset to restaurants in Los Angeles
        filter(facility_city=="LOS ANGELES"&grepl("RESTAURANT", pe_description)) %>%
        ## Adjust date
        mutate(activity_date=str_split_i(activity_date, " ", i=1) %>% as.Date(format="%m/%d/%Y")) %>% 
        ## Arrange by facility id and date
        arrange(facility_id, desc(activity_date)) %>%
        ## Keep most recent rating
        .[, `:=` (n = .N,
                  i = seq_len(.N)
                  ), by=facility_id] %>%
        filter(i==1) %>%
        ## Keep B/C ratings
        filter(grade%in%c("B", "C"))


## Pull these from Yelp API
client_id <- "1k6RL2M_a5ODeKt8fzbGnw"
key <- "kfaX9sfGUq1C_2gt8jjt3gW9MsezLhphXHoXjDTShZ5HG7uvcmQuxdfqyFkmdlmNUXIVqGeK-C2pB2jOSQQJp_YzLXvV5uhU7GFSfDERDIQ80hojC_gFnfXlANsCZHYx"

business_match_name <- function (api_key, name, city, state, country, 
          ...) 
{
  require(httr)
  require(rjson)
  parameters <- c(as.list(environment()), list(...))
  parameters <- parameters[2:length(parameters)]
  res <- GET("https://api.yelp.com/v3/businesses/matches", add_headers(Authorization = prepare_header(api_key)), 
             query = parameters)
  fromJSON(content(res, type = "text"))
}

## Query to get Business ID's
result <- pblapply(1:nrow(df), function(i) {
  res <- business_match_name(key, 
                      name=df$facility_name[i], 
                      address1=df$facility_address[i],
                      city="Los Angeles", 
                      state="CA", 
                      country="US",
                      limit=1, 
                      match_threshold="default")
  if (length(res$businesses)==0) {
    return(NULL)
  } else {
    return(unlist(res) %>% cbind %>% t %>% as.data.table %>% cbind(serial_number=df$serial_number[i]))
  }
}) %>% rbindlist(fill=T)
## Save output
export(result, "data/business_ids.rds")

## Import Yelp business ID's
ids <- import("data/business_ids.rds")

## Lookup business data
res <- pblapply(ids$businesses.id, function(x) {
  business_lookup_id(key, x) %>% unlist %>% t %>% as.data.table
}) %>% rbindlist(fill=T)

## Save output
export(res, "data/business_data.rds")

## Load business data
yf <- import("data/business_data.rds")
## Subset columns
cols <- c("id", grep("categories|coordinates|rating|review_count|price", names(yf), value=T))
yf <- yf %>% select(!!cols)
yf <- yf %>% setcolorder(cols)

## Pull id's to merge onto ratings data
ids <- import("data/business_ids.rds")
ids <- ids[, .(id=businesses.id, serial_number)]
yf <- merge(yf, ids, by="id", all.x=T)

## Merge onto ratings data
df <- merge(df, yf, by="serial_number", all.x=T)

export(df, "data/prepped_list.rds")



