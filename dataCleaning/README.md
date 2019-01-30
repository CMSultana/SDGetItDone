SD GetItDone Data Cleaning in R
================
Camille Sultana
20180930

-   [Project Description](#project-description)
-   [Setup](#setup)
-   [Initialize Functions](#initialize-functions)
-   [Select columns from data set](#select-columns-from-data-set)
-   [Consolidate service types](#consolidate-service-types)
-   [Recategorize "other" service type](#recategorize-other-service-type)
-   [Recategorize remaining "other" records](#recategorize-remaining-other-records)
-   [Recategorize select service types](#recategorize-select-service-types)
-   [Modify subtypes](#modify-subtypes)
-   [Clean up source](#clean-up-source)

Project Description
-------------------

**Data cleaning of San Diego Get It Done (GID) data set (<https://data.sandiego.gov/datasets/get-it-done-311/>), downloaded 09/27/2018. The Get It Done data set is comprised of reports of local resident SD City service requests (e.g. illegal dumping, potholes, graffiti, etc).**

**R learning goals** -- Tidyverse packages/paradigms (piping, dplyr) -- Writing functions -- iterating with purrr -- R markdown: html document, github document

Note this was pretty much my first project ever in r. There is a lot I would now do differently, especially regarding when to utilize functions and how they operate, but it gets the job done. Also would probably redo this use the data.table package considering the size of the data set.

Setup
-----

``` r
#packages to load
library(rprojroot)
library(tidyverse)

#set global chunk options
#note warning and messages are set to FALSE, if you have issues switch this to TRUE
knitr::opts_chunk$set(collapse = TRUE, results = "as.is", fig.show = "hold", cache = TRUE, message = FALSE, warning = FALSE)

#get root project directory
rootDir <- find_root(is_rstudio_project)
```

Initialize Functions
--------------------

I'm initializing functions before loading data so the functions doen't have the large data set in their initialized environment. I think that will save some memory on my computer side..... The output of many of these functions can be generated with piping and the basic functions in dplyr, however creating functions to make things more concise.

``` r
#initialzing functions early to save memory
#(don't want them to have the dataset in their initialized environment)
#replace values in column based on string matches in same or other column
#note this is a "replacement" function
str_RowDfReplace <- function(df, colMatch, colReplace, match = "", value) {
  row_IDX <- str_detect(df[[colMatch]], match)
  row_IDX[is.na(row_IDX)] <- FALSE
  df[[colReplace]][row_IDX] <- value
  df
}
```

``` r
#search for a string in a specific dataframe column using str_detect
#return all dataframe rows where str_detect returned true
str_RowDfFilter <- function(string, df, col) {
  row_IDX <- str_detect(df[[col]], string)
  row_IDX[is.na(row_IDX)] <- FALSE
  newDF <- df[row_IDX, ]
}
```

``` r
#for select rows in the dataframe (filter is logical which selects rows)
#replace values in column based on string matches in same or other column
str_RowDfReplace_FilterFirst <- function(df, colMatch, colReplace, matchDF, lgFilter) {
  
  #if no lgFilter then do string match on entire df
  if (missing(lgFilter)) {
    #perform string match/replacement
    for (i in seq_len(nrow(matchDF))){
      df <- str_RowDfReplace(df, colMatch, colReplace,
                             matchDF$matchExp[i], matchDF$replaceName[i])
    }
  } else {
    #if any NA in logical just switch to FALSE, only want to mess with TRUE
    lgFilter[is.na(lgFilter)] <- FALSE
    
    #create temporary data frame selecting rows with lgFilter
    tmpDF <- df[lgFilter, ]
    
    #perform string match/replacement
    for (i in seq_len(nrow(matchDF))){
      tmpDF <- str_RowDfReplace(tmpDF, colMatch, colReplace,
                                matchDF$matchExp[i], matchDF$replaceName[i])
    }
    df[lgFilter, ] <- tmpDF
  }
  df
}
```

``` r
#want to be able to control number of rows of data frame
#need to be able to limit or add rows (using na) to a certain number as needed
#note didn't use dplyr::add_row as couldn't figure out how to get colname from df
#as input into add_row
#numRow is number of desired rows for dataframe
padDataFrame <- function(numRow) {
  function(df) {
    numRowDF <- nrow(df)
    if (numRowDF >= numRow) {
      df[1:numRow, ]
    } else {
      df[(numRowDF+1):numRow, ] <- NA
      df
    }
  }
}

padDataFrame_20 <- padDataFrame(20)
```

``` r
#modify subtypes for records with a specific serviceName within the dataframe
modifySubTypes <- function(df, service, currentSubtypes, renameSubtypes) {
  
  #set up variables
  selectService <- df$service_nameMOD == service #index for records with correct service name
  selectEmptySubtype <- !(df$service_subtype %in% currentSubtypes) | is.na(df$service_subtype) #index for records with Na/other as subtype
  
  #first modify service data with NA/other subtype
  selectToModify <- selectService & selectEmptySubtype #modify records with right service name and no subtype
  df <- str_RowDfReplace_FilterFirst(df, "description", "service_subtype",
                                     renameSubtypes, selectToModify)
  #
  # tmpDF <- df %>%
  #   select(service_nameMOD, service_subtype, description) %>%
  #   filter(selectToModify)
  # for (i in seq_len(nrow(renameSubtypes))){
  #   tmpDF <- str_RowDfReplace(tmpDF, "description", "service_subtype",
  #                    renameSubtypes$matchExp[i], renameSubtypes$replaceName[i])
  # }
  #
  # #add new subtypes back to original dataframe
  # df$service_subtype[selectToModify] <- tmpDF$service_subtype
  #
  
  #everything now not with a valid subtype change to "other"
  selectEmptySubtype <- !(df$service_subtype %in% renameSubtypes$replaceName) | is.na(df$service_subtype)
  selectToModify <- selectService & selectEmptySubtype
  df$service_subtype[selectToModify] <- "other"
  
  #return dataframe
  df
}
```

``` r
#create barplot of subtypes for a specific serviceName
subTypePlot <- function(df, service) {
  #initialize variables
  selectService <- df$service_nameMOD == service #index for records with correct service name
  selectDesc <- df %>%
    filter(selectService) %>%
    count(twoWordDesc, sort = TRUE)
  selectDesc <- selectDesc$twoWordDesc[1:10] #top 10 first two word phrases in description
  
  #create temp df with data of interest
  tmp <- filter(df, selectService)
  tmp$twoWordDesc[!(tmp$twoWordDesc %in% selectDesc)] <- "other" #set all two word phrases outside of top 7 to other so plot legend isn't gigantic
  
  #make plot of service_subtype colored by top 11 twoWordDesc
  #this is a quick check if new subtype assignment make sense with other info in df
  tmp %>%
    ggplot(aes(x = service_subtype, fill = twoWordDesc)) +
    geom_bar() +
    coord_flip() +
    labs(title = service)
}
```

``` r
#create a table that allows you to quickly compare changes in the
#the number of records of a particular service after making
#modifications to the data set
modServiceCount <- function() {
  serviceCount = tibble(service_nameMOD = character(), n= integer())
  function() {
    tmp <- allGIDRequests %>%
      count(service_nameMOD, sort = T) %>%
      arrange(service_nameMOD)
    
    nrowTmp <- nrow(tmp)
    nrowSC <- nrow(serviceCount)
    if (nrowSC > 0) {
      if (nrowTmp < nrowSC) {
        tmpPad <- padDataFrame(nrowSC)
        tmp <- tmpPad(tmp)
      } else if (nrowTmp > nrowSC) {
        tmpPad <- padDataFrame(nrowTmp)
        serviceCount <<- tmpPad(serviceCount)
      }
      serviceCount <<- cbind(serviceCount, tmp)
    } else {
      serviceCount <<- rbind(serviceCount, tmp)
    }
    serviceCount
  }
}
```

Select columns from data set
----------------------------

``` r
#read in data
allGIDRequests <- read_csv(paste0(rootDir, "/data/get_it_done_311_requests_datasd.csv"))
```

``` r
#check out if open/closed columns are redundant
hasClosedDate <- !is.na(allGIDRequests$closed_date)
markedClosed <- allGIDRequests$closed
sum(hasClosedDate & !markedClosed) #0, everything with closed date marked as closed
## [1] 0
sum(markedClosed & !hasClosedDate) #0, everything marked as closed has closed date
## [1] 0
```

The **closed** column simply indicates if there is data in **closed\_date**, making it relatively redundant.

``` r
#get a better idea of which columns are used the most, and which are more optional
sort(map_dbl(allGIDRequests, function(x) mean(is.na(x))))
##      service_request_id      parent_case_number      status_description 
##            0.0000000000            0.0000000000            0.0000000000 
##       mobile_web_status      duplicate_verified      override_duplicate 
##            0.0000000000            0.0000000000            0.0000000000 
##                  source      requested_datetime        updated_datetime 
##            0.0000000000            0.0000000000            0.0000000000 
##                    open                  closed             coordinates 
##            0.0000000000            0.0000000000            0.0000000000 
##                     lat                    long        case_record_type 
##            0.0000000000            0.0000000000            0.0000000000 
##                 address             description      agency_responsible 
##            0.0001544665            0.0026738680            0.0027218059 
##                district            service_name     functional_location 
##            0.0037711126            0.0050121709            0.0622712964 
##             closed_date    sap_problem_category        sap_problem_type 
##            0.3059022174            0.3085494532            0.3086666347 
##        sap_problem_code sap_notification_number         service_subtype 
##            0.3087571840            0.4321119829            0.7073392883 
##   referred_email_update          referral_email 
##            0.7939843296            0.9965218410
```

For each column we can see the proportion of entries where there is no data (NaN). Columns with fewer data entries (ie output closer to zero) are probably going to be less useful in the analysis.

``` r
#columns to get rid of
#coordinates is duplicate of lat and long
#open and closed redundant with having a closed date
#sap problem code is redundant with sap problem type
#functional location, some internal city location code
#referred_email_update, specific email for department that request was referred to
#case_record_type, so many descriptions of service already, redundant
#referral_email missing 99% of time
#sap_notification_number mission 43% of time, seems to be some sort of internal city code for record
allGIDRequests <- select(allGIDRequests, -open, -closed, -coordinates, -sap_problem_code,
                         -functional_location, -referred_email_update, -case_record_type,
                         -sap_notification_number, -referral_email)

#lowercase all character columns to make string matching easier
isCharColumn <- map_lgl(allGIDRequests, is_character)
allGIDRequests[isCharColumn] <- map(allGIDRequests[isCharColumn], tolower)
```

Minimized the number of columns, making looking at the table in the viewer easier and downsizing the data set some.

Consolidate service types
-------------------------

``` r
#get number of records for each service_name in original data set
serviceCount <- allGIDRequests %>%
  count(service_name, sort = T)
serviceCount
## # A tibble: 35 x 2
##    service_name        n
##    <chr>           <int>
##  1 other           51629
##  2 pothole         38798
##  3 graffiti        23930
##  4 traffic signal  15998
##  5 street light    14538
##  6 sidewalk         9407
##  7 traffic sign     6844
##  8 curb             6141
##  9 tree hazard      5799
## 10 illegal dumping  4561
## # ... with 25 more rows
```

Wow that is a lot of different types of service types! I want to examine and visualize the data set grouped by the service type, but doing so for 35 distinct services will be unwieldy. Consolidating these types is priority \#1. There are many services which could be thoughtfully combined into broader categories (eg "pothole" and "pavement" to "street repair"). In addition 16 service types have fewer than 100 records and are therefore pretty inconsequential given the large size of the data set.

``` r
#pull first two words from description column into new column
#to be used later

#regex to pull first two words out of character vector
twoWord = "([^\\s]+\\s*){1,2}";

#adding a new service name column, and a column which pulls the
#first two words from the description column, then reorganize columns
#twoWordDesc column is used to help understand service name and subtypes later
allGIDRequests <- allGIDRequests %>%
  mutate(service_nameMOD = service_name,
         twoWordDesc = str_extract(.$description, twoWord)) %>%
  select(service_request_id:mobile_web_status,
         source:description, service_name, service_nameMOD,
         service_subtype, everything())
```

``` r
#create consolidated service types in column service_nameMOD

#35 different service names ~15 with <100 requests, going to consolidate
#many service names combined into service_nameMOD to make visualizations easier
#replaceName: new service name type
#matchExp: regex to find in old service name
consolidateNames <- tribble(
  ~replaceName,            ~matchExp,
  "vegetation service",    "^tree",
  "street repair",         "(pothole)|(pavement)",
  "storm drain",           "drain",
  "street flooded",        "flood",
  "street striping",       "faded striping",
  "sidewalk repair",       "sidewalk"
)

#service_nameMOD is currently a copy of original service_name
#whenever there is string match from matchExp in service_nameMOD
#replace the name with the new type from replaceName
#this function loops through each row of consolidateNames, so the
#order matters if multiple matchExp are possible
allGIDRequests <- str_RowDfReplace_FilterFirst(allGIDRequests, "service_nameMOD",
                                               "service_nameMOD", consolidateNames)
```

``` r
#consolidate low count service types into "other"

#names of all services that now have <200 counts
otherService <- allGIDRequests %>%
  count(service_nameMOD, sort = T) %>%
  filter(n < 200)
knitr::kable(otherService,
             caption = "Services < 200 records after initial consolidation",
             format = "markdown",
             table.attr = "style='width:40%;'")
```

| service\_nameMOD  |    n|
|:------------------|----:|
| row maintenance   |  170|
| abandoned vehicle |   15|
| debris fence      |   15|
| channel           |    3|
| conveyances       |    3|
| parking meter     |    2|
| bridge            |    1|
| gearbox           |    1|
| illegal discharge |    1|
| parking           |    1|
| spillway          |    1|
| transformer       |    1|

``` r

#combine all service types with low counts into "other type"
otherIDX <- allGIDRequests$service_nameMOD %in% otherService$service_nameMOD #everything with low counts
otherIDX[is.na(allGIDRequests$service_nameMOD)] <- TRUE #everything named NA
#replace with other
allGIDRequests$service_nameMOD[otherIDX] <- "other"

#check out service name counts now
modCount <- modServiceCount()
serviceCountMOD <- modCount()
knitr::kable(serviceCountMOD,
             caption = "Current service type counts",
             format = "markdown",
             table.attr = "style='width:40%;'")
```

| service\_nameMOD   |      n|
|:-------------------|------:|
| curb               |   6141|
| damaged guardrail  |    610|
| graffiti           |  23930|
| illegal dumping    |   4561|
| other              |  52784|
| sidewalk repair    |   9407|
| storm drain        |   2668|
| street flooded     |   1421|
| street light       |  14538|
| street repair      |  39042|
| street striping    |   1943|
| street sweeping    |   1955|
| traffic sign       |   6844|
| traffic signal     |  15998|
| vegetation service |   5901|

The number of service types have now been cut down to a much more digestable 15. However, 28% of all records are currently categorized as other. Ideally we could find a way to recategorize some of these requests using more descriptive service types. To do this we can look to see if there is additional information in other fields in the dataset.

Recategorize "other" service type
---------------------------------

``` r
#get list of "two word descriptions" for other service type

#get current other reports
otherIDX <- allGIDRequests$service_nameMOD == "other" | is.na(allGIDRequests$service_nameMOD)
otherGID <- filter(allGIDRequests, otherIDX)

#get unique list of first two words in description, will help give range
#of descriptions in other
twoWordTable <- otherGID %>%
  count(twoWordDesc, sort = TRUE)
twoWordTable
## # A tibble: 251 x 2
##    twoWordDesc              n
##    <chr>                <int>
##  1 "other  at "         35676
##  2 other                 4101
##  3 "other problem "       951
##  4 "debris in "           816
##  5 "trim tree "           624
##  6 "evaluate tree "       524
##  7 "concrete sidewalk "   487
##  8 <NA>                   482
##  9 "signals stuck "       476
## 10 "evaluate for "        472
## # ... with 241 more rows
```

Reports currently categorized as an "other" service often have information in the **description** field which indicates the request could be classified under an existing more descriptive service type. The table created summarizes the instances of the first two words in the **description**, making obvious some repeated key words and phrases which could be used to reclassify requests.

``` r
#group other data based on description
#can look at data grouped by description to
#see if grouping by regular expressions (matchExp) is actually bundling
#similar records together

#based on two word list and looking through other create keyword searches to
#reclassify other service_name based on info in description
consolidateNames <- tribble(
  ~replaceName,           ~matchExp,
  "graffiti",            "graffiti",
  "sidewalk repair",     "sidewalk",
  "vegetation service",  "(^tree[s ])|([^a-z]tree[s ])|(limb)|(frond)|( trim)|(sidewalk encroachment)|(weed)|(vegetation)|(parkway encroachment)",
  "traffic sign",        "(traffic sign )|(street name)|(signage evaluations)|(stop sign[^s])|(stop sign$)",
  "traffic signal",      "(traffic signal)|(signals)|(knock over)|(signal facing)|(^lights out)|(timing)",
  "street repair",       "(resurfacing)|(asphalt)|(pothole)|(concrete street)|(concrete pavement)|(alley maintenance)|(bump maintenance)",
  "street sweeping",     "(debris in street)|(sweep)|(debris)",
  "storm drain",         "(drain)|(clog)|(^grate missing)|(manhole)|(channel cleaning)",
  "street flooded",      "(flood)|(ponding)",
  "curb",                "([^.//]curb)|(^curb)|(ramp installation)|(eval.*z)|(parking eval)",
  "street light",        "^(other light)|(street light)|(light out)",
  "damaged guardrail",   "guardrail",
  "street striping",     "(paint striping)|(faded striping)",
  "illegal dumping",     "(dumping)|(bike removal)|(abandoned)"
  # "traffic calming",     "(road hump)|(speeding)|(calming)|(crosswalk)|(xwalk)|(x-walk)",
)

#do a check on the names, make sure didn't mistype anything,
#should return empty if didn't make any new names
which(!(consolidateNames$replaceName %in% unique(allGIDRequests$service_nameMOD)))
## integer(0)

#split all records of type other based on key word searches in description,
#can quickly scroll thorugh each split to make sure grouping data this way
#makes sense
splitOtherGIDDescription <- tibble(
  searchTerm = consolidateNames$matchExp,
  data = map(searchTerm, str_RowDfFilter, df = otherGID, col = "description"),
  count = map_int(data, nrow),
  searchIDX = map(searchTerm, unlist(str_detect), string = otherGID$description)
)

#there is also info in sap_problem_type sometimes
#can also split other data based on keyword searches in here
splitOtherGIDType <- tibble(
  searchTerm = consolidateNames$matchExp,
  data = map(searchTerm, str_RowDfFilter, df = otherGID, col = "sap_problem_type"),
  count = map_int(data, nrow),
  searchIDX = map(searchTerm, unlist(str_detect), string = otherGID$sap_problem_type)
)

#check out other records that didn't match any key word searches
allSplit <- Reduce(`|`, c(splitOtherGIDDescription$searchIDX, splitOtherGIDType$searchIDX))
```

Can use data viewer to visually examine records grouped by matching regular expressions to **description** and **sap\_problem\_type**. Doing so provides some assurance that regular expressions are performing as expected and records being grouped together all make sense.

``` r
#check out other records remaining that didn't match any key terms.
twoWordRemainTable <- otherGID[!allSplit, ] %>%
  count(twoWordDesc, sort = TRUE)
twoWordRemainTable
## # A tibble: 82 x 2
##    twoWordDesc                   n
##    <chr>                     <int>
##  1 <NA>                      31558
##  2 "other  at "               2195
##  3 "other problem "            947
##  4 "place/pick up "            248
##  5 "temp stop "                189
##  6 "loose steel "               81
##  7 "slide/slope failure "       61
##  8 "traffic calming/safety "    59
##  9 "routine maintenance "       52
## 10 "sign + "                    42
## # ... with 72 more rows

twoWordTypeRemainTable <- otherGID[!allSplit, ] %>%
  mutate(twoWordDesc = str_extract(.$sap_problem_type, twoWord)) %>%
  count(twoWordDesc, sort = TRUE)
twoWordTypeRemainTable
## # A tibble: 93 x 2
##    twoWordDesc             n
##    <chr>               <int>
##  1 <NA>                31558
##  2 other problem        1684
##  3 "place/pick up "      329
##  4 referral              282
##  5 "temp stop "          193
##  6 "sign + "             173
##  7 slide/slope failure   126
##  8 "chain link "         115
##  9 road humps            115
## 10 speeding              113
## # ... with 83 more rows
```

The vast majority of **other** records that don't match any key terms in either the **description** or **sap\_problem\_type** fields have no data in those fields. Happy enough with the key terms being used to reclassify **other** records.

``` r
#make modifications to service_name "other" records
#by modifying otherGID in place, preventing duplication of records
#which could happen by unnesting and combining the splitOther... data.
#The search terms used to split the data are not exclusive.
#However this means if multiple terms matched, whichever one is last in
#consolidateNames will be the one assigned in otherGID. Could avoid this
#but doesn't seem like there is huge overlap, and not worth the effort right now
for (i in seq_along(consolidateNames$replaceName)) {
  #replace service_nameMOD with new types based on keyword matches
  otherGID <- str_RowDfReplace(otherGID, "description", "service_nameMOD",
                               consolidateNames$matchExp[i], consolidateNames$replaceName[i])
  otherGID <- str_RowDfReplace(otherGID, "sap_problem_type", "service_nameMOD",
                               consolidateNames$matchExp[i], consolidateNames$replaceName[i])
  
  #if there isn't a useful description use info in sap_problem_type
  noDescription <- !splitOtherGIDDescription$searchIDX[[i]] | is.na(!splitOtherGIDDescription$searchIDX[[i]])
  replaceIDX <- splitOtherGIDType$searchIDX[[i]] & noDescription #rows to replace
  replaceIDX[is.na(replaceIDX)] <- FALSE
  replaceName <- otherGID$sap_problem_type[replaceIDX]
  otherGID$description[replaceIDX] <- replaceName #do replacement
}

#based on sap_problem_category info most "knock over" descriptions are "street lights"
#not traffic signals, recategorize service name based on this info
replaceIDX <- str_detect(otherGID$sap_problem_category, "^LIGHTS") & str_detect(otherGID$description, "knock over")
replaceIDX[is.na(replaceIDX)] <- FALSE
otherGID$service_nameMOD[replaceIDX] <- "street light"

#do some checks
#make sure everything that was given a name also has a description
tmpIDX <- allSplit
tmpIDX[is.na(tmpIDX)] <- FALSE
any(is.na(otherGID$description[tmpIDX])) #this should be false
## [1] FALSE

#move other records back to entire dataset
allGIDRequests[otherIDX, ] <- otherGID

#check out service name counts now
serviceCountMOD <- modCount()
knitr::kable(serviceCountMOD,
             caption = "Record of service type counts",
             align = "l",
             format = "markdown",
             table.attr = "style='width:80%;'")
```

| service\_nameMOD   | n     | service\_nameMOD   | n     |
|:-------------------|:------|:-------------------|:------|
| curb               | 6141  | curb               | 6604  |
| damaged guardrail  | 610   | damaged guardrail  | 731   |
| graffiti           | 23930 | graffiti           | 24212 |
| illegal dumping    | 4561  | illegal dumping    | 4659  |
| other              | 52784 | other              | 35844 |
| sidewalk repair    | 9407  | sidewalk repair    | 12608 |
| storm drain        | 2668  | storm drain        | 3135  |
| street flooded     | 1421  | street flooded     | 1789  |
| street light       | 14538 | street light       | 14838 |
| street repair      | 39042 | street repair      | 41394 |
| street striping    | 1943  | street striping    | 2010  |
| street sweeping    | 1955  | street sweeping    | 3584  |
| traffic sign       | 6844  | traffic sign       | 7808  |
| traffic signal     | 15998 | traffic signal     | 17495 |
| vegetation service | 5901  | vegetation service | 11032 |

The first two columns on the left show the count of records by service name for the original data. The following two columns show the counts after the reclassification of the **other** records. The number of **other** records was cut by about 20,000 and redistributed across the rest of the existing service types.

Recategorize remaining "other" records
--------------------------------------

``` r
#recategorizing remaining "other" services based on info in sap_problem_category #

#get remaining other records labeled other
otherIDX <- allGIDRequests$service_nameMOD == "other" | is.na(allGIDRequests$service_nameMOD)
otherIDX <- otherIDX & !is.na(allGIDRequests$sap_problem_category) #get records with NA
otherGID <- filter(allGIDRequests, otherIDX)

#category counts for remaining other records
categoriesCount <- otherGID %>%
  count(sap_problem_category, sort = T)
categoriesCount
## # A tibble: 27 x 2
##    sap_problem_category                          n
##    <chr>                                     <int>
##  1 signals - traffic signals                  1293
##  2 csr - referral                              462
##  3 lights - street lights                      456
##  4 standby - night/weekend priority requests   454
##  5 traffic - traffic control                   280
##  6 calming - teo - traffic calming             234
##  7 teo-sgns - teo - signs and markings         233
##  8 parking - teo - parking                     223
##  9 tree/sup - tree/support/code compliance     178
## 10 drains - drain maintenance                  143
## # ... with 17 more rows

#split data by sap_category to get a quick look and decide how to sort
otherGID <- otherGID %>%
  group_by(sap_problem_category) %>%
  nest() %>%
  mutate(count = map_int(data, nrow))

#only going to use a few names from sap_category
#most of the other records that remain have very little info, difficult to tell
#if sap_category is informative or a mistake for most
consolidateNames <- tribble(
  ~replaceName,           ~matchExp,
  "street striping",      "striping",
  "traffic signal",       "timing"
)

#make changes to service name
allGIDRequests <- str_RowDfReplace_FilterFirst(allGIDRequests, "sap_problem_category", "service_nameMOD",
                                               consolidateNames, otherIDX)

#check out service name counts now
serviceCountMOD <- modCount()
knitr::kable(serviceCountMOD,
             caption = "Record of service type counts",
             align = "l",
             format = "markdown",
             table.attr = "style='width:80%;'")
```

| service\_nameMOD   | n     | service\_nameMOD   | n     | service\_nameMOD   | n     |
|:-------------------|:------|:-------------------|:------|:-------------------|:------|
| curb               | 6141  | curb               | 6604  | curb               | 6604  |
| damaged guardrail  | 610   | damaged guardrail  | 731   | damaged guardrail  | 731   |
| graffiti           | 23930 | graffiti           | 24212 | graffiti           | 24212 |
| illegal dumping    | 4561  | illegal dumping    | 4659  | illegal dumping    | 4659  |
| other              | 52784 | other              | 35844 | other              | 35723 |
| sidewalk repair    | 9407  | sidewalk repair    | 12608 | sidewalk repair    | 12608 |
| storm drain        | 2668  | storm drain        | 3135  | storm drain        | 3135  |
| street flooded     | 1421  | street flooded     | 1789  | street flooded     | 1789  |
| street light       | 14538 | street light       | 14838 | street light       | 14838 |
| street repair      | 39042 | street repair      | 41394 | street repair      | 41394 |
| street striping    | 1943  | street striping    | 2010  | street striping    | 2065  |
| street sweeping    | 1955  | street sweeping    | 3584  | street sweeping    | 3584  |
| traffic sign       | 6844  | traffic sign       | 7808  | traffic sign       | 7808  |
| traffic signal     | 15998 | traffic signal     | 17495 | traffic signal     | 17561 |
| vegetation service | 5901  | vegetation service | 11032 | vegetation service | 11032 |

The final 2 columns show the new service counts after the latest reclassification. Only a few hundred **other** records were reclassified.

``` r
#in redoing the service names, made some mods to descriptions, have to redo the twoWord description snapshot
allGIDRequests <- mutate(allGIDRequests, twoWordDesc = str_extract(description, twoWord))
```

Recategorize select service types
---------------------------------

``` r
#recategorizing named services based on sap_problem_type #

#for some reason it looks like a lot of records with "sidewalk" service name
#have details in sap_problem_type/category that indicate actual issue is somehitng
#else and sidewalk is just being used as some sort of indication of location in description
#going to reassign abunch of records with "sidewalk" service name due to this
otherIDX <- str_detect(allGIDRequests$service_name, "sidewalk")

consolidateNames <- tribble(
  ~replaceName,           ~matchExp,
  "vegetation service",   "(tree[^t])|(frond)|(weeds)|(sidewalk encroachment)",
  "curb",                 "(broken curb)|(^curb)|(paint curb)|(zone)",
  "street sweeping",      "debris in street",
  "graffiti",             "graffiti",
  "damaged guardrail",    "guardrail",
  "street light",        "(pole knock)|(light out)",
  "street repair",        "(pothole)|(asphalt)"
)

#modify service name
#if sap_problem_type matches matchExp replace service_nameMOD with replaceName
allGIDRequests <- str_RowDfReplace_FilterFirst(allGIDRequests, "sap_problem_type", "service_nameMOD",
                                               consolidateNames, otherIDX)

#there are a couple problem types that look like they are scattered amoungst services,
#want to consolidate
consolidateNames <- tribble(
  ~replaceName,           ~matchExp,
  "illegal dumping",      "bike removal",
  "vegetation service",   "(evaluate tree)|(parkway)|(trim tree)",
  "graffiti",             "graffiti"
)

#modify service name
#if sap_problem_type matches matchExp replace service_nameMOD with replaceName
allGIDRequests <- str_RowDfReplace_FilterFirst(allGIDRequests, "sap_problem_type", "service_nameMOD",
                                               consolidateNames)

#check out service name counts
serviceCountMOD <- modCount()
knitr::kable(serviceCountMOD,
             caption = "Record of service type counts",
             align = "l",
             format = "markdown",
             table.attr = "style='width:95%;'")
```

<table>
<colgroup>
<col width="18%" />
<col width="6%" />
<col width="18%" />
<col width="6%" />
<col width="18%" />
<col width="6%" />
<col width="18%" />
<col width="6%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">service_nameMOD</th>
<th align="left">n</th>
<th align="left">service_nameMOD</th>
<th align="left">n</th>
<th align="left">service_nameMOD</th>
<th align="left">n</th>
<th align="left">service_nameMOD</th>
<th align="left">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">curb</td>
<td align="left">6141</td>
<td align="left">curb</td>
<td align="left">6604</td>
<td align="left">curb</td>
<td align="left">6604</td>
<td align="left">curb</td>
<td align="left">6669</td>
</tr>
<tr class="even">
<td align="left">damaged guardrail</td>
<td align="left">610</td>
<td align="left">damaged guardrail</td>
<td align="left">731</td>
<td align="left">damaged guardrail</td>
<td align="left">731</td>
<td align="left">damaged guardrail</td>
<td align="left">742</td>
</tr>
<tr class="odd">
<td align="left">graffiti</td>
<td align="left">23930</td>
<td align="left">graffiti</td>
<td align="left">24212</td>
<td align="left">graffiti</td>
<td align="left">24212</td>
<td align="left">graffiti</td>
<td align="left">24484</td>
</tr>
<tr class="even">
<td align="left">illegal dumping</td>
<td align="left">4561</td>
<td align="left">illegal dumping</td>
<td align="left">4659</td>
<td align="left">illegal dumping</td>
<td align="left">4659</td>
<td align="left">illegal dumping</td>
<td align="left">4705</td>
</tr>
<tr class="odd">
<td align="left">other</td>
<td align="left">52784</td>
<td align="left">other</td>
<td align="left">35844</td>
<td align="left">other</td>
<td align="left">35723</td>
<td align="left">other</td>
<td align="left">35723</td>
</tr>
<tr class="even">
<td align="left">sidewalk repair</td>
<td align="left">9407</td>
<td align="left">sidewalk repair</td>
<td align="left">12608</td>
<td align="left">sidewalk repair</td>
<td align="left">12608</td>
<td align="left">sidewalk repair</td>
<td align="left">11202</td>
</tr>
<tr class="odd">
<td align="left">storm drain</td>
<td align="left">2668</td>
<td align="left">storm drain</td>
<td align="left">3135</td>
<td align="left">storm drain</td>
<td align="left">3135</td>
<td align="left">storm drain</td>
<td align="left">3132</td>
</tr>
<tr class="even">
<td align="left">street flooded</td>
<td align="left">1421</td>
<td align="left">street flooded</td>
<td align="left">1789</td>
<td align="left">street flooded</td>
<td align="left">1789</td>
<td align="left">street flooded</td>
<td align="left">1789</td>
</tr>
<tr class="odd">
<td align="left">street light</td>
<td align="left">14538</td>
<td align="left">street light</td>
<td align="left">14838</td>
<td align="left">street light</td>
<td align="left">14838</td>
<td align="left">street light</td>
<td align="left">14853</td>
</tr>
<tr class="even">
<td align="left">street repair</td>
<td align="left">39042</td>
<td align="left">street repair</td>
<td align="left">41394</td>
<td align="left">street repair</td>
<td align="left">41394</td>
<td align="left">street repair</td>
<td align="left">41415</td>
</tr>
<tr class="odd">
<td align="left">street striping</td>
<td align="left">1943</td>
<td align="left">street striping</td>
<td align="left">2010</td>
<td align="left">street striping</td>
<td align="left">2065</td>
<td align="left">street striping</td>
<td align="left">2065</td>
</tr>
<tr class="even">
<td align="left">street sweeping</td>
<td align="left">1955</td>
<td align="left">street sweeping</td>
<td align="left">3584</td>
<td align="left">street sweeping</td>
<td align="left">3584</td>
<td align="left">street sweeping</td>
<td align="left">3601</td>
</tr>
<tr class="odd">
<td align="left">traffic sign</td>
<td align="left">6844</td>
<td align="left">traffic sign</td>
<td align="left">7808</td>
<td align="left">traffic sign</td>
<td align="left">7808</td>
<td align="left">traffic sign</td>
<td align="left">7408</td>
</tr>
<tr class="even">
<td align="left">traffic signal</td>
<td align="left">15998</td>
<td align="left">traffic signal</td>
<td align="left">17495</td>
<td align="left">traffic signal</td>
<td align="left">17561</td>
<td align="left">traffic signal</td>
<td align="left">17537</td>
</tr>
<tr class="odd">
<td align="left">vegetation service</td>
<td align="left">5901</td>
<td align="left">vegetation service</td>
<td align="left">11032</td>
<td align="left">vegetation service</td>
<td align="left">11032</td>
<td align="left">vegetation service</td>
<td align="left">12418</td>
</tr>
</tbody>
</table>

The latest reclassifications should have decreased the number of sidewalk records and added to types illegal dumping, vegetation service, and graffiti among others which appears to be the case.

Modify subtypes
---------------

``` r
#modify subtypes for some services
#create bar plots showing subtypes colored by two word desc
#can visually check to make sure subtypes and two word desc generally make sense for service type

#CURB
#modify records with other/na as subtype or not in new subtype list
#initalize some variables
service <- "curb"
currentSubtypes <- c("illegal painting", "faded paint", "damage") #leave alone records with these current subtype assignments
#tibble of subtypes to use (replaceName) based on regex matches in description column (matchExp)
renameSubtypes <- tribble(
  ~replaceName,           ~matchExp,
  "damage",               "(damage)|(broken)",
  "faded paint",          "(paint curb)|(curb maintenance)|(faded paint)",
  "illegal painting",     "(illegal)|(legality)",
  "parking change/eval",  "(parking evaluation)|(eval.*z)|(designation evaluation)"
)

allGIDRequests <- modifySubTypes(allGIDRequests, service, currentSubtypes, renameSubtypes)

#finally if parking/zone eval indicated in description change to parking change/eval even if currently has a
#valid subtype
selectService <- allGIDRequests$service_nameMOD == service
selectSTR <- str_detect(allGIDRequests$description, renameSubtypes$matchExp[[4]])
selectToModify <- selectService & selectSTR
allGIDRequests$service_subtype[selectToModify] <- "parking change/eval"

#check out plot of subtypes
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc

#STORM DRAIN
#modify records with other/na as subtype or not in new subtype list
#initalize some variables
service <- "storm drain"
currentSubtypes <- c("clogged storm drain", "channel cleaning", "foul odor",
                     "grate frame broken or missing", "object in drain") #leave alone records with these current subtype assignments
#tibble of subtypes to use (replaceName) based on regex matches in description column (matchExp)
renameSubtypes <- tribble(
  ~replaceName,           ~matchExp,
  "clogged storm drain",  "(blocked at)|(drain plugged)|(debris)|(clog)",
  "drain repair",          "(broken at)|(drain repair)",
  "channel cleaning",     "channel cleaning",
  "grate frame broken or missing","missing",
  "foul odor",            "foul",
  "object in drain",      "object in"
)

allGIDRequests <- modifySubTypes(allGIDRequests, service, currentSubtypes, renameSubtypes)

#check out plot of subtypes
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc

#STREET LIGHT
#modify records with other/na as subtype or not in new subtype list
#initalize some variables
service <- "street light"
#leave alone records with these current subtype assignments
currentSubtypes <- c("light out", "light on during day", "damage")
#tibble of subtypes to use (replaceName) based on regex matches in description column (matchExp)
renameSubtypes <- tribble(
  ~replaceName,           ~matchExp,
  "light out",            "light out",
  "light on during day",  "(light on)|(day burner)",
  "damage",               "damage"
)

allGIDRequests <- modifySubTypes(allGIDRequests, service, currentSubtypes, renameSubtypes)
#check out plot of subtypes
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc

#STREET REPAIR
#modify records with other/na as subtype or not in new subtype list
#initalize some variables
service <- "street repair"
currentSubtypes <- NA #leave alone records with these current subtype assignments
#tibble of subtypes to use (replaceName) based on regex matches in description column (matchExp)
renameSubtypes <- tribble(
  ~replaceName,           ~matchExp,
  "street repair",        "(asphalt)|(resurfacing)|(concrete pavement)|(concrete street)|(pavement)|(road bump)",
  "pothole",              "pothole",
  "alley repair",         "alley"
)

allGIDRequests <- modifySubTypes(allGIDRequests, service, currentSubtypes, renameSubtypes)

#check out plot of subtypes
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc

#TRAFFIC SIGNAL
#rename flashing red
changeIDX <- allGIDRequests$service_name == "traffic signal" & allGIDRequests$service_subtype == "flashing red"
changeIDX[is.na(changeIDX)] <- FALSE
allGIDRequests$service_subtype[changeIDX] <- "lights on flash"

#modify records with other/na as subtype or not in new subtype list
#initalize some variables
service <- "traffic signal"
#leave alone records with these current subtype assignments
currentSubtypes <- c("light out", "damage", "lights on flash", "all lights out",
                     "signal facing wrong direction", "timing")
#tibble of subtypes to use (replaceName) based on regex matches in description column (matchExp)
renameSubtypes <- tribble(
  ~replaceName,           ~matchExp,
  "all lights out",       "(lights out)|(all out)",
  "lights on flash",      "(on flash)|(flashing)",
  "damage",               "(damage)|(knock)",
  "timing",               "(timing)|(retime)|(coord)|(phasing)",
  "light out",            "light out",
  "stuck light",          "stuck light",
  "signal facing wrong direction", "wrong direction"
)

allGIDRequests <- modifySubTypes(allGIDRequests, service, currentSubtypes, renameSubtypes)
#check out plot of subtypes
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc

#VEGETATION SERVICE

#modify records with other/na as subtype or not in new subtype list
#initalize some variables
service <- "vegetation service"
currentSubtypes <- NA #leave alone records with these current subtype assignments
#tibble of subtypes to use (replaceName) based on regex matches in description column (matchExp)
renameSubtypes <- tribble(
  ~replaceName,             ~matchExp,
  "tree planting request",  "free tree",
  "remove weeds",           " weed",
  "tree infestation",       "infestation",
  "tree/limb/palm frond fallen/hanging", "(fallen)|(frond)",
  "evaluate tree for removal", "for removal",
  "encroachment on ped/vehicle/signage", "(trim tree for peds)|(blocking (signage|traffic))|(encroachment)|(visibility)",
  "root concrete damage",   "concrete",
  "contract trimming",      "contract",
  "tree grate missing/reset", "tree grate"
)

allGIDRequests <- modifySubTypes(allGIDRequests, service, currentSubtypes, renameSubtypes)

#records stil listed as tree hazard, na, or other have info in sap_problem_type
#use sap_problem_type to get more specific service_subtype
changeIDX <- allGIDRequests$service_nameMOD == "vegetation service" &
  str_detect(allGIDRequests$service_subtype, "(other)|(tree hazard)")

allGIDRequests <- str_RowDfReplace_FilterFirst(allGIDRequests, "sap_problem_type", "service_subtype",
                                               renameSubtypes, changeIDX)

#check out plot of subtypes
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc

#SERVICE WITHOUT SUBTYPE
#a bunch of services are really narrow in scope and/or have fewer records and are
#not worth assigning subtypes
useNaSubtype <- c("damaged guardrail", "graffiti", "illegal dumping", "other", "sidewalk repair",
                  "street flooded", "street striping", "street sweeping", "traffic sign")

selectService <- allGIDRequests$service_nameMOD %in% useNaSubtype

allGIDRequests$service_subtype[selectService] = NA
service = "sidewalk repair"
subTypePlot(allGIDRequests, service) #bar plot of subtypes colored by twoWordDesc
```

![](README_files/figure-markdown_github/modify%20subtypes-1.png)![](README_files/figure-markdown_github/modify%20subtypes-2.png)![](README_files/figure-markdown_github/modify%20subtypes-3.png)![](README_files/figure-markdown_github/modify%20subtypes-4.png)![](README_files/figure-markdown_github/modify%20subtypes-5.png)![](README_files/figure-markdown_github/modify%20subtypes-6.png)![](README_files/figure-markdown_github/modify%20subtypes-7.png)

Clean up source
---------------

``` r
#clean up "source"
#any source that contributes less than 1% of records group together as other
#source is how the request was supplied to GID by the public
keepSource <- allGIDRequests %>%
  count(source) %>%
  mutate(prop = n/sum(n)) %>%
  filter(prop > 0.01)

changeIDX <- !(allGIDRequests$source %in% keepSource$source)
allGIDRequests$source[changeIDX] <- "other"
```

Huzzah!
