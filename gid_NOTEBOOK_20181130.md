SD Get It Done Analyses
================
20181130

-   [Refine date/time data](#refine-datetime-data)
-   [Set elements in plotting/analysis](#set-elements-in-plottinganalysis)
-   [Volume of requests made: 2016-2018](#volume-of-requests-made-2016-2018)
-   [Volume requests closed: 2016-2018](#volume-requests-closed-2016-2018)
-   [Limited data set: complex analyses](#limited-data-set-complex-analyses)
-   [How/when requests are made](#howwhen-requests-are-made)
-   [Request time distribution anomolies](#request-time-distribution-anomolies)
-   [When requests are closed](#when-requests-are-closed)
-   [Time to complete work orders](#time-to-complete-work-orders)

**Examination of San Diego Get It Done (GID) data set (<https://data.sandiego.gov/datasets/get-it-done-311/>), downloaded 09/27/2018.**

-   Investigating
    -   historical request made/closed volume
    -   distribution of requests made/closed by time of day/weekday
    -   time to close requests
-   R learning goals
    -   tidyverse packages/paradigms (piping, tibbles, dplyr, lubridate, etc)
    -   plotting with ggplot2
    -   r markdown: html document

``` r
#set global chunk options
knitr::opts_chunk$set(collapse = TRUE, fig.show = "hold", fig.width = 7, fig.asp = 0.618, results = "hide", cache = TRUE)
```

Refine date/time data
---------------------

``` r
#function to pull out time for each datetime
#sets date to single value to allow easy analysis for time of day
get_TIME <- function(x) {
  tmp <- str_sub(x, 12, 19)
  tmp <- paste("2016-01-01", tmp, sep = " ")
  tmp <- ymd_hms(tmp)
}
```

``` r
#change date/time column names to make consistent
allGIDRequests <- rename(allGIDRequests, updated_date = updated_datetime)
allGIDRequests <- rename(allGIDRequests, requested_date = requested_datetime)

#referred cases look like they are no longer updated in GID database
#but don't have a "closed date" indicated
#create new column (referred_date) to store date case was close/referred 
#using updated_date
changeIDX <- allGIDRequests$status_description == "closed - referred"
allGIDRequests <- mutate(allGIDRequests, referred_date = updated_date)
allGIDRequests$referred_date[!changeIDX] <- NA 

#create new time only columns
#makes time of day comparisons easier
allGIDRequests <- allGIDRequests %>% 
  mutate(requested_time = get_TIME(requested_date),
         updated_time = get_TIME(updated_date),
         closed_time  = get_TIME(closed_date),
         referred_time = get_TIME(referred_date))
## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone 'zone/tz/2018i.1.0/
## zoneinfo/America/Los_Angeles'
## Warning: 57431 failed to parse.
## Warning: 149599 failed to parse.

#create timeToX columns
#difference in time between various case status changes 
#measured in days
allGIDRequests <- allGIDRequests %>%
  mutate(
    timeToClose  = difftime(closed_date, requested_date, units = "days"), 
    timeToUpdate = difftime(updated_date, requested_date, units = "days"),
    timeToRefer  = difftime(referred_date, requested_date, units = "days"), 
    timeOngoing = difftime(now(), requested_date, units = "days"),
    timeSinceUpdate = difftime(now(), updated_date, units = "days"))
## Warning in C_valid_tz(tzone): System timezone name is unknown. Please set
## environment variable TZ.

#everything that is closed should have NA for timeOngoing and timeSinceUpdate
changeIDX <- str_detect(allGIDRequests$status_description, "close")
allGIDRequests[changeIDX, c("timeSinceUpdate", "timeOngoing")] <- NA
```

``` r
#modify date columns to floor 
# ,
#     requested_date = floor_date(requested_date, unit = "day"),
#     updated_date = floor_date(updated_date, unit = "day"),
#     referred_date = floor_date(referred_date, unit = "day"),
#     closed_date = floor_date(closed_date, unit = "day"),
#   )

#conversion from number day of week to day abbreviation
#want week to start on Monday so weekend days are grouped visually in plots
weekConvert <- c("Su", "M", "Tu", "W", "Th", "F", "Sa")
weekOrder <- c("M", "Tu", "W", "Th", "F", "Sa", "Su")
weekDay <- c("M", "Tu", "W", "Th", "F")

#create day of week columns (generates number)
allGIDRequests <- allGIDRequests %>%
  mutate(requestDay   = wday(requested_date),
         closeDay     = wday(closed_date),
         referDay     = wday(referred_date),
         updateDay    = wday(updated_date)
  )
    
#create factor for day of week columns, use abbreviations for day of week
#could just do this in wday with label, abbr, and week_start input, realized this after the fact
#also would still end up redoing factor to get the two letter abbreviations
allGIDRequests <- allGIDRequests %>%
  mutate(requestDay = factor(weekConvert[requestDay], weekOrder),
         closeDay = factor(weekConvert[closeDay], weekOrder),
         referDay = factor(weekConvert[referDay], weekOrder),
         updateDay = factor(weekConvert[updateDay], weekOrder)
  )
```

``` r
#rearrange columns to make viewing relevant columns easier
allGIDRequests <- allGIDRequests %>% 
  select(contains("date"), contains("time"), contains("day"), everything())
```

Set elements in plotting/analysis
---------------------------------

``` r
#set date time variables
startDay <- ymd("2017-10-01") #for many analyses will only use more recent data after this date
secDay <- 86400 #for converting sec to day
secWeek <- secDay * 7
minTime <- min(allGIDRequests$requested_date)
minTime <- floor_date(minTime, "day")
maxTime = max(allGIDRequests$requested_date)
maxTime = ceiling_date(maxTime, "day")
seqTime = seq(minTime, maxTime, secDay) #bin edges to bin by day
midTime = seqTime[-length(seqTime)] + secDay/2 #bin midpoint for bin by day

#current minTime is a Friday. When binning by week want to start on a Monday 
#and not have bins that span across weeks. Fix this by adding three days to left
#edge of first week bin
seqTimeWeek = seq(minTime+days(3), maxTime, secWeek) #align bin edges to bin by week Monday-Sunday
seqTimeWeek[1] = minTime #set the very first day to minTime so don't have to deal with data lying outside
#this way don't have to deal with data falling outside bins. means first "week" bin is actually 10 days but no big deal
midTimeWeek = seqTimeWeek[-length(seqTimeWeek)] + secWeek/2 #bin midpoint for bin by day

#date axes for pretty plotting
timeYTicks <- ymd_hms(c("2016-01-01 00:00:00", "2016-01-01 06:00:00",
                       "2016-01-01 12:00:00", "2016-01-01 18:00:00",
                       "2016-01-02 00:00:00"))

timeXTicks <- ymd_hms(c("2016-01-01 00:00:00", "2016-01-01 06:00:00",
                       "2016-01-01 09:00:00","2016-01-01 12:00:00", 
                       "2016-01-01 15:00:00","2016-01-01 18:00:00",
                       "2016-01-02 00:00:00"))

dayTimeYAxis <- scale_y_datetime(breaks = timeYTicks, date_labels = "%H:%M")
dayTimeXAxis <- scale_x_datetime(breaks = timeXTicks, date_labels = "%H") 
```

``` r
#function to return n discrete default colors from ggplot
#taken from stack overflow 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#want colors for each reporting source (mobile, phone, web, other) to be consistent between plots 
#even when using 3 or 4 sources in the plot. 
#setting color scale manuallly to ensure consistency in color and enforce desired order in stacked bars
col4 <- gg_color_hue(4)
colMob <- col4[1]
colWeb <- col4[4]
colPho <- col4[3]
colOth <- col4[2]
colValues <- c("mobile" = colMob, "web" = colWeb, "phone" = colPho, "other" = colOth)
colBreaks <- c("mobile", "web", "phone", "other")

mpwoSourceColor <- scale_color_manual(values = colValues, breaks = colBreaks)
mpwSourceColor <- scale_color_manual(values = colValues[-4], breaks = colBreaks[-4])
mpwoSourceFill <- scale_fill_manual(values = colValues, breaks = colBreaks)
mpwSourceFill <- scale_fill_manual(values = colValues[-4], breaks = colBreaks[-4])

#colors for different case actions
colReq <- "red"
colRef <- "blue"
colClo <- "black"
```

``` r
#taken from stack overflow
colFmt = function(x,color){
    paste("<font color='",color,"'>",x,"</font>",sep="")
}
```

``` r
#top right justify legend
#also manipulate margins to decrease white space between
#legend and other elements (plot and title)
topLegend <-  theme(legend.position = "top", legend.justification = c(1,0), 
                    legend.margin = margin(t = -0.7, b = -0.7, unit = "lines"),
                    legend.key.height = unit(1, "lines"))
```

``` r
#function to create a title left aligned with left edge of entire plot area
#in ggplot can only left align as far as left axis
#using draw_text from cowplot library to get desired far left aligned title
leftTitle <- function(fig) {
  #get curent title on figure
  tmpTitle <- fig$labels$title
  #have to keep empty title to give space for new title using draw_text
  fig <- fig + labs(title = "")
  #why didn't I pipe this all in one go? cause I was getting errors and this worked
  ggdraw(fig) + draw_text(tmpTitle, x = 0.01, y = 0.96, hjust = 0, vjust = 0, size = 14)
}
```

``` r
#for scales 0-1 don't like tick labels 0.00 or 1.00, just want 0 and 1
propX <- scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1"))
propY <- scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1"))
propBar <- scale_fill_continuous(breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1"))
```

Volume of requests made: 2016-2018
----------------------------------

``` r
#function to return daily frequency given date input vector/column
get_TIMEcounts <- function(x) {
  tmp <- hist(x, breaks = seqTime, plot = FALSE)
  tmp <- tmp$counts
  tmp
}

#get daily frequency for complaints made, closed, updated, and referred
timeColumns <- c("requested_date", "closed_date", "updated_date", "referred_date")
timeColumns <- allGIDRequests[timeColumns]
allTimeCounts <- as.tibble(map(timeColumns, get_TIMEcounts)) %>% 
  mutate(dateCut = midTime)

#get cumulative counts for complaints made, closed, etc
allTimeCum <- allTimeCounts %>% 
  mutate(cumRequested = cumsum(requested_date),
         cumClosed = cumsum(closed_date),
         cumReferred = cumsum(referred_date),
         cumOpen = cumRequested - cumClosed - cumReferred) %>% 
  select(dateCut, starts_with("cum"))

#gathering data into single column to make plotting different case statuses easier in ggplot
allTimeCounts <- allTimeCounts %>% 
  gather(requested_date, closed_date, updated_date, referred_date, key = "action", value = "dateTotal")

allTimeCum <- allTimeCum %>% 
  gather(cumRequested, cumClosed, cumReferred, cumOpen, key = "action", value = "cumulativeTotal")
```

``` r
# get proportion of each complaint source (mobile, web, etc) to absolute counts over time
# for each time bin want #sourceX/#total
# can't figure out an easier way to do this cleverly with ggplot 
# by specifying the stat or postion or group or something
timeSourceTable <- allGIDRequests %>% 
  select(source, requested_date) %>%
  mutate(dateCut = findInterval(requested_date, seqTime)) %>% 
  count(source, dateCut) %>% 
  group_by(dateCut) %>% 
  mutate(dateTotal = sum(n), 
         prop = n/dateTotal) %>% 
  ungroup() %>% 
  mutate(dateCut = midTime[dateCut])
```

``` r
##daily absolute counts over time split by reporting source
tmpFig <- allTimeCounts %>% 
  filter(action == "requested_date") %>% 
  ggplot(aes(x = dateCut, y = dateTotal)) + 
  geom_line() +
  labs(title = "Daily volume of Get It Done requests",
       y = "# of requests made", x = "date")
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/daily%20count%20plot:%20number%20requests-1.png)

The **Get it Done (GID)** site/app launched in 2016. The daily volume of requests jumped at the beginning of 2017 and has since leveled out and remained generally consistent over the past year. It appears that there are also regular spikes and dips in the number of requests. Intuitively this is likely related weekly cycles and will be examined later.

``` r
##daily proportion of each reporting source to total counts 
tmpFig <- timeSourceTable %>% 
  ggplot(aes(x = dateCut, y = prop, color = source)) + 
  geom_line() +
  labs(title = "Proportion of requests by reporting method",
       y = "proportion of daily total", 
       x = "date",
       color = "method") + 
  mpwoSourceColor + 
  topLegend +
  guides(color = guide_legend(keywidth = 0.5)) +
  propY
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/time-source%20plot:%20proportion-1.png)

Requests can be made to the Get It Done program either by <font color='#F8766D'>the mobile app</font>, <font color='#C77CFF'>the website</font>, <font color='#00BFC4'>by phone call</font>, or occasionally by some method/s labeled as <font color='#7CAE00'>other</font>. Making requests with the GID mobile app has become increasingly popular over time. The proportion of requests made via the GID website has dropped by nearly 50% over two years. A strange spike in the proportion of requests made by phone is evident in July of 2018.

``` r
#box to highlight in plot phone spike period
addRec <- annotate("rect",ymin = -Inf, ymax = +Inf,
                   xmin = as.POSIXct("2018-07-23 00:00:00"), xmax = as.POSIXct("2018-08-02 00:00:00"), 
                   fill = "grey", alpha = 0.35)

#filtering data to highlight phone spike period
tmpFig <- timeSourceTable %>% 
  filter(dateCut > ymd("2018-07-01")) %>% 
  ggplot(aes(x = dateCut, y = prop)) + 
  geom_line(aes(color= source)) +
  addRec +
  labs(title = "Proportion of requests by reporting method (late 2018)",
       y = "proportion of daily total", 
       x = "date",
       color = "") + 
  mpwoSourceColor +
  topLegend +
  propY
leftTitle(tmpFig)

tmpFig <- timeSourceTable %>% 
  filter(dateCut > ymd("2018-07-01")) %>% 
  ggplot(aes(x = dateCut, y = n)) + 
  geom_line(aes(color = source)) +
  addRec +
  labs(title = "Daily volume of requests by reporting method (late 2018)",
       y = "# of requests made", 
       x = "date",
       color = "") +
  mpwoSourceColor +
  guides(color = FALSE)
leftTitle(tmpFig)

tmpFig <- allTimeCounts %>% 
  filter(dateCut > ymd("2018-07-01"),
         action == "requested_date") %>% 
  ggplot(aes(x = dateCut, y = dateTotal)) + 
  geom_line() +
  addRec +
  labs(title = "Daily volume of all requests (late 2018)",
       y = "# of requests made",
       x = "date") 
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/time-source%20plot:%20phone%20spike-1.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/time-source%20plot:%20phone%20spike-2.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/time-source%20plot:%20phone%20spike-3.png)

At the <span style="background:#d8d8d8">end of July 2018 </span>, the spike in the proportion of requests made by phone (top panel) is a result in a precipitous drop in the number of mobile and web requests (middle panel), rather than a sudden increase in the number of phone requests. Maybe IT issues on the city end during these days? The total number of requests on these days clearly falls below the usual volume (bottom panel). Notice by zooming in, the weekly rhythm to the daily request volume is more evident.

Volume requests closed: 2016-2018
---------------------------------

``` r
tmpFig <- allTimeCounts %>% 
  filter(action %in% c("closed_date", "referred_date", "requested_date")) %>% 
  ggplot(aes(x = dateCut, y = dateTotal, color = action)) +
  geom_line(size = 0.3) +
  scale_color_manual(values = c("closed_date" = colClo, "referred_date" = colRef, "requested_date" = colReq),
                     breaks = c("requested_date", "closed_date", "referred_date"),
                     labels = c("requested", "closed", "referred")) +
  labs(title = "Daily volume of requests made, closed, and referred",
       y = "# of actions",
       x = "date",
       color = "") +
  topLegend
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/daily%20count%20plot:%20closed/referred-1.png)

At the beginning of 2017 the daily volume of **cases closed** rose, likely in response to the increase in <font color='red'>requests made</font> by the public. The number of <font color='blue'>cases referred</font> oddly falls to nearly zero during August 2017. This period is followed by two spikes, with the number of referred cases exceeding 1500 daily. There is also an odd drop in the number of cases closed during March 2017.

``` r
#get day of week for the two spikes in daily referred case count
#will annotate onto plot
maxReferred <- allTimeCounts %>% 
  filter(dateCut > ymd("2017-07-01"), dateCut < ymd("2017-11-01"),
         action == "referred_date") %>%
  arrange(desc(dateTotal)) %>% 
  head(2) %>% 
  mutate(dayWeek = wday(dateCut)) %>% 
  mutate(dayWeek = factor(weekConvert[dayWeek], weekOrder))

tmpFig <- allTimeCounts %>% 
  filter(dateCut > ymd("2017-07-01"), dateCut < ymd("2017-11-01")) %>% 
  filter(action %in% c("closed_date","referred_date")) %>% 
  ggplot(aes(x = dateCut, y = dateTotal, color = action)) +
  geom_line() +
  geom_text(aes(label = dayWeek, hjust = 1.1), data = maxReferred, show.legend = FALSE) +
  scale_color_manual(values = c("closed_date" = colClo, "referred_date" = colRef),
                     labels = c("closed", "referred")) +
  labs(title = "Daily volume of closed and referred cases (late 2017)", 
       y = "# of actions",
       x = "date",
       color = "") +
  topLegend
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/daily%20count%20plot:%20closed/referred%20zoom%20in-1.png)

Zooming into 2017, it is obvious that the spikes in the number of cases referred are isolated to just to one Wednesday and one Monday. These spikes are soon after the lull in referred cases through August. The strong weekly cycle of the closed case volume is apparent here.

``` r
tmpFig <- allTimeCum %>% 
  ggplot(aes(x = dateCut, y = cumulativeTotal, color = action)) +
  geom_line() +
  scale_color_manual(values = c("cumClosed" = colClo, "cumReferred" = colRef, "cumRequested" = colReq, "cumOpen" = "grey"),
                     breaks = c("cumRequested", "cumClosed", "cumReferred", "cumOpen"),
                     labels = c("requested", "closed", "referred", "still open")) +
  labs(title = "Cumulative totals over lifetime of GID",
       y = "cumulative total cases",
       x = "date",
       color = "") +
  topLegend
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/daily%20count%20plot:%20cumulative-1.png)

Over 150,000 <font color='red'>requests have been made</font> via the Get It Done program. Since early 2017 the number of <font color='gray'>open complaints</font> has hovered between 1500 and 2000.

Limited data set: complex analyses
----------------------------------

To examine more complex relations between request type, reporting method, timing, location etc only data for complaints made after 2017-10-01 is examined. Data grooming done previous to this analysis made obvious that keywords used to describe complaints in the data set have evolved over time. I spent time attempting to homogenize the data set after the fact. However, by using more recent cases some of the biggest inconsistencies in labeling cases can be avoided. This is a very new program, established in the summer of 2016, so it is not surprising that the methods to organize and label the cases are still evolving.

``` r
recentGIDRequests <- allGIDRequests %>% 
  filter(requested_date > startDay)
```

How/when requests are made
--------------------------

``` r
#get total number of complaints by type
#to be used as a label on the plot
totalLabel <- recentGIDRequests %>% 
  count(service_nameMOD)

#proportion of each reporting method by complaint type
tmpFig <- recentGIDRequests %>%
  group_by(service_nameMOD) %>% 
  mutate(isMobile = source == "mobile") %>%
  ungroup() %>% 
  #reorder data so plot displays with types with greatest proportion of mobile complaints on top
  ggplot(aes(x = fct_reorder(service_nameMOD, isMobile, fun = mean))) +
  geom_bar(aes(fill = source), position = "fill") +
  labs(title = "Reporting method for each request type",
       y = "proportion of total cases", 
       x = "request type", 
       fill = "reporting\nmethod") +
  #add label to show total number of cases for each complaint type
  geom_text(aes(x = service_nameMOD, y = 0.02, label = n, hjust = "inward"),
            color = "grey",
            fontface = "bold",
            size = 3,
            data = totalLabel) +
  mpwoSourceFill +
  propY +
  coord_flip()
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/request%20type-source%20plot-1.png)

The contribution of each source to the number of requests made varies widely by request type. The total number of cases for each service type are annotated on the left hand side of the chart. Unfortunately, roughly 30% of requests are categorized as "other", making meaningful examination of a large part of the data set difficult.

``` r
#show the distribution of number of complaints made by day of week and source type as boxplot
tmpFig <- recentGIDRequests %>%
  filter(source %in% c("mobile", "phone", "web")) %>%
  count(floor_date(requested_date, "day"), source, requestDay) %>% 
  ggplot(aes(x = requestDay, color = source)) + 
  geom_boxplot(aes(y = n)) +
  mpwSourceColor +
  labs(title = "Daily request volume by reporting method and day of week",
       fill = "reporting\nmethod", 
       x = "day of week", y = "# of requests made") +
  coord_cartesian(ylim = c(0, 250))
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/request%20volume-source-day-1.png)

The weekly pattern apparent in plotting the daily complaint volume from 2016-2018 is clarified by looking at the daily volume of requests grouped by day of the week. For all reporting methods, the daily request volume was lower over the weekend than compared to the week. However, the requests made by the mobile app only dropped slightly over the weekend, while the request volume via web and phone plummeted roughly 50%. The steep drop in web activity on Saturday and Sunday may be due to overall lower use/access to a personal computer over the weekend. The staff necessary to process requests via phone are likely only avaiable during regular office hours, resulting in the low volume of phone requests during weekends. This hypothesis is supported by examining the time of day requests are made (figure below).

``` r
# show distribution of of time of day of complaints by source and day of week
# use a violin plot to show overall shape of distribution overlaid by jittered
# points 
tmpFig <- recentGIDRequests %>% 
  filter(source %in% c("mobile", "phone", "web")) %>%
  ggplot(aes(x = requestDay, color = source, y = requested_time)) + 
  geom_violin(adjust = 0.5) +
  geom_jitter(shape = ".", alpha = 0.2) +
  dayTimeYAxis +
  mpwSourceColor +
  labs(title = "Request time by reporting method and day of week",
       color = "reporting\nmethod", 
       x = "day of week", y = "time") +
  guides(color = FALSE) +
  facet_wrap(~source)
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/hour-source-day%20plot-1.png)

During the week, the requests made by the mobile app are more evenly distributed across the entire day when compared to web and phone requests. A greater fraction of mobile requests are made in the very early morning (6-7am) and after work hours (after ~5) compared to the web and phone requests. There is also little difference between mobile weekday and weekend distributons.

In contrast, during the weekday most phone requests are packed into normal office hours (~8-5). For most services the city likely only supports phone requests during weekday working hours, resulting in the tight distribution. This was difficult to verify as no central GID phone number is advertised by the city. It appears problems can be reported directly to various departments, which then potentially input the request into the GID system. Similar to phone requests, during the week, web requests are also more likely to occur during normal office hours, though the tail into the evening is more significant. Requests can be made any time of day via the city website, so the the higher density of web requests during office hours is potentially due to greater use/access to personal computers at work.

Over the weekend, requests are spread out more evenly throughout the day, and the distinctions between the mobile, web, and phone distributions largely vanish.

This visualization also makes evident two groupings of mobile requests which are at odds with the relativley smooth distributions on the other days: the cluster of requests after 6pm (18:00) on Tuesday and the narrow band of requests around 4am on Saturday.

``` r
#list of all service types by volume, can be used to limit faceting to top services
useService <- recentGIDRequests %>%
  count(service_nameMOD, sort = TRUE) 

# show distribution of of time of day of complaints by day or week and service type
# use a violin plot to show overall shape of distribution overlaid by jittered points 
tmpFig <- recentGIDRequests %>% 
  # only showing top 12 services with most cases
  # data is limited to mobile requests to limit confounding influence of reporting type on distributions 
  filter(service_nameMOD %in% useService$service_nameMOD[1:12],
         source == "mobile") %>%
  # consolidating day of week to weekday and weekend, clear differences between M-F and Sat-Sun
  # did not pop out when plotting days individually and made visualization very dense
  mutate(weekDayEnd = if_else(requestDay %in% weekDay, "weekday", "weekend")) %>% 
  ggplot(aes(x = weekDayEnd, y = requested_time)) +
  geom_violin() + 
  geom_jitter(shape = ".", alpha = 0.4, color = col4[1]) +
  labs(title = "Request time by request type and weekday/end (mobile data only)",
       x = "", y = "time") + 
  dayTimeYAxis +
  facet_wrap(~service_nameMOD)
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/hour-day-type%20plot-1.png)

The hour requests are made is also clearly impacted by the request type (e.g. street light, graffiti, etc). To help highlight the relationship between request type and request time, only mobile data is utilized in this example. Each reporting method has a distinct request time distribution which could obscure the influence of request type (see previous figure). To simplify the visualization, only the aggregate weekday and weekend distributions are displayed. The dissimilarity in the request time distributions for some service types can be intuitively explained. For example, broken street lights are only apparent in the dark, and thus most street lights requests occur after 6pm. Likewise, the bimodal distribution for weekday traffic signal requests conspicuously peaks around 8am and 5:30pm, coincident with rush hour.

Request time distribution anomolies
-----------------------------------

``` r
#Isolating the "blob" periods on Tuesday and Saturday where there is a strange grouping of complaints 
#in time with very high density that does not follow the smooth trends seen for the other days.  

#getting all data within the blob time period for all Tuesdays/Saturdays
#this could be done more elegantly by splitting dataset into two with column of lists, but was feeling lazy
tuesBlob <- recentGIDRequests %>% 
filter(source == "mobile",
requestDay == "Tu",
(requested_time > ymd_hms("2016-01-01 18:00:00") & requested_time < ymd_hms("2016-01-01 19:00:00")))
satBlob <- recentGIDRequests %>% 
filter(source == "mobile",
requestDay == "Sa",
(requested_time > ymd_hms("2016-01-01 04:00:00") & requested_time < ymd_hms("2016-01-01 05:30:00"))) 

#gather daily counts into one variable for plotting 
tuesCount <- get_TIMEcounts(tuesBlob$requested_date)
blobCount <- tibble(count = tuesCount, day = "Tues", date = midTime)
satCount <- get_TIMEcounts(satBlob$requested_date)
blobCount2 <- tibble(count = satCount, day = "Sat", date = midTime)
blobCount <- rbind(blobCount, blobCount2)

#percent of the data in the time period due to the one day spike
maxPercent <- blobCount %>% 
filter(date > startDay) %>% 
group_by(day) %>% 
summarize(max_percent = max(count)/sum(count)) %>% 
mutate(max_percent = round(max_percent, 2))
```

``` r
#plot to see if there are clear spikes in the isolated data on a specific day
#a spike in the data would indicate one/a few days contributed to the strange high density,
#rather than the blob being due to regularly occuring (normal?) high request density
#a spike on a specific day would indicate something "weird"
tmpFig <- blobCount %>% 
  filter(date > startDay) %>% 
  ggplot(aes(x = date, y = count, color = day)) +
  geom_line(size = 0.3) +
  labs(y = "# of requests made",
       x = "date",
       title = "Volume of requests made",
       subtitle = "data isolated to Tuesdays 6pm-7pm and Saturdays 4am-5:30am") +
    scale_color_manual(values = c("Sat" = "black", "Tues" = "red"))
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/timeblob%20complaint%20plot:%20spike-1.png)

There are clear single day spikes in the volume of requests made when the data is limited to the days and time periods of odd high density noted previously (Tuesday 6pm-7pm and Saturday 4am-5:30am). 42% of the Tuesday requests and 59% of the Saturday requests in the data shown are located in the single day spikes. This suggests some sort of "weird" behavior on these days, either a big problem causing a sudden flood of requests or processing of some sort of backlog on the city's end.

``` r
#compare the request type during the spike day versus 
#all other days. 
tmpFig <- tuesBlob %>% 
  add_count(floor_date(requested_date, "day")) %>%
  mutate(inblob = if_else(n == max(n), "spikeDay", "otherDays")) %>% 
  ggplot(aes(x = inblob, fill = service_nameMOD)) +
  geom_bar(position = "fill") +
  labs(x = "", y = "proportion", fill = "request type",
       title = "Request type on spike day vs all other days",
       subtitle = "6pm-7pm Tuesday data")
leftTitle(tmpFig)

tmpFig <- satBlob %>% 
  add_count(floor_date(requested_date, "day")) %>%
  mutate(inblob = if_else(n == max(n), "spikeDay", "otherDays")) %>% 
  ggplot(aes(x = inblob, fill = service_nameMOD)) +
  geom_bar(position = "fill") +
  labs(x = "", y = "proportion", fill = "request type",
       title = "Request type on spike day vs all other days",
       subtitle = "4am-5:30am Saturday data") +
  propY
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/timeblob%20complaint%20plot:%20request%20type-1.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/timeblob%20complaint%20plot:%20request%20type-2.png)

The type of requests made on the spike day is similar to that of the aggregate of all other days. If the spike was due to a specific big problem causing a flood of requests (downed power line, sinkhole, etc) then the requests on the spike day should be dominated by a single type.

When requests are closed
------------------------

``` r
# have to go through this hoopla because I decided referred_date/Day and closed_date/Day
# should be separated out..which turns out to be a mistake and make everything harder
# would be better to have single closed_date and just separte out the two using status_description
referCloseTimeCount <- recentGIDRequests %>% 
  filter(status_description %in%  c("closed", "closed - referred")) %>%
  mutate(date = if_else(status_description == "closed", closed_date, referred_date),
         day = if_else(status_description == "closed", closeDay, referDay),
         status_description = if_else(status_description == "closed", "closed: work order",
                                    "closed: referred to dep outside GID system")) %>% 
  select(date, day, status_description) %>% 
  count(date = floor_date(date,"day"), day, status_description)
```

``` r
#create plot
#wanted this to be a single panel with the two violin plots for each day next to each other
#but this causes the overlaid jittered points to be mixed up and not separated by the 
#status description (though they are differently colored). Sure there is a way to get around
#this but not worth the effort
tmpFig <- referCloseTimeCount %>% 
  ggplot(aes(x = day, y = n, color = status_description)) + 
  geom_violin(scale = "width") +
  geom_jitter(shape = ".", alpha = 0.5, width = 0.3) +
  scale_color_manual(values = c("closed: work order" = colClo, 
                                "closed: referred to dep outside GID system" = colRef), 
                     guide = FALSE) +
  labs(title = "Daily volume of requests closed by day of week",
       x = "day of week", y = "# of requests closed", color = "close type") +
  facet_wrap(~status_description)
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/closing%20volume-day%20plot-1.png)

On the GID website, requests can be indicated as closed because a <font color='black'>**work order has been completed**</font> or the request has been <font color='blue'>referred to a department outside the GID system</font>. During the week, far more cases are closed by a work order (ie presumably by fulfilling the request) than by referral. Unsurprisingly, the volume of cases closed by work order dramatically falls on the weekend, and cases are closed more frequently by referral than work order.

Is this increase in the proportion of referred cases because

1.  request types that would normally be referred are higher on weekends?

2.  request types are similar across the entire week, but are more frequently sent to "referral" on weekends?

``` r
#list of all services sorted by volume 
#used to limit faceted plots to top types
useService <- recentGIDRequests %>%
  count(service_nameMOD, sort = TRUE) 

#use this to reverse day order so aligns with plot below
revDay = 7:1

#for all closed cases get the fractional contribution
#of each request type for each day
#have to do 2 things to allCloseDay after combining closeDay/referDay
#1, convert to numeric so geom_line displays properly in fig below
#2, reverse values to align with day order below
fracServiceDay <- recentGIDRequests %>%
  filter(status_description %in% c("closed", "closed - referred")) %>% 
  mutate(allCloseDay = if_else(status_description == "closed", closeDay, referDay),
         allCloseDay = as.numeric(allCloseDay),
         allCloseDay = revDay[allCloseDay]) %>% 
  count(allCloseDay, service_nameMOD) %>% 
  group_by(allCloseDay) %>% 
  mutate(fracService = n/sum(n)) %>% 
  ungroup()
```

``` r
#for a specific service are cases more likely to be referred vs closed on certain days
#want referred/(referred + closed) by day by service 
tmpFig <- recentGIDRequests %>%
  filter(status_description %in% c("closed", "closed - referred")) %>% 
  #combine referDay/closeDay into single column to make analysis/plotting easier
  #reverse the factor order so that Monday displays on top in plot
  mutate(allCloseDay = if_else(status_description == "closed", closeDay, referDay),
         allCloseDay =  factor(allCloseDay, rev(weekOrder))) %>%
  group_by(service_nameMOD, allCloseDay) %>%
  #for each date get total referred, total closed, and referred proportion
  summarize(numRefer = sum(!is.na(referred_date)),
            numClose = sum(!is.na(closed_date)),
            fracRefer = numRefer/(numRefer + numClose)) %>% 
  ggplot(aes(x = allCloseDay)) +
  #frac referral bar 
  geom_bar(aes(weight = fracRefer)) +
  #frac service type line 
  geom_path(aes(y = fracService), data = fracServiceDay, color = "red") +
  labs(title = "Fraction of referrals in 'closed cases' by request type",
       y = "# referred / (# referred + # work order complete)", x = "day case closed") +
  propY +
  coord_flip() +
  facet_wrap(~service_nameMOD)
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/referred%20proportion-day-service%20plot-1.png)

For most service types, the fraction of referrals that make up closed cases (grey bars) is relatively higher on the weekend, particularly on Sunday. It seems odd that the referred fraction of closed cases for nominally similar requests is dramatically different on the weekend than during the week. Are the requests being closed actually substantially different, resulting in their referral, or is this a result of city/system policies? The red line indicates the fraction of all requests closed on a specific weekday of a specific request type (e.g. Graffiti requests make up about 20% of all requests closed during the week, and roughly 10% on the weekend.) On the weekend the proportion of closed illegal dumping and *other* cases rises while the fraction of most other case types remains level or drops slightly.

``` r
#for closed work order vs referred cases get the fraction that close right
#after midnight and the fraction that close within two hours of the request
closeVsRefer <- recentGIDRequests %>% 
  filter(!is.na(closeDay) | !is.na(referDay)) %>%
  mutate(closed_time = if_else(status_description == "closed - referred", referred_time, closed_time),
         timeToClose = if_else(status_description == "closed - referred", timeToRefer, timeToClose)
         ) %>% 
  group_by(status_description) %>% 
  summarize(fracMidnight = mean(updated_time < ymd_hms("2016-01-01 00:10:00")),
            close2hours = mean(timeToClose < 2*(1/24)),
            fracTrafficRush = sum((closed_time > ymd_hms("2016-01-01 17:00:00") & closed_time < ymd_hms("2016-01-01 18:00:00") & service_nameMOD == "traffic signal")) / sum((closed_time > ymd_hms("2016-01-01 17:00:00") & closed_time < ymd_hms("2016-01-01 18:00:00"))),
            fracTrafficRest = sum(!(closed_time > ymd_hms("2016-01-01 17:00:00") & closed_time < ymd_hms("2016-01-01 18:00:00")) & service_nameMOD == "traffic signal") / sum(!(closed_time > ymd_hms("2016-01-01 17:00:00") & closed_time < ymd_hms("2016-01-01 18:00:00"))))

closeVsRefer[2:5] <- round(closeVsRefer[2:5],2)*100
```

``` r
#compare closing time and request time for closed-work order and closed-referred requests

#mostly same setup for two plots below
tmp <- recentGIDRequests %>% 
  filter(!is.na(closeDay) | !is.na(referDay)) %>%
  #have to go through this rigamarole cause I separated referral and closing data out into seperate variables...stupidly
  mutate(closed_time = if_else(status_description == "closed - referred", referred_time, closed_time),
         status_description = if_else(status_description == "closed - referred", "closed: referred", "closed: work order"),
         closeDay = if_else(status_description == "closed: referred", referDay, closeDay)) %>%
  ggplot(aes(x = closeDay, color = status_description)) + 
  dayTimeYAxis +
  scale_color_manual(values = c("closed: work order" = "black", "closed: referred" = "blue"),
                     breaks = c("closed: work order", "closed: referred"),
                     guide = FALSE) +
  labs(x = "day of week", y = "time") +
  facet_wrap(~status_description)

#close time by day and closing type
tmpFig <- tmp +
  geom_violin(aes(y = closed_time), adjust = 0.5, scale = "width") +
  geom_jitter(aes(y = closed_time), shape = ".", alpha = 0.2, width = 0.4) +
  labs(title = "Time request closed")
leftTitle(tmpFig)

#request time by day and closing type
tmpFig <- tmp +
  geom_violin(aes(y = requested_time), adjust = 0.5, scale = "width") +
  geom_jitter(aes(y = requested_time), shape = ".", alpha = 0.2, width = 0.4) +
  labs(title = "Time request made")
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/close/request%20time-day%20plot-1.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/close/request%20time-day%20plot-2.png)

Most closing times of requests completed by work order are unsurprisingly between 7am and 4pm, with a lull during the middle of the day, presumably during lunch hour. There is also a small mode of work orders completed between 5 and 6pm, likely due to requests that necessitate quick resolution during rush hour. Over 53% of work orders completed between 5 and 6pm are traffic signal requests, which make up only 15% of work orders closures during the rest of the day.

75% of referrals occur within 2 hours of the initial request, resulting in a broad distribution mimicking that of the request times. In comparison, only 4% of work orders are completed within two hours of the initial request.

There is a strange concentration of work orders which have a close time in the GID database immediately after midnight. 30% of all requests closed by work orders have a closing time within the ten minutes after midnight. Near 0% of referrals are made during this time. It is improbable that this high volume of work orders completed at or near midnight, and the peak at this time is likely due to some form of automated batch processing done by the city.

``` r
#when requests are closed (work order only) during day by service type
#removing weird blob of requests between midnight and 00:10 which can obscure
#the trends throughout the rest of the day
tmpFig <- recentGIDRequests %>% 
  filter(!is.na(closeDay),
         closed_time > ymd_hms("2016-01-01 00:10:00"),
         closeDay %in% weekDay) %>%
  #for plot want scale of time to close to only go from 0-20 days
  #currently the range is 0 to +300, making visualizing differences in any
  #days <50 difficult
  mutate(timeToClose = if_else(timeToClose > 20, 20, as.numeric(timeToClose))) %>% 
  ggplot(aes(x = service_nameMOD, y = closed_time)) + 
  geom_violin(adjust = 0.5, scale = "width") +
  geom_jitter(aes(color = timeToClose), shape = ".", alpha = 0.3, width = 0.4) +
  scale_color_gradientn(colors = c("cyan3","gold","magenta")) +
  dayTimeYAxis +
  labs(title = "Time work order fulfilled by request type (weekdays)",
       x = "",
       y = "time",
       color = "Days taken to\nclose request") +
  coord_flip()
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/closeTime-service%20plot-1.png)

As indicated in the previous plot, during the week most work orders are completed between 7am and 4pm. However, the distribution of work order times varies depending on the request type, as demonstrated in the figure above. In this visualization each dot is a closed request colored by the number of days between the request time and close time. Requests closed before 00:10 are not included.

Street sweeping, flooding, and traffic signals all have large modes outside of normal working hours. For flooding and traffic signals this is likely due to the need to immediately resolve issues: most requests closed after 5pm were completed within less than a day.

Time to complete work orders
----------------------------

``` r
#didn't finish this, prob not going to use
get_AVERAGEbyTime <- function(df, timeColumn, dataColumn, timeBreaks) {
  eqTimeColumn <- enquo(timeColumn)
  eqDataColumn <- enquo(dataColumn)
  
  df %>% 
    mutate(dateCut = findInterval(!!timeColumn, timeBreaks)) %>%
    group_by(dateCut) %>% 
}
```

``` r
#only look at closed (work order) data
tmpFig <- recentGIDRequests %>% 
  filter(status_description == "closed") %>%
  ggplot(aes(x = timeToClose)) +
  geom_histogram(aes(fill = closeDay),
                 color = "black", lwd = 0.1, binwidth = 1, center = 0) +
  #like the single column version of this plot
  facet_grid(requestDay ~ .) +
  coord_cartesian(xlim = c(0,100)) +
  labs(title = "Time to fulfill by day of week request made",
       x = "days taken to close request",
       y = "# of requests closed", 
       fill = "day of week\nrequest closed") +
    topLegend
leftTitle(tmpFig)
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.

#just modify yaxis range and display again
tmpFig <- tmpFig +
  coord_cartesian(xlim = c(0,100), ylim = c(0,200)) +
  labs(title = "Zoom in- time to fulfill")
leftTitle(tmpFig)
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20day%20plot-1.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20day%20plot-2.png)

The figure above shows the distribution for the number of days taken to fulfill a work order for a request, grouped by the day of the week the request is made (grey box at right). The bars are colored by the day of the week the request is closed (legend at right). The second figure is a zoomed in version of the first plot.

This plot reveals three interesting trends in the number of days it takes to complete a work order for a request

-   Most requests are closed in the first 25 days. The probability that a request will be closed in a specific number of days generally decreases as time goes on.

-   Very few requests are closed on the weekend (Saturday and Sunday), resulting in cyclical dips in the volume of requests closed, overlaid on the general decay trend described above.

-   The dips in the volume of requests closed occurs on days depending on the day of the week the request is made. For example, if a request is made on a Monday (top panel) then the work order is very unlikely to be fulfilled on the fifth or sixth day following, as this is the weekend. However, if a request is made on a Friday (fifth panel), the work order is much more likely to be fulfilled on the third day (Monday), rather than the first or second day which is again the weekend.

``` r
#number of days taken to close request 
#grouped by day of week request closed
tmpFig <- recentGIDRequests %>% 
  filter(status_description == "closed") %>%
  ggplot(aes(x = timeToClose)) +
  geom_histogram(binwidth = 1, center = 0) +
  facet_grid(closeDay ~ .) +
  coord_cartesian(xlim = c(0,100)) +
  labs(title = "Time to fulfill by day of week request closed",
       x = "days taken to close request",
       y = "# of requests closed")
leftTitle(tmpFig)
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-close%20day%20plot-1.png)

Are work orders completed on some days of the week more urgent than others? Requests that are closed in just a few days, may be considered more urgent than requests that are closed after more time. The figure above shows that while few requests are closed on the weekend (Saturday and Sunday, bottom two panels), a large fraction of requests closed on these days were open for less than a day, suggesting many requests closed on the weekend are of a critical nature.

``` r
#bin data by days to close and do some summary calcs for data grouped in each bin
#only looking at requests closed by work order

#bins for days to close calc
#bins are 1/4 day (0-6, 6-12, 12-18, 18-24 hours)
timeToCloseSeq = seq(from = 0, to = 100, by = 0.25)
maxClose <- max(recentGIDRequests$timeToClose, na.rm = TRUE)
timeToCloseSeq = seq(from = 0, to = as.numeric(maxClose), by = 0.25)

timeBinCalcs <- recentGIDRequests %>% 
  filter(status_description == "closed") %>%
  #is the request closed during the day?, dayTime
  #is the request closed from 00:00-00:10? midnightTime
  #time bin request is closed, timeToCloseBin
  #mid point of bin request is closed, timetoCloseMid
  mutate(dayTime = closed_time > ymd_hms("2016-01-01 06:00:00") & 
         closed_time < (ymd_hms("2016-01-01 18:00:00")),
         midnightTime = closed_time < ymd_hms("2016-01-01 00:10:00"),
         timeToCloseBin = findInterval(timeToClose, timeToCloseSeq),
         timeToCloseMid = timeToCloseSeq[timeToCloseBin]+0.125) %>% 
  group_by(service_nameMOD, timeToCloseMid) %>%
  #frac of requests closed during the day?, dayTimeFrac
  #frac of request closed from 00:00-00:10? midNightFrac
  # # of requests closed in each time bin, n
  summarize(count = n(), dayTimeFrac = mean(dayTime), midNightFrac = mean(midnightTime)) %>% 
  group_by(service_nameMOD) %>% 
  #for closures grouped by request type
  #frac of all closures in each time bin, fracCount
  #num of closures in each time bin normalized by max, normCount
  #cumulative fration of closures over time, cumCount
  mutate(fracCount = count/sum(count), normCount = count/max(count), cumCount = cumsum(fracCount))
```

``` r
#histogram distribution days to close a request (bars)
#and cumulative total out of all closed requests (red line)
#using normalized values so that everything can be plotted on one y axis

#using geom_col rather than geom_hist,
#faster (I presume) as calcs are already done and stored and don't have to redo 
#each time the script is run
#have to adjust x axis data for geom_step, as geom_col and geom_step plot data a little differently
tmpFig <- timeBinCalcs %>% 
  ggplot(aes(x = timeToCloseMid, y = normCount)) +
  geom_col() +
  geom_step(aes(x = timeToCloseMid-0.125, y = cumCount),
            color = "red", size = 0.2) +
  coord_cartesian(xlim = c(0, 100)) +
  propY +
  facet_wrap(~service_nameMOD) +
  labs(x = "days taken to close request",
       y = "normalized count",
       title = "Time to fulfill by request type")
leftTitle(tmpFig)

#same plot as above just zoom in 
tmpFig <- tmpFig +
  labs(title = "Zoom in") +
  coord_cartesian(xlim = c(0, 20))
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20type%20plot-1.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20type%20plot-2.png)

The amount of time it takes to complete a work order for a request varies depending on the type of request made. For each request type, the plot above shows the normalized distribution of the number of days taken to fufill a request (grey bars) as well as the cumulative fraction of closed requests over time (red line). For example, for sidewalk repair while more requests are closed within the first day than during any other period (grey bars), less than 50% of all closed sidewalk requests occur within the first 100 days (red line).

``` r
#histogram distribution of days to close a request (bars)
#colored by either fraction of requests closed during day or at midnight

#only plot request types not listed in removeType, just a lot otherwise
#create request type label strings for plots (not using the titles provided by facet_wrap)
removeType = c("illegal dumping","damaged guardrail", "other", "street striping", "street flooded")
labelService <- filter(useService, !(service_nameMOD %in% removeType))

#set up for two plots is pretty much the same
#geom_text and theme calls are to add request type labels to plot and remove
#grey box titles normally employed by facet_wrap. Gives more space for the data in plot. 
tmpFig <- timeBinCalcs %>% 
  filter(!(service_nameMOD %in% removeType)) %>% 
  ggplot(aes(x = timeToCloseMid, y = normCount)) +
  scale_fill_gradientn(colors = c("cyan3","gold","magenta"), 
                       limits = c(0,1), breaks = c(0, .5, 1),
                       labels = c("0", ".5", 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5), labels = c("","","")) +
  geom_text(aes(x = 18.4, y = 0.8, label = service_nameMOD, hjust = "inward"),
            color = "grey28",
            size = 3, data = labelService) +
  coord_cartesian(xlim = c(0, 18)) +
  labs(x = "days taken to close request",
       y = "normalized count",
       title = "Time to fulfill by request type",
       fill = "fraction closed: 6am-6pm") +
  facet_wrap(~service_nameMOD, nrow = 5, ncol = 2) +
  theme(strip.text = element_blank()) +
  topLegend

#color bars by fraction of requests closed during day (6am-6pm)
tmpFig2 <- tmpFig +
  geom_col(aes(fill = dayTimeFrac))
leftTitle(tmpFig2)

#color bars by fraction of requests closed at midnight (00:00-00:10)
tmpFig <- tmpFig +
  geom_col(aes(fill = midNightFrac)) +
    labs(fill = "fraction closed: midnight")
leftTitle(tmpFig)
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20type-coloredBars%20plot-1.png)![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20type-coloredBars%20plot-2.png)

Within the GID database most work orders are reported as fulfilled either during daylight hours (nominally 6am to 6pm) or within the ten minutes after midnight. Most requests closed within one day are indicated as fulfilled during the day. As more days are taken to close the request, it becomes increasingly likely that the work order is closed at midnight within the GID database.

``` r
#timeline over lifetime of dataset for each request type
#showing boxplot-ish distribution of days to close a request for each time bin
#median is red dot, lines are 25-75 quantiles

#only plot request types not listed in removeType, just a lot otherwise
#create request type label strings for plots (not using the titles provided by facet_wrap)
removeType = c("illegal dumping","damaged guardrail", "other", "street striping", "street flooded")
labelService <- filter(useService, !(service_nameMOD %in% removeType))

#using own geom_segment/point visualization rather than boxplot as less cluttered and 
#can make median value pop more
#geom_text and theme calls are to add request type labels to plot and remove
#grey box titles normally employed by facet_wrap. Gives more space for the data in plot. 
tmpFig <- allGIDRequests %>% 
    filter(status_description == "closed",
           !(service_nameMOD %in% removeType)) %>% 
  #group data into weekly time bins
  mutate(dateCut = findInterval(closed_date, seqTimeWeek), 
         dateBin = midTimeWeek[dateCut]) %>%
  group_by(service_nameMOD, dateBin) %>%
  summarize(median = median(timeToClose), twentyFive = quantile(timeToClose, 0.25),
            seventyFive = quantile(timeToClose, 0.75)) %>% 
  ggplot(aes(x = dateBin)) +
  geom_segment(aes(xend = dateBin, y = twentyFive, yend = seventyFive), size = 0.2) +
  geom_point(aes(y = median), color = "red", size = 0.5) +
  geom_text(aes(x = ymd_hms("2016-05-01 00:00:00"), y = 105, label = service_nameMOD, hjust = "inward"),
            color = "grey28",
            size = 3, data = labelService) +
  coord_cartesian(ylim = c(0, 125)) +
  facet_wrap(~service_nameMOD, nrow = 5, ncol = 2) +
  labs(x = "date",
       y = "days taken to close request",
       title = "Time to fulfill distribution: 2016-2018") +
  theme(strip.text = element_blank())
leftTitle(tmpFig)
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
## Warning: Removed 10 rows containing missing values (geom_segment).
## Warning: Removed 10 rows containing missing values (geom_point).
```

![](gid_NOTEBOOK_20181130_files/figure-markdown_github/days%20to%20close-request%20type-timeline-1.png)

Midyear during both 2017 and 2018 (roughly April to October) the time taken to complete work orders is generally higher than the rest of the year. The plot above shows the median (red dot) and 25th and 75th percentiles (black lines) days to close for each week in the dataset. The full range for the days to close (y axis) is not shown. The maximum days to close increases with the lifetime of the GID database, and showing the full range visually compresses the majority of the data into a small portion of the plot.

``` r
#was playing around with all these plots at some point, didn't end up using

#changes in time to close for entire dataset lifetime
allGIDRequests %>% 
  filter(status_description == "closed") %>% 
  mutate(dateCut = findInterval(closed_date, seqTimeWeek),
         isSunday = closeDay %in% c("Sa","Su")) %>%
  group_by(dateCut) %>% 
  ggplot(aes(x = closed_date, y = timeToClose)) +
  geom_point(aes(color = isSunday), shape = ".", alpha = 0.3)

allGIDRequests %>% 
  filter(status_description == "closed",
         closeDay %in% weekDay) %>% 
  mutate(dateCut = findInterval(closed_date, seqTimeWeek)) %>%
  group_by(dateCut) %>% 
  ggplot(aes(x = closed_date, y = timeToClose)) +
  geom_point(shape = ".", alpha = 0.2, color = "grey") +
  geom_boxplot(aes(group = dateCut), outlier.shape = NA, color = "red", 
               lwd = 0.2, fatten = 3, coef = 0, alpha = 0) +
  coord_cartesian(ylim = c(0, 125))

allGIDRequests %>% 
  filter(status_description == "closed",
         closeDay %in% weekDay) %>% 
  mutate(dateCut = findInterval(closed_date, seqTimeWeek)) %>%
  group_by(dateCut) %>%
  summarize(lessOne = mean(timeToClose < 1)) %>% 
  ggplot(aes(x = dateCut)) +
  geom_point(aes(y = lessOne))

##distribution of timeToClose by request day by service
recentGIDRequests %>% 
  filter(service_nameMOD %in% useService$service_nameMOD[1:9],
         !is.na(closeDay)) %>% 
  mutate(timeToClose = as.numeric(timeToClose)) %>%
  ggplot(aes(x = requestDay, y = timeToClose)) +
  geom_violin(scale = "width", adjust = 0.5) +
  coord_cartesian(ylim = c(0.01, 300)) +
  scale_y_log10() +
  facet_wrap(~service_nameMOD)



##distribution of timeToClose by request day by source
allGIDRequests %>% 
  filter(requested_date > startDay,
         service_nameMOD %in% useService$service_nameMOD[1:9],
         !is.na(closeDay)) %>% 
  mutate(timeToClose = as.numeric(timeToClose)) %>%
  ggplot(aes(x = source, y = timeToClose)) +
  geom_violin(scale = "width", adjust = 0.5) +
  coord_cartesian(ylim = c(0.01, 300)) +
  scale_y_log10() +
  facet_wrap(~service_nameMOD)



##check out distribution of time of day for referrals
recentGIDRequests %>% 
  filter(source %in% c("mobile", "phone", "web")) %>%
  ggplot(aes(x = referDay, color = source)) + 
  dayTimeYAxis +
  geom_violin(aes(y = referred_time), adjust = 0.5)

recentGIDRequests %>% 
  filter(service_nameMOD %in% useService$service_nameMOD[1:9],
         !is.na(referDay)) %>% 
  mutate(timeToRefer = (as.numeric(timeToRefer)),
         timeToRefer2 = ifelse(timeToRefer > 1, 1, timeToRefer)) %>%  
  ggplot(aes(x = referDay, y = referred_time)) +
  geom_violin(scale = "width") + 
  geom_jitter(aes(color = timeToRefer2), size = 1, alpha = 0.3, shape = 20) +
  scale_color_gradientn(colors = c("darkblue", "cyan", "magenta", "gold"), limits = c(0, 1)) +
  dayTimeYAxis +
  facet_wrap(~service_nameMOD)








##check out distribtion of time of day by close day and service, color with time to close
recentGIDRequests %>% 
  filter(service_nameMOD %in% useService$service_nameMOD[1:9],
         !is.na(closeDay)) %>% 
  mutate(timeToClose = (as.numeric(timeToClose)),
         timeToClose2 = ifelse(timeToClose > 100, 100, timeToClose)) %>%  
  ggplot(aes(x = closeDay, y = closed_time)) +
  geom_violin(scale = "width") + 
  geom_jitter(aes(color = timeToClose2), size = 1, alpha = 0.3, shape = 20) +
  scale_color_gradientn(colors = c("darkblue", "cyan", "magenta", "gold"), limits = c(0, 100)) +
  dayTimeYAxis +
  facet_wrap(~service_nameMOD)



##check out distribtion of time of day by close day and service, color with source
allGIDRequests %>% 
  filter(requested_date > startDay,
         service_nameMOD %in% useService$service_nameMOD[1:9],
         !is.na(closeDay)) %>% 
  mutate(timeToClose = (as.numeric(timeToClose)),
         timeToClose2 = ifelse(timeToClose > 100, 100, timeToClose)) %>%  
  ggplot(aes(x = closeDay, y = closed_time)) +
  geom_violin(scale = "width") + 
  geom_jitter(aes(color = source), size = 1, alpha = 0.3, shape = 20) +
  dayTimeYAxis +
  facet_wrap(~service_nameMOD)

dayCuts <- c(0, 0.1, 1, 2, 5, 10, 25, 50, 100, 150, 300)

#distribution of timeToClose by service name
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         lateClose = timeToClose > 100,
         timeToClose = cut(as.numeric(timeToClose), dayCuts)) %>%
  ggplot(aes(x = fct_reorder(service_nameMOD, lateClose, fun = mean))) +
  geom_bar(aes(fill = timeToClose), position = "fill") +
  coord_flip()

#distribution of timeToClose by service name
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         lateClose = timeToClose > 100,
         timeToClose = as.numeric(timeToClose)) %>%
  ggplot(aes(x = fct_reorder(service_nameMOD, lateClose, fun = mean))) +
  geom_violin(aes(y = timeToClose), scale = "width", adjust = 0.5) +
  coord_cartesian(ylim = c(0.01, 300)) +
  scale_y_log10() +
  coord_flip() 

#distribution of timeToClose by service name
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         lateClose = timeToClose > 100,
         timeToClose = as.numeric(timeToClose)) %>%
  ggplot(aes(x = fct_reorder(service_nameMOD, lateClose, fun = mean))) +
  geom_violin(aes(y = timeToClose, color = midNightClose), scale = "width", adjust = 0.5) +
  coord_cartesian(ylim = c(0.01, 300)) +
  scale_y_log10() +
  coord_flip() 


#distribution of timeToClose by service name
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         lateClose = timeToClose > 100,
         timeToClose = as.numeric(timeToClose)) %>%
  ggplot(aes(x = timeToClose)) +
  geom_histogram(aes(color = midNightClose)) +
  coord_cartesian(ylim = c(0.01, 300)) +
  scale_y_log10() +
  facet_wrap() 


#strangely a bunch of cases seem to be closed within a few minutes of midnight
#gonna look at those more closely
closeMidnight <- allGIDRequests %>% 
  filter(closed_time < ymd_hms("2016-01-01 00:30:00"))


#time to close compared between ~midnight closers and rest of day
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         timeToClose = as.numeric(timeToClose)) %>% 
  ggplot(aes(x = timeToClose)) +
  geom_freqpoly(aes(y = ..density.., color = midNightClose), bins = 150) +
  coord_cartesian(xlim = c(0.01, 300), ylim = c(0.01, 1)) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~service_nameMOD) 

#time to close compared between ~midnight closers and rest of day
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         timeToClose = cut(as.numeric(timeToClose), dayCuts)) %>%
  group_by(service_nameMOD, midNightClose, timeToClose) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(service_nameMOD, midNightClose) %>% 
  mutate(percent = count/sum(count)) %>% 
  ungroup() %>% 
  ggplot(aes(x = timeToClose)) +
  geom_bar(aes(fill = midNightClose, weight = percent), position = "dodge") +
  facet_wrap(~service_nameMOD)

#closed time by service name, split by  ~midnight closers and rest of day
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay")) %>% 
  ggplot(aes(x = closed_time)) +
  geom_freqpoly(aes(y = ..ncount.., color = midNightClose), bins = 96) +
  dayTimeXAxis +
  facet_wrap(~service_nameMOD) 

#closed_time by service name 
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  mutate(midNightClose = ifelse(closed_time < ymd_hms("2016-01-01 00:30:00"), "midnight", "restOfDay"),
         timeToClose = as.numeric(timeToClose)) %>% 
  ggplot(aes(x = closed_time)) +
  geom_freqpoly(aes(y = ..ncount..), bins = 96) +
  dayTimeXAxis +
  facet_wrap(~service_nameMOD)

#fraction closed between 00:00-00:30 by service name 
allGIDRequests %>% 
  filter(requested_date > startDay,
         !is.na(closeDay)) %>% 
  group_by(service_nameMOD) %>% 
  summarize(fracMidnight = mean(closed_time < ymd_hms("2016-01-01 00:30:00"))) %>% 
  ggplot(aes(fct_reorder(service_nameMOD, fracMidnight))) +
  geom_bar(aes(weight = fracMidnight)) +
  coord_flip()

#compare timeToClose

allGIDRequests %>% 
  filter(requested_date > startDay,
         service_nameMOD %in% useService$service_nameMOD[1:6],
         !is.na(closeDay)) %>% 
  mutate(timeToClose = (as.numeric(timeToClose)),
         timeToClose2 = ifelse(timeToClose > 100, 100, timeToClose),
         closeDay = ifelse(closeDay %in% weekDay, "weekDay", "weekEnd")) %>%
  
  ggplot(aes(x = closed_time, y = closed_time)) +
  geom_freqpoly(scale = "width") + 
  geom_jitter(aes(color = timeToClose2), size = 1, alpha = 0.3, shape = 20) +
  scale_color_gradientn(colors = c("darkblue", "cyan", "magenta", "gold"), limits = c(0, 100)) +
  facet_wrap(~service_nameMOD)





scale_color_distiller(palette = "Spectral", limits = c(0, 150))
```
