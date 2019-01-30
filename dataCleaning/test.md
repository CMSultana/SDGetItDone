test
================
Camille Sultana
1/30/2019

``` r
#consolidate low count service types into "other"

#names of all services that now have <200 counts
tmp <- mtcars %>%
  count(cyl)
knitr::kable(tmp,
             caption = "Services < 200 records after initial consolidation",
             format = "markdown",
             table.attr = "style='width:40%;'")
```

|  cyl|    n|
|----:|----:|
|    4|   11|
|    6|    7|
|    8|   14|

``` r
#combine all service types with low counts into "other type"
#names of all services that now have <200 counts
tmp2 <- mtcars %>%
  count(gear)
knitr::kable(tmp,
             caption = "Services < 200 records after initial consolidation",
             format = "markdown",
             table.attr = "style='width:40%;'")
```

|  cyl|    n|
|----:|----:|
|    4|   11|
|    6|    7|
|    8|   14|
