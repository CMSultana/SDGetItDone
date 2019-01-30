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
             format = "html",
             table.attr = "style='width:40%;'")
```

<table style="width:40%;">
<caption>
Services &lt; 200 records after initial consolidation
</caption>
<thead>
<tr>
<th style="text-align:right;">
cyl
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
14
</td>
</tr>
</tbody>
</table>
``` r
#combine all service types with low counts into "other type"
#names of all services that now have <200 counts
tmp2 <- mtcars %>%
  count(gear)
knitr::kable(tmp,
             caption = "Services < 200 records after initial consolidation",
             format = "html",
             table.attr = "style='width:40%;'")
```

<table style="width:40%;">
<caption>
Services &lt; 200 records after initial consolidation
</caption>
<thead>
<tr>
<th style="text-align:right;">
cyl
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
14
</td>
</tr>
</tbody>
</table>
