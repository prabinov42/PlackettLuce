An Introduction to the Plackett Luce Model
================
Peter Rabinovitch
2023-03-19 11:38:10

``` r
library(tidyverse)
library(PlackettLuce)
library(igraph)
library(tictoc)
library(knitr)
```

# Intro

This note in an intro to using the R [PlackettLuce
package](https://cran.r-project.org/web/packages/PlackettLuce/vignettes/Overview.html)
to analyze user ratings of some videos.

The videos are as follows, where we have a *short* name associated with
each video to ease discussion. We also have a few features of the video
listed as potential covariates.

| Short Name | Video                                                                                                                                                            | Length (minutes) | Mentions Coding? |
|------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------|------------------|
| Spot       | [3 Ways to Spot a Bad Statistic](https://www.ted.com/talks/mona_chalabi_3_ways_to_spot_a_bad_statistic)                                                          | 12               | N                |
| Mine       | [Was A Minecraft Speedrunner Too Lucky To Pull Off His World Record?](https://digg.com/video/was-a-minecraft-speedrunner-too-lucky-to-pull-off-his-world-record) | 40               | N                |
| Best       | [The best stats you’ve ever seen](https://www.youtube.com/watch?v=hVimVzgtD6w)                                                                                   | 20               | N                |
| Going      | [Data Science: Where are We Going?](https://www.youtube.com/watch?v=3_1reLdh5xw)                                                                                 | 13               | N                |
| Pain       | [Statistics Without the Agonizing Pain](https://www.youtube.com/watch?v=5Dnw46eC-0o)                                                                             | 12               | Y                |
| GUI        | [You can’t do data science in a GUI](https://www.youtube.com/watch?v=cpbtcsGE0OA)                                                                                | 75               | Y                |
| Good       | [What does it take to apply data science for social good?](https://www.youtube.com/watch?v=vE-f_3mLw6Q)                                                          | 12               | N                |

# Video Data

Here are the ratings of the videos by the viewers.

``` r
vdf <- structure(list(viewer = c("a", "a", "a", "a", "a", "b", "b", 
"b", "b", "c", "c", "c", "c", "d", "d", "d", "d", "e", "e", "e", 
"e", "f", "f", "f", "f", "g", "g", "g", "g", "g"), video = c("Spot", 
"Best", "Pain", "Good", "Going", "Best", "Pain", "Spot", "Going", 
"Mine", "Spot", "Pain", "Best", "Spot", "Good", "Going", "Pain", 
"Going", "Good", "Pain", "Spot", "Spot", "Pain", "Going", "Best", 
"Best", "Pain", "Mine", "Spot", "Gui"), rank = c(1, 2, 3, 4, 
5, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 
1, 2, 3, 4, 5)), row.names = c(NA, -30L), class = c("tbl_df", 
"tbl", "data.frame"))
```

``` r
vdf %>%
  head(10) %>%
  kable()
```

| viewer | video | rank |
|:-------|:------|-----:|
| a      | Spot  |    1 |
| a      | Best  |    2 |
| a      | Pain  |    3 |
| a      | Good  |    4 |
| a      | Going |    5 |
| b      | Best  |    1 |
| b      | Pain  |    2 |
| b      | Spot  |    3 |
| b      | Going |    4 |
| c      | Mine  |    1 |

Next we show the aggregate: how many time the video *best* was rated
1st, 2nd, etc as well as the total number of ratings each video
received.

``` r
vdf %>%
  group_by(video, rank) %>%
  summarize(n = n()) %>%
  pivot_wider(
    names_from = rank,
    values_from = n
  ) %>%
  replace_na(list(`1` = 0, `2` = 0, `3` = 0, `4` = 0, `5` = 0)) %>%
  mutate(ttl = `1` + `2` + `3` + `4` + `5`) %>%
  select(`1`, `2`, `3`, `4`, `5`,ttl) %>%
  kable()
```

    ## `summarise()` has grouped output by 'video'. You can override using the
    ## `.groups` argument.
    ## Adding missing grouping variables: `video`

| video |   1 |   2 |   3 |   4 |   5 | ttl |
|:------|----:|----:|----:|----:|----:|----:|
| Best  |   2 |   1 |   0 |   2 |   0 |   5 |
| Going |   1 |   0 |   2 |   1 |   1 |   5 |
| Good  |   0 |   2 |   0 |   1 |   0 |   3 |
| Gui   |   0 |   0 |   0 |   0 |   1 |   1 |
| Mine  |   1 |   0 |   1 |   0 |   0 |   2 |
| Pain  |   0 |   3 |   3 |   1 |   0 |   7 |
| Spot  |   3 |   1 |   1 |   2 |   0 |   7 |

To use PlackettLuce, we need to transform the data a little:

``` r
R <- vdf %>%
  pivot_wider(
    names_from = video,
    values_from = rank
  ) %>%
  select(-viewer) %>%
  replace_na(list(Spot = 0, Going = 0, Good = 0, Mine = 0, Pain = 0, Spot = 0, GUI = 0)) %>%
  as.matrix()
```

``` r
R
```

    ##      Spot Best Pain Good Going Mine Gui
    ## [1,]    1    2    3    4     5    0  NA
    ## [2,]    3    1    2    0     4    0  NA
    ## [3,]    2    4    3    0     0    1  NA
    ## [4,]    1   NA    4    2     3    0  NA
    ## [5,]    4   NA    3    2     1    0  NA
    ## [6,]    1    4    2    0     3    0  NA
    ## [7,]    4    1    2    0     0    3   5

and then convert it to a *rankings* object:

``` r
R <- as.rankings(R)
```

``` r
R
```

    ## [1] "Spot > Best > Pain > Good > Going" "Best > Pain > Spot > Going"       
    ## [3] "Mine > Spot > Pain > Best"         "Spot > Good > Going > Pain"       
    ## [5] "Going > Good > Pain > Spot"        "Spot > Pain > Going > Best"       
    ## [7] "Best > Pain > Mine > Spot > Gui"

Just for fun we can plot (as a graph) which videos were rated better
than which others. In this case not much can bee seen.

``` r
net <- graph_from_adjacency_matrix(adjacency(R))
plot(net, edge.arrow.size = 0.5, vertex.size = 30)
```

![](PlackettLuce_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Now we can estimate the model

``` r
mod <- PlackettLuce(R)
summary(mod)
```

    ## Call: PlackettLuce(rankings = R)
    ## 
    ## Coefficients:
    ##       Estimate Std. Error z value Pr(>|z|)
    ## Spot   0.00000         NA      NA       NA
    ## Best  -0.31365    0.86332  -0.363    0.716
    ## Pain  -0.18718    0.67236  -0.278    0.781
    ## Good  -0.09439    0.90861  -0.104    0.917
    ## Going -0.77772    0.81854  -0.950    0.342
    ## Mine   0.49644    1.06279   0.467    0.640
    ## Gui   -1.73958    1.94972  -0.892    0.372
    ## 
    ## Residual deviance:  46.588 on 44 degrees of freedom
    ## AIC:  58.588 
    ## Number of iterations: 8

The coefficients are the logarithm of the ‘worth’ of each video,
relative to the base level 9in tis case ‘Spot’).

``` r
coef(mod) # log of worth
```

    ##        Spot        Best        Pain        Good       Going        Mine 
    ##  0.00000000 -0.31364937 -0.18717966 -0.09438936 -0.77772081  0.49644437 
    ##         Gui 
    ## -1.73958108

qvcalc estimates a proper variance here, allowing for the following plot
of the likely range of relative (log) worths:

``` r
qv <- qvcalc(mod)
qv$qvframe <- qv$qvframe[order(coef(mod), decreasing = TRUE), ]
plot(qv, xlab = "Video", ylab = "log(Worth)", main = NULL, xaxt = "n")
axis(1, at = seq_len(length(coef(mod))), labels = rownames(qv$qvframe), las = 2, cex.axis = 0.6)
```

![](PlackettLuce_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

itempar then can be used to ‘un-log’ the worths, and scale tehm by the
sum in order to get values that sum to one. In other words, itempar(mod)
== exp(coef(mod))/sum(exp(coef(mod)))

``` r
itempar(mod) %>% sort(decreasing = TRUE)
```

    ##       Mine       Spot       Good       Pain       Best      Going        Gui 
    ## 0.28582013 0.17397617 0.15830586 0.14427752 0.12713747 0.07993370 0.03054916

# Scaling to bigger problems

One question that immediately arises is how does the performance
(wall-clock) scale to increasing problem sizes? Here we have 1k videos
and 100k viewers, each rating a Poisson distributed number of videos
with mean five, but at least one.

``` r
n_videos <- 1000
n_viewers <- 100000
e_n_ratings <- 5
```

Just building the data takes (on my laptop) about five minutes.

``` r
tic()
df_ratings <- tibble()
n_ratings <- pmax(1, rpois(n_viewers, e_n_ratings))
for (i in 1:n_viewers) {
  ratings <- sample(n_videos, n_ratings[i])
  ans <- tibble(rater = i, videos = ratings, ratings = 1:n_ratings[i])
  df_ratings <- df_ratings %>% bind_rows(ans)
}
toc() # 275s
```

    ## 368.06 sec elapsed

Converting to a matrix for the routine is quick, only five seconds

``` r
tic()
R1 <- df_ratings %>%
  pivot_wider(
    names_from = videos,
    values_from = ratings
  ) %>%
  select(-rater) %>%
  mutate(
    across(everything(), ~ replace_na(.x, 0))
  ) %>%
  as.matrix()
toc() # 5s
```

    ## 2.927 sec elapsed

and then actually estimating the model takes a little over a minute and
uses about 8 GB of memory.

``` r
tic()
mod1 <- PlackettLuce(R1)
```

    ## Rankings with only 1 item set to `NA`

``` r
toc()
```

    ## 70.148 sec elapsed

``` r
# 70s
```

Another experiment, just with 1M viewers took about 13 minutes and used
about 18 GB.

# Video covariates

Now we look atthe video covariates to see how to use them

``` r
R3 <- vdf %>%
  pivot_wider(
    names_from = video,
    values_from = rank
  ) %>%
  select(-viewer) %>%
  replace_na(list(Spot = 0, Going = 0, Good = 0, Mine = 0, Pain = 0, Best = 0, GUI = 0)) %>%
  as.matrix()
```

``` r
R3
```

    ##      Spot Best Pain Good Going Mine Gui
    ## [1,]    1    2    3    4     5    0  NA
    ## [2,]    3    1    2    0     4    0  NA
    ## [3,]    2    4    3    0     0    1  NA
    ## [4,]    1    0    4    2     3    0  NA
    ## [5,]    4    0    3    2     1    0  NA
    ## [6,]    1    4    2    0     3    0  NA
    ## [7,]    4    1    2    0     0    3   5

``` r
features <- tibble(
  video = c("Spot", "Best", "Pain", "Good", "Going", "Mine","Gui"), 
  coding = c("N", "N", "Y", "N", "N", "N", "Y"), 
  len = c(12, 20, 12, 12, 13, 40, 75)
  )

features
```

    ## # A tibble: 7 × 3
    ##   video coding   len
    ##   <chr> <chr>  <dbl>
    ## 1 Spot  N         12
    ## 2 Best  N         20
    ## 3 Pain  Y         12
    ## 4 Good  N         12
    ## 5 Going N         13
    ## 6 Mine  N         40
    ## 7 Gui   Y         75

``` r
Rr <- R3 %>% as.rankings()
```

As described in the vignette, we do the following step to get *rho*

``` r
mod <- PlackettLuce(Rr)
summary(mod)
```

    ## Call: PlackettLuce(rankings = Rr)
    ## 
    ## Coefficients:
    ##       Estimate Std. Error z value Pr(>|z|)
    ## Spot   0.00000         NA      NA       NA
    ## Best  -0.31365    0.86332  -0.363    0.716
    ## Pain  -0.18718    0.67236  -0.278    0.781
    ## Good  -0.09439    0.90861  -0.104    0.917
    ## Going -0.77772    0.81854  -0.950    0.342
    ## Mine   0.49644    1.06279   0.467    0.640
    ## Gui   -1.73958    1.94972  -0.892    0.372
    ## 
    ## Residual deviance:  46.588 on 44 degrees of freedom
    ## AIC:  58.588 
    ## Number of iterations: 8

``` r
# note residual deviance = 46 -> rho = 46/20~2.3
```

``` r
mod <- pladmm(R3, ~ coding + len, data = features, rho = 2.3)
summary(mod)
```

    ## Call: pladmm(rankings = R3, formula = ~coding + len, data = features, rho = 2.3)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)   5.1438         NA      NA       NA  
    ## codingY      -2.6998     1.0925  -2.471   0.0135 *
    ## len          -0.5267     0.2475  -2.128   0.0333 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual deviance:  128.23 on 48 degrees of freedom
    ## AIC:  132.23 
    ## Number of iterations: 501

Of course we still have the ratings

``` r
itempar(mod) %>% sort(decreasing = TRUE)
```

    ##         Spot         Good        Going         Pain         Best         Mine 
    ## 3.741696e-01 3.741696e-01 2.209737e-01 2.515046e-02 5.536624e-03 1.474642e-07 
    ##          Gui 
    ## 9.787839e-17

But we can also see the effect of the video mentioning coding, and the
effect of video length, on the ratings, and we can predict the rating of
a new video:

``` r
mod %>% predict(newdata = tibble(len=10, coding='Y'))
```

    ##         1 
    ## -2.822642

# Judge covariates

``` r
################################################################################
# videos, rater covariates

judge_features <- vdf %>%
  mutate(
    jf=if_else(viewer %in% c('a','d','f'),1,2)
  ) %>% 
  group_by(viewer)%>%
  summarize( jf=first(jf))%>%
  select(jf)
judge_features
```

``` r
grouped_videos <- group(as.rankings(R3), 1:nrow(R3))
grouped_videos
mod3 <- pltree(grouped_videos ~ ., data = judge_features,rho = 2)
plot(mod3, ylines = 1)
mod3
```

``` r
################################################################################
# videos, item & rater covariates

grouped_videos <- group(as.rankings(R3), 1:nrow(R3))
grouped_videos
mod4 <- pltree(
  grouped_videos ~ ., 
  worth =~ gender + len ,
  data = list(judge_features, features),
  rho = 2)
plot(mod4, ylines = 1)
mod4
```

``` r
################################################################################
################################################################################
library(tidyverse)
library(PlackettLuce)
library(igraph)
library(prefmod)


salad %>% glimpse()
# Model 1 - plain----
mod1 <- PlackettLuce(salad)
summary(mod1)

# Model 2 - item covariates----
salad_features <- tibble(salad = LETTERS[1:4], acetic = c(0.5, 0.5, 1, 0), gluconic = c(0, 10, 0, 10))
salad_features 

mod2 <- pladmm(salad, ~ salad, data = salad_features, rho = 8)
summary(mod2)


# Model 3 - rater covariates----
set.seed(1)
judge_features <- tibble(varC = rpois(nrow(salad), lambda = salad$C^2))
judge_features
grouped_salad <- group(as.rankings(salad), 1:nrow(salad))
grouped_salad
mod3 <- pltree(grouped_salad ~ ., data = judge_features,rho = 2, minsize = 10)
plot(mod3, ylines = 2)
mod3
```

``` r
# Model 4 - rater covariates and item covariates----
set.seed(1)
judge_features <- tibble(varC = rpois(nrow(salad), lambda = salad$C^2))
grouped_salad <- group(as.rankings(salad), 1:nrow(salad))

mod4 <- pltree(
  grouped_salad ~ ., 
  worth = ~acetic + gluconic, 
  data = list(judge_features, salad_features),
  rho = 2, minsize = 10
)
plot(mod4, ylines = 2)
mod4
```

``` r
# Model 3A - rater covariates----
set.seed(1)
judge_features <- tibble(varC=c(rep(1,5),rep(2,11),rep(3,16)))
judge_features
grouped_salad <- group(as.rankings(salad), 1:nrow(salad))
grouped_salad
mod3 <- pltree(grouped_salad ~ ., data = judge_features,rho = 2, minsize = 10)
plot(mod3, ylines = 4)
mod3
```

# Appendices

<details>
<summary>
References
</summary>
</details>
<details>
<summary>
SessionInfo
</summary>

``` r
sessionInfo()
```

    ## R version 4.2.2 (2022-10-31)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur ... 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.41         tictoc_1.1         igraph_1.4.1       PlackettLuce_0.4.2
    ##  [5] lubridate_1.9.2    forcats_1.0.0      stringr_1.5.0      dplyr_1.1.0       
    ##  [9] purrr_1.0.1        readr_2.1.4        tidyr_1.3.0        tibble_3.2.0      
    ## [13] ggplot2_3.4.1      tidyverse_2.0.0   
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.10        mvtnorm_1.1-3      lattice_0.20-45    zoo_1.8-11        
    ##  [5] digest_0.6.31      utf8_1.2.3         gmp_0.6-9          RSpectra_0.16-1   
    ##  [9] R6_2.5.1           evaluate_0.19      highr_0.10         pillar_1.8.1      
    ## [13] qvcalc_1.0.2       rlang_1.1.0        rstudioapi_0.14    rpart_4.1.19      
    ## [17] Matrix_1.5-3       partykit_1.2-16    rmarkdown_2.19     splines_4.2.2     
    ## [21] bit_4.0.5          munsell_0.5.0      compiler_4.2.2     xfun_0.36         
    ## [25] pkgconfig_2.0.3    CVXR_1.0-11        libcoin_1.0-9      htmltools_0.5.4   
    ## [29] tidyselect_1.2.0   matrixStats_0.63.0 fansi_1.0.4        psychotree_0.16-0 
    ## [33] tzdb_0.3.0         withr_2.5.0        grid_4.2.2         gtable_0.3.2      
    ## [37] lifecycle_1.0.3    magrittr_2.0.3     scales_1.2.1       cli_3.6.0         
    ## [41] stringi_1.7.12     Rmpfr_0.8-9        ellipsis_0.3.2     generics_0.1.3    
    ## [45] vctrs_0.5.2        sandwich_3.0-2     Formula_1.2-4      tools_4.2.2       
    ## [49] bit64_4.0.5        glue_1.6.2         hms_1.1.2          fastmap_1.1.0     
    ## [53] survival_3.5-3     yaml_2.3.6         timechange_0.1.1   colorspace_2.1-0  
    ## [57] inum_1.0-4         psychotools_0.7-2

</details>
