
## Exercise: logistic regression?

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

NH11 <- readRDS("dataSets/NatHealth2011.rds")

labs <- attributes(NH11)$labels

xtabs(~ everwrk + r_maritl, data = NH11,exclude = c(NA), drop.unused.levels = TRUE)

NH11$r_maritl <- factor(NH11$r_maritl)

NH11$everwrk <- factor(NH11$everwrk, levels = c("2 No", "1 Yes"))

work.output <- glm(everwrk~age_p+r_maritl, data = (NH11), na.action = na.omit,  family="binomial")

summary(work.output)

##   2. Predict the probability of working for each level of marital
##      status.

work.output2 <- glm(everwrk~r_maritl, data = (NH11), na.action = na.omit,  family="binomial")

summary(work.output2)

newdata2 <- with(NH11, expand.grid(r_maritl = factor(NH11$r_maritl)))

head(newdata2)

newdata2$r_maritlP <- predict(work.output2, newdata = newdata2, type = "response")

head(newdata2, 100)

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

r_maritl
everwrk        1 Married - spouse in household
  1 Yes                                   4834
  2 No                                     624
  7 Refused                                  5
  9 Don't know                               1
              r_maritl
everwrk        2 Married - spouse not in household 4 Widowed
  1 Yes                                        186      2214
  2 No                                          26       304
  7 Refused                                      1         2
  9 Don't know                                   0         5
              r_maritl
everwrk        5 Divorced 6 Separated 7 Never married
  1 Yes              1806         412            2141
  2 No                101          55             702
  7 Refused             1           1               6
  9 Don't know          0           0               2
              r_maritl
everwrk        8 Living with partner 9 Unknown marital status
  1 Yes                          531                       29
  2 No                            70                        5
  7 Refused                        0                        1
  9 Don't know                     0                        0
> 
> NH11$r_maritl <- factor(NH11$r_maritl)
> 
> NH11$everwrk <- factor(NH11$everwrk, levels = c("2 No", "1 Yes"))
> 
> work.output <- glm(everwrk~age_p+r_maritl, data = (NH11), na.action = na.omit,  family="binomial")
> 
> summary(work.output)

Call:
glm(formula = everwrk ~ age_p + r_maritl, family = "binomial", 
    data = (NH11), na.action = na.omit)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.7308   0.3370   0.4391   0.5650   1.0436  

Coefficients:
                                             Estimate
(Intercept)                                  0.440248
age_p                                        0.029812
r_maritl2 Married - spouse not in household -0.049675
r_maritl4 Widowed                           -0.683618
r_maritl5 Divorced                           0.730115
r_maritl6 Separated                          0.128091
r_maritl7 Never married                     -0.343611
r_maritl8 Living with partner                0.443583
r_maritl9 Unknown marital status            -0.395480
                                            Std. Error z value
(Intercept)                                   0.093538   4.707
age_p                                         0.001645  18.118
r_maritl2 Married - spouse not in household   0.217310  -0.229
r_maritl4 Widowed                             0.084335  -8.106
r_maritl5 Divorced                            0.111681   6.538
r_maritl6 Separated                           0.151366   0.846
r_maritl7 Never married                       0.069222  -4.964
r_maritl8 Living with partner                 0.137770   3.220
r_maritl9 Unknown marital status              0.492967  -0.802
                                            Pr(>|z|)    
(Intercept)                                 2.52e-06 ***
age_p                                        < 2e-16 ***
r_maritl2 Married - spouse not in household  0.81919    
r_maritl4 Widowed                           5.23e-16 ***
r_maritl5 Divorced                          6.25e-11 ***
r_maritl6 Separated                          0.39742    
r_maritl7 Never married                     6.91e-07 ***
r_maritl8 Living with partner                0.00128 ** 
r_maritl9 Unknown marital status             0.42241    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 11082  on 14039  degrees of freedom
Residual deviance: 10309  on 14031  degrees of freedom
  (18974 observations deleted due to missingness)
AIC: 10327

Number of Fisher Scoring iterations: 5


work.output2 <- glm(everwrk~r_maritl, data = (NH11), na.action = na.omit,  family="binomial")
> 
> summary(work.output2)

Call:
glm(formula = everwrk ~ r_maritl, family = "binomial", data = (NH11), 
    na.action = na.omit)

Deviance Residuals: 
    Min       1Q   Median       3Q  
-2.4241   0.3299   0.4928   0.5073  
    Max  
 0.7531  

Coefficients:
                                            Estimate
(Intercept)                                  2.04728
r_maritl2 Married - spouse not in household -0.07963
r_maritl4 Widowed                           -0.06175
r_maritl5 Divorced                           0.83647
r_maritl6 Separated                         -0.03359
r_maritl7 Never married                     -0.93218
r_maritl8 Living with partner               -0.02101
r_maritl9 Unknown marital status            -0.28942
                                            Std. Error
(Intercept)                                    0.04254
r_maritl2 Married - spouse not in household    0.21365
r_maritl4 Widowed                              0.07450
r_maritl5 Divorced                             0.11074
r_maritl6 Separated                            0.14973
r_maritl7 Never married                        0.06084
r_maritl8 Living with partner                  0.13408
r_maritl9 Unknown marital status               0.48610
                                            z value
(Intercept)                                  48.129
r_maritl2 Married - spouse not in household  -0.373
r_maritl4 Widowed                            -0.829
r_maritl5 Divorced                            7.553
r_maritl6 Separated                          -0.224
r_maritl7 Never married                     -15.323
r_maritl8 Living with partner                -0.157
r_maritl9 Unknown marital status             -0.595
                                            Pr(>|z|)
(Intercept)                                  < 2e-16
r_maritl2 Married - spouse not in household    0.709
r_maritl4 Widowed                              0.407
r_maritl5 Divorced                          4.24e-14
r_maritl6 Separated                            0.822
r_maritl7 Never married                      < 2e-16
r_maritl8 Living with partner                  0.875
r_maritl9 Unknown marital status               0.552
                                               
(Intercept)                                 ***
r_maritl2 Married - spouse not in household    
r_maritl4 Widowed                              
r_maritl5 Divorced                          ***
r_maritl6 Separated                            
r_maritl7 Never married                     ***
r_maritl8 Living with partner                  
r_maritl9 Unknown marital status               
---
Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’
  0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 11082  on 14039  degrees of freedom
Residual deviance: 10661  on 14032  degrees of freedom
  (18974 observations deleted due to missingness)
AIC: 10677

Number of Fisher Scoring iterations: 5

> 
> newdata2 <- with(NH11, expand.grid(r_maritl = factor(NH11$r_maritl)))
> 
> head(newdata2)
                         r_maritl
1                      5 Divorced
2                 7 Never married
3                       4 Widowed
4                     6 Separated
5 1 Married - spouse in household
6 1 Married - spouse in household
> 
> newdata2$r_maritlP <- predict(work.output2, newdata = newdata2, type = "response")
                          
> head(newdata2, 100)
                               r_maritl
1                            5 Divorced
2                       7 Never married
3                             4 Widowed
4                           6 Separated
5       1 Married - spouse in household
6       1 Married - spouse in household
7                       7 Never married
8                       7 Never married
9                       7 Never married
10      1 Married - spouse in household
11                      7 Never married
12                           5 Divorced
13      1 Married - spouse in household
14                           5 Divorced
15      1 Married - spouse in household
16                          6 Separated
17      1 Married - spouse in household
18      1 Married - spouse in household
19                      7 Never married
20                           5 Divorced
21                      7 Never married
22                           5 Divorced
23                           5 Divorced
24      1 Married - spouse in household
25      1 Married - spouse in household
26      1 Married - spouse in household
27                      7 Never married
28      1 Married - spouse in household
29      1 Married - spouse in household
30                      7 Never married
31                            4 Widowed
32      1 Married - spouse in household
33                            4 Widowed
34                            4 Widowed
35      1 Married - spouse in household
36                           5 Divorced
37                            4 Widowed
38      1 Married - spouse in household
39                      7 Never married
40      1 Married - spouse in household
41      1 Married - spouse in household
42      1 Married - spouse in household
43                           5 Divorced
44      1 Married - spouse in household
45                            4 Widowed
46      1 Married - spouse in household
47      1 Married - spouse in household
48                8 Living with partner
49      1 Married - spouse in household
50      1 Married - spouse in household
51                            4 Widowed
52                8 Living with partner
53                      7 Never married
54                      7 Never married
55      1 Married - spouse in household
56                            4 Widowed
57      1 Married - spouse in household
58      1 Married - spouse in household
59      1 Married - spouse in household
60                            4 Widowed
61      1 Married - spouse in household
62                8 Living with partner
63                            4 Widowed
64                            4 Widowed
65                      7 Never married
66      1 Married - spouse in household
67                           5 Divorced
68                            4 Widowed
69                            4 Widowed
70      1 Married - spouse in household
71      1 Married - spouse in household
72                      7 Never married
73                8 Living with partner
74                      7 Never married
75      1 Married - spouse in household
76                      7 Never married
77                      7 Never married
78      1 Married - spouse in household
79      1 Married - spouse in household
80      1 Married - spouse in household
81                      7 Never married
82                           5 Divorced
83      1 Married - spouse in household
84      1 Married - spouse in household
85      1 Married - spouse in household
86                            4 Widowed
87      1 Married - spouse in household
88                            4 Widowed
89                           5 Divorced
90      1 Married - spouse in household
91                      7 Never married
92                           5 Divorced
93      1 Married - spouse in household
94                      7 Never married
95                      7 Never married
96                          6 Separated
97      1 Married - spouse in household
98                8 Living with partner
99  2 Married - spouse not in household
100               8 Living with partner
    r_maritlP
1   0.9470372
2   0.7530777
3   0.8792693
4   0.8822270
5   0.8856724
6   0.8856724
7   0.7530777
8   0.7530777
9   0.7530777
10  0.8856724
11  0.7530777
12  0.9470372
13  0.8856724
14  0.9470372
15  0.8856724
16  0.8822270
17  0.8856724
18  0.8856724
19  0.7530777
20  0.9470372
21  0.7530777
22  0.9470372
23  0.9470372
24  0.8856724
25  0.8856724
26  0.8856724
27  0.7530777
28  0.8856724
29  0.8856724
30  0.7530777
31  0.8792693
32  0.8856724
33  0.8792693
34  0.8792693
35  0.8856724
36  0.9470372
37  0.8792693
38  0.8856724
39  0.7530777
40  0.8856724
41  0.8856724
42  0.8856724
43  0.9470372
44  0.8856724
45  0.8792693
46  0.8856724
47  0.8856724
48  0.8835275
49  0.8856724
50  0.8856724
51  0.8792693
52  0.8835275
53  0.7530777
54  0.7530777
55  0.8856724
56  0.8792693
57  0.8856724
58  0.8856724
59  0.8856724
60  0.8792693
61  0.8856724
62  0.8835275
63  0.8792693
64  0.8792693
65  0.7530777
66  0.8856724
67  0.9470372
68  0.8792693
69  0.8792693
70  0.8856724
71  0.8856724
72  0.7530777
73  0.8835275
74  0.7530777
75  0.8856724
76  0.7530777
77  0.7530777
78  0.8856724
79  0.8856724
80  0.8856724
81  0.7530777
82  0.9470372
83  0.8856724
84  0.8856724
85  0.8856724
86  0.8792693
87  0.8856724
88  0.8792693
89  0.9470372
90  0.8856724
91  0.7530777
92  0.9470372
93  0.8856724
94  0.7530777
95  0.7530777
96  0.8822270
97  0.8856724
98  0.8835275
99  0.8773585
100 0.8835275

> 


