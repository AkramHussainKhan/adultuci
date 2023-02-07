Adultuci Data Analysis
================
Akram Hussain Khan
2022-11-09

## Data Descriptions

``` r
data('AdultUCI')
dim(AdultUCI)
```

    ## [1] 48842    15

It has 15 variables and 48882 observations.

Using `str()`, we will get a basic idea about the data set.

``` r
str(AdultUCI)
```

    ## 'data.frame':    48842 obs. of  15 variables:
    ##  $ age           : int  39 50 38 53 28 37 49 52 31 42 ...
    ##  $ workclass     : Factor w/ 8 levels "Federal-gov",..: 7 6 4 4 4 4 4 6 4 4 ...
    ##  $ fnlwgt        : int  77516 83311 215646 234721 338409 284582 160187 209642 45781 159449 ...
    ##  $ education     : Ord.factor w/ 16 levels "Preschool"<"1st-4th"<..: 14 14 9 7 14 15 5 9 15 14 ...
    ##  $ education-num : int  13 13 9 7 13 14 5 9 14 13 ...
    ##  $ marital-status: Factor w/ 7 levels "Divorced","Married-AF-spouse",..: 5 3 1 3 3 3 4 3 5 3 ...
    ##  $ occupation    : Factor w/ 14 levels "Adm-clerical",..: 1 4 6 6 10 4 8 4 10 4 ...
    ##  $ relationship  : Factor w/ 6 levels "Husband","Not-in-family",..: 2 1 2 1 6 6 2 1 2 1 ...
    ##  $ race          : Factor w/ 5 levels "Amer-Indian-Eskimo",..: 5 5 5 3 3 5 3 5 5 5 ...
    ##  $ sex           : Factor w/ 2 levels "Female","Male": 2 2 2 2 1 1 1 2 1 2 ...
    ##  $ capital-gain  : int  2174 0 0 0 0 0 0 0 14084 5178 ...
    ##  $ capital-loss  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ hours-per-week: int  40 13 40 40 40 40 16 45 50 40 ...
    ##  $ native-country: Factor w/ 41 levels "Cambodia","Canada",..: 39 39 39 39 5 39 23 39 39 39 ...
    ##  $ income        : Ord.factor w/ 2 levels "small"<"large": 1 1 1 1 1 1 1 2 2 2 ...

All of the variable types in this data collection are visible here. This
data collection has 48842 observations of each of the 15 variables,
which are subdivided into 9 factors and 6 integer variables. Education
and income are two ordered factor variables, as is evident.

Now, we will see the first 6 rows of this data set for better
understanding about data.

``` r
head(AdultUCI)
```

    ##   age        workclass fnlwgt education education-num     marital-status
    ## 1  39        State-gov  77516 Bachelors            13      Never-married
    ## 2  50 Self-emp-not-inc  83311 Bachelors            13 Married-civ-spouse
    ## 3  38          Private 215646   HS-grad             9           Divorced
    ## 4  53          Private 234721      11th             7 Married-civ-spouse
    ## 5  28          Private 338409 Bachelors            13 Married-civ-spouse
    ## 6  37          Private 284582   Masters            14 Married-civ-spouse
    ##          occupation  relationship  race    sex capital-gain capital-loss
    ## 1      Adm-clerical Not-in-family White   Male         2174            0
    ## 2   Exec-managerial       Husband White   Male            0            0
    ## 3 Handlers-cleaners Not-in-family White   Male            0            0
    ## 4 Handlers-cleaners       Husband Black   Male            0            0
    ## 5    Prof-specialty          Wife Black Female            0            0
    ## 6   Exec-managerial          Wife White Female            0            0
    ##   hours-per-week native-country income
    ## 1             40  United-States  small
    ## 2             13  United-States  small
    ## 3             40  United-States  small
    ## 4             40  United-States  small
    ## 5             40           Cuba  small
    ## 6             40  United-States  small

# Data Preprocessing

## Missing Value Handling

Analytical issues arise from missing values. We must therefore take care
of any missing values. Here, we first depict the values that are
missing.

``` r
AdultUCI %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Here, we can see that there are few missing values in our data set. Now
we will remove the missing values for further analysis.

``` r
AdultUCI<-na.omit(AdultUCI)
```

Now, again we are plotting missing values. Now we can see that there are
no missing values anymore.

``` r
AdultUCI %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
dim(AdultUCI)
```

    ## [1] 30162    15

Now we have 30162 observations and 15 variables.

## Grouping Categories

As lowering categories have a greater impact on our model, we will
attempt to record them into the same variable for analysis purposes.

Here, we attempted to condense the categories for education.

``` r
AdultUCI <- AdultUCI %>% 
  mutate(education=recode(education,
                          "Preschool"="HS-grad or less",
                            "1st-4th"="HS-grad or less",
                            "5th-6th"="HS-grad or less",
                            "7th-8th"="HS-grad or less",
                            "9th"="HS-grad or less",
                             "10th"="HS-grad or less",
                          "11th"="HS-grad or less",
                          "12th"="HS-grad or less",
                          "HS-grad"="HS-grad or less"))
```

Here, we keeping 11 most frequent countries and keep others as rest.

``` r
AdultUCI$`native-country`<-as.character(AdultUCI$`native-country`)
AdultUCI$`native-country`<- with(AdultUCI, ifelse(`native-country` %in% c("United-States","Mexico", "Philippines","Germany","Puerto-Rico","Canada", "El-Salvador","India","Cuba","England","China"),`native-country`, "Rest"))
AdultUCI$`native-country`<-as.factor(AdultUCI$`native-country`)
```

# Summary of the Dataset

## Summary Measures for Numeric Variables

Now, we can find the summary measures for all the numerical columns.

``` r
numeric_cols_df <- select_if(AdultUCI, is.numeric) %>%
  tidyr::gather(key = "Variable", value = "value") %>%
  group_by(Variable)%>% 
  summarise(Minimum = min(value, na.rm = T),
            Maximum = max(value, na.rm = T),
            Mean= round(mean(value, na.rm=T), 2),
            Median = round(median(value,na.rm = T),2),
            Sd = round(sd(value, na.rm = TRUE),2))


flextable(numeric_cols_df)
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

We present here,minimum, maximum, mean, median and standard deviation.

## Plots of Categorical Variable

For summary measures of categorical variables we can choose each factor
variable and depict its categories frequency.

``` r
AdultUCI%>%group_by(income)%>%
                         summarise(count = n()) %>% 
  mutate(share=count/sum(count)*100.0) %>%
  arrange(count)%>%
  ggplot(aes("", y = share, fill = income)) +
  ggtitle("Income Percentage")+
  geom_col() +
   geom_text(aes(label = paste0(income," (",round(share), "%)"),col="red"), 
              position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Here, 75 % observations are small income and 25 % are large income.

``` r
AdultUCI%>%group_by(workclass)%>%
                         summarise(total = n()) %>%
  arrange(total)%>% filter(workclass!="NA")%>% 
  mutate(name=factor(workclass, levels= workclass)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle(" Workclasses by Total observations")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="blue") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,40000))+
    coord_flip() +
    xlab("Workclass") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

We can see that most observations are from private category in
workclass.

``` r
AdultUCI%>%group_by(education)%>%
                         summarise(total = n()) %>%
  arrange(total)%>% filter(education!="NA")%>% 
  mutate(name=factor(education, levels= education)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle("Total observations by Education")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="green") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,15000))+
    coord_flip() +
    xlab("Education") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

In education maximum observations are under HS-grad or less category.

``` r
AdultUCI%>%group_by(sex)%>%
                         summarise(count = n()) %>% 
  mutate(share=count/sum(count)*100.0) %>%
  arrange(count)%>%
  ggplot(aes("", y = share, fill = sex)) +
  ggtitle("Gender Percentage")+
  geom_col() +
   geom_text(aes(label = paste0(round(share), "%")), 
              position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Here, 68 % observations are male and 32 % are female.

``` r
AdultUCI%>%group_by(race)%>%
                         summarise(total = n()) %>%
  arrange(total)%>% filter(race!="NA")%>% 
  mutate(name=factor(race, levels= race)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle("Total observations by Race")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="orange") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,35000))+
    coord_flip() +
    xlab("Race") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

White races observations are highest.

``` r
AdultUCI%>%group_by(`marital-status`)%>%
  summarise(total = n()) %>%
  arrange(total)%>%
  mutate(name=factor(`marital-status`, levels= `marital-status`)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle("Total observations by marital status")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="black") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,15000))+
    coord_flip() +
    xlab("Marital status") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Married-civ-spouse and never married persons are highest in our dataset.

``` r
AdultUCI%>%group_by(occupation)%>%
                         summarise(total = n()) %>%
  arrange(total)%>% 
  mutate(name=factor(occupation, levels= occupation)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle("Total observations by Occupation")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="skyblue") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,5000))+
    coord_flip() +
    xlab("Occupation") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Maximum peoples are from Prof-specialty, craft-repair occupations.

``` r
AdultUCI%>%group_by(relationship)%>%
                         summarise(total = n()) %>%
  arrange(total)%>% 
  mutate(name=factor(relationship, levels= relationship)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle("Total observations by relationship status")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="yellow") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,15000))+
    coord_flip() +
    xlab("RElationship status") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Husbands are the highest number of respondents in our datascience.

``` r
AdultUCI%>%group_by(`native-country`)%>%
                         summarise(total = n()) %>%
  arrange(total)%>% 
  mutate(name=factor(`native-country`, levels=`native-country`)) %>%  
  ggplot( aes(x=name, y=total)) +
  ggtitle("Total observations by native-country")+
    geom_segment( aes(xend=name, yend=0)) +
    geom_point( size=4, color="red") +
  geom_text(aes(label=total),hjust = -0.3)+ 
  ylim(c(0,30000))+
    coord_flip() +
    xlab("Native country") +
    ylab("Total Number of observation")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

A huge number of observatons are from United states.

## Multicollinearity Checking

``` r
cx<-c('age','fnlwgt','education-num','capital-gain','capital-loss','hours-per-week')
corr_data<- AdultUCI[,cx]
round(cor(corr_data), 2)
```

    ##                  age fnlwgt education-num capital-gain capital-loss
    ## age             1.00  -0.08          0.04         0.08         0.06
    ## fnlwgt         -0.08   1.00         -0.04         0.00        -0.01
    ## education-num   0.04  -0.04          1.00         0.12         0.08
    ## capital-gain    0.08   0.00          0.12         1.00        -0.03
    ## capital-loss    0.06  -0.01          0.08        -0.03         1.00
    ## hours-per-week  0.10  -0.02          0.15         0.08         0.05
    ##                hours-per-week
    ## age                      0.10
    ## fnlwgt                  -0.02
    ## education-num            0.15
    ## capital-gain             0.08
    ## capital-loss             0.05
    ## hours-per-week           1.00

One way to test for multicollinearity is by creating a correlation
matrix.

A correlation matrix (or correlogram) visualizes the correlation between
multiple continuous variables. Correlations range always between -1 and
+1, where -1 represents perfect negative correlation and +1 perfect
positive correlation.

Correlations close to-1 or +1 might indicate the existence of
multicollinearity. As a rule of thumb, one might suspect
multicollinearity when the correlation between two (predictor) variables
is below -0.75 or above +0.75. Since, all the values are lies between
-0.75 to 0.75, we can say that there are no multicollinearity problem.

``` r
library("corrplot")
```

    ## Warning: package 'corrplot' was built under R version 4.2.1

    ## corrplot 0.92 loaded

``` r
corrplot(cor(corr_data), method = "number")
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

We show here the correlation matrix graphically.

## Data Partition

``` r
set.seed(555)
ind <- sample(2, nrow(AdultUCI),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- AdultUCI[ind==1,]
testing <- AdultUCI[ind==2,]
```

We split the data set into train and test with 60% and 40% for model
training and testing.

# Models

We will use training and testing data to estimate what factors
contribute to little or high incomes for analytical purposes. To
accurately predict the category of income, three well-known ML
algorithms—Logistic Regression (LR), Decision Tree (DT), and Naive Bayes
Classier—have been taken into consideration. Additionally, accuracy was
used to conduct a thorough evaluation of the algorithms.

## Logistic Regression Model

An algorithm for categorization is the logistic regression model. A set
of independent factors are utilized to predict a binary result using
this method\[1\].When dealing with binary data, the proper form of
analysis to use is logistic regression. When the dependent variable or
output is categorical or dichotomous, that is, if it falls into one of
two categories (such as “yes” or “no,” “pass” or “fail,” and so on), we
are working with binary data. The independent variables, however, can
belong to any of the following groups: 1. Continuous 2. Discrete and
ordinal 3. Discrete, nominal

## Decision Tree (DT)

A classification or regression tree analysis can be performed using the
machine learning technique known as Decision Tree. The decision tree can
be seen graphically as a tree structure with leaves and branches\[2\].
The branches are the criteria to make decisions for the class of data
set, whereas the leaves are often the data points.

## Naive Bayes Classifier

A supervised non-linear classification algorithm is Naive Bayes. A
family of straightforward probabilistic classifiers known as Naive Bayes
classifiers is built on using Baye’s theorem with strong (Naive)
independence assumptions across the features or variables\[3\]. Because
it assumes that the occurrence of one feature is unrelated to the
occurrence of other features, the Naive Bayes method is known as
“Naive.”

## Accuracy

The most popular machine learning model validation technique for
categorization issues is probably accuracy. Its success may be
attributed in part to its relative simplicity. \[4\] It is simple to
comprehend and put into practice. For straightforward circumstances,
accuracy is a useful statistic to evaluate the performance of a model.
In classification issues, accuracy is a metric that expresses the
proportion of accurate predictions.

# Results

## Models

### Logistic Regression Model

Now, we will try to fit a logistic regression model to our training data

``` r
options(scipen = 999)
model_lr <- glm(income~., family="binomial", data=training)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(model_lr)
```

    ## 
    ## Call:
    ## glm(formula = income ~ ., family = "binomial", data = training)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -5.0899  -0.5199  -0.1915  -0.0059   3.8519  
    ## 
    ## Coefficients:
    ##                                             Estimate     Std. Error z value
    ## (Intercept)                            -8.4318385340   0.7529997675 -11.198
    ## age                                     0.0211269060   0.0021733676   9.721
    ## workclassLocal-gov                     -0.5991517922   0.1459122413  -4.106
    ## workclassPrivate                       -0.4569749767   0.1213523957  -3.766
    ## workclassSelf-emp-inc                  -0.2411936825   0.1581477334  -1.525
    ## workclassSelf-emp-not-inc              -0.9116620635   0.1430403206  -6.373
    ## workclassState-gov                     -0.6540037490   0.1594486987  -4.102
    ## workclassWithout-pay                  -11.9700973005 150.3697173507  -0.080
    ## fnlwgt                                  0.0000005603   0.0000002308   2.427
    ## education.L                             0.2115149805   0.1820417520   1.162
    ## education.Q                             0.2749343672   0.1365271438   2.014
    ## education.C                            -0.0125416792   0.1597905578  -0.078
    ## education^4                            -0.2753528533   0.1701950357  -1.618
    ## education^5                             0.3750942481   0.1266878321   2.961
    ## education^6                            -0.2752992170   0.0989938565  -2.781
    ## education^7                             0.0882837902   0.0928370646   0.951
    ## `education-num`                         0.2577794681   0.0313504069   8.223
    ## `marital-status`Married-AF-spouse       2.6436516852   0.7048528950   3.751
    ## `marital-status`Married-civ-spouse      1.8114500296   0.3625205065   4.997
    ## `marital-status`Married-spouse-absent  -0.1450252697   0.3288104872  -0.441
    ## `marital-status`Never-married          -0.5126359703   0.1154658099  -4.440
    ## `marital-status`Separated               0.0440741351   0.2088361971   0.211
    ## `marital-status`Widowed                 0.3902472377   0.1987389049   1.964
    ## occupationArmed-Forces                 -0.6164436660   1.8889995375  -0.326
    ## occupationCraft-repair                  0.1117654175   0.1039738758   1.075
    ## occupationExec-managerial               0.8067631936   0.0998422442   8.080
    ## occupationFarming-fishing              -0.9639949635   0.1801213056  -5.352
    ## occupationHandlers-cleaners            -0.6754605215   0.1864485554  -3.623
    ## occupationMachine-op-inspct            -0.1712628607   0.1307604589  -1.310
    ## occupationOther-service                -0.6794449799   0.1489711933  -4.561
    ## occupationPriv-house-serv              -3.5991567650   2.2350582578  -1.610
    ## occupationProf-specialty                0.5073215131   0.1066541030   4.757
    ## occupationProtective-serv               0.7196631738   0.1612629950   4.463
    ## occupationSales                         0.2282883107   0.1066882580   2.140
    ## occupationTech-support                  0.6425405314   0.1445511253   4.445
    ## occupationTransport-moving             -0.1111587595   0.1305528775  -0.851
    ## relationshipNot-in-family               0.0455337110   0.3585397101   0.127
    ## relationshipOther-relative             -0.5791163222   0.3385662667  -1.710
    ## relationshipOwn-child                  -1.0101949288   0.3604001559  -2.803
    ## relationshipUnmarried                  -0.0768765191   0.3770475690  -0.204
    ## relationshipWife                        1.1620371602   0.1356423809   8.567
    ## raceAsian-Pac-Islander                  1.2113431887   0.3567870972   3.395
    ## raceBlack                               0.7766150771   0.3199870271   2.427
    ## raceOther                               0.0047713215   0.5394811612   0.009
    ## raceWhite                               0.8363904274   0.3067074797   2.727
    ## sexMale                                 0.7593579181   0.1042199863   7.286
    ## `capital-gain`                          0.0003195414   0.0000135584  23.568
    ## `capital-loss`                          0.0006906166   0.0000501048  13.783
    ## `hours-per-week`                        0.0306490849   0.0021756303  14.087
    ## `native-country`China                  -1.3827916789   0.6445747596  -2.145
    ## `native-country`Cuba                   -0.1505419899   0.5504672059  -0.273
    ## `native-country`El-Salvador            -0.8268904312   0.7339890467  -1.127
    ## `native-country`England                -0.0996849011   0.5356530225  -0.186
    ## `native-country`Germany                 0.1780923759   0.4780307795   0.373
    ## `native-country`India                  -1.0863892135   0.5243301870  -2.072
    ## `native-country`Mexico                 -0.6509856466   0.4522813571  -1.439
    ## `native-country`Philippines             0.0831002487   0.4901269914   0.170
    ## `native-country`Puerto-Rico            -0.2098483725   0.6378773290  -0.329
    ## `native-country`Rest                   -0.6594151826   0.3862475994  -1.707
    ## `native-country`United-States          -0.1215133649   0.3558330053  -0.341
    ##                                                   Pr(>|z|)    
    ## (Intercept)                           < 0.0000000000000002 ***
    ## age                                   < 0.0000000000000002 ***
    ## workclassLocal-gov                    0.000040213840758460 ***
    ## workclassPrivate                                  0.000166 ***
    ## workclassSelf-emp-inc                             0.127230    
    ## workclassSelf-emp-not-inc             0.000000000184807951 ***
    ## workclassState-gov                    0.000041020339380075 ***
    ## workclassWithout-pay                              0.936552    
    ## fnlwgt                                            0.015209 *  
    ## education.L                                       0.245275    
    ## education.Q                                       0.044034 *  
    ## education.C                                       0.937440    
    ## education^4                                       0.105691    
    ## education^5                                       0.003069 ** 
    ## education^6                                       0.005420 ** 
    ## education^7                                       0.341628    
    ## `education-num`                       < 0.0000000000000002 ***
    ## `marital-status`Married-AF-spouse                 0.000176 ***
    ## `marital-status`Married-civ-spouse    0.000000582831632307 ***
    ## `marital-status`Married-spouse-absent             0.659169    
    ## `marital-status`Never-married         0.000009007561379743 ***
    ## `marital-status`Separated                         0.832851    
    ## `marital-status`Widowed                           0.049574 *  
    ## occupationArmed-Forces                            0.744172    
    ## occupationCraft-repair                            0.282403    
    ## occupationExec-managerial             0.000000000000000646 ***
    ## occupationFarming-fishing             0.000000087025485531 ***
    ## occupationHandlers-cleaners                       0.000291 ***
    ## occupationMachine-op-inspct                       0.190282    
    ## occupationOther-service               0.000005093114768713 ***
    ## occupationPriv-house-serv                         0.107328    
    ## occupationProf-specialty              0.000001967838096888 ***
    ## occupationProtective-serv             0.000008094549827084 ***
    ## occupationSales                                   0.032373 *  
    ## occupationTech-support                0.000008786138512857 ***
    ## occupationTransport-moving                        0.394522    
    ## relationshipNot-in-family                         0.898942    
    ## relationshipOther-relative                        0.087174 .  
    ## relationshipOwn-child                             0.005063 ** 
    ## relationshipUnmarried                             0.838439    
    ## relationshipWife                      < 0.0000000000000002 ***
    ## raceAsian-Pac-Islander                            0.000686 ***
    ## raceBlack                                         0.015223 *  
    ## raceOther                                         0.992943    
    ## raceWhite                                         0.006391 ** 
    ## sexMale                               0.000000000000319041 ***
    ## `capital-gain`                        < 0.0000000000000002 ***
    ## `capital-loss`                        < 0.0000000000000002 ***
    ## `hours-per-week`                      < 0.0000000000000002 ***
    ## `native-country`China                             0.031931 *  
    ## `native-country`Cuba                              0.784484    
    ## `native-country`El-Salvador                       0.259924    
    ## `native-country`England                           0.852367    
    ## `native-country`Germany                           0.709480    
    ## `native-country`India                             0.038270 *  
    ## `native-country`Mexico                            0.150055    
    ## `native-country`Philippines                       0.865365    
    ## `native-country`Puerto-Rico                       0.742171    
    ## `native-country`Rest                              0.087778 .  
    ## `native-country`United-States                     0.732735    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 20439  on 18193  degrees of freedom
    ## Residual deviance: 11790  on 18134  degrees of freedom
    ## AIC: 11910
    ## 
    ## Number of Fisher Scoring iterations: 12

The model’s significant variables are those variables which have a very
low P-Value (less than .05).These variables have a significant effect on
our dependent variable.

The difference between Null deviance and Residual Deviance tells us that
the model is a good fit. The greater the difference, the better the
model. Null deviance is the value when there is only an intercept in our
equation with no variables, and residual deviance is the value when all
variables are considered. It makes sense to consider the model good if
that difference is big enough.

Now we are calculating odds ratio and express them together with
estimate and z value.

``` r
sk<-summary(model_lr)
dh<-sk$coefficients
odds_ratio <- exp(dh[ ,"Estimate"])
cbind(dh[,c("Estimate","z value")],odds_ratio)
```

    ##                                               Estimate       z value
    ## (Intercept)                            -8.431838534000 -11.197664193
    ## age                                     0.021126905992   9.720815966
    ## workclassLocal-gov                     -0.599151792154  -4.106247609
    ## workclassPrivate                       -0.456974976654  -3.765685662
    ## workclassSelf-emp-inc                  -0.241193682492  -1.525116278
    ## workclassSelf-emp-not-inc              -0.911662063497  -6.373462110
    ## workclassState-gov                     -0.654003749016  -4.101656233
    ## workclassWithout-pay                  -11.970097300479  -0.079604441
    ## fnlwgt                                  0.000000560326   2.427371397
    ## education.L                             0.211514980540   1.161903674
    ## education.Q                             0.274934367170   2.013770738
    ## education.C                            -0.012541679150  -0.078488237
    ## education^4                            -0.275352853304  -1.617866538
    ## education^5                             0.375094248132   2.960775647
    ## education^6                            -0.275299216953  -2.780972745
    ## education^7                             0.088283790237   0.950954133
    ## `education-num`                         0.257779468094   8.222523839
    ## `marital-status`Married-AF-spouse       2.643651685171   3.750643154
    ## `marital-status`Married-civ-spouse      1.811450029574   4.996820862
    ## `marital-status`Married-spouse-absent  -0.145025269699  -0.441060353
    ## `marital-status`Never-married          -0.512635970273  -4.439720907
    ## `marital-status`Separated               0.044074135053   0.211046436
    ## `marital-status`Widowed                 0.390247237659   1.963617732
    ## occupationArmed-Forces                 -0.616443665981  -0.326333413
    ## occupationCraft-repair                  0.111765417518   1.074937494
    ## occupationExec-managerial               0.806763193613   8.080379203
    ## occupationFarming-fishing              -0.963994963502  -5.351920807
    ## occupationHandlers-cleaners            -0.675460521469  -3.622771547
    ## occupationMachine-op-inspct            -0.171262860720  -1.309745026
    ## occupationOther-service                -0.679444979870  -4.560915201
    ## occupationPriv-house-serv              -3.599156765014  -1.610318994
    ## occupationProf-specialty                0.507321513148   4.756699448
    ## occupationProtective-serv               0.719663173782   4.462667791
    ## occupationSales                         0.228288310662   2.139769782
    ## occupationTech-support                  0.642540531414   4.445074573
    ## occupationTransport-moving             -0.111158759533  -0.851446262
    ## relationshipNot-in-family               0.045533711015   0.126997679
    ## relationshipOther-relative             -0.579116322210  -1.710496228
    ## relationshipOwn-child                  -1.010194928808  -2.802981387
    ## relationshipUnmarried                  -0.076876519052  -0.203890770
    ## relationshipWife                        1.162037160232   8.566918044
    ## raceAsian-Pac-Islander                  1.211343188736   3.395142925
    ## raceBlack                               0.776615077145   2.427020509
    ## raceOther                               0.004771321542   0.008844278
    ## raceWhite                               0.836390427364   2.726997164
    ## sexMale                                 0.759357918143   7.286106483
    ## `capital-gain`                          0.000319541381  23.567725145
    ## `capital-loss`                          0.000690616580  13.783452766
    ## `hours-per-week`                        0.030649084936  14.087450581
    ## `native-country`China                  -1.382791678872  -2.145277423
    ## `native-country`Cuba                   -0.150541989856  -0.273480397
    ## `native-country`El-Salvador            -0.826890431202  -1.126570532
    ## `native-country`England                -0.099684901067  -0.186099764
    ## `native-country`Germany                 0.178092375914   0.372554203
    ## `native-country`India                  -1.086389213464  -2.071956260
    ## `native-country`Mexico                 -0.650985646645  -1.439337785
    ## `native-country`Philippines             0.083100248651   0.169548403
    ## `native-country`Puerto-Rico            -0.209848372493  -0.328979199
    ## `native-country`Rest                   -0.659415182591  -1.707234384
    ## `native-country`United-States          -0.121513364893  -0.341489865
    ##                                            odds_ratio
    ## (Intercept)                            0.000217820651
    ## age                                    1.021351659059
    ## workclassLocal-gov                     0.549277339908
    ## workclassPrivate                       0.633196184552
    ## workclassSelf-emp-inc                  0.785689437363
    ## workclassSelf-emp-not-inc              0.401855758884
    ## workclassState-gov                     0.519959815116
    ## workclassWithout-pay                   0.000006330715
    ## fnlwgt                                 1.000000560326
    ## education.L                            1.235548474673
    ## education.Q                            1.316444270069
    ## education.C                            0.987536639948
    ## education^4                            0.759304152972
    ## education^5                            1.455128551303
    ## education^6                            0.759344880368
    ## education^7                            1.092298061573
    ## `education-num`                        1.294053407090
    ## `marital-status`Married-AF-spouse     14.064468961049
    ## `marital-status`Married-civ-spouse     6.119314188691
    ## `marital-status`Married-spouse-absent  0.865000434534
    ## `marital-status`Never-married          0.598914774709
    ## `marital-status`Separated              1.045059827583
    ## `marital-status`Widowed                1.477346004301
    ## occupationArmed-Forces                 0.539860953564
    ## occupationCraft-repair                 1.118250507895
    ## occupationExec-managerial              2.240643706624
    ## occupationFarming-fishing              0.381366294240
    ## occupationHandlers-cleaners            0.508921997127
    ## occupationMachine-op-inspct            0.842600057901
    ## occupationOther-service                0.506898253040
    ## occupationPriv-house-serv              0.027346772483
    ## occupationProf-specialty               1.660836702688
    ## occupationProtective-serv              2.053741340202
    ## occupationSales                        1.256447520434
    ## occupationTech-support                 1.901305073969
    ## occupationTransport-moving             0.894796680148
    ## relationshipNot-in-family              1.046586285505
    ## relationshipOther-relative             0.560393354990
    ## relationshipOwn-child                  0.364147989719
    ## relationshipUnmarried                  0.926004190423
    ## relationshipWife                       3.196438304972
    ## raceAsian-Pac-Islander                 3.357992041822
    ## raceBlack                              2.174100633411
    ## raceOther                              1.004782722422
    ## raceWhite                              2.308020953776
    ## sexMale                                2.136903712808
    ## `capital-gain`                         1.000319592440
    ## `capital-loss`                         1.000690855110
    ## `hours-per-week`                       1.031123603586
    ## `native-country`China                  0.250877205952
    ## `native-country`Cuba                   0.860241607827
    ## `native-country`El-Salvador            0.437407321935
    ## `native-country`England                0.905122576265
    ## `native-country`Germany                1.194935699414
    ## `native-country`India                  0.337432694094
    ## `native-country`Mexico                 0.521531477593
    ## `native-country`Philippines            1.086650738358
    ## `native-country`Puerto-Rico            0.810707162157
    ## `native-country`Rest                   0.517153686552
    ## `native-country`United-States          0.885579217598

#### Model Prediction

To check the efficiency of the model, we are now going to run the model
on testing data set, after which we will evaluate the accuracy of the
model by using a confusion matrix.

``` r
# and those who are not
predict_lr<-predict(model_lr,testing,type = "response" )
```

#### Confusion Matrix and Accuracy

``` r
library(caret)
predict_results <- ifelse(predict_lr > 0.5,'large','small')
predict_results<-factor(predict_results,levels=c('small','large'), ordered=TRUE)
cm_lr <- table(testing$income, predict_results)
confusionMatrix(cm_lr)
```

    ## Confusion Matrix and Statistics
    ## 
    ##        predict_results
    ##         small large
    ##   small  8350   648
    ##   large  1183  1787
    ##                                                
    ##                Accuracy : 0.847                
    ##                  95% CI : (0.8404, 0.8534)     
    ##     No Information Rate : 0.7965               
    ##     P-Value [Acc > NIR] : < 0.00000000000000022
    ##                                                
    ##                   Kappa : 0.5637               
    ##                                                
    ##  Mcnemar's Test P-Value : < 0.00000000000000022
    ##                                                
    ##             Sensitivity : 0.8759               
    ##             Specificity : 0.7339               
    ##          Pos Pred Value : 0.9280               
    ##          Neg Pred Value : 0.6017               
    ##              Prevalence : 0.7965               
    ##          Detection Rate : 0.6977               
    ##    Detection Prevalence : 0.7518               
    ##       Balanced Accuracy : 0.8049               
    ##                                                
    ##        'Positive' Class : small                
    ## 

Our predicted results show that the model correctly classified 8350+1787
= 10137 from 11968 observations, and our model accuracy is 84.70% based
on testing data.

### Decision Tree

The decision tree algorithm works based on the decision on the
conditions of the features. Nodes are the conditions or tests on an
attribute. A branch represents the outcome of the tests, and leaf nodes
are the decisions based on the conditions. This time, we will use a
decision tree model using the `rpart()` function for prediction of the
income category for the training data.

``` r
##Build Decision tree model
set.seed(84)
tmodel <- rpart(income~., training, cp=0, method = "class")
printcp(tmodel)
```

    ## 
    ## Classification tree:
    ## rpart(formula = income ~ ., data = training, method = "class", 
    ##     cp = 0)
    ## 
    ## Variables actually used in tree construction:
    ##  [1] age            capital-gain   capital-loss   education      education-num 
    ##  [6] fnlwgt         hours-per-week marital-status native-country occupation    
    ## [11] race           relationship   sex            workclass     
    ## 
    ## Root node error: 4538/18194 = 0.24942
    ## 
    ## n= 18194 
    ## 
    ##             CP nsplit rel error  xerror     xstd
    ## 1  0.133208462      0   1.00000 1.00000 0.012861
    ## 2  0.063904804      2   0.73358 0.73358 0.011492
    ## 3  0.037681798      3   0.66968 0.66968 0.011087
    ## 4  0.008814456      4   0.63200 0.62979 0.010816
    ## 5  0.008373733      6   0.61437 0.62032 0.010749
    ## 6  0.005949758      7   0.60599 0.60401 0.010632
    ## 7  0.003790216      8   0.60004 0.59916 0.010597
    ## 8  0.001983253     14   0.57536 0.58219 0.010472
    ## 9  0.001762891     22   0.55311 0.57360 0.010407
    ## 10 0.001615984     24   0.54958 0.57272 0.010401
    ## 11 0.001322168     27   0.54473 0.57250 0.010399
    ## 12 0.001211988     34   0.53504 0.57338 0.010406
    ## 13 0.001101807     36   0.53261 0.56897 0.010372
    ## 14 0.000991626     39   0.52931 0.56787 0.010364
    ## 15 0.000881446     41   0.52732 0.56919 0.010374
    ## 16 0.000771265     52   0.51719 0.56963 0.010377
    ## 17 0.000661084     54   0.51565 0.57316 0.010404
    ## 18 0.000587630     66   0.50727 0.57580 0.010424
    ## 19 0.000550903     73   0.50309 0.57999 0.010456
    ## 20 0.000514177     81   0.49868 0.58308 0.010479
    ## 21 0.000440723     84   0.49714 0.59299 0.010552
    ## 22 0.000385632    113   0.48039 0.59387 0.010559
    ## 23 0.000367269    128   0.47334 0.59630 0.010576
    ## 24 0.000330542    137   0.47003 0.59960 0.010600
    ## 25 0.000293815    154   0.46364 0.60115 0.010612
    ## 26 0.000275452    160   0.46188 0.60886 0.010667
    ## 27 0.000220361    220   0.43323 0.61415 0.010705
    ## 28 0.000198325    284   0.41560 0.62847 0.010807
    ## 29 0.000154253    303   0.41097 0.63530 0.010854
    ## 30 0.000146908    315   0.40877 0.64213 0.010901
    ## 31 0.000110181    318   0.40833 0.64412 0.010915
    ## 32 0.000094441    347   0.40458 0.65646 0.010999
    ## 33 0.000088145    354   0.40392 0.65712 0.011003
    ## 34 0.000073454    369   0.40260 0.65888 0.011015
    ## 35 0.000055090    378   0.40194 0.65976 0.011021
    ## 36 0.000033902    386   0.40150 0.66417 0.011050
    ## 37 0.000024485    399   0.40106 0.66527 0.011058
    ## 38 0.000000000    408   0.40084 0.66527 0.011058

``` r
min(tmodel$cptable[,4])
```

    ## [1] 0.5678713

Here, we can observe that the minimum value of xerror (cross validation
error) is 0.56787. So, we can use the value of cp for this to get the
optimal pruned decision tree model.

``` r
tmodel<-prune(tmodel,cp=0.000991626)
```

#### Decision Tree Plot

The decision tree plot visualizes how the full model works under certain
conditions and also describes the root nodes and leaf nodes that
actually illustrate the final decision.

``` r
rpart.plot(tmodel, type = 0, extra = 0)
```

![](adultucifinal_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

#### Model Prediction

To check the efficiency of the model, we are now going to run the model
on testing data set, after which we will evaluate the accuracy of the
model by using a confusion matrix.

#### Confusion Matrix and Accuracy

To check the efficiency of the model, we are now going to run the model
on testing data set, after which we will evaluate the accuracy of the
model by using a confusion matrix.

``` r
predicted_class <- predict(tmodel, testing, type = "class")
predicted_class <-factor(predicted_class,levels=c('small','large'), ordered=TRUE)
confusionMatrix(predicted_class, testing$income)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction small large
    ##      small  8505  1211
    ##      large   493  1759
    ##                                                
    ##                Accuracy : 0.8576               
    ##                  95% CI : (0.8512, 0.8638)     
    ##     No Information Rate : 0.7518               
    ##     P-Value [Acc > NIR] : < 0.00000000000000022
    ##                                                
    ##                   Kappa : 0.5848               
    ##                                                
    ##  Mcnemar's Test P-Value : < 0.00000000000000022
    ##                                                
    ##             Sensitivity : 0.9452               
    ##             Specificity : 0.5923               
    ##          Pos Pred Value : 0.8754               
    ##          Neg Pred Value : 0.7811               
    ##              Prevalence : 0.7518               
    ##          Detection Rate : 0.7106               
    ##    Detection Prevalence : 0.8118               
    ##       Balanced Accuracy : 0.7687               
    ##                                                
    ##        'Positive' Class : small                
    ## 

Our predicted results show that the model correctly classified 8505+1759
= 10264 from 11968 observations, and our model accuracy is 85.76% based
on testing data.

### Naive Bayes Classifier

Naive Bayes classifiers are a family of simple probabilistic classifiers
based on applying Baye’s theorem with strong(Naive) independence
assumptions between the features or variables.

``` r
####  
#Naive Bayes classifier
classifier_cl <- naiveBayes(income ~ ., data =training)
summary(classifier_cl)
```

    ##           Length Class  Mode     
    ## apriori    2     table  numeric  
    ## tables    14     -none- list     
    ## levels     2     -none- character
    ## isnumeric 14     -none- logical  
    ## call       4     -none- call

#### Model Prediction

To check the efficiency of the model, we are now going to run the model
on testing data set.

``` r
# Predicting on train data'
y_pred <- predict(classifier_cl, newdata = testing)
y_pred<-factor(y_pred,levels=c('small','large'), ordered=TRUE)
```

#### Confusion Matrix and Accuracy

Now, we will evaluate the accuracy of the model by using a confusion
matrix.

``` r
# Confusion Matrix
confusionMatrix(testing$income, y_pred)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction small large
    ##      small  8366   632
    ##      large  1523  1447
    ##                                              
    ##                Accuracy : 0.8199             
    ##                  95% CI : (0.8129, 0.8268)   
    ##     No Information Rate : 0.8263             
    ##     P-Value [Acc > NIR] : 0.9671             
    ##                                              
    ##                   Kappa : 0.4635             
    ##                                              
    ##  Mcnemar's Test P-Value : <0.0000000000000002
    ##                                              
    ##             Sensitivity : 0.8460             
    ##             Specificity : 0.6960             
    ##          Pos Pred Value : 0.9298             
    ##          Neg Pred Value : 0.4872             
    ##              Prevalence : 0.8263             
    ##          Detection Rate : 0.6990             
    ##    Detection Prevalence : 0.7518             
    ##       Balanced Accuracy : 0.7710             
    ##                                              
    ##        'Positive' Class : small              
    ## 

Our predicted results show that the model correctly classified 8366+1447
from 11968 observations, and our model accuracy is 81.99% based on
testing data.

Among the three classification algorithm Logistic regression predicts
well our testing dataset.

# Conclusion

We have used three classification models to predict or classify our
dependent variable income. The variable income has two categories: small
and large. We intend to classify that income variable into those two
categories based on the impact of other independent variables.

### Logistic Regression

Using logistic regression model, we create a generalized linear model
where independent variables stay at the right side of the equation and
logit function of dependent variable stays at the right side of the
equation. By using that model equation we can predict the value of
income that it will be small or large. After predicting the values of
income from the test dataset, we have checked the accuracy by comparing
the predicted values with actual (Reference) value. We want to achieve
the highest percentage of match between those predicted and actual
values.

We have seen from the confusion matrix that the accuracy of logistic
regression was 84.70%.

### Decision Tree

Using decision tree model, we create a classification model where for
different conditional state of independent variables, the value of
dependent variable income is predicted. By using that decision tree plot
and it’s nodes, we can predict the value of income that it will be small
or large. After predicting the values of income from the test dataset,
we have checked the accuracy by comparing the predicted values with
actual (Reference) value. We want to achieve the highest percentage of
match between those predicted and actual values.

We have seen from the confusion matrix that the accuracy of decision
tree was 85.76%.

### Naive Bayes Classifier

Using naive bayes classifier model, we create a classification model
where the probabilities of independent variables are used for predicing
dependent variable income. By using that naive bayes classifier model,
we can predict the value of income that it will be small or large based
on the posterior probabilities of different independent variables. The
higher probability in each independent variable is responsible here for
the prediction of income to be small or large. After predicting the
values of income from the test dataset, we have checked the accuracy by
comparing the predicted values with actual (Reference) value. We want to
achieve the highest percentage of match between those predicted and
actual values.

We have seen from the confusion matrix that the accuracy of naive bayes
classifier was 81.99%.

**Finally, we can conclude that the best model for predicting income
will be the decision tree model because it shows the highest accuracy
(85.76%) among those three models.**

### Prediction from Decision Tree

From the decision tree, we can say that if **relationship** is
*Not-in-family, Other-relative, Own-child, or Unmarried*, **capital
gain** is *less than 7140*, **education-num** is *less than 14*,
**capital-loss** is *not less than 2219*, and **fnlwgt** is *not greater
than 138000* the probability of more than 50k income is ***higher***.

We can also say that if **relationship** is *Not-in-family,
Other-relative, Own-child, or Unmarried*, **capital gain** is *less than
7140*, **education-num** is *not* *less than 14*, **hours-per-week** is
*less than 49*, and **capital-loss** is *not less than 2157* the
probability of more than 50k income is ***higher***.

In this way we can explain each of those nodes from the Decision Tree to
predict the income greater than 50k or not.

# Recommendations

-   Further analysis can be done using other types of classification and
    clustering methods.

-   Different new variables can be calculated from the existing one to
    increase the model accuracy.

-   We can also introduce dimensionality reduction techniques on the
    numeric variables and use those new variables for the models to get
    the optimal model.

# References

\[1\]Hosmer, David W., Trina Hosmer, Saskia Le Cessie, and Stanley
Lemeshow. “A comparison of goodness‐of‐fit tests for the logistic
regression model.” Statistics in medicine 16, no. 9 (1997): 965-980.

\[2\]Charbuty, Bahzad, and Adnan Abdulazeez. “Classification based on
decision tree algorithm for machine learning.” Journal of Applied
Science and Technology Trends 2, no. 01 (2021): 20-28.

\[3\]Rish, Irina. “An empirical study of the naive Bayes classifier.” In
IJCAI 2001 workshop on empirical methods in artificial intelligence,
vol. 3, no. 22, pp. 41-46. 2001.

\[4\]Nindrea, Ricvan Dana, Teguh Aryandono, Lutfan Lazuardi, and Iwan
Dwiprahasto. “Diagnostic accuracy of different machine learning
algorithms for breast cancer risk calculation: a meta-analysis.” Asian
Pacific journal of cancer prevention: APJCP 19, no. 7 (2018): 1747.
