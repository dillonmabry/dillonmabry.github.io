---
layout: post
title:  "Stanly County Housing Data Analysis"
date:   2019-06-17 23:00:40 +0100
categories: r jekyll
---

## Summary:
This notebook is to demo general analysis of 2018 Stanly County Tax Property Dataset.
The dataset contains almost 40k observations of all properties valued in Stanly County, NC.

The filtered dataset for analysis will contain around ~24k observations which will only contain residential dwellings/homes and not apartments, commercial, vacant, or industrial properties.

### Note on outliers:
The following outliers will be removed:
- Records with critical features missing (YearBuilt, Mapping/Location information)
- Records that are not residential dwellings (Commericial, Industrial, etc.)
- Vacant Properties
- Properties with values over 2 million USD

* For setting jekyll compatibility for images

{% highlight r %}
knitr::opts_chunk$set(fig.path="https://dillonmabry.github.io//images/stanly-county-housing-data")
{% endhighlight %}




{% highlight r %}
# Libraries
library(ggplot2)
library(corrplot)
library(scales)
library(dplyr)
library(tidyr)
library(hexbin)
library(randomForest)
library(usdm)
library(car)
{% endhighlight %}


{% highlight r %}
download.file(url = "https://docs.google.com/uc?export=download&id=1Ujnu-4M2p6dyQoPmb2K9Az9kLnV19NlQ", destfile = "stanly_house_data.csv", mode="wb")
house.data <- read.csv('stanly_house_data.csv', header=TRUE)
{% endhighlight %}

## Data validations, cleanup:
Only get features we know will be useful.


{% highlight r %}
house.data.new <-
  house.data %>%
  dplyr::select(YearBuilt,
         Sqft = FinishedArea,
         Stories,
         Submap,
         Block,
         CityCode,
         OwnerID,
         PrimaryOwner = Name1,
         TaxPayerCity,
         Zip,
         PhyStreetName,
         PhyStreetType,
         ListedAcres = DeedAcres,
         ActualAcres = MapAcres,
         DistrictCode,
         TownshipCode,
         PropertyCode = DescCode1, # Property type
         MarketAreaCode = NBHDCode, # Market area
         LandMarketValue = LandFMVCurrent,
         LandAssessedValue = LandASVCurrent,
         TotalMarketValue = TotalFMVCurrent,
         TotalAssessedValue = TotalASVCurrent,
         DateSold,
         SaleAmount,
         TotalTaxOwed,
         IsMultiSegments = MultiLandSegments,
         QualityGradeCode,
         HVACCompCode,
         HVACUnits,
         Baths,
         Bedrooms,
         PctComplete,
         IsFirePlace = FirePlace,
         IsBasement = Basement,
         IsGarageAttached = AttachedGarage)
{% endhighlight %}

Explore feature types and missing values


{% highlight r %}
sapply(house.data.new, class)
{% endhighlight %}



{% highlight text %}
##          YearBuilt               Sqft            Stories 
##          "integer"          "integer"          "numeric" 
##             Submap              Block           CityCode 
##          "integer"          "integer"           "factor" 
##            OwnerID       PrimaryOwner       TaxPayerCity 
##          "integer"           "factor"           "factor" 
##                Zip      PhyStreetName      PhyStreetType 
##           "factor"           "factor"           "factor" 
##        ListedAcres        ActualAcres       DistrictCode 
##          "numeric"          "numeric"          "integer" 
##       TownshipCode       PropertyCode     MarketAreaCode 
##          "integer"           "factor"          "integer" 
##    LandMarketValue  LandAssessedValue   TotalMarketValue 
##          "numeric"          "numeric"          "numeric" 
## TotalAssessedValue           DateSold         SaleAmount 
##          "numeric"           "factor"          "numeric" 
##       TotalTaxOwed    IsMultiSegments   QualityGradeCode 
##          "numeric"           "factor"           "factor" 
##       HVACCompCode          HVACUnits              Baths 
##          "integer"          "integer"          "numeric" 
##           Bedrooms        PctComplete        IsFirePlace 
##          "integer"          "integer"           "factor" 
##         IsBasement   IsGarageAttached 
##           "factor"           "factor"
{% endhighlight %}

{% highlight r %}
colSums(is.na(house.data.new))
{% endhighlight %}



{% highlight text %}
##          YearBuilt               Sqft            Stories 
##              12347                  0                  0 
##             Submap              Block           CityCode 
##                  2                  1              12658 
##            OwnerID       PrimaryOwner       TaxPayerCity 
##                  0                  0                295 
##                Zip      PhyStreetName      PhyStreetType 
##                318                  0              11668 
##        ListedAcres        ActualAcres       DistrictCode 
##                  0                  0                  0 
##       TownshipCode       PropertyCode     MarketAreaCode 
##                  0                  8                  0 
##    LandMarketValue  LandAssessedValue   TotalMarketValue 
##                  0                  0                  0 
## TotalAssessedValue           DateSold         SaleAmount 
##                  0              22842                  0 
##       TotalTaxOwed    IsMultiSegments   QualityGradeCode 
##                  0              32953              12323 
##       HVACCompCode          HVACUnits              Baths 
##              12548                  0                  0 
##           Bedrooms        PctComplete        IsFirePlace 
##                  0                  0                  0 
##         IsBasement   IsGarageAttached 
##                  0                  0
{% endhighlight %}


{% highlight r %}
colSums(house.data.new == "")
{% endhighlight %}



{% highlight text %}
##          YearBuilt               Sqft            Stories 
##                 NA                  0                  0 
##             Submap              Block           CityCode 
##                 NA                 NA                 NA 
##            OwnerID       PrimaryOwner       TaxPayerCity 
##                  0                  0                 NA 
##                Zip      PhyStreetName      PhyStreetType 
##                 NA                  0                 NA 
##        ListedAcres        ActualAcres       DistrictCode 
##                  0                  0                  0 
##       TownshipCode       PropertyCode     MarketAreaCode 
##                  0                 NA                  0 
##    LandMarketValue  LandAssessedValue   TotalMarketValue 
##                  0                  0                  0 
## TotalAssessedValue           DateSold         SaleAmount 
##                  0                 NA                  0 
##       TotalTaxOwed    IsMultiSegments   QualityGradeCode 
##                  0                 NA                 NA 
##       HVACCompCode          HVACUnits              Baths 
##                 NA                  0                  0 
##           Bedrooms        PctComplete        IsFirePlace 
##                  0                  0                  0 
##         IsBasement   IsGarageAttached 
##                  0                  0
{% endhighlight %}

### Cleanup data:
- Replace missing values
- Auto generate any data
- Filter out bad instances
- Reclassify feature types


{% highlight r %}
# Filter outliers
# NA and Empty data
house.data.new <- house.data.new %>% drop_na(YearBuilt, Submap, Block)
house.data.new <- house.data.new %>% filter(!(PropertyCode == ""))
# Filter out Apartments, Commercial, Exempt, Industrial, and Vacant
nonWantedTypes <- c("A", "C", "E","I","V")
house.data.new <- house.data.new %>% filter(!as.character(PropertyCode) %in% nonWantedTypes)
# Filter out properties > 2e6 value
house.data.new <- house.data.new %>% filter(TotalMarketValue <= 2e6)
# Substitution
house.data.new <- within(house.data.new, 
                         HVACCompCode <- ifelse(!is.na(HVACCompCode), HVACCompCode, 9999)
                         )
# Dates/Times https://www.statmethods.net/input/dates.html
house.data.new$YearBuilt.new <- as.Date(paste(house.data.new$YearBuilt, 1, 1, sep = "-"))
house.data.new$DateSold <- as.Date(house.data.new$DateSold, format = "%d-%b-%y")


# Factors
house.data.new <- within(house.data.new, 
                         IsMultiSegments <- ifelse(IsMultiSegments == "", "N", "Y")
                         )
house.data.new$IsMultiSegments <- factor(house.data.new$IsMultiSegments, levels=c("N", "Y"))
house.data.new$Submap <- factor(house.data.new$Submap)
house.data.new$Block <- factor(house.data.new$Block)
house.data.new$DistrictCode <- factor(house.data.new$DistrictCode)
house.data.new$TownshipCode <- factor(house.data.new$TownshipCode)
house.data.new$MarketAreaCode <- factor(house.data.new$MarketAreaCode)
house.data.new$HVACCompCode <- factor(house.data.new$HVACCompCode)
house.data.new$OwnerID <- factor(house.data.new$OwnerID)

# Simplify Property Quality Codes/Factors
simplify_quality_code <- function(code) {
  qc <- as.character(code)
  if(grepl("A", qc)) {
    return ("A")
  } else if (grepl("B", qc)) {
    return ("B")
  } else if (grepl("C", qc)) {
    return ("C")
  } else if (grepl("D", qc)) {
    return ("D")
  } else {
    return ("E")
  }
}
house.data.new$QualityGradeCode.new <- sapply(house.data.new$QualityGradeCode, simplify_quality_code)
house.data.new$QualityGradeCode.new <- as.factor(house.data.new$QualityGradeCode.new)
{% endhighlight %}

### Initial View post-filtration:


{% highlight r %}
dim(house.data.new)
{% endhighlight %}



{% highlight text %}
## [1] 23866    37
{% endhighlight %}


{% highlight r %}
summary(house.data.new)
{% endhighlight %}



{% highlight text %}
##    YearBuilt         Sqft          Stories      Submap  
##  Min.   :1885   Min.   :    5   Min.   :0.000   1:5808  
##  1st Qu.:1949   1st Qu.: 1155   1st Qu.:1.000   2:6011  
##  Median :1970   Median : 1476   Median :1.000   3:5635  
##  Mean   :1968   Mean   : 1621   Mean   :1.105   4:6412  
##  3rd Qu.:1994   3rd Qu.: 1899   3rd Qu.:1.000           
##  Max.   :2018   Max.   :15826   Max.   :4.760           
##                                                         
##      Block          CityCode       OwnerID     
##  83     :  397   ALBEM  :9741   10913  :   41  
##  45     :  339   NORWO  :3255   15492  :   30  
##  94     :  320   OAKBO  :2089   11737  :   28  
##  51     :  319   LOCUS  :2068   10709  :   24  
##  49     :  316   NEWLO  :1903   12669  :   24  
##  75     :  315   (Other):3488   10024  :   22  
##  (Other):21860   NA's   :1322   (Other):23697  
##                PrimaryOwner       TaxPayerCity       Zip      
##  MCCOY FRANK T       :   41   ALBEMARLE :8972   28001  :8086  
##  SWINK DONALD L      :   37   NORWOOD   :2791   28128  :2640  
##  HUNEYCUTT RICHARD S :   32   OAKBORO   :2156   28129  :2035  
##  LITTLE MILES E      :   32   LOCUST    :2096   28097  :1989  
##  CAMPBELL LAND CO LLC:   24   NEW LONDON:1766   28163  :1683  
##  HINSON CLAUDE EUGENE:   24   (Other)   :6084   (Other):7431  
##  (Other)             :23676   NA's      :   1   NA's   :   2  
##      PhyStreetName   PhyStreetType    ListedAcres     
##  MAIN       :  395   RD     :10787   Min.   :  0.000  
##  AUSTIN     :  304   ST     : 4568   1st Qu.:  0.000  
##  US 52      :  276   DR     : 2758   Median :  0.610  
##  MILLINGPORT:  190   LN     : 1251   Mean   :  4.117  
##  NC 138     :  188   HWY    : 1243   3rd Qu.:  2.470  
##  ST MARTIN  :  164   (Other): 2472   Max.   :583.550  
##  (Other)    :22349   NA's   :  787                    
##   ActualAcres       DistrictCode    TownshipCode   PropertyCode
##  Min.   :  0.000   500    : 5836   101    : 5861   A:    0     
##  1st Qu.:  0.380   110    : 1530   125    : 1976   C:    0     
##  Median :  0.790   112    : 1447   102    : 1404   D:23849     
##  Mean   :  4.122   550    : 1306   124    : 1269   E:    0     
##  3rd Qu.:  2.210   230    : 1282   126    : 1198   I:    0     
##  Max.   :556.150   540    : 1247   118    : 1195   V:    0     
##                    (Other):11218   (Other):10963   W:   17     
##  MarketAreaCode  LandMarketValue   LandAssessedValue
##  9901   : 1575   Min.   :      0   Min.   :     0   
##  5401   : 1246   1st Qu.:  11664   1st Qu.: 11664   
##  5003   : 1180   Median :  19000   Median : 18954   
##  5007   : 1089   Mean   :  37253   Mean   : 31980   
##  9920   :  804   3rd Qu.:  33968   3rd Qu.: 32340   
##  5008   :  777   Max.   :1626037   Max.   :922769   
##  (Other):17195                                      
##  TotalMarketValue  TotalAssessedValue    DateSold    
##  Min.   :   1838   Min.   :   1838    Min.   :NA     
##  1st Qu.:  58986   1st Qu.:  58296    1st Qu.:NA     
##  Median : 101335   Median :  98784    Median :NA     
##  Mean   : 128195   Mean   : 122922    Mean   :NA     
##  3rd Qu.: 165404   3rd Qu.: 157684    3rd Qu.:NA     
##  Max.   :1714787   Max.   :1403248    Max.   :NA     
##                                       NA's   :23866  
##    SaleAmount       TotalTaxOwed      IsMultiSegments
##  Min.   :      0   Min.   :   17.42   N   :    0     
##  1st Qu.:      0   1st Qu.:  545.73   Y   : 3587     
##  Median :      0   Median :  912.29   NA's:20279     
##  Mean   :  25329   Mean   : 1149.40                  
##  3rd Qu.:      0   3rd Qu.: 1486.12                  
##  Max.   :1445000   Max.   :17566.09                  
##                                                      
##  QualityGradeCode  HVACCompCode     HVACUnits         Baths       
##  C+5    :4679     17     :19766   Min.   :    0   Min.   : 0.000  
##  C      :3838     12     : 2193   1st Qu.: 1107   1st Qu.: 1.000  
##  C-5    :2604     11     :  727   Median : 1394   Median : 2.000  
##  C+10   :1952     4      :  463   Mean   : 1493   Mean   : 1.715  
##  D+5    :1696     19     :  415   3rd Qu.: 1762   3rd Qu.: 2.000  
##  D      :1683     9999   :  181   Max.   :12000   Max.   :10.000  
##  (Other):7414     (Other):  121                                   
##     Bedrooms      PctComplete     IsFirePlace IsBasement
##  Min.   :0.000   Min.   :  9.00   N:12179     N:23866   
##  1st Qu.:2.000   1st Qu.:100.00   Y:11687               
##  Median :3.000   Median :100.00                         
##  Mean   :2.745   Mean   : 99.77                         
##  3rd Qu.:3.000   3rd Qu.:100.00                         
##  Max.   :9.000   Max.   :100.00                         
##                                                         
##  IsGarageAttached YearBuilt.new        QualityGradeCode.new
##  N:23866          Min.   :1885-01-01   A:  253             
##                   1st Qu.:1949-01-01   B: 2467             
##                   Median :1970-01-01   C:14568             
##                   Mean   :1968-05-06   D: 5973             
##                   3rd Qu.:1994-01-01   E:  605             
##                   Max.   :2018-01-01                       
## 
{% endhighlight %}

## Exploratory Analysis:

What are the majority of houses valued at based on market?  (Filtered up to TMV < 1e6)

The vast majority are valued under $250,000 based on market value in Stanly County from tax assessors


{% highlight r %}
ggplot(house.data.new, aes(x=TotalMarketValue)) +
    geom_histogram(binwidth = 10000, fill = "blue") +
    scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    labs(x = "Price $",title = "Price Distribution")
{% endhighlight %}

![plot of chunk unnamed-chunk-12](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-12-1.png)

What are the general distributions of specific attributes (Beds, Baths, Acres)?  (Filtered up to Beds/Baths < 10)


{% highlight r %}
ggplot(house.data.new, aes(x=Bedrooms)) +
    geom_histogram(bins = 10, binwidth = 1, fill = "blue") +
    scale_x_continuous(limits = c(0,10)) +
    labs(x = "Beds",
       title = "Beds Distribution")
{% endhighlight %}



{% highlight text %}
## Warning: Removed 2 rows containing missing values (geom_bar).
{% endhighlight %}

![plot of chunk unnamed-chunk-13](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-13-1.png)

{% highlight r %}
ggplot(house.data.new, aes(x=Baths)) +
    geom_histogram(bins = 10, binwidth = 1, fill = "blue") +
    scale_x_continuous(limits = c(0,10)) +
    labs(x = "Baths",
       title = "Baths Distribution")
{% endhighlight %}



{% highlight text %}
## Warning: Removed 2 rows containing missing values (geom_bar).
{% endhighlight %}

![plot of chunk unnamed-chunk-14](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-14-1.png)

Which cities have the higher quality properties?

Based on general distributions we can see that areas such as Norwood, Stanfield, New London, and Richfield cover more C, D, and E properties than other areas with even higher populations

For example, Albemarle, the seat of the county has a much higher population, so it is expected to fill the majority of the property distributions

Locust seems to have higher valued properties in general than most cities on the CityCode in the B and C quality category


{% highlight r %}
ggplot(house.data.new, aes(x=QualityGradeCode.new, fill = CityCode)) +
    geom_histogram(bins = 10, binwidth = 1, stat = "count") +
    labs(x = "Quality",
       title = "Property Grade Distribution")
{% endhighlight %}

![plot of chunk unnamed-chunk-15](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-15-1.png)

Which areas generally have the lower quality based properties?

We can see that Albemarle has an overwhelming distribution of D grade properties (over 1000+ in our dataset of ~25k records)

We can also see that Norwood has a large portion of lower grade properties, though has a smaller population in general

Albemarle NC population: ~16k (2017)
Norwood NC population: ~2.5k (2017)


{% highlight r %}
lowQualityCodes <- c("D","E")
lowQualityProperties <- house.data.new %>% filter(as.character(QualityGradeCode) %in% lowQualityCodes)
ggplot(lowQualityProperties, aes(x=QualityGradeCode.new, fill = CityCode)) +
    geom_histogram(bins = 10, binwidth = 1, stat = "count") +
    labs(x = "Quality",
       title = "Property Grade Distribution")
{% endhighlight %}

![plot of chunk unnamed-chunk-16](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-16-1.png)

Which cities generally have the highest quality properties?


{% highlight r %}
highQualityCodes <- c("A","B")
highQualityProperties <- house.data.new %>% filter(as.character(QualityGradeCode) %in% highQualityCodes)
ggplot(highQualityProperties, aes(x=QualityGradeCode.new, fill = CityCode)) +
    geom_histogram(bins = 10, binwidth = 1, stat = "count") +
    labs(x = "Quality",
       title = "Property Grade Distribution")
{% endhighlight %}

![plot of chunk unnamed-chunk-17](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-17-1.png)

Total Market Value by YearBuilt (Filtered up to TMV < 1e6)


{% highlight r %}
ggplot(house.data.new, aes(x=YearBuilt.new, y=TotalMarketValue)) +
    geom_point(color = "red") +
    labs(x = "YearBuilt",
       title = "Total Market Value by Year")
{% endhighlight %}

![plot of chunk unnamed-chunk-18](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-18-1.png)

### Anova (Analysis of Variance):
Do housing market values vary based on year built?


{% highlight r %}
# One way anova test http://www.sthda.com/english/wiki/one-way-anova-test-in-r
marketValueByDecade <- data.frame(TotalMarketValue = house.data.new$TotalMarketValue, Decade = as.factor(floor(as.numeric(format(house.data.new$YearBuilt.new, '%Y')) / 10) * 10)) # Floor to decade

anova <- aov(TotalMarketValue ~ Decade, data=marketValueByDecade)
summary(anova)
{% endhighlight %}



{% highlight text %}
##                Df    Sum Sq   Mean Sq F value Pr(>F)    
## Decade         12 3.524e+13 2.937e+12   308.3 <2e-16 ***
## Residuals   23853 2.272e+14 9.525e+09                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
{% endhighlight %}


{% highlight r %}
plot(anova, 1)
{% endhighlight %}

![plot of chunk unnamed-chunk-20](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-20-1.png)


{% highlight r %}
# Levene test to validate our assumptions of p-value http://www.sthda.com/english/wiki/one-way-anova-test-in-r
leveneTest(TotalMarketValue ~ Decade, data = marketValueByDecade)
{% endhighlight %}



{% highlight text %}
## Levene's Test for Homogeneity of Variance (center = median)
##          Df F value    Pr(>F)    
## group    12  94.732 < 2.2e-16 ***
##       23853                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
{% endhighlight %}

Two part test between all bins of decades for significance p values:


{% highlight r %}
TukeyHSD(anova)
{% endhighlight %}



{% highlight text %}
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = TotalMarketValue ~ Decade, data = marketValueByDecade)
## 
## $Decade
##                 diff          lwr        upr     p adj
## 1900-1880  70650.023 -253107.3280 394407.373 0.9999577
## 1910-1880  19841.627 -303732.0689 343415.323 1.0000000
## 1920-1880  21669.046 -301776.0502 345114.143 1.0000000
## 1930-1880  31040.863 -292470.8748 354552.601 1.0000000
## 1940-1880  34490.896 -288933.8696 357915.662 1.0000000
## 1950-1880  52315.284 -271091.8464 375722.415 0.9999985
## 1960-1880  79453.970 -243956.6095 402864.550 0.9998498
## 1970-1880  87360.431 -236046.1291 410766.991 0.9995975
## 1980-1880  78367.345 -245059.1250 401793.814 0.9998702
## 1990-1880  97350.898 -226050.8650 420752.661 0.9988044
## 2000-1880 140147.698 -183260.4703 463555.866 0.9689197
## 2010-1880 157391.983 -166115.7242 480899.689 0.9267223
## 1910-1900 -50808.395  -70970.7963 -30645.994 0.0000000
## 1920-1900 -48980.976  -66961.9678 -30999.985 0.0000000
## 1930-1900 -39609.160  -58751.5241 -20466.795 0.0000000
## 1940-1900 -36159.127  -53770.6172 -18547.636 0.0000000
## 1950-1900 -18334.738  -35619.3457  -1050.131 0.0259826
## 1960-1900   8803.948   -8545.0792  26152.975 0.9025000
## 1970-1900  16710.408    -563.5172  33984.333 0.0693828
## 1980-1900   7717.322   -9925.4330  25360.077 0.9665616
## 1990-1900  26700.875    9516.9887  43884.762 0.0000198
## 2000-1900  69497.675   52193.6634  86801.687 0.0000000
## 2010-1900  86741.960   67667.8436 105816.076 0.0000000
## 1920-1910   1827.419  -12470.5121  16125.350 0.9999999
## 1930-1910  11199.236   -4534.3239  26932.796 0.4740202
## 1940-1910  14649.269     818.8898  28479.648 0.0264374
## 1950-1910  32473.657   19062.0042  45885.310 0.0000000
## 1960-1910  59612.343   46117.7696  73106.917 0.0000000
## 1970-1910  67518.803   54120.9202  80916.687 0.0000000
## 1980-1910  58525.717   44655.5485  72395.886 0.0000000
## 1990-1910  77509.271   64227.6767  90790.865 0.0000000
## 2000-1910 120306.071  106869.4190 133742.722 0.0000000
## 2010-1910 137550.355  121899.9018 153200.809 0.0000000
## 1930-1920   9371.817   -3447.6431  22191.277 0.4281191
## 1940-1920  12821.850    2426.0317  23217.668 0.0030207
## 1950-1920  30646.238   20814.3470  40478.129 0.0000000
## 1960-1920  57784.924   47840.2187  67729.629 0.0000000
## 1970-1920  65691.384   55878.2847  75504.484 0.0000000
## 1980-1920  56698.298   46249.6030  67146.994 0.0000000
## 1990-1920  75681.852   66028.1275  85335.576 0.0000000
## 2000-1920 118478.652  108612.6869 128344.616 0.0000000
## 2010-1920 135722.936  123005.6116 148440.261 0.0000000
## 1940-1930   3450.033   -8845.7833  15745.849 0.9994074
## 1950-1930  21274.421    9451.5558  33097.286 0.0000000
## 1960-1930  48413.107   36496.2608  60329.954 0.0000000
## 1970-1930  56319.568   44512.3245  68126.811 0.0000000
## 1980-1930  47326.482   34985.9265  59667.037 0.0000000
## 1990-1930  66310.035   54634.9137  77985.156 0.0000000
## 2000-1930 109106.835   97255.6187 120958.051 0.0000000
## 2010-1930 126351.119  112038.7329 140663.506 0.0000000
## 1950-1940  17824.388    8685.7546  26963.022 0.0000000
## 1960-1940  44963.074   35703.1764  54222.972 0.0000000
## 1970-1940  52869.535   43751.1208  61987.949 0.0000000
## 1980-1940  43876.449   34077.2762  53675.621 0.0000000
## 1990-1940  62860.002   53913.3301  71806.674 0.0000000
## 2000-1940 105656.802   96481.5196 114832.084 0.0000000
## 2010-1940 122901.087  110711.7924 135090.381 0.0000000
## 1960-1950  27138.686   18516.6937  35760.679 0.0000000
## 1970-1950  35045.147   26575.2873  43515.006 0.0000000
## 1980-1950  26052.060   16853.3201  35250.801 0.0000000
## 1990-1950  45035.614   36750.9303  53320.298 0.0000000
## 2000-1950  87832.414   79301.3617  96363.466 0.0000000
## 2010-1950 105076.698   93364.6561 116788.741 0.0000000
## 1970-1960   7906.460    -694.0979  16507.019 0.1089287
## 1980-1960  -1086.626  -10405.8484   8232.597 1.0000000
## 1990-1960  17896.928    9478.6696  26315.186 0.0000000
## 2000-1960  60693.728   52032.8999  69354.555 0.0000000
## 2010-1960  77938.012   66131.1065  89744.918 0.0000000
## 1980-1970  -8993.086  -18171.7392    185.567 0.0613794
## 1990-1970   9990.467    1728.0929  18252.842 0.0041871
## 2000-1970  52787.267   44277.8784  61296.656 0.0000000
## 2010-1970  70031.552   58335.2796  81727.824 0.0000000
## 1990-1980  18983.553    9975.4937  27991.613 0.0000000
## 2000-1980  61780.353   52545.2027  71015.504 0.0000000
## 2010-1980  79024.638   66790.2155  91259.060 0.0000000
## 2000-1990  42796.800   34471.7073  51121.892 0.0000000
## 2010-1990  60041.084   48478.2020  71603.967 0.0000000
## 2010-2000  17244.285    5503.6238  28984.946 0.0000857
{% endhighlight %}

There seems to be a significant variance in price detected between the decade the home was built in and the TotalMarketValue

We can see that the lower quartile range in the 2010s is close to the top quartile market valued assessed properties in the 1960s, which means there are a denser amount of properties valued in the upper 200,000-400,000 USD range for Stanly County now.

It seems that the newer built homes have a significant difference in TotalMarketValue compared to the previous decades (Specifically homes built after 2000)


{% highlight r %}
ggplot(marketValueByDecade, aes(x=Decade, y=TotalMarketValue)) + 
  geom_boxplot(outlier.colour="red",
                outlier.size=2)
{% endhighlight %}

![plot of chunk unnamed-chunk-23](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-23-1.png)

### Correlations between TotalMarketValue

We can see that the biggest predictors of TotalMarketValue are the land value and square footage of the dwelling(s) total.

We can see that Baths and HVAC Units have a pretty strong correlation with TotalMarketValue (~0.60).

We can also see that Baths and HVAC units in general have a strong correlation which would be expected since home owners with larger/bigger HVAC systems would have to support more bathrooms.

No other features were found that had strong correlations in general.

We can also see certain features provide less return on market value compared to Taxable value which is a negative trait. You would want to have the most market value from upgrading these features compared to your taxable amount they contribute on your property assessment.

Features which have a low return compared to their taxable correlation:
- Baths (0.07 difference)
- Beds (0.05 difference)
- HVACUnits (0.05 difference)


{% highlight r %}
numericVars <- which(sapply(house.data.new, is.numeric))
numericVarNames <- names(numericVars)
all_numVar <- house.data.new[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with TotalMarketValue
cor_sorted <- as.matrix(sort(cor_numVar[,'TotalMarketValue'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3))) # Abs threshold of correlation we want
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
{% endhighlight %}

![plot of chunk unnamed-chunk-24](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-24-1.png)

### Analyzing variable importance

From initial quick random forest we can see important features:
- LandMarketValue
- ListedAcres/ActualAcres
- DistrictCode
- TownshipCode
- QualityGradeCode
- Sqft
- HVACUnits

We can see that the land/acres associated with a property has an enormous value on the TotalMarketValue assessment of the property itself


{% highlight r %}
# Filter exceedingly large factors > 53 categories for a quick RandomForest for variable importance
# Certain features we will use to locate the actual Long/Lat of the property from the actual address
house.data.rfdata <- house.data.new %>% dplyr::select(-Block, -OwnerID, -PrimaryOwner, -TaxPayerCity, -Zip, -PhyStreetName, -MarketAreaCode)

set.seed(2018)
# Ignore TMV (prediction) and DateSold (NAs, irrelevant)
quick_RF <- randomForest(x=house.data.rfdata[1:5000, c(-5, -6, -14, -16, -19)], y=house.data.rfdata$TotalMarketValue[1:5000], ntree=100, importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")
{% endhighlight %}

![plot of chunk unnamed-chunk-25](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-25-1.png)

### Exploring how districtCode affects median TotalMarketValue

Upon further exploration we can see that these properties identified by DistrictCode 548 contain waterfront properties and most are owned by a developer corporation known as EDGEWATER DEVELOPERS INC

Other high-value properties are owned by individuals which are located on Lake Tillery via the town of Norwood.

Other higher than median districts include:
- 550
- 116


{% highlight r %}
ggplot(house.data.rfdata[!is.na(house.data.rfdata$TotalMarketValue),], aes(x=DistrictCode, y=TotalMarketValue)) + geom_bar(stat='summary', fun.y = "median", fill='blue') +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
        geom_hline(yintercept=median(house.data.rfdata$TotalMarketValue), linetype="dashed", color = "red")
{% endhighlight %}

![plot of chunk unnamed-chunk-26](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-26-1.png)

By far the lowest valued district is 520 in which the median market value is $46,876

Upon further exploration it seems that 520 distict is located in the city of Badin near the previous area of the Alcoa Plant as well as employer Electronic Recyclers.

In previous years many homes were built for long standing employees working under factories throughout the U.S. which resulted in many homes being grouped together with the same architecture style and upkeep. However, if the resulting employer goes under these homes are worth a lot less than their previous values.


{% highlight r %}
median(house.data.new[house.data.new$DistrictCode == "520", "TotalMarketValue"])
{% endhighlight %}



{% highlight text %}
## [1] 46876
{% endhighlight %}

We can also see that the following properties are waterfront based on the addresses listed in this district.

District 548 contains high valued properties via Lake Tillery managed under a real estate company known as Edgewater. This makes our dataset highly skewed with these properties. From this point on we will remove these properties from our dataset to be more representative of our actual data.


{% highlight r %}
median(house.data.new[house.data.new$DistrictCode == "548", "TotalMarketValue"])
{% endhighlight %}



{% highlight text %}
## [1] 682354.5
{% endhighlight %}


{% highlight r %}
# Remove outliers
house.data.new <- house.data.new %>% filter(DistrictCode != 548)

# Rexamined Medians post-filter
ggplot(house.data.new[!is.na(house.data.new$TotalMarketValue),], aes(x=DistrictCode, y=TotalMarketValue)) + geom_bar(stat='summary', fun.y = "median", fill='blue') +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
        geom_hline(yintercept=median(house.data.new$TotalMarketValue), linetype="dashed", color = "red")
{% endhighlight %}

![plot of chunk unnamed-chunk-29](/https://dillonmabry.github.io//images/stanly-county-housing-dataunnamed-chunk-29-1.png)

### Exploring "Value" Properties

What are homes with the highest quality codes and critical features with the lowest price? Aka Best valued homes assessed via the County.

Our formula seeks to gain the most features for the lowest market value possible aka the "deals" of the assessed properties

The sum of each column vector for the specified features $X_{ij}$ scaled as a portion of the TotalMarketValue $V$

$\frac{(\sum_{j=1}^n Scale(X_{ij})}{Scale(V)} $


{% highlight r %}
qualityProperties <- house.data.new %>%
   filter(QualityGradeCode.new == "A" | QualityGradeCode.new == "B" | QualityGradeCode.new == "C")

qualityProperties["QualityGradeCode.new.numeric"] <- as.numeric(factor(qualityProperties$QualityGradeCode.new, levels = c("A","B","C","D","E"), labels = c(4, 3, 2, 1, 0)))

qualityProperties <- qualityProperties %>%
  mutate_at(c(2, 14, 29, 30, 31, 38, 21), funs(c(scale(.))))
{% endhighlight %}



{% highlight text %}
## Warning: funs() is soft deprecated as of dplyr 0.8.0
## please use list() instead
## 
##   # Before:
##   funs(name = f(.))
## 
##   # After: 
##   list(name = ~ f(.))
## This warning is displayed once per session.
{% endhighlight %}



{% highlight r %}
qualityProperties["TotalPropertyScore"] <- qualityProperties$Sqft + qualityProperties$ActualAcres +
     qualityProperties$QualityGradeCode.new.numeric +
     qualityProperties$Bedrooms +
     qualityProperties$Baths +
     qualityProperties$HVACUnits

qualityProperties <- transform(qualityProperties, TotalPropertyValueScore = qualityProperties$TotalPropertyScore / qualityProperties$TotalMarketValue)

qualityProperties$TotalPropertyValueScore <- scale(qualityProperties$TotalPropertyValueScore)

# Top 10 "value" based properties, higher score indicates more features for the market price gained
head(qualityProperties[order(-qualityProperties$TotalPropertyValueScore ),], 10)
{% endhighlight %}



{% highlight text %}
##       YearBuilt         Sqft Stories Submap Block CityCode OwnerID
## 17185      2016 -0.807355063    1.00      1    25    LOCUS 1514566
## 7899       1961  2.373274704    1.00      1    38    ALBEM   27991
## 12858      1988  0.947813812    1.00      1    49    STANF   32340
## 8880       1974 -0.009040843    1.00      2    78    ALBEM 1511859
## 13222      2000  0.338906304    1.65      3     0    RICHF 1516855
## 8417       1974  0.498849751    1.00      1    29    OAKBO   25007
## 16386      2003  0.094782096    1.00      3     2    STANF 1497638
## 8026       1930  1.796636489    1.51      4    74    ALBEM  179599
## 4258       1987  0.049885690    1.00      3     4    ALBEM 1512491
## 324        1990 -0.585679058    1.00      1    16    ALBEM 1506001
##                        PrimaryOwner TaxPayerCity       Zip
## 17185               CURTIS FREEDA R       LOCUST     28097
## 7899            HASH RICHARD HOBSON    ALBEMARLE     28001
## 12858              YOW BOYCE EDWARD    STANFIELD     28163
## 8880        RABON MAX D & JOANN LER    ALBEMARLE     28001
## 13222                  DUNN LAYNE E    RICHFIELD     28137
## 8417                 CLARK DONALD W      OAKBORO     28129
## 16386                BARBEE BRENT W    STANFIELD 281630000
## 8026            HARRIS BRYAN DANIEL    ALBEMARLE     28001
## 4258  AMMERMAN ROBERT W & ANITA LER    ALBEMARLE     28001
## 324               ALBERTINES JOHN G    ALBEMARLE     28001
##             PhyStreetName PhyStreetType ListedAcres  ActualAcres
## 17185               BROAD            ST        0.14 -0.304489478
## 7899                US 52           HWY        7.07  0.251058528
## 12858              TUCKER            ST        2.23 -0.143858531
## 8880       MOUNTAIN CREEK            RD        1.00 -0.224565787
## 13222              SUMMER            ST        0.00 -0.266878329
## 8417  LIBERTY HILL CHURCH            RD        2.47 -0.146992794
## 16386          RENEE FORD            RD        4.13  0.005018931
## 8026            WHITE OAK            AV        0.00 -0.279415379
## 4258                 BOST            RD        6.60  0.206395289
## 324                 ARBOR           WAY        0.00 -0.262176936
##       DistrictCode TownshipCode PropertyCode MarketAreaCode
## 17185          550          125            D            305
## 7899           500          101            D           5001
## 12858          570          127            D           5701
## 8880           500          101            D           5001
## 13222          600          104            D           6001
## 8417           117          120            D           9929
## 16386          110          126            D           9925
## 8026           500          101            D           5004
## 4258           113          117            D           9920
## 324            500          101            D             25
##       LandMarketValue LandAssessedValue TotalMarketValue
## 17185           30000             30000    -1.557165e-05
## 7899            43855             43855     5.835917e-04
## 12858           32103             32103     1.680429e-04
## 8880            16000             16000    -2.475058e-04
## 13222           18076             18076     3.130018e-04
## 8417            25922             25922     4.579607e-04
## 16386           44757             44757    -5.567514e-04
## 8026             8825              8825     5.135300e-03
## 4258            52750             52750    -1.658439e-03
## 324             27422             27422    -1.784070e-03
##       TotalAssessedValue DateSold SaleAmount TotalTaxOwed
## 17185             152859     <NA>     165000      1727.31
## 7899              152921     <NA>     130000      2003.26
## 12858             152878     <NA>          0      1666.37
## 8880              152835     <NA>          0      2002.13
## 13222             152893     <NA>     105000      1467.77
## 8417              152908     <NA>          0      1116.22
## 16386             152803     <NA>     150000      1176.58
## 8026              153392     <NA>          0      2009.44
## 4258              152689     <NA>          0      1114.63
## 324               152676     <NA>     169500      2000.06
##       IsMultiSegments QualityGradeCode HVACCompCode    HVACUnits
## 17185            <NA>             B-10           17 -0.726687556
## 7899                Y              C+5           17  0.161095309
## 12858            <NA>              C+5           17 -0.001408298
## 8880             <NA>             B-10           17  0.246623524
## 13222            <NA>              C+5           17  0.670843467
## 8417             <NA>              C+5           17  0.865847796
## 16386               Y                C           17 -1.227882892
## 8026             <NA>             C+10           17  2.448119761
## 4258                Y                C           17 -0.228913348
## 324              <NA>             C+10           17 -0.456418398
##            Baths   Bedrooms PctComplete IsFirePlace IsBasement
## 17185  0.1134955 -1.4739623         100           N          N
## 7899   0.1134955  1.6201937         100           Y          N
## 12858  0.1134955  0.0731157         100           N          N
## 8880   0.1134955  0.0731157         100           Y          N
## 13222  0.8588791  0.0731157         100           Y          N
## 8417  -0.6318881  0.0731157         100           Y          N
## 16386 -0.6318881  0.0731157         100           Y          N
## 8026   1.6042627  1.6201937         100           Y          N
## 4258  -1.3772717 -1.4739623         100           Y          N
## 324    0.1134955 -1.4739623         100           Y          N
##       IsGarageAttached YearBuilt.new QualityGradeCode.new
## 17185                N    2016-01-01                    B
## 7899                 N    1961-01-01                    C
## 12858                N    1988-01-01                    C
## 8880                 N    1974-01-01                    B
## 13222                N    2000-01-01                    C
## 8417                 N    1974-01-01                    C
## 16386                N    2003-01-01                    C
## 8026                 N    1930-01-01                    C
## 4258                 N    1987-01-01                    C
## 324                  N    1990-01-01                    C
##       QualityGradeCode.new.numeric TotalPropertyScore
## 17185                   -2.0259639          -5.224963
## 7899                     0.4142379           4.933356
## 12858                    0.4142379           1.403396
## 8880                    -2.0259639          -1.826336
## 13222                    0.4142379           2.089104
## 8417                     0.4142379           1.073170
## 16386                    0.4142379          -1.272616
## 8026                     0.4142379           7.604035
## 4258                     0.4142379          -2.409629
## 324                      0.4142379          -2.250503
##       TotalPropertyValueScore
## 17185             128.7899901
## 7899                3.2385048
## 12858               3.1993440
## 8880                2.8260735
## 13222               2.5556383
## 8417                0.8931918
## 16386               0.8710906
## 8026                0.5620767
## 4258                0.5514103
## 324                 0.4779017
{% endhighlight %}

