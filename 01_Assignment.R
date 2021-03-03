# libraries 
library(xlsx) # excel import
library(dplyr) # data data preprocessing and data wrangling
library(corrr) # simple correlation analysis
library(tsibble) # dealing with time-series tibbles
library(ggplot2) # general plotting
library(patchwork) # side-by-side plotting
library(AlgDesign) # first stage interaction plot 
library(tidyverse) # advanced data wrangling

# load the data 
sdat <- read.xlsx("cloverleaf_search_data.xlsx", 
                  sheetIndex = 1, 
                  as.data.frame = T) %>% 
  mutate(ID = seq(1:nrow(.)))

# transform to homogeneous date formats
character_dates <- sdat %>% 
  filter(grepl("/", datestring)) %>% 
  mutate(date = as.Date(datestring, format = "%m/%d/%Y"))

numeric_dates <- sdat %>% 
  filter(!grepl("/", datestring)) %>% 
  mutate(date = as.Date(as.numeric(datestring), origin = "1899-12-30"))

# combine data sources
sdat <- character_dates %>% 
  full_join(numeric_dates) %>% 
  arrange(ID) %>% 
  select(-datestring, -ID) %>% 
  relocate(date, .before = "advertID") %>% 
  mutate(id = seq(1:nrow(sdat)))


# ADVANCED PRE-PROCESSING
# transform to long data format to facilitate comprehensive analysis
sdat_longer <- sdat %>% 
  select_if(is.numeric) %>% 
  select(-advertID) %>% 
  pivot_longer(cols = c(impressions, clicks, bidprice, conversions, numberofwords, 
                        retailer, brandname, adQuality, landQuality, revenue, adrank),
    names_to = "variables",
    values_to = "value") 

# check distribution for all variables
sdat_longer %>% 
  ggplot(aes(value)) +
  stat_density() + 
  labs(title = "Distribution of all Variables",
       x = "Values",
       y = "Density",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 01.05.2020)") +
  facet_wrap(~variables, scales = "free") +
  theme_bw()

ggsave("01_app_variables.png", width = 8, height = 6)


# check distribution for quality and rank variables
sdat_longer %>% 
  filter(variables %in% c("adQuality", "landQuality", "adrank", "bidprice")) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 60) + 
  labs(title = "Close Look into Quality, Rank, and Price Variables",
       subtitle = "Quality and rank variables seem to include zeroes, indicating possible data errors",
       x = "Values",
       y = "Count",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 01.05.2020)") +
    facet_wrap(~variables, nrow = 2, scales = "free") +
  scale_x_continuous(n.breaks = 6) +
  theme_bw()

ggsave("02_app_variables.png", width = 8, height = 6)

# check distribution for clicks and impression
sdat_longer %>% 
  filter(variables %in% c("clicks", "impressions")) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 300) +
  labs(title = "Close Look into Clicks and Impressions",
       subtitle = "High concentration of zero clicks and impressions that may impair further calculations",
       x = "Values",
       y = "Count",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 01.05.2020)") +
  
  facet_wrap(~variables, nrow = 2, scales = "free") +
  scale_x_continuous(n.breaks = 6) +
  theme_bw()

ggsave("03_app_variables.png", width = 8, height = 6)

# TASK 1.1: CTR AND CR ANALYSIS
# overall CTR and CR
sdat <- sdat %>% 
  mutate(CTR = ifelse(is.nan(clicks/impressions), # correct for zero cases
                      0, 
                      (clicks/impressions)) * 100,
         CR = ifelse(is.nan(conversions/clicks), # correct for zero cases
                     0, 
                     (conversions/clicks)) * 100)

# delete entries for which the identified criterias hold true
aj <- sdat %>% 
  filter(clicks == 0 & impressions == 0 | adrank == 0) 

# delete data that may lead to bias
sdat <- sdat %>% 
  anti_join(aj)

metrics_mean <- sdat %>% 
  summarise(mean_CTR = mean(CTR),
            mean_CR = mean(CR))

# 2) no grouping at all ------

p_CTR <- sdat %>% 
  ggplot() +
  geom_histogram(aes(CTR), fill = "royalblue", bins = 100) +
  geom_vline(aes(xintercept = metrics_mean[,"mean_CTR"], color = "Cloverleaf"), size = 1) +
  annotate(geom = "label", x = metrics_mean[[1]], y = 75.0, label = round(metrics_mean[[1]],2), colour = "#00BFC4") +
  
  geom_vline(aes(xintercept = 2.69, color = "Benchmark"), size = 1) + 
  annotate(geom = "label", x = 2.69, y = 75.0, label = 2.69, colour = "#F8766D") +
  
  labs(x = "CTR [in percent]",
       y = "Count",
       colour = "Mean CTR",
       title = "Benchmark Cloverleaf CTR per Ad-Format vs. Google Search Average",
       subtitle = "Strong concentration of ads with CTR until 10%, current ads perform better than the average",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 05.10.2020)") +
  theme_bw()
  

p_CR <- sdat %>% 
  ggplot() +
  geom_histogram(aes(CR), fill = "orange2", bins = 100) +
  geom_vline(aes(xintercept = 2.81, color = "Benchmark"),size = 1) +
  annotate(geom = "label", x = 2.81, y = 260, label = 2.81, colour = "#F8766D") +
  geom_vline(aes(xintercept = metrics_mean[,"mean_CR"], color = "Cloverleaf"), size = 1) +
  annotate(geom = "label", x = metrics_mean[[2]], y = 220.0, label = round(metrics_mean[[2]],2), colour = "#00BFC4") +
  
  labs(x = "CR [in percent]",
       y = "Count",
       colour = "Mean CR",
       title = "Benchmark Cloverleaf CTR per Ad-Format vs. Google Search Average",
       subtitle = "Strong concentration of ads with CR around 0%, still better than benchmark",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 01.05.2020)") +
  theme_bw()

p_CTR / p_CR

ggsave("04_hist.png", width = 9, height = 5)


# TASK 1.2: CORRELATION COEFFICIENTS
# check data by description and smoothing function
p_corr_CTR <- 
  sdat %>% 
  ggplot(aes(adrank, CTR)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess", colour = "#00BFC4") +
  
  labs(title = "Correlation between CTR and Adrank",
       subtitle = "The better the adrank, the higher the CTR",
       x = "Adrank",
       y = "Click-Through Rate (CTR)",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  coord_cartesian(ylim = c(-20,100)) +
  theme_bw()

p_corr_CR <- 
  sdat %>% 
  ggplot(aes(adrank, CR)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess", colour = "#00BFC4") +
  
  labs(title = "Correlation between CR and Adrank",
       subtitle = "No clear correlation indication for CR",
       x = "Adrank",
       y = "Conversion Rate (CR)",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") + 
  coord_cartesian(ylim = c(-20,100)) +
  theme_bw()

p_corr_CTR + p_corr_CR

ggsave("05_visualcorrelation.png", width = 8, height = 3)


# compute actual spearman correlation coefficients
sdat %>% 
  select(adrank, CTR, CR) %>% 
  correlate(method = "spearman") %>% 
  rearrange() %>%
  shave() %>% 
  focus(adrank) %>% 
  mutate(term = reorder(term, adrank)) %>% 
  mutate(adrank = adrank * (-1)) %>%
  ggplot(aes(adrank, term)) +
  geom_col() +
  geom_label(aes(label = round(adrank, 3))) +
  labs(title = "Ordinality-Adjusted Correlation Analysis for CTR, CR, and Adrank",
       subtitle = "Strong positive correlation between adrank and CTR (the higher/better the rank, the higher the CTR), \nweaker positive correlation between adrank and CR  (the higher/better the rank, the higher the CR)",
       x = "Correlation Coefficient",
       y = "Term") +
  coord_cartesian(xlim = c(-1,1)) +
  theme_bw()

ggsave("06_spearmancorrelation.png", width = 8, height = 3)


# TASK 1.3: COMPUTE ROI

# costs part of the profit margin
sdat_roi <- sdat %>% 
  mutate(costs = bidprice * clicks,
         profit = (revenue * 0.035),
         roi = (profit/costs) * 100) %>%
  filter(clicks > 0 & bidprice > 0) 

metrics_roi <- sdat_roi %>% 
  summarise(mean(roi))

# how many observations are deleted for ROI calculation 
nrow(sdat) - nrow(sdat_roi)

sdat_roi_summary <- sdat_roi %>% 
  group_by(advertID) %>%
  summarise(mean_roi = mean(roi),
            sum_revenue = sum(revenue),
            sum_profit = sum(profit)) %>% 
  mutate(scale = sum_revenue/sum(sum_revenue),
         wmean_roi = scale * mean_roi) %>% 
  arrange(desc(mean_roi))

# mean roi accross all campaigns
sdat_roi_summary %>% 
  summarise(mean_roi_all_campaigns = mean(mean_roi))

# relationship between roi and revenue
sdat_roi_summary %>% 
  ggplot(aes(sum_revenue, mean_roi)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "loess", colour = "#00BFC4") +
  labs(title = "Relationship between ROI and Revenue (averaged by campaign)",
       subtitle = "Highest campaign ROIs were achieved on the lower end of the Revenue scale",
       x = "Sum of Revenue per Campaign",
       y = "Average ROI per Campaign",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  theme_bw()

ggsave("07_ROI+RevenueRelationship.png", width = 8, height = 4)

# TASK 2.1: FACTORIAL PLOT FOR CR
# FACTORIAL DESIGN APPROACH (NOT USED) -----

sdat_factorial <- sdat %>% 
  select(CR, numberofwords, retailer) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer)) 

# ANOVA SPECIFIC MODELLING (NOT USED) ------

interaction.plot(x.factor = sdat_factorial$numberofwords, 
                 trace.factor = sdat_factorial$retailer, 
                 response = sdat_factorial$CR, 
                 data = sdat_factorial,
                 main = "Factorial Plot of CR using 2 Factors (Retailer Name & Number of Keywords)",
                 xlab = "Number of Keywords",
                 ylab = "Conversion Rate",
                 trace.label = "Ret. Name")


# DPLYR BASED INTERACTION PLOTS (USED) ------

# observations per group (Appendix C)

sdat %>% 
  select(CR, numberofwords, retailer, id) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer)) %>% 
  group_by(retailer, numberofwords) %>% 
  count()

# create summary statistics
sdat %>% 
  select(CR, numberofwords, retailer) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer)) %>% 
  group_by(retailer, numberofwords) %>% 
  summarise(mean_CR = round(mean(CR),3)) %>% 
  
  # bring to ggplot
  ggplot(aes(numberofwords, mean_CR, colour = retailer, label = mean_CR)) +
  geom_line(aes(group = retailer), size = 1) +
  
  geom_hline(aes(yintercept = metrics_mean[[2]]), linetype = "dashed", colour = "gray60") +
  annotate(geom = "label", x = 0.6, y = 5.8, label = "Ø CR", colour = "gray60") +
  
  geom_label(vjust = -0.5) +
  geom_point(pch = 19) +
  labs(title = "Factorial Plot of CR using 2 Factors (Retailer Name & Number of Keywords)",
       subtitle = "Number of Keywords have a considerable effect on CR, especially if combined with keywords \ncontaining the retailer name, leading to a change in the slope",
       x = "Number of Keywords",
       y = "Conversion Rate",
       colour = "Retailer Name",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  coord_cartesian(xlim = c(1,2),
                  ylim = c(0,14)) +
  theme_bw()

ggsave("08_FactorialPlotCR.png", width = 8, height = 4)


# QUESTION 2.2: CTR BASED ON CASES (RETAILER NAME; BRAND; KEYWORD LENGTH)

# observations per group (Appendix C)
sdat %>% 
  select(numberofwords, retailer, brandname, CTR) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer),
         brandname = factor(brandname)) %>% 
  group_by(retailer, brandname, numberofwords) %>%
  count()


# create summary statistics
sdat %>% 
  select(numberofwords, retailer, brandname, CTR) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer),
         brandname = factor(brandname)) %>% 
  group_by(retailer, brandname, numberofwords) %>% 
  summarise(mean_CTR = round(mean(CTR),3)) %>% 

# to ggplot
  ggplot(aes(numberofwords, mean_CTR, colour = retailer, label = mean_CTR)) +
    geom_line(aes(group = retailer), size = 1) +
    
    geom_hline(aes(yintercept = metrics_mean[[1]]), linetype = "dashed", colour = "gray60") +
    annotate(geom = "label", x = 0.65, y = 13.0, label = "Ø CTR", colour = "gray60") +
  
    geom_label(vjust = -0.5, hjust = -0.3)+
    geom_point(pch = 19) +
    labs(title = "CTR Factorial Plot (split by Brand Name)",
         subtitle = "Effect of Retailer Name in general positive, but strongly negative if Brand Name is included",
         x = "Number of Keywords",
         y = "Click-Through Rate",
         colour = "Retailer Name",
         caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  facet_wrap(~brandname) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0,45)) +
    theme_bw()

ggsave("09_FactorialPlotCTR.png", width = 8, height = 4)


# QUESTION 2.3: ROI BASED ON CASES (RETAILER NAME; BRAND; KEYWORD LENGTH)

# observations per group (Appendix C)
sdat_roi %>% 
  select(numberofwords, retailer, brandname, roi) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer),
         brandname = factor(brandname)) %>% 
  group_by(retailer, brandname, numberofwords) %>%
  count()

# summary statistics
sdat_roi %>% 
  select(numberofwords, retailer, brandname, roi) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer),
         brandname = factor(brandname)) %>% 
  group_by(retailer, brandname, numberofwords) %>% 
  summarise(mean_roi = round(mean(roi),1)) %>% 
  
  # to ggplot
  ggplot(aes(numberofwords, mean_roi, colour = retailer, label = mean_roi)) +
  geom_line(aes(group = retailer), size = 1) +
  
  geom_hline(aes(yintercept = metrics_roi[[1]]), linetype = "dashed", colour = "gray60") +
  annotate(geom = "label", x = 0.65, y = 75.0, label = "Ø ROI", colour = "gray60") +
  
  geom_hline(yintercept = 100, colour = "red4") +
  annotate(geom = "label", x = 0.75, y = 130, label = "Breakeven", colour = "red4") +
  
  geom_label(position = position_dodge(width = 0.8, preserve = c("total"))) +
  geom_point(pch = 19) +
  
  
  labs(title = "ROI Factorial Plot (split by Brand Name)",
       subtitle = "Effect of Retailer Name in general positive, but strongly negative if Brand Name is included",
       x = "Number of Keywords",
       y = "Click-Through Rate",
       colour = "Retailer Name",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  facet_wrap(~brandname) +
  theme_bw()

ggsave("10_FactorialPlotROI.png", width = 8, height = 5)

## APPENDIX ------

# CHECKING IF SAME STRATEGY WOULD BE PROPOSED FOR CR WITH SAME SET-UP

sdat %>% 
  select(numberofwords, retailer, brandname, CR) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer),
         brandname = factor(brandname)) %>% 
  group_by(retailer, brandname, numberofwords) %>% 
  summarise(mean_CR = round(mean(CR),1)) %>% 
  
  # to ggplot
  ggplot(aes(numberofwords, mean_CR, colour = retailer, label = mean_CR)) +
  geom_line(aes(group = retailer), size = 1) +
  
  geom_hline(aes(yintercept = metrics_mean[[2]]), linetype = "dashed", colour = "gray60") +
  annotate(geom = "label", x = 0.65, y = 6.0, label = "Ø CR", colour = "gray60") +
  
  geom_label(vjust = -0.5, hjust = -0.3)+
  geom_point(pch = 19) +
  labs(title = "CTR Factorial Plot (split by Brand Name)",
       subtitle = "Effect of Retailer Name in general positive, but strongly negative if Brand Name is included",
       x = "Number of Keywords",
       y = "Click-Through Rate",
       colour = "Retailer Name",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  facet_wrap(~brandname) +
  coord_cartesian(xlim = c(1,2),
                  ylim = c(0,20)) +
  theme_bw()


ggsave("11_FactorialPlotCR2.png", width = 8, height = 4)

