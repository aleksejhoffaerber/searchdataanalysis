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

sdat <- character_dates %>% 
  full_join(numeric_dates) %>% 
  arrange(ID) %>% 
  select(-datestring, -ID) %>% 
  relocate(date, .before = "advertID") %>% 
  mutate(id = seq(1:nrow(sdat)))

# how to handle zeroes
# assumption: zero impressions, clicks etc. are handled as ineffective campaigns with no result
# consequence: they will be included and set to zero (for CTR, CR calculations)

# GENERAL DATA OVERVIEW
# TODO: Should I include a general data overview?
# TODO: How to motivate a good Appendix part

sdat_longer <- sdat %>% 
  select_if(is.numeric) %>% 
  select(-advertID) %>% 
  pivot_longer(cols = c(impressions, clicks, bidprice, conversions, numberofwords, 
                        retailer, brandname, adQuality, landQuality, revenue, adrank),
    names_to = "variables",
    values_to = "value") 

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

  
# adranks should not include zeroes (no rank, meaning not a listed or sold ad)
# bidprice of zero is strange because it indicates a free ad (if it is looked at from an individual POV)
# --> check how bidproces are affected by campaign structure

# adQuality and landQuality should also not include any zero values (seem to be wrong)

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

# COMMONALITIES BETWEEN STANGE OUTLIERS ------

sdat %>% 
  filter(adrank == 0 | 
           adQuality == 0 | landQuality == 0) %>% 
  summarise(mean_clicks = mean(clicks),
            mean_impressions = mean(impressions),
            mean_adrank = mean(adrank))


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

sdat <- sdat %>% 
  anti_join(aj)

metrics_mean <- sdat %>% 
  summarise(mean_CTR = mean(CTR),
            mean_CR = mean(CR))

# BCGR: ID per different paid search ad campaign

# 1) group per AdvertID ------

sdat_advertID <- sdat %>% 
  group_by(advertID) %>% 
  summarise(mean_CTR = mean(CTR),
            mean_CR = mean(CR))

metrics_mean_advertID <- sdat_advertID %>% 
  summarise(mean_CTR = mean(mean_CTR),
            mean_CR = mean(mean_CR))

p_mean_CTR <- sdat_advertID %>% 
  ggplot() +
  geom_histogram(aes(mean_CTR), fill = "royalblue") +
  labs(x = "CTR",
       y = "Count",
       title = "Average CTR per Campaign") +
  geom_vline(xintercept = metrics_mean_advertID[,"mean_CTR"],
             size = 1) +
  theme_minimal()

p_mean_CR <- sdat_advertID %>% 
  ggplot() +
  geom_histogram(aes(mean_CR), fill = "orange4") +
  labs(x = "CR",
       y = "Count",
       title = "Average CR per Campaign") +
  geom_vline(xintercept = metrics_mean_advertID[,"mean_CR"],
             size = 1) +
  theme_minimal()

p_mean_CTR / p_mean_CR

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
       subtitle = "Strong concentration of ads with CTR until 20%, current ads perform better than the average",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 05.10.2020)") +
  theme_bw()
  

p_CR <- sdat %>% 
  ggplot() +
  geom_histogram(aes(CR), fill = "orange2", bins = 100) +
  geom_vline(aes(xintercept = metrics_mean[,"mean_CR"], color = "Cloverleaf"), size = 1) +
  annotate(geom = "label", x = metrics_mean[[2]], y = 275.0, label = round(metrics_mean[[2]],2), colour = "#00BFC4") +
  geom_vline(aes(xintercept = 2.81, color = "Benchmark"),size = 1) +
  annotate(geom = "label", x = 2.81, y = 260, label = 2.81, colour = "#F8766D") +
  
  labs(x = "CR [in percent]",
       y = "Count",
       colour = "Mean CR",
       title = "Benchmark Cloverleaf CTR per Ad-Format vs. Google Search Average",
       subtitle = "Strong concentration of ads with CR around 0%, still better than benchmark",
       caption = "Source: Cloverleaf Search Data & WordStream Benchmark (from 01.05.2020)") +
  theme_bw()

p_CTR / p_CR

ggsave("04_hist.png", width = 9, height = 10)


# TASK 1.2: CORRELATION COEFFICIENTS
# check data by description and smoothing function
p_corr_CTR <- 
  sdat %>% 
  ggplot(aes(adrank, CTR)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess", colour = "#00BFC4") +
  
  labs(title = "Correlation between CTR and Adrank",
       subtitle = "The better (higher) the adrank, the higher the CTR",
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
       subtitle = "No clear correlation indication based on the data for CR",
       x = "Adrank",
       y = "Conversion Rate (CR)",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") + 
  coord_cartesian(ylim = c(-20,100)) +
  theme_bw()

p_corr_CTR / p_corr_CR

ggsave("05_visualcorrelation.png", width = 8, height = 6)


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

ggsave("06_spearmancorrelation.png", width = 8, height = 6)


# TASK 1.3: COMPUTE ROI

# costs not part of profit margin
sdat_roi_cost <- sdat %>% 
  mutate(costs = bidprice * clicks,
         profit = (revenue * 0.035) - costs,
         roi = (profit/costs) * 100) %>%
    filter(clicks > 0 & bidprice > 0) 

sdat_roi_cost %>% 
  summarise(mean(roi))

# costs part of the profit margin
sdat_roi <- sdat %>% 
  mutate(costs = bidprice * clicks,
         profit = (revenue * 0.035),
         roi = (profit/costs) * 100) %>%
  filter(clicks > 0 & bidprice > 0) 

metrics_roi <- sdat_roi %>% 
  summarise(mean(roi))

# TODO: Analyze how many observations are thrown out (and by how much the average changes)
# Do not think this is necessary, too many details. Many part of the Appendix

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
  geom_point() +
  geom_smooth(method = "loess", colour = "#00BFC4") +
  labs(title = "Relationship between ROI and Revenue (averaged by campaign)",
       subtitle = "Highest campaign ROIs were achieved on the lower end of the Revenue scale",
       x = "Sum of Revenue per Campaign",
       y = "Average ROI per Campaign",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  theme_bw()

# TASK 2.1: FACTORIAL PLOT FOR CR
# FACTORIAL DESIGN APPROACH -----

sdat_factorial <- sdat %>% 
  select(CR, numberofwords, retailer) %>% 
  filter(numberofwords == 2 | numberofwords == 3) %>% 
  mutate(numberofwords = factor(numberofwords),
         retailer = factor(retailer)) 

# ANOVA SPECIFIC MODELLING ------

interaction.plot(x.factor = sdat_factorial$numberofwords, 
                 trace.factor = sdat_factorial$retailer, 
                 response = sdat_factorial$CR, 
                 data = sdat_factorial,
                 main = "Factorial Plot of CR using 2 Factors (Retailer Name & Number of Keywords)",
                 xlab = "Number of Keywords",
                 ylab = "Conversion Rate",
                 trace.label = "Ret. Name")


# DPLYR BASED ATTEMPTS ------

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
  annotate(geom = "label", x = 0.6, y = 5.0, label = "Ø CR", colour = "gray60") +
  
  geom_label(vjust = -0.5) +
  geom_point(pch = 19) +
  labs(title = "Factorial Plot of CR using 2 Factors (Retailer Name & Number of Keywords)",
       subtitle = "Number of Keywords have a considerable effect on CR, especially if combined with keywords \ncontaining the retailer name, leading to a change in the slope",
       x = "Number of Keywords",
       y = "Conversion Rate",
       colour = "Retailer Name",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  coord_cartesian(xlim = c(1,2),
                  ylim = c(0,12)) +
  theme_bw()

# QUESTION 2.2: CTR BASED ON CASES (RETAILER NAME; BRAND; KEYWORD LENGTH)

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
    annotate(geom = "label", x = 0.65, y = 12.0, label = "Ø CTR", colour = "gray60") +
  
    geom_label(vjust = -0.5)+
    geom_point(pch = 19) +
    labs(title = "Factorial Plot of CTR with 3 Factors (Retailer Name, Number of Keywords & Brand Name)",
         subtitle = "Interaction Plots split by Brand Name \nEffect of Retailer Name in general positive, but strongly negative if Brand Name is included",
         x = "Number of Keywords",
         y = "Click-Through Rate",
         colour = "Retailer Name",
         caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  facet_wrap(~brandname) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0,45)) +
    theme_bw()

# QUESTION 2.3: ROI BASED ON CASES (RETAILER NAME; BRAND; KEYWORD LENGTH)
# TODO: Why just one observation for brand name = 0, retailer name = 1

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
  annotate(geom = "label", x = 0.65, y = 65.0, label = "Ø ROI", colour = "gray60") +
  
  geom_label(position = position_dodge(width = 0.8, preserve = c("total"))) +
  geom_point(pch = 19) +
  labs(title = "Factorial Plot of CTR with 3 Factors (Retailer Name, Number of Keywords & Brand Name)",
       subtitle = "Interaction Plots split by Brand Name \nEffect of Retailer Name in general positive, but strongly negative if Brand Name is included",
       x = "Number of Keywords",
       y = "Click-Through Rate",
       colour = "Retailer Name",
       caption = "Source: Cloverleaf Search Data (16.01.2012 to 03.12.2012)") +
  facet_wrap(~brandname) +
  theme_bw()




