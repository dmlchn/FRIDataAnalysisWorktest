#Install required packages if non installed yet

if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")

#Load required packages

library(tidyr)
library(lubridate)
library(readr)
library(dplyr)


#Load in datasets

prediction_sets <- read_csv('rct-a-prediction-sets.csv')
qa_pairs <- read_csv('rct-a-questions-answers.csv')


#Rename column names with underscores for easier handling

names(qa_pairs) <- gsub("\\s+", "_", names(qa_pairs))
names(prediction_sets) <- gsub("\\s+", "_", names(prediction_sets))

#Select last forecasts per question-answer

ind_forecasts <- prediction_sets %>% 
  arrange(updated_at) 

ind_forecasts$updated_date <- as_date(ind_forecasts$updated_at)


latest_forecasts <- ind_forecasts %>% 
  group_by(discover_question_id,answer_sort_order,membership_guid) %>% 
  slice_tail(n=1)

#Calculate 'most recent' methods per question-answer

agg_forecasts_most_recent <- latest_forecasts %>% 
  group_by(discover_question_id,answer_sort_order) %>%
  mutate(forecasted_probability = case_when(forecasted_probability == 0 ~ 0.001,
                                            TRUE ~forecasted_probability)) %>% 
  summarize(mean_most_recent_forecast = mean(forecasted_probability),
            median_most_recent_forecast = median(forecasted_probability),
            geometric_mean_most_recent_forecast = exp(mean(log(forecasted_probability))))

#Calculate trimmed mean per question-answer

agg_forecasts_trim <- ind_forecasts %>% 
  group_by(discover_question_id,answer_sort_order) %>%
  filter(forecasted_probability >= quantile(forecasted_probability,0.1),
         forecasted_probability <= quantile(forecasted_probability,0.9)) %>% 
  summarise(trim_mean = mean(forecasted_probability))

#Calculate geom_odds per question-answer including extremized geom_odds

agg_forecasts_geom_odds <- ind_forecasts %>% 
  mutate(odds = case_when(forecasted_probability == 0 ~ 0.01,
                          forecasted_probability == 1 ~ 99,
                          TRUE ~forecasted_probability/(1-forecasted_probability))) %>% 
  group_by(discover_question_id,answer_sort_order) %>%
  summarise(geom_odds_mean = prod(odds)^(1/length(odds)),
            extrem_geom_odds_mean = geom_odds_mean ^ 2.5) %>% 
  mutate(prob_geom_odds_mean = replace_na(geom_odds_mean / (1 + geom_odds_mean),1),
         extrem_prob_geom_odds_mean = replace_na(extrem_geom_odds_mean/(1+extrem_geom_odds_mean),1)) %>% 
  select(-geom_odds_mean,-extrem_geom_odds_mean) 

#Combine previous methods in one dataframe
agg_forecasts <- agg_forecasts_most_recent %>% 
  left_join(agg_forecasts_trim) %>% 
  left_join(agg_forecasts_geom_odds)

#Join forecasts with dataset which shows whether they resolved true or false
resolved_questions <- qa_pairs %>% 
  filter(question_status != "voided") %>% 
  select(discover_question_id,answer_sort_order,answer_resolved_probability) %>% 
  inner_join(agg_forecasts) 

#Calculate Brier scores for different methods
brier_scores <- resolved_questions %>% 
  mutate(across(mean_most_recent_forecast:extrem_prob_geom_odds_mean, ~(. - answer_resolved_probability)^2))

brier_scores <- brier_scores %>% 
  summarise(across(mean_most_recent_forecast:extrem_prob_geom_odds_mean,sum)) %>% 
  mutate(across(everything(), ~./nrow(resolved_questions)))

#Select top half of forecaster by Brier score on non-latest forecast

subset_trial <- qa_pairs %>% 
  filter(question_status != "voided") %>% 
  select(discover_question_id,answer_sort_order,answer_resolved_probability) %>% 
  inner_join(latest_forecasts)

test_df <- setdiff(ind_forecasts, latest_forecasts)

brier_by_forecaster <- test_df %>% 
  group_by(membership_guid) %>% 
  summarise(brier = sum((final_probability - answer_resolved_probability)^2)/n()) %>% 
  filter(brier != 0) 

top_half_forecaster <- brier_by_forecaster %>% 
  slice_min(brier, n = nrow(brier_by_forecaster)/2)

#Calculate top half forecaster Brier score on latest forecasts

latest_forecasts_top_forecaster <- semi_join(subset_trial,top_half_forecaster) %>%
  summarise(brier = sum((final_probability - answer_resolved_probability)^2)/n()) %>% 
  rename(top_half_forecaster = brier)

#Suggested improvements:

#remove solved after anwers known
#improve runtime (make dataframes smaller)
#write function to calculate brier score