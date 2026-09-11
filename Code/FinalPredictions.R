## MAKING SURE VAR NAMES ARE ALIKE AND DATA IS FORMATTED PROPERLY

model_vars <- setdiff(colnames(train_ranger_final2_trimmed), "Contract_Term_Class")
model_vars2 <- setdiff(colnames(train_ranger_final2_trimmed), "Contract_Term_Class")

# Compare with prediction dataset
missing_vars <- setdiff(model_vars, colnames(COLLAPSED_2025_filtered))
extra_vars <- setdiff(colnames(COLLAPSED_2025_filtered), model_vars)


missing_vars <- setdiff(model_vars, colnames(COLLAPSED_2025_filtered))
extra_vars <- setdiff(colnames(COLLAPSED_2025_filtered), model_vars)


print("Missing variables:")
print(missing_vars)

print("Missing variables:")
print(missing_vars)

print("Extra variables:")
print(extra_vars)

print("Extra variables:")
print(extra_vars)

###########
# MAKING FIXES FOR TERM MODEL
##########

COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  rename(
    GP_wgt = gp_wgt,
    PIM_wgt = pim_wgt,
    EVG_per_60_wgt = evg_per_60_wgt,
    SPCT_wgt = spct_wgt,
    GWG_per_60_wgt = gwg_per_60_wgt
  )

COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(
    Age_x_G_per_60 = Age * EVG_per_60_wgt,
    ATOI_x_GameScore = ATOI_wgt * gameScore_wgt,
    PrevLength_x_Age = prev_Length * Age,
    PenaltyDrawn_x_GameScore = penalityMinutesDrawn_per_60_wgt * gameScore_wgt
  )

COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(Age_Over_32 = ifelse(Age >= 32, 1, 0))

ranger_data_all = ranger_data_all %>%
  mutate(Age_Over_32 = ifelse(Age >= 32, 1, 0))






#######
# CONTRACT TERM FINAL MODEL
#######


ranger_data_all <- bind_rows(train_ranger_final2_trimmed, test_ranger_final2_trimmed)

ranger_data_all = ranger_data_all %>% select(-Predicted_Contract_Term)




ranger_model_final <- ranger(
  Contract_Term_Class ~ ., 
  data = ranger_data_all,
  num.trees = 500,
  probability = TRUE,
  classification = TRUE,
  importance = "impurity"
)


library(dplyr)
library(ranger)

# Step 1: Add the Age_Over_32 variable to your prediction dataset
COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(Age_Over_32 = ifelse(Age >= 32, 1, 0))

# Step 2: Get the full list of model variables (excluding target and unused helper columns)
model_vars_final <- setdiff(colnames(ranger_data_all), c("Contract_Term_Class", "Predicted_Contract_Term"))

# Step 3: Select those variables from your 2025 prediction dataset
COLLAPSED_2025_term_input <- COLLAPSED_2025_filtered %>%
  select(all_of(model_vars_final))

# Step 4: Predict class probabilities with the final model
term_probs_2025 <- predict(ranger_model_final, data = COLLAPSED_2025_term_input)$predictions

# Step 5: Convert probabilities to final class predictions (most likely class)
predicted_term_classes <- colnames(term_probs_2025)[apply(term_probs_2025, 1, which.max)]
# Bind predictions
COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(Predicted_Contract_Term = predicted_term_classes)


COLLAPSED_2025_filtered <- bind_cols(COLLAPSED_2025_filtered, term_probs_2025)

library(purrr)



COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(Original_Term = Predicted_Contract_Term) %>%
  mutate(
    Predicted_Contract_Term = case_when(
      Age >= 34 & Original_Term == "Long" ~ pmap_chr(list(Short, Medium), function(short, medium) {
        probs <- c(Short = short, Medium = medium)  # Only consider Short or Medium
        sorted_terms <- names(sort(probs, decreasing = TRUE))
        sorted_terms[1]  # highest of allowed terms
      }),
      Age >= 36 & Original_Term == "Medium" ~ pmap_chr(list(Short), function(short) {
        probs <- c(Short = short)
        sorted_terms <- names(sort(probs, decreasing = TRUE))
        sorted_terms[1]
      }),
      TRUE ~ Predicted_Contract_Term
    )
  )

COLLAPSED_2025_filtered = COLLAPSED_2025_filtered %>% select(-Original_Term
                                                             )










########
# AAV FINAL MODEL
########


## contract term needs to be added back to the data


contract_term_all <- c(TRAIN_AAV$Contract_Term_Class, TEST_AAV$Contract_Term_Class)  # or wherever you get these from

contract_term_all <- case_when(
  contract_length_all %in% c(1, 2) ~ "Short",
  contract_length_all %in% c(3, 4) ~ "Medium",
  contract_length_all %in% c(5, 6, 7) ~ "Long"
)

# Add this term to your training data
cubist_all_x$Predicted_Contract_Term <- as.factor(contract_term_all)



library(Cubist)
library(caret)
cubist_all_x <- bind_rows(cubist_train_x_trimmed, cubist_test_x_trimmed)
cubist_all_y <- c(cubist_train_y_trimmed, cubist_test_y_trimmed)

# Step 2: Add Age_Over_32 column
cubist_all_x <- cubist_all_x %>%
  mutate(Age_Over_32 = ifelse(Age >= 32, 1, 0))

# Step 3: Re-train Cubist model with best hyperparameters
set.seed(42)
cubist_model_final <- train(
  x = cubist_all_x,
  y = cubist_all_y,
  method = "cubist",
  trControl = trainControl(method = "none"),
  tuneGrid = cubist_model_trimmed$bestTune,
  metric = "RMSE"
)

library(dplyr)

# Fix column names to match what Cubist expects
COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  rename(
    PPG_per_60_wgt = ppg_per_60_wgt,
    EV_per_60_wgt  = ev_per_60_wgt,
    PP_per_60_wgt  = pp_per_60_wgt
  ) %>%
  # Add interaction terms
  mutate(
    Age_x_GameScore   = Age * gameScore_wgt,
    Captain_x_ATOI    = ifelse(Captain == 1, ATOI_wgt, 0),
    Age_x_Captain     = Age * Captain
  )






library(dplyr)

# Step 1: Add Age_Over_32 dummy to prediction data (if not already present)
COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(Age_Over_32 = ifelse(Age >= 32, 1, 0))

# Step 2: Prepare final prediction input data
# Match the predictors used in the final Cubist model (i.e., cubist_all_x)
predictors_needed <- colnames(cubist_all_x)

# Select only those columns
COLLAPSED_2025_aav_input <- COLLAPSED_2025_filtered %>%
  select(all_of(predictors_needed))

##ADDING APPROPRIATE DUMMIES


# Only encode this one variable
dummies <- dummyVars(~ Predicted_Contract_Term, data = COLLAPSED_2025_filtered)
contract_term_dummies <- predict(dummies, newdata = COLLAPSED_2025_filtered) %>% as.data.frame()

dummies2 <- dummyVars(~ Position, data = COLLAPSED_2025_filtered)
Position_dummies <- predict(dummies2, newdata = COLLAPSED_2025_filtered) %>% as.data.frame()

dummies3 <- dummyVars(~ FA_type, data = COLLAPSED_2025_filtered)
FA_dummies <- predict(dummies3, newdata = COLLAPSED_2025_filtered) %>% as.data.frame()


# Bind dummy columns
COLLAPSED_2025_filtered <- bind_cols(COLLAPSED_2025_filtered, contract_term_dummies, Position_dummies, FA_dummies)

COLLAPSED_2025_filtered = COLLAPSED_2025_filtered %>% select(-Position, -FA_type)


COLLAPSED_2025_filtered = COLLAPSED_2025_filtered %>% rename(Contract_Term_Class.Medium = Predicted_Contract_TermMedium)
COLLAPSED_2025_filtered = COLLAPSED_2025_filtered %>% rename(Contract_Term_Class.Short = Predicted_Contract_TermShort)



# Step 3: Predict AAV (Cap Hit %)
predicted_aav_cap_pct <- predict(cubist_model_final, newdata = COLLAPSED_2025_aav_input)

# Step 4: Add predictions back to your dataset
COLLAPSED_2025_filtered$Predicted_Cap_Pct <- predicted_aav_cap_pct





############
# CALCULATING ACTUAL AAV FOR PREDICTIONS
###########
# Load the scales package
library(scales)

# Format the Predicted_AAV column
COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(Predicted_AAV = dollar(Predicted_Cap_Pct * 95500000, accuracy = 1))




###########
# REORGANIZING COLS FOR INTERPRETATION
###########

COLLAPSED_2025_filtered <- COLLAPSED_2025_filtered %>%
  mutate(
    AAV_change = round(Predicted_Cap_Pct - prev_Cap_Pct, 5)
  )





COLLAPSED_2025_PREDICTIONS = COLLAPSED_2025_filtered %>% select(name, Age, prev_Length, prev_Cap_Pct, Predicted_Contract_Term, Predicted_Cap_Pct, Predicted_AAV, AAV_change, PositionD,PositionC,PositionW)





write.csv(COLLAPSED_2025_PREDICTIONS, "FA_Predictions.csv")










######################################################
# STUFF FOR PRESENTATION
######################################################




library(dplyr)

# Select 3 rows for Zemgus Girgensons
Zemgus_presentation_three_rows <- merged_ALL4 %>%
  filter(name == "Zemgus Girgensons")
Zemgus_presentation_three_rows = Zemgus_presentation_three_rows %>% select(playerId, name, Age, Pos, season, GP, G, A, PTS, plus_minus, PIM, gameScore, I_F_xGoals, everything())

# View as table
knitr::kable(Zemgus_presentation_three_rows, caption = "Zemgus Girgensons – Individual Seasons")


Zemgus_collapsed <- COLLAPSED %>%
  filter(name == "Zemgus Girgensons")
Zemgus_collapsed = Zemgus_collapsed %>% select(playerId, name, Age, Position, GP_wgt, G_wgt, A_wgt, PTS_wgt, plus_minus_wgt, PIM_wgt, gameScore_wgt, I_F_xGoals_wgt, everything())


knitr::kable(Zemgus_collapsed, caption = "Zemgus Girgensons – Collapsed Weighted Row")




## CONTRACT DIST HISTOGRAM
COLLAPSED$Signed_Length = as.numeric(COLLAPSED$Signed_Length)

library(ggplot2)

ggplot(COLLAPSED, aes(x = Signed_Length)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", boundary = 0.5) +
  scale_x_continuous(breaks = seq(min(merged_ALL4$Signed_Length), max(merged_ALL4$Signed_Length), by = 1)) +
  labs(
    title = "Distribution of Signed Contract Lengths",
    x = "Contract Length (Years)",
    y = "Number of Contracts"
  ) +
  theme_minimal()

library(dplyr)

length_dist <- COLLAPSED %>%
  count(Signed_Length) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))

length_dist

library(dplyr)

train_ranger_final2_trimmed %>%
  count(Contract_Term_Class) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))

library(dplyr)

COLLAPSED_2025_filtered %>%
  count(Predicted_Contract_Term) %>%
  mutate(Percentage = round(100 * n / sum(n), 1))




library(dplyr)
library(knitr)

# Create a table with name and predicted term
predicted_term_table <- COLLAPSED_2025_filtered %>%
  select(name, Predicted_Contract_Term) %>%
  arrange(Predicted_Contract_Term)  # optional: sort by predicted term

# Display table nicely
kable(predicted_term_table, caption = "Predicted Contract Terms for 2025 Free Agents")




top_climbers <- COLLAPSED_2025_filtered %>%
  arrange(desc(AAV_change)) %>%
  slice_head(n = 10) %>%
  mutate(
    prev_Cap_Pct = round(prev_Cap_Pct, 5),
    Predicted_Cap_Pct = round(Predicted_Cap_Pct, 5)
  ) %>%
  select(name, Age, prev_Cap_Pct, Predicted_Cap_Pct, AAV_change)

top_fallers <- COLLAPSED_2025_filtered %>%
  arrange(AAV_change) %>%
  slice_head(n = 10) %>%
  mutate(
    prev_Cap_Pct = round(prev_Cap_Pct, 5),
    Predicted_Cap_Pct = round(Predicted_Cap_Pct, 5)
  ) %>%
  select(name, Age,  prev_Cap_Pct, Predicted_Cap_Pct, AAV_change)



