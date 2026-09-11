###########################
# ATTEMPTING CUBIST --- BEFORE FINAL RESULTS ADD INTERACTION TERMS TO TRAIN AAV MODEL
##########################

install.packages("Cubist")
install.packages("caret")

library(Cubist)
library(caret)





# Features and target
cubist_train_x <- TRAIN_AAV %>%
  select(-Signed_Cap_Pct) %>%
  select(where(is.numeric))

cubist_train_y <- TRAIN_AAV$Signed_Cap_Pct

cubist_test_x <- TEST_AAV %>%
  select(-Signed_Cap_Pct) %>%
  select(where(is.numeric))

cubist_test_y <- TEST_AAV$Signed_Cap_Pct


cubist_grid <- expand.grid(
  committees = c(1, 5, 10, 20),     # Like number of boosting rounds
  neighbors = c(0, 1, 3, 5, 7)      # Smoothing factor
)


train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)


set.seed(42)
cubist_model <- train(
  x = cubist_train_x,
  y = cubist_train_y,
  method = "cubist",
  trControl = train_control,
  tuneGrid = cubist_grid,
  metric = "RMSE"
)


cubist_preds <- predict(cubist_model, newdata = cubist_test_x)

rmse_cubist <- sqrt(mean((cubist_preds - cubist_test_y)^2))
mae_cubist <- mean(abs(cubist_preds - cubist_test_y))
rsq_cubist <- 1 - sum((cubist_test_y - cubist_preds)^2) / sum((cubist_test_y - mean(cubist_test_y))^2)

print(paste("Cubist RMSE:", round(rmse_cubist, 5)))
print(paste("Cubist MAE:", round(mae_cubist, 5)))
print(paste("Cubist R-squared:", round(rsq_cubist, 5)))


cubist_model$bestTune


var_imp <- varImp(cubist_model)
print(var_imp)

#imp vars plotted

library(ggplot2)

top_vars <- var_imp$importance[order(-var_imp$importance$Overall), , drop = FALSE]
top_vars$Variable <- rownames(top_vars)

ggplot(top_vars[1:20, ], aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Cubist Model Variable Importance",
    x = "Variable",
    y = "Importance Score"
  ) +
  theme_minimal()


## PREDICTIONS COMPARED TO ACTUALS


comparison_df <- data.frame(
  Player = TEST_AAV$name,  # or however your player names are stored
  Predicted_Cap_Pct = cubist_preds,
  Actual_Cap_Pct = TEST_AAV$Signed_Cap_Pct,
  Error = cubist_preds - TEST_AAV$Signed_Cap_Pct,
  Abs_Error = abs(cubist_preds - TEST_AAV$Signed_Cap_Pct)
)


# Top underestimates (predicted too low)
head(comparison_df[order(comparison_df$Error), ], 10)

# Top overestimates (predicted too high)
head(comparison_df[order(-comparison_df$Error), ], 10)


library(ggplot2)

ggplot(comparison_df, aes(x = Actual_Cap_Pct, y = Predicted_Cap_Pct)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Cap Percentage",
       x = "Actual Cap %",
       y = "Predicted Cap %") +
  theme_minimal()


#############
# ADDING INTERACTION TERMS
###########################




add_interactions <- function(df) {
  df <- df %>%
    mutate(
      Age_x_GameScore = Age * gameScore_wgt,
      ATOI_x_GameScore = ATOI_wgt * gameScore_wgt,
      Captain_x_ATOI = Captain * ATOI_wgt,
      PrevLength_x_Age = prev_Length * Age,
      EVG_x_PPG = EVG_per_60_wgt * PPG_per_60_wgt,
      Age_x_Captain = Age * Captain,
      PenaltyDrawn_x_GameScore = penalityMinutesDrawn_per_60_wgt * gameScore_wgt,
      Give_x_Points = GIVE_per_60_wgt * (EV_per_60_wgt + PP_per_60_wgt)
    )
  return(df)
}

TRAIN_AAV2 <- add_interactions(TRAIN_AAV)
TEST_AAV2 <- add_interactions(TEST_AAV)

library(caret)

cubist_train_x2 <- TRAIN_AAV2 %>%
  select(-Signed_Cap_Pct) 

# Only encode this one variable
dummies <- dummyVars(~ Contract_Term_Class, data = cubist_train_x2)
contract_term_dummies <- predict(dummies, newdata = cubist_train_x2) %>% as.data.frame()

dummies2 <- dummyVars(~ Position, data = cubist_train_x2)
Position_dummies <- predict(dummies2, newdata = cubist_train_x2) %>% as.data.frame()

dummies3 <- dummyVars(~ FA_type, data = cubist_train_x2)
FA_dummies <- predict(dummies3, newdata = cubist_train_x2) %>% as.data.frame()

# Drop the factor column
cubist_train_x2_trimmed <- cubist_train_x2 %>% select(-Contract_Term_Class)

# Bind dummy columns
cubist_train_x2_final <- bind_cols(cubist_train_x2_trimmed, contract_term_dummies, Position_dummies, FA_dummies)

cubist_train_x2_final = cubist_train_x2_final %>% select(-Position, -FA_type, -name)


cubist_train_y2 <- TRAIN_AAV2$Signed_Cap_Pct


cubist_test_x2 <- TEST_AAV2 %>%
  select(-Signed_Cap_Pct) 


# Only encode this one variable
dummies4 <- dummyVars(~ Contract_Term_Class, data = cubist_test_x2)
contract_term_dummies <- predict(dummies4, newdata = cubist_test_x2) %>% as.data.frame()

dummies5 <- dummyVars(~ Position, data = cubist_test_x2)
Position_dummies2 <- predict(dummies5, newdata = cubist_test_x2) %>% as.data.frame()

dummies6 <- dummyVars(~ FA_type, data = cubist_test_x2)
FA_dummies2 <- predict(dummies6, newdata = cubist_test_x2) %>% as.data.frame()

# Drop the factor column
cubist_test_x2_trimmed <- cubist_test_x2 %>% select(-Contract_Term_Class)

# Bind dummy columns
cubist_test_x2_final <- bind_cols(cubist_test_x2_trimmed, contract_term_dummies, Position_dummies2, FA_dummies2)

cubist_test_x2_final = cubist_test_x2_final %>% select(-Position, -FA_type, -name)




set.seed(42)
cubist_model2 <- train(
  x = cubist_train_x2_final,
  y = cubist_train_y2,
  method = "cubist",
  trControl = train_control,
  tuneGrid = cubist_grid,
  metric = "RMSE"
)




cubist_preds2 <- predict(cubist_model2, newdata = cubist_test_x2_final)

rmse_cubist2 <- sqrt(mean((cubist_preds2 - cubist_test_y)^2))
mae_cubist2 <- mean(abs(cubist_preds2 - cubist_test_y))
rsq_cubist2 <- 1 - sum((cubist_test_y - cubist_preds2)^2) / sum((cubist_test_y - mean(cubist_test_y))^2)

print(paste("Cubist RMSE:", round(rmse_cubist2, 5)))
print(paste("Cubist MAE:", round(mae_cubist2, 5)))
print(paste("Cubist R-squared:", round(rsq_cubist2, 5)))


cubist_model2$bestTune

# Extract all variable importances
full_var_imp <- varImp(cubist_model2)$importance

# Add variable names as a column
full_var_imp$Variable <- rownames(full_var_imp)

# Sort by importance
full_var_imp <- full_var_imp[order(-full_var_imp$Overall), ]

# View the full table
View(full_var_imp)  # Opens in a data viewer

# Remove zero-importance variables from training and testing sets
zero_vars <- full_var_imp %>%
  filter(Overall == 0) %>%
  pull(Variable)

cubist_train_x_trimmed <- cubist_train_x2_final %>% select(-FA_typeUFA, -PositionW, -Contract_Term_Class.Long, -Give_x_Points, -Age_x_Captain, -BLK_per_60_wgt, -TAKE_per_60_wgt, -EV_per_60_wgt, -PPG_per_60_wgt)
cubist_test_x_trimmed  <- cubist_test_x2_final %>% select(-FA_typeUFA, -PositionW, -Contract_Term_Class.Long, -Give_x_Points, -Age_x_Captain, -BLK_per_60_wgt, -TAKE_per_60_wgt, -EV_per_60_wgt, -PPG_per_60_wgt)




#########################
# FINAL MODEL FR this time -- USE THIS ONE 
########################


library(Cubist)
library(caret)

# Define training and testing target variables
cubist_train_y_trimmed <- cubist_train_y
cubist_test_y_trimmed <- cubist_test_y

# Define tuning grid
cubist_grid <- expand.grid(
  committees = c(1, 5, 10, 20),
  neighbors = c(0, 1, 3, 5, 7)
)

# Training control
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

# Fit the Cubist model
set.seed(42)
cubist_model_trimmed <- train(
  x = cubist_train_x_trimmed,
  y = cubist_train_y_trimmed,
  method = "cubist",
  trControl = train_control,
  tuneGrid = cubist_grid,
  metric = "RMSE"
)

# Predictions
cubist_preds_trimmed <- predict(cubist_model_trimmed, newdata = cubist_test_x_trimmed)

# Performance metrics
rmse_cubist_trimmed <- sqrt(mean((cubist_preds_trimmed - cubist_test_y_trimmed)^2))
mae_cubist_trimmed <- mean(abs(cubist_preds_trimmed - cubist_test_y_trimmed))
rsq_cubist_trimmed <- 1 - sum((cubist_test_y_trimmed - cubist_preds_trimmed)^2) / 
  sum((cubist_test_y_trimmed - mean(cubist_test_y_trimmed))^2)

# Output results
print(paste("Cubist Trimmed RMSE:", round(rmse_cubist_trimmed, 5)))
print(paste("Cubist Trimmed MAE:", round(mae_cubist_trimmed, 5)))
print(paste("Cubist Trimmed R-squared:", round(rsq_cubist_trimmed, 5)))


cubist_train_x_trimmed = cubist_train_x_trimmed %>%select(-SHG_per_60_wgt, -EVG_x_PPG)
cubist_test_x_trimmed = cubist_test_x_trimmed %>%select(-SHG_per_60_wgt, -EVG_x_PPG)

