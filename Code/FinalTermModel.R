
ranger_model2 <- ranger(
  Contract_Term_Class ~ ., 
  data = train_ranger_final2,
  num.trees = 500,
  probability = TRUE,
  classification = TRUE,
  importance = "impurity"
)

# Predict class probabilities
ranger_preds_prob2 <- predict(ranger_model2, data = test_ranger_final2)$predictions

# Pick highest probability class
ranger_final_preds2 <- colnames(ranger_preds_prob2)[max.col(ranger_preds_prob2, ties.method = "first")]

# Turn into factor
ranger_final_preds2 <- factor(ranger_final_preds2, levels = levels(test_ranger_final2$Contract_Term_Class))

library(caret)

confusionMatrix(ranger_final_preds2, test_ranger_final2$Contract_Term_Class) ## BEST YET



importance_df <- data.frame(
  Variable = names(ranger_model2$variable.importance),
  Importance = ranger_model2$variable.importance
) %>%
  arrange(desc(Importance))

# View the full importance table
print(importance_df)


# Drop low-importance variables
vars_to_drop <- c(
  "Captain",
  "SHG_per_60_wgt",
  "Age_x_Captain",
  "EVG_x_PPG",
  "PPG_per_60_wgt",
  "Captain_x_ATOI"
)

train_ranger_final2_trimmed <- train_ranger_final2 %>%
  select(-all_of(vars_to_drop))

test_ranger_final2_trimmed <- test_ranger_final2 %>%
  select(-all_of(vars_to_drop))


###############
# FINAL MODEL (USE THIS ONE)
##############


ranger_model3 <- ranger(
  Contract_Term_Class ~ ., 
  data = train_ranger_final2_trimmed,
  num.trees = 500,
  probability = TRUE,
  classification = TRUE,
  importance = "impurity"
)



# Predict class probabilities
ranger_preds_prob3 <- predict(ranger_model3, data = test_ranger_final2_trimmed)$predictions

# Pick highest probability class
ranger_final_preds3 <- colnames(ranger_preds_prob3)[max.col(ranger_preds_prob3, ties.method = "first")]

# Turn into factor
ranger_final_preds3 <- factor(ranger_final_preds3, levels = levels(test_ranger_final2_trimmed$Contract_Term_Class))

library(caret)

confusionMatrix(ranger_final_preds3, test_ranger_final2_trimmed$Contract_Term_Class) ## BEST YET



importance_df <- data.frame(
  Variable = names(ranger_model3$variable.importance),
  Importance = ranger_model3$variable.importance
) %>%
  arrange(desc(Importance))

# View the full importance table
print(importance_df)



