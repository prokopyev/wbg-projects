## Question 1.	

Is risk related to the region in which a project is being implemented? 
  For which risk is the relationship the strongest? 
  How would you visualize the relationship between risk and region?
  
  



```{r}
model_rforest <- joined_data %>%  build_model(model_func = randomForestMulti, 
                                              formula = risk_rating ~ 
                                                risk_rating_sequence + region + tl + len_instr_type + practice + 
                                                risk_rating_code_key + prod_line + fcs_indicator + proj_emrg_recvry_flg + 
                                                approval_fy + project_track + tl_since + net_commit_amt + grant + scale_up, 
                                              na.action = "na.omit", 
                                              ntree = 100,
                                              test_rate = 0.3, 
                                              seed = 1337)
```

```{r}
model_rforest %>% rf_evaluation()
model_rforest %>% rf_importance()

model_rforest %>% prediction(data = "test") %>% evaluate_multi(predicted_label, risk_rating)

model_rforest_testdata <- model_rforest %>% prediction(data = "test")
confusionMatrix(model_rforest_testdata$predicted_label, model_rforest_testdata$risk_rating)
```

```{r}
model_xgboost <- joined_data %>% 
  build_model(model_func = xgboost_multi, 
              formula = risk_rating ~ country + region + grant + net_commit_amt + practice + tl + tl_since,
              na.action = na.pass, 
              booster = "gbtree", 
              eval_metric = "mlogloss", 
              nrounds = 500, 
              max_depth = 6, 
              min_child_weight = 1, 
              gamma = 0, 
              subsample = 0.75, 
              colsample_bytree = 1, 
              learning_rate = 0.3, 
              early_stopping_rounds = 50, 
              test_rate = 0.33, 
              seed = 1337)

model_xgboost
```

```{r}
model_xgboost %>% rf_evaluation()
model_xgboost %>% rf_importance()

model_xgboost %>% prediction(data = "test") %>% evaluate_multi(predicted_label, risk_rating)

model_xgboost_testdata <- model_xgboost %>% prediction(data = "test")
confusionMatrix(model_xgboost_testdata$predicted_label, model_xgboost_testdata$risk_rating)
```

```{r}
joined_data_filtered <- joined_data %>% 
  filter(risk_rating_code == "Overall") 

model_xgb_filtered <- joined_data_filtered %>%
  select(-risk_rating_code, -risk_rating_code_key, -project_track, -scale_up, -len_instr_type, -risk_numeric) %>%
  build_model(model_func = xgboost_multi, formula = risk_rating ~ . , eval_metric = "mlogloss", watchlist_rate = 0.05, nrounds = 500, gamma = 0, early_stopping_rounds = 50, min_child_weight = 1, max_depth = 6, learning_rate = 0.2, colsample_bytree = 1, subsample = 0.75, test_rate = 0.3, seed = 1337) 

model_xgb_filtered
model_xgb_filtered_testdata <- model_xgb_filtered %>% prediction(data = "test")
confusionMatrix(model_xgb_filtered_testdata$predicted_label, model_xgb_filtered_testdata$risk_rating)
```


```{r, results='hide',message=FALSE}


library(dummies)
library(dplyr)
dummy(joined_data_filtered$region, sep='.')

#dummy.data.frame(joined_data_filtered, names=c("region"), sep="_")
#library(Matrix)
#sparse.model.matrix(region~.-1, data=joined_data_filtered)


joined_data_filtered_dummies <- cbind(joined_data_filtered, dummy(joined_data_filtered$region))
```

```{r, echo=FALSE}
var_corrs_filtered <- joined_data_filtered_dummies %>% 
  do_cor(which(sapply(., is.numeric)), 
         use = "pairwise.complete.obs", 
         method = "spearman", 
         distinct = TRUE, 
         diag = FALSE)

plot_ly(x = var_corrs_filtered$pair.name.x, 
        y = var_corrs_filtered$pair.name.y,
        z = var_corrs_filtered$value, 
        colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)")),
        # colorscale = "Blues",
        zauto = F,
        zmin = -1,
        zmax = 1,
        type = "heatmap",
        autosize = F, width = 650, height = 400)
```

## Question 2.	

Please choose one other project attribute and analyze and report on its relationship to risk. 

## Deprecated 

```{r, echo=FALSE, message=FALSE, results='hide'}
joined_data <- joined_data %>%
  mutate(
    proj_emrg_recvry_flg = ifelse(proj_emrg_recvry_flg==c("N"),0,1), 
    fcs_indicator = ifelse(fcs_indicator==c("N"),0,1), 
    practice = factor(practice), 
    region = factor(region), 
    country = factor(country), 
    tl_since = mdy(tl_since), 
    project_track = parse_number(project_track), 
    scale_up = ifelse(scale_up==c("No"),0,1), 
    len_instr_type = factor(len_instr_type), 
    risk_rating = factor(risk_rating), 
    risk_rating = fct_relevel(risk_rating, "L", "M", "S", "H"), 
    risk_rating_code = factor(risk_rating_code),
    risk_rating_code_key = factor(risk_rating_code_key),
    prod_line = factor(prod_line),
    project_id = factor(project_id),
    risk_numeric = ifelse(risk_rating==c("L"),1,
                          ifelse(risk_rating==c("M"),2,
                                 ifelse(risk_rating==c("S"),3,
                                        ifelse(risk_rating==c("H"),4,0)))))
```


```{r}
library(ggalluvial)

joined_data %>% filter(region != "OTH") %>% ggplot(aes(x = approval_fy, weight = tl, alluvium = country)) +
  geom_alluvium(aes(fill = country, colour = country),
                alpha = .75, decreasing = FALSE) +
  scale_x_continuous(breaks = seq(2002, 2017, 2)) +
  theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
  scale_fill_brewer(type = "qual", palette = "Set3") +
  scale_color_brewer(type = "qual", palette = "Set3") +
  facet_wrap(~ region, scales = "fixed") +
  ggtitle("refugee volume by country and region of origin")


ggplot(as.data.frame(Titanic),
       aes(weight = Freq,
           axis1 = Survived, axis2 = Sex, axis3 = Class)) +
  geom_alluvium(aes(fill = Class),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  coord_flip() +
  ggtitle("Titanic survival by class and sex")
```

#####################################

```{r}

chisq_test_risk_reg_exp <- chisq.test(conting_risk_reg)$expected
chisq_test_risk_reg_exp
conting_risk_reg

library(corrplot)
corrplot(chisq_test_risk_reg$residuals, is.cor = FALSE)
# Visualization of Pearson residuals. For a given cell, the size of the circle is proportional to the amount of the cell contribution.

contrib <- 100*chisq_test_risk_reg$residuals^2/chisq_test_risk_reg$statistic
corrplot(contrib, is.cor = FALSE)

# 
# It can be seen that:
# 
# The column “Wife” is strongly associated with Laundry, Main_meal, Dinner
# The column “Husband” is strongly associated with the row Repairs
# The column jointly is frequently associated with the row Holidays
# From the image above, it can be seen that the most contributing cells to the Chi-square are Wife/Laundry (7.74%), Wife/Main_meal (4.98%), Husband/Repairs (21.9%), Jointly/Holidays (12.44%).
# 
# These cells contribute about 47.06% to the total Chi-square score and thus account for most of the difference between expected and observed values.
# 
# This confirms the earlier visual interpretation of the data. As stated earlier, visual interpretation may be complex when the contingency table is very large. In this case, the contribution of one cell to the total Chi-square score becomes a useful way of establishing the nature of dependency.
library(vcd)
assoc(conting_risk_reg, shade = TRUE, las=3)
```



In Part 1 we focused on the overall risk measure and its relation to region. We are tasked with exploring the association of risk and other measures. However, this has already been covered in the visualizations and analyses in Part 1. The results are valuable, yet there is an even more insightful discovery below.

For this section, we have prepared an analysis of how different types of risk influence the measure of overall risk. Thus, we use only types of risks to preduct the overall risk rating. This cannot be used in applied environments, because it presents a data leakage. In this case it is intentinal -- to show that contribution of different types of risk is not even.

```{r, echo=FALSE,results='hide',message=FALSE}
m_xgb <- joined_data %>%
  select(-tl, -project_id, -net_commit_amt, -tl_since, -approval_fy, -grant, -region, -country, -practice, -proj_emrg_recvry_flg, -fcs_indicator, -prod_line, -project_track) %>%
  build_model(
    model_func = xgboost_multi, 
    formula = risk_overall ~ . , 
    booster = "gbtree", 
    eval_metric = "mlogloss", 
    watchlist_rate = 0.05, gamma = 0, max_depth = 6, 
    min_child_weight = 1, subsample = 1, colsample_bytree = 1, 
    learning_rate = 0.3, early_stopping_rounds = 50, nrounds = 500, 
    test_rate = 0.3, seed = 1337)
m_xgb_coef <- m_xgb %>% model_coef()
m_xgb_prediction <- m_xgb %>% prediction(data = "test")
```

```{r, echo=FALSE}
m_xgb_conf <- confusionMatrix(m_xgb_prediction$predicted_label, m_xgb_prediction$risk_overall)
m_xgb_conf
```

```{r, echo=FALSE}
#m_xgb_coef$importance <- format(m_xgb_coef$importance, scientific=F)
options("scipen"=100, "digits"=4)
p <- m_xgb_coef %>% filter(importance>0.00683778) %>% 
  ggplot(aes(x = reorder(feature, importance), 
             y = importance,
             fill = importance)) +
    coord_flip() +
    scale_fill_gradient(low = "gray", high = "red", "Variable\nimportance") +
    geom_bar(stat = "identity") +
    theme_bw() +
    xlab("Variable names") +
    ylab("")
ggplotly(p, tooltip=c("y"))
```

Here we observe that the #1 contributor to overall risk is institutional capacity.

```{r, echo=FALSE,results='hide',message=FALSE}
# Running Spearman correlation analysis
j_data_corr <- j_data_corr %>% select(risk_overall, risk_environment_and_social, risk_fiduciary, risk_institutional_capacity, risk_macroeconomic, risk_political_and_governance, risk_sector_strategies_and_policies, risk_stakeholders, risk_technical_design, risk_rating_sequence) 
var_corrs <- j_data_corr %>% 
  do_cor(which(sapply(., is.numeric)), 
         use = "pairwise.complete.obs", 
         method = "spearman", 
         distinct = FALSE, 
         diag = TRUE)
```
```{r, echo=FALSE,message=FALSE}
# Plotting
plot_ly(x = var_corrs$pair.name.x, 
        y = var_corrs$pair.name.y,
        z = var_corrs$value, 
        colorscale = list(c(0, "rgb(255, 0, 0)"), list(1, "rgb(0, 255, 0)")),
        # colorscale = "Blues",
        zauto = F,
        zmin = -1,
        zmax = 1,
        type = "heatmap",
        autosize = F, width = 800, height = 400)
```


We run the correlation analysis again to magnify the relationships between types of risk.

- We indeed see that institutional capacity is the highest correlated risk with regards to overall risk at `0.63` correlation. 

- It is interesting that while fudiciary risk is also rather correlated with overall risk at `0.52` correlation, it is located in the low-value cluster in XGBoost variable importance analysis above.

- According to it, second and third runner-ups are political and stakeholder risks. Among with fudiciary, technical design, environment, sector strategies, and macroeconomics are less important for overall risk.

- It is logical to see a positive association of `0.51` between fudiciary risk and institutional capacity. There is an assocition between in-country trustees and strong institutions, and vise versa.

- Interestingly, environment risks have near-zero association with macroeconomic threats. One would expect that macro activity would threaten the environment due to means used to support it not eco-friendly for the most part.




- `risk_overall` is not a linear function of other specific types of risk. Institutional capacity is a type of risk that contributes the most to the overall risk rating accoding to two disticnt measures.


















