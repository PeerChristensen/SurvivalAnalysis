# predict survival probabilities with ML

# conclusion: a constant surv. rate of 0.07361367	from 57 months

data <- cox_surv_df %>%
  select(time,surv,strata) %>%
  filter(!strata %in%  c(1,65))


data %>%
  ggplot(aes(time,surv,colour=factor(strata)),group=strata) +
  geom_line()

ind <- caret::createDataPartition(data$surv,p=.7,list=F)

train <- data[ind,]
test <- data[-ind,]

library(h2o)
h2o.init(nthread=-1)

train_hf <- as.h2o(train)
test_hf <- as.h2o(test)

aml <- h2o.automl(x = "time", y ="surv", training_frame = train_hf,
                  leaderboard_frame = test_hf,max_runtime_secs = 100)

aml@leaderboard
mod = h2o.getModel("XGBoost_grid__1_AutoML_20200423_135203_model_4")

h2o.saveModel(mod,"model")

mod <- h2o.loadModel("model/XGBoost_grid__1_AutoML_20200423_135203_model_4")

newdata <- as.h2o(tibble(time = 1:120))

pred <- as_tibble(predict(mod, newdata))

pred$time <- newdata %>% as_tibble() %>% pull(time)

plot(pred$time,pred$predict,"l")
