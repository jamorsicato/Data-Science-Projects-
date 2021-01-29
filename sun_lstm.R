# NOTES 
# sunspot data is hard to predict because of its short term variability and long term 
# irregularities in the cycle
#keras package is for neural network implementation quickly 
# # # # # # Libraries # # # # # # # # # # # # # # # # # # # # 

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
install.packages("timetk")
install.packages("tidyquant")
install.packages("tibbletime")
library(timetk)
library(tidyquant)
library(tibbletime)

# function loading 
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

# Visualization
install.packages("cowplot")
install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(cowplot)

# Preprocessing
install.packages("recipes")
library(recipes)

# Sampling / Accuracy
install.packages("rsample")
install.packages("yardstick")
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(tfruns)

# # # # # # # Variable Declaration # # # # # # # ## # # # # # 

# sunspots data is not a tidy data type alse its 265 years long!
sun_spt <- datasets::sunspot.month %>%
  tk_tbl() %>% # converts to tidy data set using 
  mutate(index = as_date(index)) %>% #creates a new variable from a data frame 
  as_tbl_time(index = index)  # makes time series operations easier 

# # # # # # # Science # # # # # # # # # # # # # # # # # # # # 

# cross-validation and back testing strategy

periods_train   <- 12 * 100
periods_test    <- 12 * 50
skip_span       <- 12 * 22 - 1

rolling_origin_resamples <- rolling_origin(
  sun_spt,
  initial     = periods_train,
  assess      = periods_test,
  cumulative  = FALSE,
  skip        = skip_span
)

rolling_origin_resamples


# this who thing is the plot_split function 
# functionName <- function(arg1, arg2, arg3){
#      BODY }
# split divides the vector into groups defines by args
# the plot_split function will visually display our sampling strategy


plot_split <- function(split, expand_y_axis = TRUE, 
                       alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>% # create training table
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>% # create testing table 
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>% #bind_rows combinesn data frames or vectors 
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%     # used to extract date or time series index from timeseries objects 
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>% 
    tk_get_timeseries_summary() # time 
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to ", 
                      "{test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spts_time_summary <- sun_spt %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spts_time_summary$start, 
                              sun_spts_time_summary$end))
  }
  
  g
}


# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 14, fontface = "bold", 
               colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, 
                 rel_heights = c(0.05, 1, 0.05))
  
  g
  
}

# we are going to take only our most recent split to start training thr LSTM model


example_split <- rolling_origin_resamples$splits[[6]]
example_split_id <- rolling_origin_resamples$id[[6]]

example_plot <- plot_split(example_split, expand_y_axis = F, size = 0.5) +
    theme(legend.position = "bottom") +
    ggtitle(glue("Split: {example_split_id}"))

example_plot

df_trn <- analysis(example_split)[1:800, , drop = FALSE]
df_val <- analysis(example_split)[801:1200, , drop = FALSE]
df_tst <- assessment(example_split)

df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_val %>% add_column(key = "validation"),
  df_tst %>% add_column(key = "testing")) %>%
  as_tbl_time(index = index)

df

rec_obj <- recipe(value ~ ., df) %>%
  step_sqrt(value) %>%
  step_center(value) %>%
  step_scale(value) %>%
  prep()

df_processed_tbl <- bake(rec_obj, df)

df_processed_tbl

center_history <- rec_obj$steps[[2]]$means["value"]
scale_history  <- rec_obj$steps[[3]]$sds["value"]

c("center" = center_history, "scale" = scale_history)

n_timesteps <- 12
n_predictions <- n_timesteps
batch_size <- 10

# functions used
build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

# extract values from data frame
train_vals <- df_processed_tbl %>%
  filter(key == "training") %>%
  select(value) %>%
  pull()
valid_vals <- df_processed_tbl %>%
  filter(key == "validation") %>%
  select(value) %>%
  pull()
test_vals <- df_processed_tbl %>%
  filter(key == "testing") %>%
  select(value) %>%
  pull()

# build the windowed matrices
train_matrix <-
  build_matrix(train_vals, n_timesteps + n_predictions)
valid_matrix <-
  build_matrix(valid_vals, n_timesteps + n_predictions)
test_matrix <- build_matrix(test_vals, n_timesteps + n_predictions)

# separate matrices into training and testing parts
# also, discard last batch if there are fewer than batch_size samples
# (a purely technical requirement)
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

X_valid <- valid_matrix[, 1:n_timesteps]
y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]
# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)


FLAGS <- flags(
  # There is a so-called "stateful LSTM" in Keras. While LSTM is stateful
  # per se, this adds a further tweak where the hidden states get 
  # initialized with values from the item at same position in the previous
  # batch. This is helpful just under specific circumstances, or if you want
  # to create an "infinite stream" of states, in which case you'd use 1 as 
  # the batch size. Below, we show how the code would have to be changed to
  # use this, but it won't be further discussed here.
  flag_boolean("stateful", FALSE),
  # Should we use several layers of LSTM?
  # Again, just included for completeness, it did not yield any superior 
  # performance on this task.
  # This will actually stack exactly one additional layer of LSTM units.
  flag_boolean("stack_layers", FALSE),
  # number of samples fed to the model in one go
  flag_integer("batch_size", 10),
  # size of the hidden state, equals size of predictions
  flag_integer("n_timesteps", 12),
  # how many epochs to train for
  flag_integer("n_epochs", 100),
  # fraction of the units to drop for the linear transformation of the inputs
  flag_numeric("dropout", 0.2),
  # fraction of the units to drop for the linear transformation of the 
  # recurrent state
  flag_numeric("recurrent_dropout", 0.2),
  # loss function. Found to work better for this specific case than mean
  # squared error
  flag_string("loss", "logcosh"),
  # optimizer = stochastic gradient descent. Seemed to work better than adam 
  # or rmsprop here (as indicated by limited testing)
  flag_string("optimizer_type", "sgd"),
  # size of the LSTM layer
  flag_integer("n_units", 128),
  # learning rate
  flag_numeric("lr", 0.003),
  # momentum, an additional parameter to the SGD optimizer
  flag_numeric("momentum", 0.9),
  # parameter to the early stopping callback
  flag_integer("patience", 10)
)

# the number of predictions we'll make equals the length of the hidden state
n_predictions <- FLAGS$n_timesteps
# how many features = predictors we have
n_features <- 1
# just in case we wanted to try different optimizers, we could add here
optimizer <- switch(FLAGS$optimizer_type,
                    sgd = optimizer_sgd(lr = FLAGS$lr, 
                                        momentum = FLAGS$momentum)
)

# callbacks to be passed to the fit() function
# We just use one here: we may stop before n_epochs if the loss on the
# validation set does not decrease (by a configurable amount, over a 
# configurable time)
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)

# # # # # #  # LSTM Model # # # #  ## ## 

# create the model
model <- keras_model_sequential()

# add layers
# we have just two, the LSTM and the time_distributed 
model %>%
  layer_lstm(
    units = FLAGS$n_units, 
    # the first layer in a model needs to know the shape of the input data
    batch_input_shape  = c(FLAGS$batch_size, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>% time_distributed(layer_dense(units = 1))

model %>%
  compile(
    loss = FLAGS$loss,
    optimizer = optimizer,
    # in addition to the loss, Keras will inform us about current 
    # MSE while training
    metrics = list("mean_squared_error")
  )

history <- model %>% fit(
  x          = X_train,
  y          = y_train,
  validation_data = list(X_valid, y_valid),
  batch_size = FLAGS$batch_size,
  epochs     = FLAGS$n_epochs,
  callbacks = callbacks
)

# Now let’s see how well the model was able to capture the characteristics of the training set.

pred_train <- model %>%
  predict(X_train, batch_size = FLAGS$batch_size) %>%
  .[, , 1]

# Retransform values to original scale
pred_train <- (pred_train * scale_history + center_history) ^2
compare_train <- df %>% filter(key == "training")

# build a dataframe that has both actual and predicted values
for (i in 1:nrow(pred_train)) {
  varname <- paste0("pred_train", i)
  compare_train <-
    mutate(compare_train,!!varname := c(
      rep(NA, FLAGS$n_timesteps + i - 1),
      pred_train[i,],
      rep(NA, nrow(compare_train) - FLAGS$n_timesteps * 2 - i + 1)
    ))
}

# Calculate the Root Mean Square Error  RMSE of all sequences and predictions

coln <- colnames(compare_train)[4:ncol(compare_train)]
cols <- map(coln, quo(sym(.)))
rsme_train <- map_dbl(cols, function(col)
    rmse(
      compare_train,
      truth = value,
      estimate = !!col,
      na.rm = TRUE
    )) %>% mean()





## # # # # # Plotting # # # # # # # # # ## # # # # # # # # #

# plot full time series with cowplot 

p1 <- sun_spt %>%
  ggplot(aes(index,value)) + 
  geom_point(color = palette_dark()[[1]], alpha = 0.25) +
  labs(
    title = "Sunspot data from 1749 - 2013",
    x = "Year",
    y = "Number of Sunspots",
    caption = "Source: R Studio provided data set ")


p2 <- sun_spt %>%
  filter_time("1900" ~ "2013") %>%
  ggplot(aes(index,value)) + 
  geom_point(color = palette_dark()[[1]], alpha = 0.25) +
  geom_line(color = palette_dark()[[2]], alpha = 0.25) +
  geom_smooth(method = "loess",span = 0.1, se = FALSE) + #locally estimated scatter plot smoothing
  labs(
    title = "Sunspot data from 1900 - 2013",
    x = "Year",
    y = "Number of Sunspots",
    caption = "Source: R Studio provided data set ")

p

# plot p1 and p2, if doest work reload packages
p_title <- ggdraw() + 
  draw_label("Sunspot Time Series", size = 18, fontface = "bold", 
            colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

# plot Slice 1 from backtesting strategy 

rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")

# Plot entire backtesting strategy 
# if we change expand_y_axis = T we will get the splice plotted on the entire time 

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")



# Notes and Defintions

# "K-fold" CROSS-VALIDATION: A procedure that resamples data for use in a mcahine learning environmnt. 
                  # this has one variabel K that say how many groups to to put the data i. Also called
                  # backtesting in finance 

# we will use the rolling_origin() function 
# SAMPLING PLAN
#     - periods_train <- 100 years (12 by 100 samples) 
#     - periods_test <- 50 years (12 by 50 samples)
#     - skip_span <- 22 years (12 by 22) slip every 22 years so we have an even distribution
#     - cumulative will be set to false, older data has less samples and we dont want it getting pushed aside






# Sources
# https://blogs.rstudio.com/ai/posts/2018-06-25-sunspots-lstm/
# https://cran.r-project.org/web/packages/greybox/vignettes/ro.html
# https://www.tidymodels.org/learn/models/time-series/ # origin of rolling_origin() method 


