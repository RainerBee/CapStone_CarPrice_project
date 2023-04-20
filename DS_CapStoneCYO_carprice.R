

  
#define required packages
  packs <- c("tidyverse","caret","data.table", "lubridate" , "ggthemes", "knitr",
            "gridExtra","scales", "pryr","DataExplorer","skimr" ,
            "RANN", "ranger")
  
#install packages (if needed)  and load them
 for (package in packs) {
 if (!require(package, character.only=T, quietly=T)) {
  install.packages(package, repos = "http://cran.us.r-project.org")
  library(package, character.only=T)
  }
 }


# mask skim function 
  skim <- function (x) {skim_without_charts(x)}  # display tables without histograms


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import data -------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

automobile.dataset <- 
  read.csv(
    'https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data', 
    header = TRUE)

#  change column names 
column_names <- c("symboling","losses","make","fuel","aspiration","doors","body_style",
                  "wheel_drive","engine_loc","wheel_base","length","width","height",
                  "curb_weight","engine_type","cylinders","engine_size","fuel_sys",
                  "bore","stroke","compr_ratio","hp","peak_rpm",
                  "city_mpg","hwy_mpg","price")
  
colnames(automobile.dataset) <-  column_names


# glimpse of the data:
head(automobile.dataset, 6)

# assessing NAs
sum(is.na(automobile.dataset))

# summary of our data
skim(automobile.dataset) 

# new dataset
am <- automobile.dataset 

# Replace ? with NA
am[am == '?'] <- NA

# view data again
skim(am) 

# remove rows with missing price
am <- am %>% filter (!is.na(price))

# convert char to factor
to.factor <- 
  c('make','fuel','aspiration','body_style','wheel_drive', 
    'engine_loc', 'engine_type','fuel_sys')

am <- am %>% mutate(across(all_of(to.factor), as.factor) )   


# review data again
skim(am) %>% filter(skim_type == 'character')


# Check chr values for numerical transformation
am.char<- skim(am) %>% filter(skim_type == 'character') %>% select(skim_variable) %>% pull()
    
am %>% select(all_of(am.char)) %>% head(6)

# mapping doors to numeric
unique(am$doors)

am$doors <- ifelse(am$doors =="two", 2, 4)

am$doors

# mapping of cylinders to numeric
unique(am$cylinders)

am <- am %>% mutate (cylinders = case_when(
        cylinders=='four' ~ 4,
        cylinders=='six' ~ 6,
        cylinders=='five' ~ 5,
        cylinders=='three' ~ 3,
        cylinders=='twelve' ~ 12,
        cylinders=='two' ~ 2,
        cylinders=='eight' ~ 8,
        TRUE ~ as.numeric(cylinders)
      ))  

# Convert the remaining characters to numeric:
am.char <- skim(am) %>% filter(skim_type == 'character') %>% 
  select(skim_variable) %>% pull()

for ( x in am.char) {
      am[,paste(x)] <- as.numeric(am[,paste(x)])
    }

# review data
skim(am)

mu <- mean(am$price)   # 13205.69
sigma <- sd(am$price) # 7966.983
max.price <- max(am$price)  #45400
min.price <- min(am$price)  # 5118


# create variables needed for graphs

mu <- mean(am$price)   # 13205.69
sigma <- sd(am$price) # 7966.983
max.price <- max(am$price)  #45400
min.price <- min(am$price)  # 5118



# review price

summary(am$price ) 

am %>% select(price) %>% 
  mutate (price = round(price/1000)*1000) %>% 
  group_by(price) %>% 
  summarize( pertcentage= n()  / length(am$price) )  %>% 
  ggplot(aes(price, pertcentage)) +
  geom_bar (stat="identity", col="blue", fill="steelblue" )   +
  geom_vline(xintercept = mu,linetype='dashed', col='firebrick1') +
  annotate('text', label=paste('avg price =', round(mu),sep = ''  ), x=mu*0.9, y=0.1 , col="firebrick1", angle = 90 ) +
  scale_y_continuous(labels = percent)  +   #, limits = c(0, .4))
  scale_x_continuous( labels=seq(0,50000,10000), breaks = seq(0,50000,10000)) +
  ggtitle("price barplot (%) ") +
  theme_hc()  



# plot numeric engine attributes

slctn.num.engine <- c('bore','stroke','cylinders','engine_size','hp','peak_rpm','compr_ratio')

plot.title <- 'price vs numerical engine features'

am %>%   select(price,all_of(slctn.num.engine ))%>% 
    pivot_longer(!price, names_to = "key", values_to = 'value') %>% 
    ggplot(aes(value,price)) +
    
    geom_point(alpha=0.2, col="steelblue") +
    geom_smooth(method = 'lm', se = F, col="blue") +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    theme_hc() +
    guides(x = guide_axis(angle = 90) )+
    ylim(0,max(am$price)*1.1) +
    facet_wrap(~key, scales = "free_x" , ncol=4, nrow=20) +
    theme(    strip.text = element_text(face = "bold", size = rel(1), colour = "white"),
              strip.background = element_rect(fill = "steelblue")) +
    ggtitle(plot.title)


# plot numeric chassis attributes

  slctn.num.chassis <- c('curb_weight','height','length', 'wheel_base', 'width', 'doors')
  length(slctn.num.chassis) #6
  plot.title <- 'price vs numerical chassis features'
  
am %>%   select(price,all_of(slctn.num.chassis ))%>% 
    pivot_longer(!price, names_to = "key", values_to = 'value') %>% 
    ggplot(aes(value,price)) +
    
    geom_point(alpha=0.2, col="steelblue") +
    geom_smooth(method = 'lm', se = F, col="blue") +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    theme_hc() +
    guides(x = guide_axis(angle = 90) )+
    ylim(0,max(am$price)*1.1) +
    facet_wrap(~key, scales = "free_x" , ncol=4, nrow=20) +
    theme(    strip.text = element_text(face = "bold", size = rel(1), colour = "white"),
              strip.background = element_rect(fill = "steelblue")) +
    ggtitle(plot.title)


# plot  economic numeric attributes

slctn.num.eco <- c('losses', 'symboling','city_mpg','hwy_mpg')
  length(slctn.num.eco) #4
  
plot.title <- 'price vs numerical economic features'
  
am %>%   select(price,all_of(slctn.num.eco ))%>% 
    pivot_longer(!price, names_to = "key", values_to = 'value') %>% 
    ggplot(aes(value,price)) +
    geom_point(alpha=0.2, col="steelblue") +
    geom_smooth(method = 'lm', se = F, col="blue") +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    theme_hc() +
    guides(x = guide_axis(angle = 90) )+
    ylim(0,max(am$price)*1.1) +
    facet_wrap(~key, scales = "free_x" , ncol=4, nrow=20) +
    theme(    strip.text = element_text(face = "bold", size = rel(1), colour = "white"),
              strip.background = element_rect(fill = "steelblue")) +
    ggtitle(plot.title)


#plot  make attribute

slctn.make <- 'make'

am %>%   select(price,slctn.make) %>% 
    pivot_longer(!price, names_to = "key", values_to = 'value') %>% 
    #mutate(genres = reorder(genres, desc(n.movie), FUN=median))
    mutate(value=reorder(value,desc(price),FUN=median)) %>% 
    ggplot(aes(value,price)) +
    geom_boxplot() +
    #geom_point(col="steelblue", alpha=0.1) +
    geom_jitter(col="steelblue", alpha=0.2) +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    theme_hc() +
    guides(x = guide_axis(angle = 90) )+
    ylim(0,max(am$price)*1.1) +
    facet_wrap(~key, scales = "free_x" , ncol=8, nrow=20) +
    theme(    strip.text = element_text(face = "bold", size = rel(1), colour = "white"),
              strip.background = element_rect(fill = "steelblue")) +
    ggtitle('price vs make')


# plot  engine factor attributes

slctn.engine <- c('aspiration','engine_type','fuel','fuel_sys')

plot.title <- 'price vs categorical engine features' 
  
am %>%   select(price,all_of(slctn.engine)) %>% 
    pivot_longer(!price, names_to = "key", values_to = 'value') %>% 
    mutate(value=reorder(value,desc(price),FUN=median)) %>% 
    ggplot(aes(value,price)) +
    geom_boxplot() +
    geom_jitter(col="steelblue", alpha=0.2) +
    geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
    geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
    geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
    theme_hc() +
    guides(x = guide_axis(angle = 90) )+
    ylim(0,max(am$price)*1.1) +
    facet_wrap(~key, scales = "free_x" , ncol=8, nrow=20) +
    theme(    strip.text = element_text(face = "bold", size = rel(1), colour = "white"),
              strip.background = element_rect(fill = "steelblue")) +
    ggtitle(plot.title)


# plot  chassis factor attributes

slctn.chassis <- c('body_style','engine_loc','wheel_drive')
  
plot.title <- 'price vs categorical chassis features'   
  
am %>%   select(price,all_of(slctn.chassis)) %>% 
  pivot_longer(!price, names_to = "key", values_to = 'value') %>% 
  #mutate(genres = reorder(genres, desc(n.movie), FUN=median))
  mutate(value=reorder(value,desc(price),FUN=median)) %>% 
  ggplot(aes(value,price)) +
  geom_boxplot() +
  #geom_point(col="steelblue", alpha=0.1) +
  geom_jitter(col="steelblue", alpha=0.2) +
  geom_hline(yintercept=mu, linetype='solid', col="firebrick1")+
  geom_hline(yintercept=mu+sigma, linetype='dashed', col="firebrick1")+
  geom_hline(yintercept=mu-sigma, linetype='dashed', col="firebrick1")+
  theme_hc() +
  guides(x = guide_axis(angle = 90) )+
  ylim(0,max(am$price)*1.1) +
  facet_wrap(~key, scales = "free_x" , ncol=8, nrow=20) +
  theme(    strip.text = element_text(face = "bold", size = rel(1), colour = "white"),
            strip.background = element_rect(fill = "steelblue")) +
  ggtitle(plot.title)


# Check for missing data 
  plot_missing(am) #dataexplorer function

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Impute data -------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(803, sample.kind = 'Rounding')  # 
# train a prediction model for missing values
am.pp.train <-preProcess(am, method = "bagImpute", k=5) 

# predict the missing values based on the trained model
am.pp <- predict( am.pp.train, am)  #
sum(is.na(am.pp))

# Check again for missing data
  plot_missing(am.pp)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dummy variables example -------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create the dummy variables for engine_type
dummies <- dummyVars(price ~ engine_type, am.pp)
dummies

# apply dummies to data
predict(dummies, am.pp)  %>% unique()  %>% kable()
    
predict(dummies, am.pp)  %>% colSums() %>% kable()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create dummy variables  -------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create dummy variables for all attributes ( applied only to categorical data )
dummies <- dummyVars(price ~ ., am.pp)

# apply dummies to data
am.pp.dum.train <- predict(dummies, am.pp) 
str(am.pp.dum.train)
class(am.pp.dum.train)

# am.pp.dum.train is a matrix without the price. 
# generate the complete  data.frame including price: 
am.pp.dum <- data.frame(price=am.pp$price, am.pp.dum.train)

skim(am.pp.dum)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Split data in to test and training --------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(803, sample.kind="Rounding")


train.id <-  createDataPartition(am.pp.dum$price, p = .8, list = FALSE, times = 1)
    
am.pp.dum.train <- am.pp.dum[train.id,]
am.pp.dum.test <- am.pp.dum[-train.id,]

#X and Y will serve for the validation of the test data.
X <- am.pp.dum.test[, !(names(am.pp.dum.test) %in% c('price'))]
Y <- am.pp.dum.test$price  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define RMSE -------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

RMSE <- function(true_values, predicted_values){
  sqrt(mean((true_values - predicted_values)^2))}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm.1 LinReg simple --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
  # Method 
    method='lm.1 linear regr. simple'
    
  # Train  
    start <- as.numeric(Sys.time())
      set.seed(803, sample.kind = 'Rounding')
      train_lm.1 <- train(price ~ .,method = "lm", 
                          data = am.pp.dum.train) 
    end <- as.numeric(Sys.time())
    
   # Predict
    pred.lm.1 <- predict(train_lm.1, newdata = X)
    
  
  # Measure  
    RMSE.lm.1 <- RMSE(Y, pred.lm.1)
    rmse <- RMSE.lm.1
    runtime = ceiling( end - start )
    
  # Store results 
    
    comment = 'no train parameters ; warnings'
    
    results.details <- data.frame(method = method, 
                                  true_price = Y, pred_price = pred.lm.1)
    
    results <- data.frame(method = method, RMSE = rmse, 
                          runtime = runtime, comment = comment)
    
    slctn.results <- c(method)
    
  # Review results
    results.details %>% filter(method %in% all_of(slctn.results)) %>% 
      ggplot(aes(true_price, pred_price, col=method, shape=method)) +
      geom_point(size=2, alpha=0.5) +
      geom_abline(slope=1, intercept = 0) +
      ylim(min.price*0.5,max.price*1.1) +
      xlim(min.price*0.5,max.price*1.1) +
      ggtitle(method) + theme_minimal() 
    
    train_lm.1
    
    kable(results)
    
   
  


# review for warnings with preProcess 
train_lm.2 <- train(price ~ .,method = "lm", 
                  preProcess = c('zv','nzv','corr','center', 'scale'), 
                  data = am.pp.dum.train)

train_lm.2


# review for warnings with preProcess 
train_lm.3 <- train(price ~ .,method = "lm", 
                  preProcess  = c('zv','nzv','corr','center', 'scale','pca'), 
                  data = am.pp.dum.train)  

train_lm.3


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
# LinReg Preprocess V2 ----------------------------------------------------  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       
  # Method 
    method = 'lm.2 preprocess #1'
    
  # Train  
    start <- as.numeric(Sys.time())
      set.seed(803, sample.kind = 'Rounding')
      train_lm.2 <- 
        train(price ~ .,method = "lm", 
              preProcess  = c('zv','nzv','corr','center', 'scale'), 
              data = am.pp.dum.train)   
    end <- as.numeric(Sys.time())
    
  # Predict
    pred.lm.2 <- predict(train_lm.2, newdata = X)
    
    # Measure  
    RMSE.lm.2 <- RMSE(Y, pred.lm.2)
    rmse <- RMSE.lm.2
    runtime = ceiling( end - start )
    
  # Store results  
    comment = 'preProcess zv/nzv/corr/center/scale; warnings'
    
    results.details.temp <- data.frame(method = method, 
                                       true_price = Y, pred_price = pred.lm.2)
    results.details <- bind_rows(results.details, results.details.temp)
    
    results.temp <- data.frame(method = method, 
                               RMSE = rmse, runtime = runtime, comment = comment)
    results <- bind_rows (results, results.temp)
    
    slctn.results <- c(slctn.results, method)
    
  # Review results
    results.details %>% filter(method %in% all_of(slctn.results)) %>% 
      ggplot(aes(true_price, pred_price, col=method, shape=method)) +
      geom_point(size=2, alpha=0.5) +
      geom_abline(slope=1, intercept = 0) +
      ylim(min.price*0.5,max.price*1.1) +
      xlim(min.price*0.5,max.price*1.1) +
      ggtitle(method) + theme_minimal() 
    
    train_lm.2
    
    kable(results)
 


# Do not keep method in results graph  
    slctn.results <- slctn.results[slctn.results  !='lm.2 preprocess #1']  


    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
# LinReg Preprocess V3 ----------------------------------------------------  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Method 
    method = 'lm.3 preprocess #2'
    
  # Train  
    start <- as.numeric(Sys.time())
    set.seed(803, sample.kind = 'Rounding')
    train_lm.3 <- 
      train(price ~ .,method = "lm", 
                        preProcess  = c('zv','nzv','corr','center', 'scale','pca'), 
                        data = am.pp.dum.train)   
    end <- as.numeric(Sys.time())
    
  # No warnings!
    
  # Predict
    pred.lm.3 <- predict(train_lm.3, newdata = X)
    
  # Again no warnings!
    
  # Measure  
    RMSE.lm.3 <- RMSE(Y, pred.lm.3)
    rmse <- RMSE.lm.3
    runtime = ceiling( end - start )
    
  # Store results  
    comment = 'all of lm.2 + pca ; no warnings'
    
    results.details.temp <- data.frame(method = method, true_price = Y, pred_price = pred.lm.3)
    results.details <- bind_rows(results.details, results.details.temp)
    
    results.temp <- data.frame(method = method, RMSE = rmse, runtime = runtime, comment = comment)
    results <- bind_rows (results, results.temp)
    
    slctn.results <- c(slctn.results, method)
    
  # Review results
    
    results.details %>% filter(method %in% all_of(slctn.results)) %>% 
      ggplot(aes(true_price, pred_price, col=method, shape=method)) +
      geom_point(size=2, alpha=0.5) +
      geom_abline(slope=1, intercept = 0) +
      ylim(min.price*0.5,max.price*1.1) +
      xlim(min.price*0.5,max.price*1.1) +
      ggtitle(method) + theme_minimal() 
    
    train_lm.3
    
    kable(results)
    

# Do not keep method in results graph  
    slctn.results <- slctn.results[slctn.results  !='lm.1 linear regr. simple']

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # knn.1 simple ----------------------------------------------------------     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
 
  # Method
    method = 'knn.1'
    
  # Train
    start <- as.numeric(Sys.time())
      set.seed(803, sample.kind = 'Rounding')
      train_knn.1 <- 
        train(price ~ ., method = "knn", data = am.pp.dum.train)
    end <- as.numeric(Sys.time())
   
  # Predict
    pred.knn.1 <- predict(train_knn.1, newdata = X)
   
  # Measure  
    RMSE.knn.1 <- RMSE(Y, pred.knn.1)
    RMSE.knn.1
    rmse <- RMSE.knn.1
    runtime = ceiling( end - start )
    
  # Store results 
    
    comment = 'knn.1 no tuning'
    
    results.details.temp <- data.frame(method = method, 
                                       true_price = Y, pred_price = pred.knn.1)
    results.details <- bind_rows(results.details, results.details.temp)
    
    results.temp <- data.frame(method = method, RMSE = rmse, 
                               runtime = runtime, comment = comment)
    results <- bind_rows (results, results.temp)
    
    
    slctn.results <- c(slctn.results, method)
    
  # Review results
    results.details %>% filter(method %in% all_of(slctn.results)) %>% 
      ggplot(aes(true_price, pred_price, col=method, shape=method)) +
      geom_point(size=2, alpha=0.5) +
      geom_abline(slope=1, intercept = 0) +
      ylim(min.price*0.5,max.price*1.1) +
      xlim(min.price*0.5,max.price*1.1) +
      ggtitle(method) + theme_minimal() 
    
    train_knn.1
    
    ggplot(train_knn.1) +
      ggtitle(method) + theme_minimal() 
    
    kable(results)
    
 
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Train knn with tuneGrid & trainControl (knn.2) ------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # Method
    method = 'knn.2 tuneGrid & trControl'
    
  #Train
    start <- as.numeric(Sys.time())
      set.seed(803, sample.kind = 'Rounding')
        train_knn.2 <- train(price ~ ., method = "knn", 
                        data = am.pp.dum.train,
                        trControl=trainControl(method="repeatedcv", 
                                               number=10, repeats = 10),
                        tuneGrid = data.frame(k = seq(1:10)))    
    end <- as.numeric(Sys.time())

  # Predict
    pred.knn.2 <- predict(train_knn.2, newdata = X)

  # Measure  
    RMSE.knn.2 <- RMSE(Y, pred.knn.2)
    RMSE.knn.2
    rmse <- RMSE.knn.2
    runtime = ceiling( end - start )

  # Store results  
    comment = 'knn.1 + tune k=1:10 & 10x repeated 10fold-CV '
  
    results.details.temp <- data.frame(method = method, 
                                      true_price = Y, pred_price = pred.knn.2)
    results.details <- bind_rows(results.details, results.details.temp)
  
    results.temp <- data.frame(method = method, RMSE = rmse, 
                               runtime = runtime, comment = comment)
    results <- bind_rows (results, results.temp)
  
    slctn.results <- c(slctn.results, method)
  
  # Review results
    results.details %>% filter(method %in% all_of(slctn.results)) %>% 
      ggplot(aes(true_price, pred_price, col=method, shape=method)) +
      geom_point(size=2, alpha=0.5) +
      geom_abline(slope=1, intercept = 0) +
      ylim(min.price*0.5,max.price*1.1) +
      xlim(min.price*0.5,max.price*1.1) +
      ggtitle(method) + theme_minimal() 
  
    train_knn.2
    
    ggplot(train_knn.2) +
      ggtitle(method) + theme_minimal()
    
    kable(results)
 


#Do not keep knn.1 in results graph  
  slctn.results <- slctn.results[slctn.results  !='knn.1']

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
# # Train knn with tuneGrid and preprocess (knn.3) ------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Method
    method = 'knn.3 with preProcess #1'
    
    start <- as.numeric(Sys.time())
      set.seed(803, sample.kind = 'Rounding')
      train_knn.3 <- train(price ~ ., method = "knn", 
                          data = am.pp.dum.train,
                          trControl=trainControl(method="repeatedcv", 
                                                 number=10, repeats = 10),
                          tuneGrid = data.frame(k = seq(1:10)),
                          preProcess = c('zv','nzv','corr','center', 'scale') )  
    end <- as.numeric(Sys.time())
    
  # Predict
    pred.knn.3 <- predict(train_knn.3, newdata = X)
    
  # Measure  
    RMSE.knn.3 <- RMSE(Y, pred.knn.3)
    rmse <- RMSE.knn.3
    runtime = ceiling( end - start )
    
  # Store results  
    comment = 'knn.2 + zv/nzv/corr/center/scale'
    
    results.details.temp <- data.frame(method = method, 
                                       true_price = Y, pred_price = pred.knn.3)
    results.details <- bind_rows(results.details, results.details.temp)
    
    results.temp <- data.frame(method = method, RMSE = rmse, 
                               runtime = runtime, comment = comment)
    results <- bind_rows (results, results.temp)
    
    slctn.results <- c(slctn.results, method)
    
  # Review results
    results.details %>% filter(method %in% all_of(slctn.results)) %>% 
      ggplot(aes(true_price, pred_price, col=method, shape=method)) +
      geom_point(size=2, alpha=0.5) +
      geom_abline(slope=1, intercept = 0) +
      ylim(min.price*0.5,max.price*1.1) +
      xlim(min.price*0.5,max.price*1.1) +
      ggtitle(method) + theme_minimal() 
    
    train_knn.3
    
    ggplot(train_knn.3) +
      ggtitle(method) + theme_minimal()
    
    kable(results)

 

# Do not keep method in results graph      
  slctn.results <- slctn.results[slctn.results  !='knn.2 tuneGrid & trControl']


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ranger simple (rf.1) ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Method
     method = 'rf.1 ranger simple'
  
  # Train
     start <- as.numeric(Sys.time())
     set.seed(803, sample.kind = 'Rounding')
     train_ranger.rf.1 <- train(price ~ .,
                               method = "ranger",
                               data = am.pp.dum,
                               verbose=T)
     end <- as.numeric(Sys.time())
  
  # Predict
     pred.rf.1 <- predict(train_ranger.rf.1, newdata = X)
  
  # Measure
     RMSE.rf.1 <- RMSE(Y, pred.rf.1)
     rmse <- RMSE.rf.1
     runtime = ceiling( end - start )
  
  # Store results
     comment = 'no parameters'
  
     results.details.temp <- data.frame(method = method, 
                                      true_price = Y, pred_price = pred.rf.1)
     results.details <- bind_rows(results.details, results.details.temp)
  
     results.temp <- data.frame(method = method, RMSE = rmse,
                                runtime = runtime, comment = comment)
     results <- bind_rows (results, results.temp)
  
     slctn.results <- c(slctn.results, method)
  
  # Review results
     results.details %>% filter(method %in% all_of(slctn.results)) %>%
       ggplot(aes(true_price, pred_price, col=method, shape=method)) +
       geom_point(size=2, alpha=0.5) +
       geom_abline(slope=1, intercept = 0) +
       ylim(min.price*0.5,max.price*1.1) +
       xlim(min.price*0.5,max.price*1.1) +
       ggtitle(method) + theme_minimal()
  
     train_ranger.rf.1
     
     ggplot(train_ranger.rf.1) +
       ggtitle(method) + theme_minimal()
     
     kable(results)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ranger with tunegrid (rf.2) ---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Method 
    method = 'rf.2 ranger tune'
  
    tune.ranger <- data.frame(expand.grid(mtry = seq(1,67,11) , 
                                      min.node.size = seq(1,7,1), 
                                      splitrule = 'variance'))
  
  # Train  
    start <- as.numeric(Sys.time())
    set.seed(803, sample.kind = 'Rounding')
    train_ranger.rf.2 <- train(price ~ ., 
                             method = "ranger", 
                             data = am.pp.dum,
                            
                             tuneGrid = tune.ranger )
    end <- as.numeric(Sys.time())
  
  # Predict
  pred.rf.2 <- predict(train_ranger.rf.2, newdata = X)
  
  # Measure  
  RMSE.rf.2 <- RMSE(Y, pred.rf.2)
  rmse <- RMSE.rf.2
  runtime = ceiling( end - start )
  
  # Store results  
  comment = 'tuned: min.node.size, mtry'
  
  results.details.temp <- data.frame(method = method, true_price = Y, 
                                     pred_price = pred.rf.2)
  
  results.details <- bind_rows(results.details, results.details.temp)
  
  results.temp <- data.frame(method = method, RMSE = rmse, 
                             runtime = runtime, comment = comment)
  results <- bind_rows (results, results.temp)
  
  slctn.results <- c(slctn.results, method)
  
  # Review results
  results.details %>% filter(method %in% all_of(slctn.results)) %>% 
    ggplot(aes(true_price, pred_price, col=method, shape=method)) +
    geom_point(size=2, alpha=0.5) +
    geom_abline(slope=1, intercept = 0) +
    ylim(min.price*0.5,max.price*1.1) +
    xlim(min.price*0.5,max.price*1.1) +
    ggtitle(method) + theme_minimal() 
  
  train_ranger.rf.2
   
  ggplot(train_ranger.rf.2) +
    ggtitle(method) + theme_minimal()

  kable(results)   


# Do not keep method in results graph      
  slctn.results <- slctn.results[slctn.results  !='rf.1 ranger simple']


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ranger with tunegrid & preprocess (rf.3)--------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Method 
    method = 'rf.3 ranger tune + preprocess'
  
    tune.ranger <- data.frame(expand.grid(mtry = seq(6,24,3) , 
                                      min.node.size = 3, 
                                      splitrule = 'variance' ))
  # Train  
    start <- as.numeric(Sys.time())
    set.seed(803, sample.kind = 'Rounding')
    train_ranger.rf.3 <- train(price ~ ., 
                          method = "ranger", 
                          data = am.pp.dum,
                          tuneGrid = tune.ranger,
                          preProcess = c('zv','nzv','corr'
                                         ,'center', 'scale'))  
    end <- as.numeric(Sys.time())
  
  # Predict
    pred.rf.3 <- predict(train_ranger.rf.3, newdata = X)
  
  # Measure  
    RMSE.rf.3 <- RMSE(Y, pred.rf.3)
    rmse <- RMSE.rf.3
    runtime = ceiling( end - start )
  
  # Store results  
    comment = 'tuned as rf.2 + zv/nzv/corr/center/scale'
  
  results.details.temp <- data.frame(method = method, 
                                     true_price = Y, pred_price = pred.rf.3)
  results.details <- bind_rows(results.details, results.details.temp)
  
  results.temp <- data.frame(method = method, RMSE = rmse, 
                             runtime = runtime, comment = comment)
  results <- bind_rows (results, results.temp)
  
  slctn.results <- c(slctn.results, method)
  
  # Review results
  results.details %>% filter(method %in% all_of(slctn.results)) %>% 
    ggplot(aes(true_price, pred_price, col=method, shape=method)) +
    geom_point(size=2, alpha=0.5) +
    geom_abline(slope=1, intercept = 0) +
    ylim(min.price*0.5,max.price*1.1) +
    xlim(min.price*0.5,max.price*1.1) +
    ggtitle(method) + theme_minimal() 
  
  train_ranger.rf.3
  
  ggplot(train_ranger.rf.3) +
    ggtitle(method) + theme_minimal()
  
  kable(results) 
 

# Do not keep method in results graph      
  slctn.results <- slctn.results[slctn.results  !='rf.2 ranger tune']


# create final results graphs and overview

results.details %>% filter(method %in% all_of(slctn.results)) %>% 
    ggplot(aes(true_price, pred_price, col=method, shape=method)) +
    geom_point(size=2, alpha=0.5) +
    geom_abline(slope=1, intercept = 0) +
    ylim(min.price*0.5,max.price*1.1) +
    xlim(min.price*0.5,max.price*1.1) +
    ggtitle(method) + theme_minimal() 

results  %>% mutate (cluster = sub("\\..*", "", method)) %>% 
    ggplot(aes(method ,RMSE, fill=cluster )) +
    geom_col(col='steelblue') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme_hc()

kable(results)
