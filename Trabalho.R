library(tidyverse)
library(dplyr)
library(dlookr)
library(rnoaa)
library(lubridate)
require(devtools)
library(zoo)
library(data.table)
library(ggplot2)
library(cluster)
library(fpc)
library(factoextra)
library(caret)
library(naivebayes)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(nnet)
library(neuralnet)
library(caret)
library(dplyr)
library(adabag)
library(xgboost)
library(performanceEstimation)


#-------------------------------------------------------------------------------
#-------------------------Data preparation--------------------------------------
#-------------------------------------------------------------------------------

load("station_data.Rdata")

station_data <- station_data %>% 
  filter(str_starts(id,"PO"), latitude < 42.2, latitude > 36.8, 
         longitude > -9.6,longitude < -6.1, element=="TMAX") %>% 
  select(id,name) %>% 
  filter(name!="COIMBRA/CERNACHE",name!="TAVIRA")

weather_data <- ghcnd_search(station_data$id, var = c("TMAX"), 
                             date_min = "2014-01-01", date_max = "2015-12-31")

weather_data <- weather_data$tmax %>% 
  select(id,tmax,date) %>% 
  arrange(id,date) %>% 
  fill(tmax)

merge(weather_data,station_data,"id") %>% distinct(id, .keep_all = TRUE) %>% 
  select(id,name)

station_data <- station_data %>% 
  mutate(nearest_district=c("Lisboa","Porto","Coimbra","Beja","Bragança","Faro",
                            "Évora","Castelo Branco")) %>%
  add_row(nearest_district="Viseu",id="PO000008575")  %>% 
  add_row(nearest_district="Aveiro",id="PO000008575") %>% 
  add_row(nearest_district="Braga",id="PO000008575")  %>% 
  add_row(nearest_district="Viana do Castelo",id="PO000008575") %>% 
  add_row(nearest_district="Vila Real",id="PO000008575") %>% 
  add_row(nearest_district="Guarda",id="POM00008570") %>% 
  add_row(nearest_district="Leiria",id="PO000008535") %>% 
  add_row(nearest_district="Santarém",id="PO000008535") %>% 
  add_row(nearest_district="Portalegre",id="POM00008558") %>% 
  add_row(nearest_district="Setúbal",id="PO000008535")
station_data$id[2] <- "PO000008575"
station_data$id[3] <- "POM00008570"
station_data$id[4] <- "POM00008558"

temp_data <- merge(station_data,weather_data,"id",all.x = TRUE) %>% 
  as_tibble(.) %>% 
  select(nearest_district,tmax,date) %>% 
  rename(district=nearest_district) %>% 
  mutate(tmax= (tmax/10))

###############################################################################

fires_train <- read_csv("fires_train.csv", na=c("-","NA"))

fires_train
summary(fires_train)
spec(fires_train)
fires_train %>% find_na(index=FALSE)
fires_train %>% select(find_na(.)) %>% diagnose()
unique(select(fires_train,district))

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-23",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
# data_preprocessing function:
#   - takes out    
#         is not relevant in our opinion: municipality, parish, lat, lon, 
#                               firstInterv_date/hour, extincition_date/hour
#         has only NA:alert_source
#         is the sum of others: village_veget_area, total_area
#   - puts together the two Viana do Castelo of district different written 
#   - makes alert_date and Date object
#   - creates a date column to merge it with temp_data
#   - merge the dataset with temp_data by date and district
#   - because their can be dates with no tmax:
#         creates an help dataset with these rows 
#         add them to the temp_data 
#         fill it with the tmax of the date before in the same district
#   - merge the dateset again with temp_data
#   - makes a season column out of the date column
#   - creates a weekday column out of the alert_date
#   - transforms district into litoral and interior
#   - transforms alert_hour into dawn,morning,afternoon,night
#   - takes out alert_date
data_preprocessing <- function(dataset){
  dataset <- dataset %>% 
    select(-c(municipality,parish,alert_source,lat,lon,region,
              firstInterv_date,firstInterv_hour,total_area,village_veget_area,
              extinction_date,extinction_hour)) %>% 
    mutate(district=replace(district,
                            district=="Viana Do Castelo","Viana do Castelo")) %>% 
    mutate(alert_date=as.Date(alert_date)) %>% 
    mutate(date=alert_date) %>% 
    merge(temp_data,.,c("district","date"), all.y = TRUE) %>%
    as_tibble(.)
  dataset %>% find_na(index=FALSE)
  help <- dataset %>% subset(is.na(tmax)) %>% 
    select(district,date,tmax)
  temp_data <- temp_data %>% add_row(help) %>% 
    arrange(district,date) %>% 
    fill(tmax) %>%
    unique(.)
  dataset <- dataset %>% 
    select(-c(tmax)) %>%
    merge(temp_data,.,c("district","date")) %>%
    as_tibble(.) %>% 
    rename(season = date) %>%
    mutate(season = getSeason(season), weekday = as.factor(weekdays(alert_date))) %>%
    mutate(weekday = ordered(weekday, levels=c("Monday","Tuesday","Wednesday",
                                               "Thursday","Friday","Saturday","Sunday")))
  litoral <- c("Viana do Castelo","Braga","Porto","Aveiro","Coimbra","Leiria",
               "Lisboa","Setúbal","Faro")
  time <- as.POSIXct(strptime(dataset$alert_hour,"%H:%M:%S"),"UTC")
  x=as.POSIXct(strptime(c("000000","060000","120000","180000","235959"),"%H%M%S"),
               "UTC")
  labs=c("dawn","morning","afternoon","night")
  dataset <- dataset %>% 
    mutate(inLitoral=ifelse(district %in% litoral, 1,0),
           timeOfDay=labs[findInterval(time,x)]) #%>% 
    #select(-alert_date)
  return(dataset)
}

fires_train <- data_preprocessing(fires_train) %>% 
  mutate(intentional_cause=as.factor(intentional_cause))


#-------------------------------------------------------------------------------
#-------------------------data exploratory analysis-----------------------------
#-------------------------------------------------------------------------------


summary(fires_train)

# How many intentional and non intentional fires do we have?
fires_train %>% group_by(intentional_cause) %>% count()


ggplot(fires_train, aes(x=weekdays,y=origin)) + geom_s

# What's the distribution of the fires according to the season?
ggplot(fires_train, aes(x=season,fill= intentional_cause)) + geom_bar() +
  ggtitle("Relationship between season and intentional cause")

# What's the distribution of the fires according to the weekday?
ggplot(fires_train, aes(x=weekday,fill= intentional_cause)) + geom_bar() + 
  ggtitle("Relationship between weekdays and intentional cause")

# Could there be a relationship between "intentional_cause" and the maximum 
# temperature?   É MELHOR POR EM PERCENTAGEM
ggplot(fires_train, aes(x=tmax, fill= intentional_cause)) + geom_histogram(binwidth = 2) + 
  ggtitle("Relationship between maximum temperature and intentional cause")

# Could there be a relationship between "intentional_cause" and the time it was 
# alerted?
ggplot(fires_train, aes(x=alert_hour, fill= intentional_cause)) + 
  geom_histogram(binwidth = 3600) + 
  scale_x_time()

# What can we say about the "origin" and the "intentional_cause"?
fires_train %>% group_by(origin,intentional_cause) %>% count() %>% 
  arrange(desc(n))

ggplot(fires_train, aes(x=intentional_cause)) + geom_bar() + facet_wrap(~origin)
# We can see in the graph that the most fires with "intentional_cause" have 
# origin "firepit".

# Relationship between "tmax", "origin" and "intentional_cause"
ggplot(fires_train, aes(x=tmax, fill= intentional_cause)) + 
  geom_histogram(binwidth = 2) + 
  facet_grid(~origin)

# What is the correlation coefficient between all numeric attributes?
fires_train %>% select(tmax,village_area, farming_area, 
                 vegetation_area) %>% cor()

# Is there any monotonic relationship?
fires_train %>% select(tmax,village_area, farming_area, 
                 vegetation_area) %>% cor(method = "spearman")


#-------------------------------------------------------------------------------
#-------------------------------Predictive Modeling-----------------------------
#-------------------------------------------------------------------------------


fires_test <- read_csv("fires_test.csv", na=c("-","NA"))

fires_test <- data_preprocessing(fires_test)

normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}
normalize_data <- function(dataset){
  dataset <- dataset %>% 
    mutate(tmax=normalize(tmax),
           farming_area=normalize(farming_area),
           village_area=normalize(village_area),
           vegetation_area=normalize(vegetation_area)) %>% 
    mutate(firepit=ifelse(origin=="firepit",1,0),
           fire=ifelse(origin=="fire",1,0),
           agriculture=ifelse(origin=="agriculture",1,0),
           agric_burn=ifelse(origin=="agric_burn",1,0),
           false_alarm=ifelse(origin=="false_alarm",1,0)) %>% 
    mutate(winter=ifelse(season=="Winter",1,0),
           spring=ifelse(season=="Spring",1,0),
           sommer=ifelse(season=="Sommer",1,0),
           fall=ifelse(season=="Fall",1,0)) %>%
    mutate(Monday=ifelse(weekday=="Monday",1,0),
           Tuesday=ifelse(weekday=="Tuesday",1,0),
           Wednesday=ifelse(weekday=="Wednesday",1,0),
           Thursday=ifelse(weekday=="Thursday",1,0),
           Friday=ifelse(weekday=="Friday",1,0),
           Saturday=ifelse(weekday=="Saturday",1,0),
           Sunday=ifelse(weekday=="Sunday",1,0)) %>%
    mutate(district=ifelse(district=="litoral",1,0)) %>%
    mutate(dawn=ifelse(alert_hour=="dawn",1,0),
           afternoon=ifelse(alert_hour=="afternoon",1,0),
           night=ifelse(alert_hour=="night",1,0),
           morning=ifelse(alert_hour=="morning",1,0)) %>%
    select(-c(origin,season,weekday,alert_hour))
  return(dataset)
}

fires_train_norm <- normalize_data(fires_train)
fires_test_norm <- normalize_data(fires_test)
id <- fires_test %>% select(id)
fires_test_norm <- fires_test_norm %>% select(-id)
fires_train_norm <- fires_train_norm %>% select(-id)



fires_train1 <- select(fires_train,-c(tmax))
fires_test <- select(fires_test,-c(hour,id))

res <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train1), 
                             c(Workflow(learner = "randomForest",learner.pars = list(na.action=na.omit,importance=T))
                             ), 
                             EstimationTask(metrics = c("acc", "F", "rec", "prec")))
a <- summary(res)

#a1 completo                              acc:0,6616
#a2 -region                               acc:0,7544
#a3 -region,district                      acc:0,7394
#a4 -region,inlitroal                     acc:0,7533
#a5 -regin,inLitoral,district             acc:0,7353
#a6 -region inliteroal,alertdate          acc:0,7490
#a8 -region,inlitoral,weekday             acc:0,7490
#a9 -region inlitoral,weekday,alert-date  acc:0,7464
#a10 a1 + alert_hour                      acc:0,7571
#a11 a2 -hour                             acc:0,7590
#a12 a11 -alert_date                      acc:0,7513
#a13 a11 -season                          acc:0,7577
#a14 a13 -id                              acc:0,7515
#a15 a11 -id                              acc:0,7578
#a16 a15 -inlitoral                       acc:0,7559
#a17 a16 +totalarea,villagevegarea        acc:0,7501
#a18 -region hour id alertdate, +area     acc:0,7490

res <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train), 
                             c(Workflow(learner = "naiveBayes"),   
                               workflowVariants(learner = "rpart",learner.pars = list(maxdepth = c(3,5)),predictor.pars = list(type = "class")), 
                               Workflow(learner = "rpart", predictor.pars = list(type = "class")),
                               Workflow(learner = "randomForest",learner.pars = list(na.action=na.omit,importance=T)),
                               Workflow(learner = "nnet", learner.pars = list(size = 5), predictor.pars = list(type = "class"))
                               ), 
                             EstimationTask(metrics = c("acc", "F", "rec", "prec")))
res_norm <- performanceEstimation(PredTask(intentional_cause ~ ., fires_train_norm), 
                                  c(Workflow(learner = "naiveBayes"),   
                                    workflowVariants(learner = "knn3",learner.pars = list(k=c(3,5,7)),predictor.pars = list(type = "class")),    #!!!!só funciona norm
                                    workflowVariants(learner = "rpart",learner.pars = list(maxdepth = c(3,5)),predictor.pars = list(type = "class")), 
                                    Workflow(learner = "rpart", predictor.pars = list(type = "class")),
                                    Workflow(learner = "randomForest",learner.pars = list(na.action=na.omit,importance=T)),
                                    workflowVariants(learner = "svm", learner.pars = list(kernel = "radial"),predictor.pars = list(type = "class")),    #!!!!só funciona norm
                                    Workflow(learner = "nnet", learner.pars = list(size = 5), predictor.pars = list(type = "class"))
                                  ), 
                                  EstimationTask(metrics = c("acc", "F", "rec", "prec")))

randomForest_model <- randomForest(intentional_cause ~ ., fires_train)
randomForest_preds <- predict(randomForest_model, fires_test)


#plot(res)
#summary(res)
#rankWorkflows(res_norm, max = TRUE)
#pres <- pairedComparisons(res, baseline = "naiveBayes") 
#signifDiffs(pres)



result <- id %>% mutate(intentional_cause=randomForest_preds) %>% arrange(id)
write.csv(result,file="result.csv",row.names = FALSE)


