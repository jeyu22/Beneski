library(caret)
library(randomForest)

model <- readRDS("attendance_weather.Rds")

weather <- subset(model,select= -c(notes,tours,precipType,time))

weather <- weather %>%
  mutate(dow = wday(date,label = TRUE))

weather <- subset(weather,select = -date)

weather$number[is.na(weather$number)] <- 0

weather <- weather %>%
  mutate(
    year=as.factor(year),
    month = as.factor(month),
    day = as.factor(day),
    dow = as.factor(dow))

ok <- data.frame("number"= 1,"year"="2020",month="1","day"="1","temperature"=38.00,dow=wday("2020-01-01",label = TRUE))
weather<-rbind(ok,weather)

rfFit <- randomForest(number ~ ., data=weather, mtry=5, ntree=1000)

saveRDS(rfFit, "rfFit.Rds")

class(weather$dow)
