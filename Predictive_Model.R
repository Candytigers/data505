#dot<-read.csv("~/Downloads/Data Challenge/Correlation_file_with_added_variables-weather+gameEvent_model.csv")
attach(dot)
set.seed(333)
dotTrain <- 
  dot %>% 
  sample_frac(.8)
dotTest <- 
  dplyr::setdiff(dot, dotTrain) 
#1.bicycle-all
dotOrg_bic_all <- lm(BICYCLISTS ~ Temperature_Squres + Humidity + Wind.Speed.mph + 
                           Condition+ IsGameTime + Prime.Time, data = dotTrain)
summary(dotOrg_bic_all)
resultsOrg_bic_all <- 
  dotTest %>%
  mutate(predicted_bic_all = predict(dotOrg_bic_all, dotTest))
View(resultsOrg_bic_all)
#performance1 <- metric_set(rmse, mae)
#performance1(resultsOrg_bic_all, truth=BICYCLISTS, estimate=predicted_bic_all)

#2.trucks--all
dotOrg_truck_all <- lm(TRUCKS ~ Temperature_Squres + Humidity + Wind.Speed.mph + 
                           Condition+ IsGameTime + Prime.Time, data = dotTrain)
summary(dotOrg_truck_all)
resultsOrg_truck_all <- 
  dotTest %>%
  mutate(predicted_truck_all = predict(dotOrg_truck_all, dotTest))
View(resultsOrg_truck_all)
#performance2 <- metric_set(rmse, mae)
#performance2(resultsOrg_truck_all, truth=TRUCKS, estimate=predicted_truck_all)

#3.pedestrian--all
dotOrg_ped_all <- lm(PEDESTRIANS ~ Temperature_Squres + Humidity + Wind.Speed.mph + 
                           Condition+ IsGameTime + Prime.Time, data = dotTrain)
summary(dotOrg_ped_all)
resultsOrg_ped_all <- 
  dotTest %>%
  mutate(predicted_ped_all = predict(dotOrg_ped_all, dotTest))
View(resultsOrg_ped_all)
#performance3 <- metric_set(rmse, mae)
#performance3(resultsOrg_ped_all, truth=PEDESTRIANS, estimate=predicted_ped_all)


#4.buses--all
dotOrg_bus_all <- lm(BUSES ~ Temperature_Squres + Humidity + Wind.Speed.mph + 
                           Condition+ IsGameTime + Prime.Time, data = dotTrain)
summary(dotOrg_bus_all)
resultsOrg_bus_all <- 
  dotTest %>%
  mutate(predicted_bus_all = predict(dotOrg_bus_all, dotTest))
View(resultsOrg_bus_all)
#performance4 <- metric_set(rmse, mae)
#performance4(resultsOrg_bus_all, truth=BUSES, estimate=predicted_bus_all)

#5.cars--all
dotOrg_car_all <- lm(CARS ~ Temperature_Squres + Humidity + Wind.Speed.mph + 
                           Condition + IsGameTime + Prime.Time, data = dotTrain)
summary(dotOrg_car_all)
resultsOrg_car_all <- 
  dotTest %>%
  mutate(predicted_car_all = predict(dotOrg_car_all, dotTest))
#performance5 <- metric_set(rmse, mae)
#performance5(resultsOrg_car_all, truth=CARS, estimate=predicted_car_all)



