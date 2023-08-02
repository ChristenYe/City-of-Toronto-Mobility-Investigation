# Read data
library(readxl)
ctv <- read_excel("C:/Users/yeych/Desktop/lm.xlsx")
View(ctv)

signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

ctv$log_pedestrian_total = signedlog10(ctv$pedestrian_total)
ctv$log_bus_total = signedlog10(ctv$bus_total)
ctv$log_truck_total = signedlog10(ctv$truck_total)
model = lm(Collision_Number_at_Each_Intersection~car_total+log_truck_total+
            log_bus_total+log_pedestrian_total+cyclist_total
           ,data=ctv)
summary(model)
