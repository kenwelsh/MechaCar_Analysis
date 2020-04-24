library(tidyverse)

# import datasets
car_mpg_table <- read.csv('MechaCar_mpg.csv',check.names = T,stringsAsFactors = F)
coil_table <- read.csv('Suspension_Coil.csv',check.names = T,stringsAsFactors = F)

# generate multiple linear regression model for mpg data
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=car_mpg_table)

# get summary stats for multiple linear regression model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=car_mpg_table))

# do a linear regression on vehicle length
lm(mpg ~ vehicle.length,car_mpg_table)
summary(lm(mpg ~ vehicle.length,car_mpg_table))
# linear regression would be mpg = 4.673(vehicle.length) - 25.062

# calculate values for linear regression line for plot
model_length <- lm(mpg ~ vehicle.length,car_mpg_table)
yvals_length <- model_length$coefficients['vehicle.length']*car_mpg_table$vehicle.length + model_length$coefficients['(Intercept)']

# plot vehicle length and mpg with linear regression line
plt <- ggplot(car_mpg_table,aes(x=vehicle.length,y=mpg)) 
plt + geom_point() + geom_line(aes(y=yvals_length), color = "red") +
  labs(caption = "R-squared: 0.3715") +
  ggtitle("Vehicle Length and MPG")+
  theme(plot.title = element_text(hjust = 0.5))


# do a linear regression on ground clearance
lm(mpg ~ ground.clearance,car_mpg_table)
summary(lm(mpg ~ ground.clearance,car_mpg_table))
# linear regression would be mpg = 2.002(ground.clearance) + 19.418

# calculate values for linear regression line for plot
model_clearance <- lm(mpg ~ ground.clearance,car_mpg_table)
yvals_clearance <- model_clearance$coefficients['ground.clearance']*car_mpg_table$ground.clearance + model_clearance$coefficients['(Intercept)']

# plot vehicle length and mpg with linear regression line
plt <- ggplot(car_mpg_table,aes(x=ground.clearance,y=mpg)) 
plt + geom_point() + geom_line(aes(y=yvals_clearance), color = "red") +
  labs(caption = "R-squared: 0.1081") +
  ggtitle("Ground Clearance and MPG")+
  theme(plot.title = element_text(hjust = 0.5))

# create a summary table by manufacturing lot for the coil data
summarize_coil_test <- coil_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Standard_Deviation_PSI=sd(PSI),Min_PSI=min(PSI),Max_PSI=max(PSI),Num_Coils=n()) 

# check mean of total production run compared to stated population mean of 1500
mean(coil_table$PSI)

# create tables for each manufacturing lot
coil_lot1 <- coil_table %>% filter(Manufacturing_Lot=='Lot1')
coil_lot2 <- coil_table %>% filter(Manufacturing_Lot=='Lot2')
coil_lot3 <- coil_table %>% filter(Manufacturing_Lot=='Lot3')
coil_filt <- coil_table[,c("Manufacturing_Lot","PSI")]

# do an analysis of variance across the manufacturing lots
aov(PSI ~ Manufacturing_Lot, data=coil_filt)
summary(aov(PSI ~ Manufacturing_Lot, data=coil_filt))

# check the t-test for the total run and each individual lot compared
# to the mean population of 1500 psi
t.test(coil_table$PSI,mu=1500)
t.test(coil_lot1$PSI,mu=1500)
t.test(coil_lot2$PSI,mu=1500)
t.test(coil_lot3$PSI,mu=1500)









