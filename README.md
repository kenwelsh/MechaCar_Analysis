# MechaCar Analysis

### MechaCar Technical Report

<a href="https://kenwelsh.github.io/MechaCar_Analysis/" target= "_blank">Link to Technical Report - HTML version</a>

<a href="https://github.com/kenwelsh/MechaCar_Analysis/blob/master/MechaCarWriteUp.txt" target= "_blank">Link to Technical Report - TXT version</a>


#### MPG Multiple Linear Regression

The data analytics team has performed a multiple linear regression on the data provided in the MechaCar mpg dataset.  Vehicle length returned a p-value of 2.60e-12, and ground clearance yielded a p-value of 5.21e-08.  Based on these values, vehicle length and ground clearance are statistically unlikely to provide random amounts of variance to the linear model.  R-squared for the model is 0.71, which means that roughly 71% of all mpg predictions will be correct.  The p-values for vehicle length and ground clearance, as well as the r-squared value of 0.71, provide sufficient evidence to reject our null hypothesis, which means that the slope of our multiple linear model is not zero.


![]( https://github.com/kenwelsh/MechaCar_Analysis/blob/master/static/images/MPG_Multi_Linear_Regression.png "MPG Multiple Linear Regression")


While the multiple linear regression model does an excellent job of predicting mpg for the given data set, it will fail to predict future data correctly.  The intercept of the model has a p-value of 5.08e-08, which is statistically significant. The intercept being statistically significant means that some other variables and factors not included in the model contribute to the variation in mpg.  The lack of significant variables in the multiple linear model is evidence of overfitting.


Performing a simple linear regression on the vehicle length and ground clearance data, the r-squared are 0.3715 and 0.1081, respectively.  These r-squared values show that as individual measures, the linear regression model for vehicle length is only 37.2% accurate at predicting mpg, and the model for ground clearance is only 10.8% accurate.  The multiple linear regression model, with an r-squared of 0.71, does a far better job of predicting mpg but needs to be expanded to contain more significant variables.  Some other factors to include in the dataset, or to control for in the testing, are drag coefficient, engine size, horsepower, air intake, tire size, tire pressure, driving conditions, speed, and braking.


<img src="https://github.com/kenwelsh/MechaCar_Analysis/blob/master/static/images/Veh_Length_MPG_Linear_Regression_Plot.png" width="425"/> <img src="https://github.com/kenwelsh/MechaCar_Analysis/blob/master/static/images/Clearance_MPG_Linear_Regression_Plot.png" width="425"/>



#### Suspension Coil Summary
The data analytics team received a suspension coil test dataset to analyze.

##### Manufacturing Lot Summary Statistics (Coil PSI)
<table class="table table-striped">
                        <thead class="thead-light">
                          <tr>
                            <th>Lot</th>
                            <th>Mean</th>
                            <th>Median</th>
                            <th>Variance</th>
                            <th>Std Deviation</th>
                            <th>Min</th>
                            <th>Max</th>
                            <th>Count</th>
                          </tr>
                        </thead>
                        <tbody>
                          <tr>
                            <td>Lot 1</td>
                            <td>1500.0</td>
                            <td>1500.0</td>
                            <td>0.9796</td>
                            <td>0.9897</td>
                            <td>1498</td>
                            <td>1502</td>
                            <td>50</td>
                          </tr>
                          <tr>
                            <td>Lot 2</td>
                            <td>1500.2</td>
                            <td>1500.0</td>
                            <td>7.4694</td>
                            <td>2.7330</td>
                            <td>1494</td>
                            <td>1506</td>
                            <td>50</td>
                          </tr>
                          <tr>
                            <td>Lot 3</td>
                            <td>1496.1</td>
                            <td>1498.5</td>
                            <td>170.2861</td>
                            <td>13.0494</td>
                            <td>1452</td>
                            <td>1542</td>
                            <td>50</td>
                          </tr>
                        </tbody>
                    </table>


Based on the data provided, it appears that there has been a change in a variable or variables between manufacturing lots.  We see an increase in variance from lot one to lot tow, and then again between lot two and lot three.  The variance of lot three is over 170 – far exceeding the allowable variance of 100 PSI stated in the design specifications.  A review of the manufacturing process is necessary to conclude what is driving the increasing difference between lots.

#### Suspension Coil T-Test

An analysis of variance (ANOVA) test of the manufacturing lots returns a p-value 0.014, confirming there is a significant difference in PSI between a least one of the manufacturing lot.

![]( https://github.com/kenwelsh/MechaCar_Analysis/blob/master/static/images/coil_anova1.png "Coil Lot PSI ANOVA Test")

Performing a t-test to determine if the coil’s pound-per-inch results are statistically different from the mean population results of 1,500 pounds per inch, we found the p-value for the total dataset to be 0.06.  Based on a significance level of 0.05, this reading would just barely allow us to state that the mean population and the total production run are statistically similar.  Digging deeper into the individual lots, we find that lot one has a p-value of 1 and lot two a p-value of 0.61, meaning they are statistically similar to the population mean of 1500 PSI.  Lot three has a p-value of 0.04, which means there is a statistical difference to the population mean.


##### Coil Manufacturing Lot  One Sample T-test to 1500 Total Population Mean
<table class="table table-striped">
                        <thead>
                          <tr>
                            <th>Lot</th>
                            <th>P-value</th>
                            <th>Finding</th>
                          </tr>
                        </thead>
                        <tbody>
                          <tr>
                            <td>Total Run</td>
                            <td>0.06028</td>
                            <td>statistically similar</td>
                          </tr>
                          <tr>
                            <td>Lot 1</td>
                            <td>1.00000</td>
                            <td>statistically similar</td>
                          </tr>
                          <tr>
                            <td>Lot 2</td>
                            <td>0.60720</td>
                            <td>statistically similar</td>
                          </tr>
                          <tr>
                            <td>Lot 3</td>
                            <td>0.04168</td>
                            <td>statistically different</td>
                          </tr>
                        </tbody>
                    </table>



#### Suggested Studies to Support MechaCar Launch
A vehicle purchase is a significant economic decision for the average consumer, usually representing the second-largest purchase decision an individual will make in their lifetime (buying a home being the largest).  Determining the price range that MechaCar will compete in is essential to setting production targets.


##### Vehicle Sales by Price Study
We can first determine if there is a correlation between the vehicle purchase price and unit sales volumes using a simple linear regression model on US vehicle sales data for the past five years.  Binning the purchase price into logical price buckets will allow us to visualize volumes by price.  Understanding this relationship will help us determine the approximate total market potential for the price range that MechaCar will target.


##### Follow On Studies
Determining the target price range for MechaCar will allow us to segment the US vehicle sales history for further analysis.  We will need to acquire Vehicle Identification Number (VIN) as part of the US Vehicle Sales History dataset.  VIN data will allow us to determine the make, model, and trim level of each vehicle.  We can then perform a multiple linear regression to see if body style, engine size, horsepower, mpg, and optional equipment levels affect vehicle sales volumes.  Assuming these factors do affect sales volumes, the model will inform us of the significance of each element toward sales volumes.  We will be able to help inform the final design decisions of MechaCar prototypes.


We will also be able to determine the most popular colors for vehicles sold in the past five years.  Using vehicle sales by color by month, we can do a simple linear regression on sales by color by month for each color for the five years, the past year, and the most recent six months.  We will be able to tell the most popular colors, the longer-term trend (+/-) for each color, and the short-term trend (allows us to discover emerging trends in vehicle colors).


The pricing analysis will allow us to determine the total addressable market we will be targeting for MechaCar.  The study of vehicle features and color preference will enable us to set internal sales targets and to estimate production runs.

