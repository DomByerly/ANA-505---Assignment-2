# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)
##Lines 3-4 are calling the necessary libraries needed to establish our function for the linear regression and to create a graphics package
library(car)  # special functions for linear regression
library(lattice)  # graphics package
##Lines 6-7 are creating the dataset for all 81 dodger home games, as well as checking for the structure of the data frame by making sure it is clean and organized
# read in data and create a data frame called dodgers
dodgers <- read.csv("dodgers.csv")
print(str(dodgers))  # check the structure of the data frame
##Lines 12-20 establish each day of the week as a number from 1-7, as well as assigning labels or abbreviations for each day of the week
# define an ordered day-of-week variable 
# for plots and data summaries
dodgers$ordered_day_of_week <- with(data=dodgers,
  ifelse ((day_of_week == "Monday"),1,
  ifelse ((day_of_week == "Tuesday"),2,
  ifelse ((day_of_week == "Wednesday"),3,
  ifelse ((day_of_week == "Thursday"),4,
  ifelse ((day_of_week == "Friday"),5,
  ifelse ((day_of_week == "Saturday"),6,7)))))))
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))
##Lines 24-25 is labeling the x axis as "day of week" and the y-axis as "Attendence" with the xlab and ylab commands; 
# exploratory data analysis with standard graphics: attendance by day of week
with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
xlab = "Day of Week", ylab = "Attendance (thousands)", 
col = "violet", las = 1))
##Line 28 created a table that measured when bobbleheads are use mostly by day of the week and it is found that most bobblehead promotions are on tuesday
# when do the Dodgers use bobblehead promotions
with(dodgers, table(bobblehead,ordered_day_of_week)) # bobbleheads on Tuesday
##Lines 32-40 are establishing a test,yes or no ifelse function for each month of the year that the dodgers play. This assigns each month a number, as we see in line 39 with the ordered_month and levels 4-10, They also added labels for the months in line 40.
# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
  ifelse ((month == "APR"),4,
  ifelse ((month == "MAY"),5,
  ifelse ((month == "JUN"),6,
  ifelse ((month == "JUL"),7,
  ifelse ((month == "AUG"),8,
  ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))
##Lines 43-44 look to be establishing a plotting function with the dodgers home game data ordered by month and with attendence as the y variable. 
# exploratory data analysis with standard R graphics: attendance by month 
with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month", 
ylab = "Attendance (thousands)", col = "light blue", las = 1))
##Lines 51-55 look to be establishing the parameters for a scatterplot, which we see with the xyplot command. This scatterplot has attendence and temperature on the same axis and skies and day_night on the other. We also see in these lines the group commands that are used in order to establish the labels, symbols and colors. 
# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) # used for plotting 
# let us prepare a graphical summary of the dodgers data  ## in line 56 we see that the scatterplot is being setup and the pch command to create a character that will be plotted in the R function.
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black","black") ## In line 57-59 we see that we use the aspect function to set the aspect ratio of plots and layout to divide the device into many rows and columns. 
group.fill <- c("black","red") ##In line 59 we see they use the strip function to remove un wanted characters.
xyplot(attend/1000 ~ temp | skies + day_night, ## Lines 60-61 is establishing the x and y axis labels. 
    data = dodgers, groups = fireworks, pch = group.symbols, 
    aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
    layout = c(2, 2), type = c("p","g"),
    strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
    xlab = "Temperature (Degrees Fahrenheit)", 
    ylab = "Attendance (thousands)",
    key = list(space = "top", 
        text = list(rev(group.labels),col = rev(group.colors)), ##The text function establishes the the group labels
        points = list(pch = rev(group.symbols), col = rev(group.colors),  ## the points fucntion is used to draw a sequence of points at the specific coordinates.
        fill = rev(group.fill))))  ##The fill function is used to fill in missing values in the columns.
                
# attendance by opponent and day/night game ##The next group of lines 67-81 are used to prepare a graphical summary of the dodgers data to create a graph that compares the relationship between attendence and whether or not the game is at day or night.
group.labels <- c("Day","Night") ##Lines 68-70 are establishing the labels, symbols and size of the variable day_night. 
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night, ##Line 71 establishes a box and whisker plot that includes opponent, attendence and day_night
    xlab = "Attendance (thousands)",
    panel = function(x, y, groups, subscripts, ...) ##The panel command is establishing the data as observed across time, or cross-sectional time series data.
       {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
        panel.stripplot(x, y, groups = groups, subscripts = subscripts, ##The stripplot command marks data as a series of marks against a single magnitude axis.
        cex = group.symbols.size, pch = group.symbols, col = "darkblue") ##The cex command establishes the amount by which text and symbols should be scaled to the default.
       },
    key = list(space = "top", 
    text = list(group.labels,col = "black"), ##The text function establishes the group label names
    points = list(pch = group.symbols, cex = group.symbols.size, ##This establishes a sequence of points at specific coordinates.
    col = "darkblue"))) ##This establishes the column indexes as dark blue
     
# employ training-and-test regimen for model validation
set.seed(1234) # set seed for repeatability of training-and-test split
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),
rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
dodgers$training_test <- sample(training_test) # random permutation 
dodgers$training_test <- factor(dodgers$training_test, ##This is creating multiple permutations of the training-and test regiment model
  levels=c(1,2), labels=c("TRAIN","TEST")) ##Establishes two levels and the training and test regiment
dodgers.train <- subset(dodgers, training_test == "TRAIN") ##Lines 90-91 and 92-93 are establishing the subest for the training regiment and then the test regiment. The print commands are printing the argument and retuning them invisibly.
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame
##Line 96 specifies a model that includes attendence, month, day of week and bobblehead specifically entered last. 
# specify a simple model with bobblehead entered last
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}
##Line 99 establishes a predictive model fit to the training set. we see that lm is used to define a linear model. 
# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)
# summary of model fit to the training set
print(summary(train.model.fit)) ##The print and summary command develop an invisible summary of the training regiment we just employed. 
# training set predictions from the model fit to the training set
dodgers.train$predict_attend <- predict(train.model.fit) ##This line has the predictions that came from the training regiment. 
##In line 106-107 we see that they used the training set to test the predictions from the test set using newdata
# test set predictions from the model fit to the training set
dodgers.test$predict_attend <- predict(train.model.fit, 
  newdata = dodgers.test) ##newdata is used here to create an optional data frame to look for variables to accurately predict.

# compute the proportion of response variance
# accounted for when predicting out-of-sample
cat("\n","Proportion of Test Set Variance Accounted for: ", ##These lines are used for when we are predicting data based on past data we have had already. Cat is used to convert its argument to strings and generate output from our defined functions. 
round((with(dodgers.test,cor(attend,predict_attend)^2)),
  digits=3),"\n",sep="")

# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test) ##The rbind command is used to combine bot the traing and test data frames together. 

# generate predictive modeling visual for management ## the following set of lines 119-139 establish a predictive linear visual model that best encapsulates our data and business question so that it is ready to be viewed by management.
group.labels <- c("No Bobbleheads","Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")  
xyplot(predict_attend/1000 ~ attend/1000 | training_test, ##we see here that are xyplot containes predicted attendence, actual attendence as well as data from the training regiment.
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2, ##we see that bobbleheads is a group, which is important because it is the main business question.
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2, 1), xlim = c(20,65), ylim = c(20,65), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...) ##these 4 lines are using the panel command to establish our data as cross-sectional time-series data. 
            {panel.xyplot(x,y,...)
             panel.segments(25,25,60,60,col="black",cex=2)
            },
       strip=function(...) strip.default(..., style=1), ##here we are stripping unwanted characters with the strip command.
       xlab = "Actual Attendance (thousands)",  ##The x axis is defined to be the actual attendece while the ylab is using predicited attendence.
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top", 
              text = list(rev(group.labels),col = rev(group.colors)),
              points = list(pch = rev(group.symbols), 
              col = rev(group.colors),
              fill = rev(group.fill))))            
        
# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 
my.model.fit <- lm(my.model, data = dodgers)  # use all available data
print(summary(my.model.fit)) ##This uses the full data set to estimate the increase in attendence due to bobblehead promotion, which is the overreaching question we want to solve.
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
print(anova(my.model.fit))  

cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ", ##This line converts the argument into character strings and uses the my.model command from earlier to round off values we have gotten from the coefficients.
round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
digits = 0),"\n",sep="")

# standard graphics provide diagnostic plots
plot(my.model.fit) ## a diagnostic plot of our final model fit us plotted

# additional model diagnostics drawn from the car package
library(car) ## a seperate model is drawn from the car library package and subsequently graphed modeled and printed.
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

# Suggestions for the student:
# Examine regression diagnostics for the fitted model.
# Examine other linear predictors and other explanatory variables.
# See if you can improve upon the model with variable transformations.



