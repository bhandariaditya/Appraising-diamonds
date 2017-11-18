####IDS 575 Project - Appraising dm####

##importing libraries for visualizations
library(ggplot2)
library(ggvis)
library(dplyr)
#For plotting multiple ggplots in one 
library(grid)
library(gridExtra)

##reading the file and describing each varible type statistically 
dm <- read.csv("~/Desktop/IDS575/IDS575 Project/diamonds.csv")
dm <- dm[-1] #dropping the first column(X) as it is a just serial number 
attach(dm) #attaching data for using the variables directly
str(dm) #describes variable
summary(dm) #gives statistics of the data
head(dm) #looking at the first few rows of dataset

#using facet wrap, plots for each category of the given variable
#plotting boxplots for possible skews and outliers for price vs cut
ggplot(dm, aes(x=cut, y=price))+geom_boxplot()+facet_wrap(~clarity)
#boxplots doesn't explain possible bimodality, violin plots can help and understanding distribution
ggplot(dm, aes(x=cut, y=price))+geom_violin()+facet_wrap(~clarity)


##exploring patterns in data for possible relationships using ggplots
"Plotting bar charts for different categorical variables to 
identify number of diamonds in each category"
p1<-ggplot(dm, aes(x=cut,fill=color))+geom_bar()
p2<-ggplot(dm, aes(x=cut,fill=clarity))+geom_bar()
p3<-ggplot(dm, aes(x=clarity,fill=cut))+geom_bar()
p4<-ggplot(dm, aes(x=clarity,fill=color))+geom_bar()
p5<-ggplot(dm, aes(x=color,fill=clarity))+geom_bar()
p6<-ggplot(dm, aes(x=color,fill=cut))+geom_bar()
#below line is for plotting all 6 histograms in a single plot 
grid.arrange(top="Diamond Count by Categories", p1,p2,p3,p4,p5,p6, ncol=3)

#plotting price histogram with categorical variable to categorize each bin
h1<-ggplot(dm, aes(x=price,fill=cut))+geom_histogram(binwidth = 1500, bins=15)
h2<-ggplot(dm, aes(x=price,fill=clarity))+geom_histogram(binwidth = 1500, bins = 15)
h3<-ggplot(dm, aes(x=price,fill=color))+geom_histogram(binwidth = 1500, bins = 15)
grid.arrange(top="Histogram for price vs cut,color, and clarity", h1,h2,h3, ncol=1)

#Building histogram for cut using clarity bars for each cut category
ggplot(dm)+geom_bar(mapping = aes(x=cut, fill=color), position = "dodge")
ggplot(dm)+geom_bar(mapping = aes(x=cut, fill=clarity), position = "dodge")

#Plotting scatter plot of variables against the price variable for any relationship in the data 
require(ggplot2)
require(reshape2)

# plot each feature against price
dm1 <- melt(dm, id="price") #converts object into molten dataframe, Melt a data frame into form suitable for easy casting
ggplot(dm1, aes(x=value, y=price)) + #aesthetic mapping
  facet_wrap(~variable, scales="free") + 
  geom_point()# used to create scatterplots, It can be used to compare one continuous and one categorical variable, or two categorical variables

#These steps will be helpful in calculating correlation of variables
#Since each variable has to be of numeric type for cor function
a = data.frame(as.numeric(cut),as.numeric(color),as.numeric(clarity)) #converting factor level variables into numeric
dm_new<-data.frame(dm, a) #adding the new numeric variables to a new dataframe
dm_new<-dm_new[-c(2:4)] #dropping categorical columns and added their numeric value columns
corrmatrix <- cor(dm_new, use="complete.obs")[4,] #Buiding correlation matrix
#dropping correlation of price with itself and keeping only decent correlation
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5][-4] 

#Looking at the correlation, some useful features for price were carat, x,y, and z 
##Plotting all possible combinations of price with carat, x,y, and z variables 

theme_set(theme_bw()) #setting the plot background as white
#Plotting Carat vs Price
ggplot(dm, aes(x=price, y=carat, color=clarity,shape=cut))+geom_point()+ggtitle("coloring-clarity, shape-cut")
ggplot(dm, aes(x=price, y=carat, color=clarity,shape=color))+geom_point()+ggtitle("coloring-clarity, shape-color")
ggplot(dm, aes(x=price, y=carat, color=cut, shape=color))+geom_point()+ggtitle("coloring-cut, shape-color")
ggplot(dm, aes(x=price, y=carat, color=cut, shape=clarity))+geom_point()+ggtitle("coloring-cut, shape-clarity")
ggplot(dm, aes(x=price, y=carat, color=color,shape=cut))+geom_point()+ggtitle("coloring-color, shape-cut")
ggplot(dm, aes(x=price, y=carat, color=color,shape=clarity))+geom_point()+ggtitle("coloring-color, shape-clarity")

#plotting x vs price
ggplot(dm, aes(x=price, y=x, color=clarity,shape=cut))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=x, color=clarity,shape=color))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=x, color=cut, shape=color))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=x, color=color,shape=cut))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=x, color=color,shape=clarity))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=x, color=cut, shape=clarity))+geom_point()+ggtitle("coloring-, shape-")

#plotting y vs price
ggplot(dm, aes(x=price, y=y, color=clarity,shape=cut))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=y, color=clarity,shape=color))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=y, color=cut, shape=color))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=y, color=color,shape=cut))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=y, color=color,shape=clarity))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=y, color=cut, shape=clarity))+geom_point()+ggtitle("coloring-, shape-")

#plotting z vs price
ggplot(dm, aes(x=price, y=z, color=clarity,shape=cut))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=z, color=clarity,shape=color))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=z, color=cut, shape=color))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=z, color=color,shape=cut))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=z, color=color,shape=clarity))+geom_point()+ggtitle("coloring-, shape-")
ggplot(dm, aes(x=price, y=z, color=cut, shape=clarity))+geom_point()+ggtitle("coloring-, shape-")

#plotting how cut impacts depth and clarity using violin plots
ggplot(diamonds,aes(x=clarity,y=depth))+
  geom_violin(aes(fill=cut))+theme_classic()+
  labs(title="How Cut impacts depth and clarity")

#going all out in a single plot
dm_samp = dm[sample(1:length(dm$price),5000),]
install.packages("GGally") #extension of ggplot2
install.packages("plotly") #create interactive web graphics 'via plotly.js'
library(GGally)
library(plotly)

#plotting all the possible combinations like scatter, histogram, correlation, boxplots etc.
ggpairs(dm_samp) 

#using ggplot2 and plotly for interactivity in plots
#Very useful for identifying unique points just by hovering over them
#plotting price vs carat and coloring by cut
g = ggplot(dm, aes(x=carat, y=price, color=cut))+geom_point()
ggplotly(g)

#plotting price vs carat and using carat for both coloring and sizing 
plot_ly(dm, x=~carat, y=~price, color = ~carat, size=~carat,
        text=~paste("Clarity: ", clarity))

#Plotting price vs carat using cut as category
#Smoothing by cut, added for fitting lines to curve patterns 
p<- ggplot(dm, aes(x=carat, y=price))+
  geom_point(aes(text=paste("Clarity: ", clarity)))+
  geom_smooth(aes(color=cut, fill=cut))+facet_wrap(~cut)
ggplotly(p)

#2D Histogram for clarity vs cur, coloring by count 
count <- with(dm, table(cut, clarity))#counting diamonds by cut and clarity to use in histogram
plot_ly(dm, x=~cut, y=~clarity, z=~count) %>% #using %>% matrix multiplication as we do not need 3D
  add_histogram2d()

###Prediction and Modeling###
#dividing dataset into 60% training and 40% validation
sm = sample(1:nrow(dm), 0.6*nrow(dm)) #creating a sample proportion to be chosen
train = dm[sm,] #creating training set
test = dm[-sm,] #creating testing set

#using simple decision tree for identifying important variable in prediction of price
library(tree)
dm.tree = tree(price~., dm, subset=sm) #building decision tree model on training data
plot(dm.tree)     #plotting the tree
text(dm.tree, pretty=0) #labelling the tree with categorical variables also if there are any
yhat_train = predict(dm.tree, train) #predicting on traing data
yhat_test = predict(dm.tree, test) #predicting on testing data
res_train = mean((yhat_train - train$price)^2) #calculating mean square error for training
res_test = mean((yhat_test - test$price)^2) #calculating mean square error for testing
rmse_train <- sqrt(res_train) #calculatin root mean square for training 
rmse_test <- sqrt(res_test) #calculatin root mean square for testing

#plotting regression line on predicted vs actual values
plot(yhat_test, test$price, title("Actual vs Predicted Price - Decison Tree"))
abline(0,1)
plot(yhat_train, train$price, title("Actual vs Predicted Price on Training"))
abline(0,1)

#fitting linear model 
dm_lm = lm(price~.,train)
summary(dm_lm)

#All the steps will be same here as used for decision tree for calculation of RMSE
pred_train<-predict(dm_lm, train)
pred_test<-predict(dm_lm, test)
res_train = mean((pred_train - train$price)^2)
res_test = mean((pred_test - test$price)^2)
rmse_train <- sqrt(res_train)
rmse_test <- sqrt(res_test)

plot(pred_test, test$price, title("Actual vs Predicted Price - Linear"), xlim = c(-1000,18000))
abline(0,1)
grid(col = 'blue')
plot(pred_train, train$price, title("Actual vs Predicted Price on Training"), xlim = c(-1000,18000))
abline(0,1)
grid(col = 'blue')

#Using Cross-Validation method to test RMSE
set.seed(1234) #setting seed so that next time data remains same
# Fit lm model using 10-fold CV: model
install.packages("caret") #classification and regression training
library(caret)
model <- train(
  price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
print(model)

set.seed(1234)
# Fit lm model using 5*5-fold CV: model
model_5 <- train(
  price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,repeats=5,
    verboseIter = TRUE
  )
)

# Print model to console
print(model_5)

#running 10-Fold CV model on entire dataset for RMSE
p<-predict(model,diamonds)
residual<-p-diamonds$price
rmse<-sqrt(mean(residual^2))
rmse

varImp(model) #checking variable importance 

