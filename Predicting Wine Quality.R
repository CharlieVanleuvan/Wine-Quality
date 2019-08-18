#######################
"Predicting Wine Quality"
#######################

################
"Given chemical features of red and white wines, along with the quality ratings for each wine, can
quality be predicted by the chemical data? 

Red and white wine datasets were taken from the UC Irvine ML database
IST 687 Introduction to Data Science Final Project"
###############
 

#################
"Project Outline"
#################
"1.Show summary analysis of the wine datasets and understand what the chemical data means
2.Exploratory data analysis looking for relationships between wine features
3.Hypothesize which features will be the best predictors of quality
4.Test the hypothesis with two ML algorithms
5.Evaluate the models
6.Conclusion and next steps"


####################
"Necessary Packages"
###################
library(ggplot2) #for plotting
library(plyr) #for using count() function if needed
library(reshape2) #for using the melt() function
library(kernlab) #for svm models
library(viridis) #color palette
library(ggdark)


#############
"Data Import"
############
#import the red and white wine dataset from the website
redWine <- read.delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")
whiteWine <- read.delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")

#view the structure of each. There are many more white wines than red
str(redWine)
str(whiteWine)

#############################
"Understanding the wine data"
############################
#Questions we asked:
#What affects our taste of wine?
"residual sugar: leftover sugar from grapes in the wine. yeast ferments the sugar to make alcohol. 
more acidity = more tart taste
fixed acidity: also known as titratable acidity, measure of total titratable acid in the wine
volatile acidity : not measured through titration, but by steam distillation. produced by microbial action, any appreciable quantiy could indicate spoilage. humans very sensitive to this
alcohol: determined by the ripeness of the grapes, adds body and texture to the wine. neither is generally preferred over the other
citric acid may be added to make a wine taste more fresh
sulphates and sulphur dioxide are preservatives added to the wine to prevent microbial action
total sulfur dioxide is total amound of SO2 that is bonded to other wine molecules (sugars) plus the Free SO2 that is not bonded to wine molecules
Free SO2 can be detected when tasting if not balanced"


##########################
#EXPLORATORY DATA ANALYSIS
#########################

#add red/white wine classification for EDA
redWine$wineType <- rep("red", nrow(redWine))
whiteWine$wineType <- rep("white", nrow(whiteWine))


#combine both red and white into a new dataframe for EDA
redAndwhiteWine <- rbind(redWine,whiteWine)
str(redAndwhiteWine)

#bar chart for both red and white
rAndw <- ggplot(redAndwhiteWine[,12:13], aes(x = quality, fill = wineType))+
         geom_bar(aes(fill = wineType),color = "black", stat="count", position = "dodge")+
         geom_text(stat="count", aes(label=..count..),vjust=-0.5, position = position_dodge(0.9))+
         ggtitle("Quality Distribution of Red and White Wines") + xlab("Quality")+
         dark_theme_gray()
rAndw

#histogram of all variables to see their distribution
Histogram_EDA <- ggplot(melt(redAndwhiteWine), aes(x=value)) + 
                 geom_histogram(bins = 30, color="black", aes(fill = wineType)) +
                 facet_wrap(vars(variable), scales = "free") +
                 ggtitle("Distribution of all Wine parameters") +
                 dark_theme_gray()
Histogram_EDA


#histogram of all variables to see their distribution colored by quality
Quality_Histogram_EDA <- ggplot(melt(redAndwhiteWine, id = "quality"), aes(x=value)) + 
                         geom_histogram(bins = 30, color="black",stat="count", aes(fill = factor(quality))) +
                         facet_wrap(vars(variable), scales = "free") +
                         ggtitle("Distribution of all Wine parameters")
Quality_Histogram_EDA


#boxplots of all variables to see their impact to quality for red wine
Boxplot_EDA <- ggplot(melt(redWine[,-13], id = "quality"), aes(x=factor(quality), y=as.numeric(value), group=factor(quality))) +
               geom_boxplot(aes(fill = variable)) +
               facet_wrap(vars(variable), scales = "free") +
               xlab("Quality") + ylab("Distribution") +
               ggtitle("Boxplot of Red Wine parameters") + dark_theme_gray() +
               theme(legend.position="none")
Boxplot_EDA
              

#boxplot of all variables to see the impact to quality for white wine
white_Boxplot_EDA <- ggplot(melt(whiteWine[,-13], id = "quality"), aes(x=factor(quality), y=as.numeric(value), group=factor(quality))) +
                     geom_boxplot(aes(fill = variable)) +
                     facet_wrap(vars(variable), scales = "free") +
                     xlab("Quality") + ylab("Distribution") +
                     ggtitle("Boxplot of White Wine parameters") +
                     dark_theme_gray() + theme(legend.position = "none")
white_Boxplot_EDA

#scatterplot of fixed acidity colored by winetype
Fixed.Acidity.Scatter <- ggplot(redAndwhiteWine, aes(x = factor(quality), y = fixed.acidity))+
                         geom_point(aes(color = wineType), position = "jitter")+
                         ggtitle("Fixed Acidity vs. Quality, colored by Wine Type")+
                         xlab("Quality") + ylab("Fixed Acidity") + dark_theme_gray() +
                         theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Fixed.Acidity.Scatter

#scatterplot of volatile acidity colored by winetype
Volatile.Acidity.Scatter <- ggplot(redAndwhiteWine, aes(x = factor(quality), y = volatile.acidity))+
                            geom_point(aes(color = wineType), position = "jitter")+
                            ggtitle("Volatile Acidity vs. Quality, Colored by Wine Type")+
                            xlab("Quality") + ylab("Volatile Acidity")+ dark_theme_gray() +
                            theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Volatile.Acidity.Scatter

#scatterplot of citric acid colored by wine type
Citric.Acid.Scatter <- ggplot(redAndwhiteWine, aes(x=factor(quality), y = citric.acid))+
                       geom_point(aes(color = wineType), position = "jitter")+
                       ggtitle("Citric Acid vs. Quality, Colored by Wine Type")+
                       xlab("Quality") + ylab("Citric Acid")+ dark_theme_gray() +
                       theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Citric.Acid.Scatter

#scatterplot of residual sugar colored by winetype
Residual.Sugar.Scatter <- ggplot(redAndwhiteWine, aes(x = factor(quality), y=residual.sugar))+
                          geom_point(aes(color = wineType), position = "jitter")+
                          ggtitle("Residual Sugar vs Quality") +
                          xlab("Quality") + ylab("Residual Sugar")+ dark_theme_gray() +
                          theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Residual.Sugar.Scatter


#scatterplot of sulphates colored by winetype
Sulphates.Scatter <- ggplot(redAndwhiteWine, aes(x = factor(quality), y=sulphates))+
                     geom_point(aes(color = wineType), position = "jitter")+
                     ggtitle("Sulphates vs Quality") +
                     xlab("Quality") + ylab("Sulphates")+ dark_theme_gray() +
                     theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Sulphates.Scatter

#scatterplot of pH colored by winetype
pH.Scatter <- ggplot(redAndwhiteWine, aes(x = factor(quality), y=pH))+
              geom_point(aes(color = wineType), position = "jitter")+
              ggtitle("pH vs Quality") +
              xlab("Quality") + ylab("pH")+ dark_theme_gray() +
              theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
pH.Scatter

#scatterplot of free sulfur diioxide colored by winetype
Total.SO2.Scatter <- ggplot(redAndwhiteWine, aes(x = factor(quality), y=total.sulfur.dioxide))+
                     geom_point(aes(color = wineType), position = "jitter")+
                     ggtitle("Total Sulfur Dioxide vs Quality") +
                     xlab("Quality") + ylab("Total Sulfur Dioxide")+ dark_theme_gray() +
                     theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Total.SO2.Scatter

#residual sugar vs alcohol scatter plot for red wine
ResidualSugarVSAlcoholRed <- ggplot(redWine, aes(x=alcohol, y= residual.sugar)) +
                             geom_point(aes(color = factor(quality))) +
                             ggtitle("Residual Sugar vs. Alcohol, Colored by Quality") +
                             xlab("Alcohol") + ylab("Residual Sugar")+ dark_theme_gray()
ResidualSugarVSAlcoholRed


#residual sugar vs alcohol scatter plot for white wine
ResidualSugarVSAlcoholWhite <- ggplot(whiteWine, aes(x=alcohol, y= residual.sugar)) +
                               geom_point(aes(color = factor(quality))) +
                               ggtitle("Residual Sugar vs. Alcohol, Colored by Quality") +
                               xlab("Alcohol") + ylab("Residual Sugar")+ dark_theme_gray()
ResidualSugarVSAlcoholWhite

#citric acid vs volatile acid scatter plot
CitricAcidVSVolatileAcidityRed <-  ggplot(redWine, aes(x=volatile.acidity, y= citric.acid)) +
                                   geom_point(aes(color = factor(quality))) +
                                   ggtitle("Citric Acid vs. Volatile Acidity, colored by quality") +
                                   xlab("Volatile Acidity") + ylab("Citric Acid")+ dark_theme_gray()
CitricAcidVSVolatileAcidityRed

#residual sugar vs density scatter plot
Residual.Sugar.vs.Density <- ggplot(redAndwhiteWine, aes(x=density, y= residual.sugar)) +
                             geom_point(aes(color = factor(quality)), position = "jitter") +
                             xlim(c(0.98,1.015)) + ylim(c(0,32)) + 
                             ggtitle("Residual Sugar vs. Density, Colored by Quality") +
                             xlab("Density") + ylab("Residual Sugar")+ dark_theme_gray() +
                             theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Residual.Sugar.vs.Density 


#density vs alcohol scatter plot
Density.vs.Alcohol <- ggplot(redAndwhiteWine, aes(x=alcohol, y= density)) +
                      geom_point(aes(color = factor(quality)), position = "jitter") +
                      ggtitle("Density vs. Alcohol, Colored by Quality") + ylim(c(0.98,1.02)) +
                      xlab("Alcohol") + ylab("Density")+ dark_theme_gray() +
                      theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
Density.vs.Alcohol

#correlation plot
corrplot(cor(redAndwhiteWine[,-c(12,13)]), type = "upper", tl.srt = 45)

##########################
# HYPOTHESIS / PREDICTIONS
##########################


"From the histogram of red and white wine, we see that the acidity and acid concentrations have broad distributions,
while residual sugar and chlorides have tight distributions. This would indicate more change in acidity values across the dataset, while
residual sugar and chlorides remain near a central value.
Inverse relationships with quality that were observed: volatile acidity, pH
Direct relationships with quality that were observed: citric acid, alcohol

Features that showed no relation to quality from scatterplots: chlorides and residual sugar

We predicted that volatile acidity, pH, alcohol, and citric acid will be the greatest predictors of wine quality"


#########################
#PREPARATION FOR MODELING
########################

#For randomForest Classification algorithm to work better, "quality" and "wineType" column data should be factors
#convert "quality" column to a factor for all red, white,and red+white dataframe


#create a function that splits the wine set in half, assuming a rating of 5 is the selling point
#above 5 gets sold. below or equal to 5 gets thrown out
qualityConverter <- function(data)
{
  data$quality[data$quality == 3 | data$quality == 4 | data$quality ==5] <- "no"
  data$quality[data$quality == 6 | data$quality == 7 | data$quality == 8 |data$quality == 9] <- "yes"
  
  #also have to convert quality and wine type to factors
  data$quality <- as.factor(data$quality)
  data$wineType <- as.factor(data$wineType)
  
  return(data)
}

#use the qualityConverter function on the red, white, and red+white dataframes
redWine <- qualityConverter(redWine)
whiteWine <- qualityConverter(whiteWine)
redAndwhiteWine <- qualityConverter(redAndwhiteWine)


####################
#Explore the dataset 
####################

#how many wines of each quality are there?
#for red wines
rw <- ggplot(redWine, aes(quality))
rw <- rw + geom_bar(color = "black",fill = "red4") +geom_text(stat="count", aes(label=..count..),vjust=-1)
rw <- rw + ggtitle("Quality Distribution of Red Wines") + xlab("Red Wine Quality")
rw

#for white wines
ww <- ggplot(whiteWine, aes(quality))
ww <- ww + geom_bar(color = "black",fill = "lightgoldenrodyellow") +geom_text(stat="count", aes(label=..count..),vjust=-1)
ww <- ww + ggtitle("Quality Distribution of White Wines") + xlab("White Wine Quality")
ww

#####################################
#USE RANDOMFOREST CLASSIFICATION MODEL 
####################################

#load randomForest
library(randomForest)

#make training and test datasets for redAndwhitewine
wine_indexes <- sample(c(1:nrow(redAndwhiteWine)), nrow(redAndwhiteWine) * 0.8, replace = FALSE)
train_x_wine <- redAndwhiteWine[wine_indexes, -c(12,13)]
train_y_wine <- redAndwhiteWine[wine_indexes,12]
test_x_wine <- redAndwhiteWine[-wine_indexes, -c(12,13)]
test_y_wine <- redAndwhiteWine[-wine_indexes,12]

#use rF model on red wine
rfModelWine <- randomForest(x=train_x_wine, y=train_y_wine)

#show results
rfModelWine

#meanDecreaseGini
importance(rfModelRed)

#plot of MeanDecreaseGini. The variable importance
varImpPlot(rfModelRed)

#use tuneRF to tune the mtry variable
tuneRF(x=redWine[,-12], y=redWine[,12])

#plot ROC metric
plot(rfModelRed)

#use predict()
WinePredictions <- predict(rfModelWine, test_x_wine)
WinePredictionTable <- table(Actual = test_y_red,Prediction = redPredictions)

#accuracy
sum(diag(WinePredictionTable)) / sum(WinePredictionTable)




#make training and test datasets for red
red_indexes <- sample(c(1:nrow(redWine)), nrow(redWine) * 0.8, replace = FALSE)
train_x_red <- redWine[red_indexes, -c(12,13)]
train_y_red <- redWine[red_indexes,12]
test_x_red <- redWine[-red_indexes, -c(12,13)]
test_y_red <- redWine[-red_indexes,12]

#use rF model on red wine
rfModelRed <- randomForest(x=train_x_red, y=train_y_red)

#show results
rfModelRed
str(rfModelRed)

#meanDecreaseGini
importance(rfModelRed)

#plot of MeanDecreaseGini. The variable importance
varImpPlot(rfModelRed)

#use tuneRF to tune the mtry variable
tuneRF(x=redWine[,-12], y=redWine[,12])

#plot ROC metric
plot(rfModelRed)

#use predict()
redPredictions <- predict(rfModelRed, test_x_red)
table(test_y_red,redPredictions)


#make training and test datasets for white
white_indexes <- sample(c(1:nrow(whiteWine)), nrow(whiteWine) * 0.8, replace = FALSE)
train_x_white <- whiteWine[white_indexes, -c(12,13)]
train_y_white <- whiteWine[white_indexes,12]
test_x_white <- whiteWine[-white_indexes, -c(12,13)]
test_y_white <- whiteWine[-white_indexes,12]

#use rF model on white
rfModelWhite <- randomForest(x=train_x_white, y=train_y_white)

#show results
rfModelWhite

#meanDecreaseGini
importance(rfModelWhite)


#plot of MeanDecreaseGini. The variable importance
varImpPlot(rfModelWhite)

#use tuneRF to tune the mtry variable
tuneRF(x=whiteWine[,-c(12,13)], y=whiteWine[,12])

#plot ROC metric


#use predict()
whitePredictions <- predict(rfModelWhite, test_x_white)
whitePredictionsTable <- table(Actual = test_y_white,Prediction = whitePredictions)
whitePredictionsTable

sum(diag(whitePredictionsTable)) / sum(whitePredictionsTable)

Predicted.No <- 1221
Predicted.Yes <- 2697
Actual.No <- 1337
Actual.Yes <- 2587

Value <- c(1221,1337,2697,2587)
Result <- c("Predicted No","Actual No","Predicted Yes","Actual Yes")
predictionsGraphdata <- data.frame(Result,Value)
predicted.bars <- ggplot(predictionsGraphdata, aes(x=reorder(Result,Value),y=Value)) + 
                  geom_col(fill = rep(c("mediumaquamarine","royalblue"),2))+
                  dark_theme_gray()+
                  xlab("Results") + ggtitle("Random Forest Accuracy on Test Data")+
                  geom_text(aes(label=Value),vjust=-0.5)+
                  theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
predicted.bars





#############################
#USE MULTIPLE REGRESSION MODEL
#############################


#import red wine data again under different dataframe name
lm_red <- read.delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";")

#import white wine data again under a different dataframe name
lm_white <- read.delim("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")

#grab only the wines that are above a score of 5
lm_red <- lm_red[lm_red$quality > 5,]
lm_white <- lm_white[lm_white$quality > 5,]

#plot a correlation plot for red and white wine
library(corrplot)
corrplot(cor(lm_red[,-12]), type = "upper", tl.srt = 45)
corrplot(cor(lm_white[,-12]), type = "upper", tl.srt = 45)

#run a regression of citric acid as a function of other properties that have best correlation to quality
Citric_Acid_lm_red_1 <- lm(data = lm_red[,-12], formula = citric.acid ~ volatile.acidity + fixed.acidity + pH + sulphates +density)
summary(Citric_Acid_lm_red_1)

#remove the parameters that had low significance (where p value is > .05), SKIP THIS
#removing residual.sugar, sulphates
Citric_Acid_lm_red_2 <- lm(data = lm_red[,-12], formula = citric.acid ~ 
                             fixed.acidity +
                             volatile.acidity +
                             chlorides +
                             free.sulfur.dioxide +
                             total.sulfur.dioxide +
                             density +
                             pH +
                             alcohol)
summary(Citric_Acid_lm_red_2)

#sulfur as a function of everything else that has best correlation to quality
Free_SO2_lm_red_1 <- lm(data = lm_red[,-12], formula = free.sulfur.dioxide ~ total.sulfur.dioxide + residual.sugar + fixed.acidity)
summary(Free_SO2_lm_red_1)

#remove the parameters that had low significance (where p value > .05)
#removing chlorides, sulphates, alcohol, and total SO2
Free_SO2_lm_red_2 <- lm(data = lm_red[,-12], formula = free.sulfur.dioxide ~ fixed.acidity +
                          volatile.acidity +
                          citric.acid +
                          residual.sugar +
                          density +
                          pH)
summary(Free_SO2_lm_red_2)


#################################
# support vector machine models #
#################################

#load kernlab
library(kernlab)

#create training and test data
svm_indexes <- sample(c(1:nrow(whiteWine)), nrow(whiteWine) * 0.8, replace = FALSE)
train_svm <- whiteWine[svm_indexes,-13]
test_svm <- whiteWine[-svm_indexes,-13]
test_svm_actual <- whiteWine[-svm_indexes,12]

#create svm model for quality as a function of everything else
svmModel <- ksvm(quality ~ ., data= train_svm, C=5)

#view model results
svmModel

#use predict()
svmPredictions <- predict(svmModel, test_svm)
svmPredictionsTable <- table(Actual = test_svm_actual,Prediction = svmPredictions)
svmPredictionsTable

#cross validation accuracy
sum(diag(svmPredictionsTable)) / sum(svmPredictionsTable)


svmValue <- c(294,326,686,654)
svmDF <- data.frame(Result,Value)
svm.bars <- ggplot(svmDF, aes(x=reorder(Result,svmValue),y=svmValue)) + 
            geom_col(fill = rep(c("mediumaquamarine","royalblue"),2))+
            dark_theme_gray()+
            xlab("Results") + ggtitle("Support Vector Machine Accuracy on Test Data")+ ylab("Values") +
            geom_text(aes(label=svmValue),vjust=-0.5)+
            theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))

svm.bars