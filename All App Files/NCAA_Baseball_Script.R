sink("NCAABaseballoutput1.txt")
library(readr)

#Import Data
path <- "/Users/chrisgauthier/ChrisDocs/Data_Work/UGA Baseball/2020-2021/UGA_2021_Regular_Season.csv"
  # Use path variable for reproducibility
data <- read_csv(path)
library(MASS)
library(dplyr)
data2<-filter(data, PlayResult!='Sacrifice') #filter out sacrifice bunts or flies that don't count towards or against batting average
data2<-within(data2, PlayResult[KorBB=='Strikeout'] <- 'Strikeout')
data2<-filter(data2, PlayResult!='Undefined') #filter out non-plays
data2<-filter(data2,PlayResult!='Walk') #filter out walks

data2$BattingAverage<-ifelse(data2$PlayResult=='Single',"1",
                               ifelse(data2$PlayResult=='Double',"1",
                                      ifelse(data2$PlayResult=='Triple',"1",
                                             ifelse(data2$PlayResult=='HomeRun',"1","0")))) #assign values of 1 for a hit and 0 for an out or a reach on error

#Dummy Variable - Pticher Set
data2$Windup <- ifelse(data2$PitcherSet == 'Windup', 1, 0)
data2$Stretch <- ifelse(data2$PitcherSet == 'Stretch', 1, 0)

library(tidyverse)
#Cleaning up the data
data2clean <- data2 %>% drop_na(ExitSpeed) %>% drop_na(RelSpeed) %>% drop_na(VertRelAngle) %>% drop_na(SpinRate) %>%
  drop_na(SpinAxis) %>% drop_na(RelHeight) %>% drop_na(RelSide) %>% drop_na(Extension) %>% drop_na(VertBreak) %>%
  drop_na(InducedVertBreak) %>% drop_na(HorzBreak) %>% drop_na(PlateLocHeight) %>% drop_na(PlateLocSide) %>%
  drop_na(ZoneSpeed) %>% drop_na(VertApprAngle) %>% drop_na(HorzApprAngle) %>% drop_na(ZoneTime) %>%
  drop_na(ExitSpeed) %>% drop_na(Angle) %>% drop_na(Direction) %>% drop_na(HitSpinRate) %>% 
  drop_na(HorzRelAngle)

#Scatterplot of Angle and Exit Speed 
library(ggplot2)
plot <- ggplot(data2clean, aes(x=ExitSpeed, y=Angle, col=PlayResult))
plot <- plot + geom_point()
plot <- plot + xlab("Exit Speed") + ylab("Angle") +
  scale_color_discrete(name = "Play Result") + 
  geom_smooth(method="lm")
plot

#Scatterplot of Pitch Speed and Spin Rate
plot2 <- ggplot(data2clean, aes(x=RelSpeed, y=SpinRate, col=PlayResult))
plot2 <- plot2 + geom_point()
plot2 <- plot2 + xlab("Pitch Speed") + ylab("Spin Rate") +
  scale_color_discrete(name = "Play Result") + 
  geom_smooth(method="lm")
plot2

#Dummy Variable for Pitch Type
library(fastDummies)
data3 <- dummy_cols(data2clean, select_columns = "TaggedPitchType")

#Changing Datatype
class(data3$TaggedPitchType_ChangeUp) = "Numeric"
class(data3$TaggedPitchType_Curveball) = "Numeric"
class(data3$TaggedPitchType_Fastball) = "Numeric"
class(data3$TaggedPitchType_Slider) = "Numeric"

#Split the Data into Training and Testing Data for Regression
library(caTools)
class(data3)
set.seed(123)
split = sample.split(data3$BattingAverage, SplitRatio=0.7)
training_set = subset(data3, split == TRUE)
test_set = subset(data3, split == FALSE)

#Filtering Data for Boosting Model
data4<- data3 %>% dplyr::select(BattingAverage, PAofInning, PitchofPA, Windup,
                                    Balls, Strikes, RelSpeed, VertRelAngle, SpinRate, SpinAxis, 
                                    RelHeight, RelSide, Extension, VertBreak, InducedVertBreak,
                                    HorzBreak, PlateLocHeight, PlateLocSide, ZoneSpeed, VertApprAngle, 
                                    HorzApprAngle, ZoneTime, ExitSpeed, Angle, Direction,
                                    HitSpinRate, TaggedPitchType_ChangeUp, TaggedPitchType_Curveball,
                                    TaggedPitchType_Fastball, TaggedPitchType_Slider, pfxx, pfxz, 
                                    x0, y0, z0, vx0, vy0, vz0, ax0, az0)

#Regression
class(training_set$BattingAverage) = "numeric"
model_1 <- glm(formula = BattingAverage ~ PAofInning + PitchofPA + Windup +
                 Balls + Strikes + RelSpeed + VertRelAngle + SpinRate + SpinAxis +
                 RelHeight + RelSide + Extension + VertBreak + InducedVertBreak +
                 HorzBreak + PlateLocHeight + PlateLocSide + ZoneSpeed + VertApprAngle +
                 HorzApprAngle + ZoneTime + ExitSpeed + Angle + Direction +
                 HitSpinRate + TaggedPitchType_ChangeUp + TaggedPitchType_Curveball +
                 TaggedPitchType_Fastball + TaggedPitchType_Slider + pfxx + pfxz + 
                 x0 + y0 + z0 + vx0 + vy0 + vz0 + ax0 + az0, data=training_set)
summary(model_1)
library(pscl) 
pR2(model_1) #R-squared

#Step Model
library(MASS)
step.model <- stepAIC(model_1, direction = "both", 
                      trace = FALSE)
summary(step.model)
pR2(step.model)

#Split the Data into Training and Testing Data for Boosting
class(data4$BattingAverage) = "numeric"
library(dplyr)

colSums(is.na(data4))

#Boosting Model 
#Splitting the Data
library(caTools)
set.seed(123)
split = sample.split(data4$BattingAverage, SplitRatio=0.7)
training_set_2 = subset(data4, split == TRUE)
test_set_2 = subset(data4, split == FALSE)
#Running the Boost
class(training_set_2$BattingAverage) = "numeric"
library(gbm)
boost.BA = gbm(BattingAverage~., data = training_set_2, distribution = "gaussian", n.trees = 10000, shrinkage = .01, interaction.depth = 4)
summary(boost.BA)
plot(boost.BA,i="Angle")
plot(boost.BA,i="ExitSpeed")

class(test_set_2$BattingAverage) = "numeric"
n.trees = seq(from = 100, to =10000, by = 100)
predmat = predict(boost.BA, newdata=training_set_2, n.trees = n.trees)
dim(predmat)
boost.err = with(training_set_2, apply( (predmat - BattingAverage)^2, 2, mean))
plot(n.trees, boost.err, pch = 23, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Testing Error")
abline(h = min(boost.err), col = "red")   
