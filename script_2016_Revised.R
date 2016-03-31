options(digits=2)

## You need to load these packages
require(likert)
require(reshape2)
require(plyr)

## Load file
mydata<-read.csv("2014_Raw_Mod.csv", header=TRUE, sep=",")

##Define levels of likert scale
mylevels <- c('Strongly Disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly Agree')

##Select only columns starting with "QA"
items3 <- mydata[,substr(names(mydata), 1,2) == 'QA']
head(items3); ncol(items3)
str(items3)

##Add full column descriptions
items3 <- rename(items3, c(QA2 = "I believe 3-1-1 is a good place to work.", QA3 = "I have the training and tools I
need to do my job.", QA4 = "I have the training and skills I need to do my job.", QA5 = "I feel that my work is
important to the success of 3-1-1.", QA6 = "I want a career at 3-1-1.", QA7 = "I believe overall employee morale is
good at 3-1-1.", QA8 = "I feel like I am part of a team and I enjoy the people I work with.", QA9 = "I am treated
with respect.", QA10 = "I feel employees are recognized when they perform well.", QA11 = "I believe the 3-1-1
workplace is clean and comfortable."))

tryCatch({
## This will throw an error because all the items must have the same number of levels.
lbad <- likert(items3)
}, error=function(e) {
print("This is good that an error was thrown!")
print(e)
})
sapply(items3, class) #Verify that all the columns are indeed factors
sapply(items3, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(items3)) {
items3[,i] <- factor(items3[,i], levels=mylevels)
}

##Build diverging bar charts and heatmaps and centered bar chart
lgood <- likert(items3)
lgood
summary(lgood)
plot(lgood)  ## diverging bar chart
plot(lgood, type = "heat")  ## heatmap
plot(lgood, centered = FALSE, wrap = 30)  ## Centered bar chart ##

##Build charts with grouping by variable
Grouped <- likert(items3, grouping = mydata$Department)  ##Grouping by Department or "Year"
print(Grouped)
summary(Grouped)
plot(Grouped)

##Customize your chart if needed
plot(Grouped, text.size=5, text.color="black", high.color = "#4A6491", low.color = "#FF9800") +
theme(axis.text.x=element_text(colour="black", face="bold", size=12),
axis.text.y=element_text(colour="black", face="bold", size=12),
axis.title.x=element_text(colour="black", face="bold", size=12),
strip.text=element_text(colour="black", face="bold", size=12),
legend.text=element_text(colour="black", face="bold", size=12),
legend.title=element_text(colour="black", face="bold", size=12),
plot.title=element_text(colour="black", face="bold", size=14)) +
ggtitle("My Survey Questions")
