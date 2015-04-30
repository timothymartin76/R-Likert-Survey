options(digits=2)
require(likert)
require(reshape)
mydata<-read.csv("2014_Raw_Mod.csv", header=TRUE, sep=",")
mylevels <- c('Strongly Disagree', 'Disagree', 'Neither Agree nor Disagree', 'Agree', 'Strongly Agree')
items3 <- mydata[,substr(names(mydata), 1,2) == 'QA']
head(items3); ncol(items3)
str(items3)
groups <- Department(c('Operations','Support'), 292, replace=TRUE)
tryCatch({
# This will throw an error because all the items must have the same number of levels.
lbad <- likert(items3)
}, error=function(e) {
print("This is good that an error was thrown!")
print(e)
})
sapply(items, class) #Verify that all the columns are indeed factors
sapply(items3, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(items3)) {
items3[,i] <- factor(items3[,i], levels=mylevels)
}
lgood <- likert(items3)
lgood
summary(lgood)
plot(lgood)
plot(lgood, include.histogram=TRUE)
plot(lgood, type = "heat")
