
library(tree)
library(naivebayes)
library(aod)
library('lattice')


mydata=read.csv("placement_data.csv")

head(mydata)

# To see number of rows and columns in our data set
dim(mydata)

# There are 67 missing values in salary column

summary(mydata)


sapply(mydata[,c('ssc_p','hsc_p','degree_p','etest_p')], sd)

sapply(mydata[,c('ssc_p','hsc_p','degree_p','etest_p')], mean)



sum(is.na(mydata['salary']==TRUE))

mydata[is.na(mydata)] <- 0

# Exploratory analysis


#Students percentages in respective level of education
boxplot(mydata$ssc_p,mydata$hsc_p,mydata$degree_p,mydata$mba_p, names = c("Secondary Education","Higher Secondary","Degree","MBA"), xlab='Different Levels of Education', ylab='Percentage', main='Students percentages in respective  
        level of education')

#Summary of students from different types of board: Central or others
summary(mydata$ssc_b)

#Students who had enrolled in particular type of board
barchart(mydata$ssc_b,mydata$ssc_p,xlab='Number of students', ylab='Type of Board', main='Students who had enrolled in particular type of board') 



#Number of Students who are placed or not
ggplot(mydata, aes(degree_t)) + geom_bar(aes(fill=status)) + xlab('Type of degree') + ylab('Number of students')
#, position = "fill")


#Scatter plot and correlation of the variables
#ggscatmat(mydata, columns = c(3,5,8,11,13), color = "status")

#plot(mydata$ssc_b)
plot(mydata$gender)
#plot(mydata$degree_t)
#plot(mydata$status)
plot(mydata$workex)
#plot(mydata$specialisation)
plot(mydata$etest_p)


pairs(mydata)

# To see possible relationships among Students marks percentage from Schooling to Masters 

pairs(mydata[,c('ssc_p','hsc_p','degree_p','mba_p','etest_p')],col=mydata$status,oma=c(5,5,5,16), main = "Student recruitment at Jain University")
par(xpd = TRUE)
legend("bottomright", fill = unique(mydata$status),legend = c('Placed','Not Placed'))



#splom(mydata[,c('ssc_p','hsc_p','degree_p','etest_p')],as.matrix = TRUE,
#      xlab = '',main = "Student Recruitment at Jain University, Bangalore",
#      pscale = 0, varname.cex = 0.8,axis.text.cex = 0.6,
#      axis.text.col = "purple",axis.text.font = 2,
#      axis.line.tck = .5,
#      type = "spline"
#)




# Decision Tree 


library(tree)


tree.mydata=tree(status~gender+ssc_p+degree_p+workex+mba_p+ssc_b+hsc_b+hsc_s+degree_t+etest_p+specialisation+hsc_b, data=mydata, method = "class")
#tree.mydata=tree(status~.-salary-sl_no-status,data=mydata, method = "class")
summary(tree.mydata)
#training error rate is 8.3%

plot(tree.mydata)
text(tree.mydata,pretty=0)

# Spliting data into train and test to get performance measures
set.seed(30) 
train=sample(1:nrow(mydata), 130) #indices for 130 observations randomly selected as training samples
mydata.test=mydata[-train,] #create test dataset which contains other observations
# Count of Placed , Not placed students in training set
table(mydata[train,]$status)

tree.mydata=tree(status~gender+ssc_p+degree_p+workex+mba_p+ssc_b+hsc_b+hsc_s+degree_t+etest_p+specialisation+hsc_b,mydata,subset=train) #fit on training data
plot(tree.mydata)
text(tree.mydata,pretty=0)

#predict on test data
tree.pred=predict(tree.mydata,mydata.test,type="class")
# Dimensions of test set, we have 85 rows in test 
dim(mydata.test)


table(tree.pred,mydata.test$status)

# In Confusion Matrix, if we sum the diagonals elements and divide it by no of samples of test set, we get accuracy of the model
# Confusion Matrix is 2*2 matrix in which 1st diagonal element represents case where predicted value is not placed and Actual value is not placed.
# The second diagonal element represents case where predicted value is placed and Actual value is also Placed
(53+14)/85
#Accuracy= 78.8%
# The Accuracy may be different because of seed 


# Pruning tree with best 5 nodes to see if we get better accuracy

prune.mydata=prune.misclass(tree.mydata,best=5)
plot(prune.mydata)
text(prune.mydata,pretty=0)

tree.pred=predict(prune.mydata,mydata.test,type="class")
table(tree.pred,mydata.test$status)

(60+14)/85
#Accuracy 87.05% 


# Logistic Regression

glm.mydata=glm(status~gender+ssc_p+degree_p+workex+mba_p+ssc_b+hsc_b+hsc_s+degree_t+etest_p+specialisation+hsc_b,data=mydata,family="binomial")
summary(glm.mydata)

## From summary statistics we can see that only ssc_p, degree_p,workex,mba_p has significant relation with p-value<0.05
## new model with only significant variables
glm.mydata=glm(status~ssc_p+degree_p+workex+mba_p,data=mydata,family="binomial")
summary(glm.mydata)


## From summary statistics, we can see the below interpretations:
##For every one unit change in ssc_perc, the log of odds of getting placed  increases by 0.21.
##For every one unit change in degree_perc, the log of  odds of getting placed  increases by 0.15.
##Suprisingly  for every one unit change in mba_perc, the log of odds of getting placed  'decreases' by 0.1910
##If a student has  work_exp, the log of odds of getting placed  is 2.11.

#plot(glm.mydata)

exp(coef(glm.mydata))

#Confidence intervals
confint(glm.mydata)

wald.test(b = coef(glm.mydata), Sigma = vcov(glm.mydata), Terms = 1:4)

# The chi-squared test statistic of 49.8, 
#with 4 degrees of freedom is associated with a p-value of 0.00000000041 indicating that the overall effect of ssc_p,degree_p,mba_p,workex is statistically significant.


glm.probs <-predict(glm.mydata,type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Placed", "Not Placed")
table(glm.pred,mydata$status)

# Accuracy
mean(glm.pred == mydata$status)


# Miss Classification rate
mean(glm.pred != mydata$status)



predict(glm.mydata, data.frame(ssc_p =70,degree_p=70,workex='No',mba_p=60), type = "response")


# The above shows that student with above attributes has 93% of getting placed


predict(glm.mydata, data.frame(ssc_p =mean(mydata$ssc_p),degree_p=mean(mydata$degree_p),workex='No',mba_p=mean(mydata$mba_p)), type = "response")

# Above is for prediction is for student with avg percentage and work ex='No'






