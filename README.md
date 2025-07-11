# Descriptive-Analysis-Using-R
Descriptive Analysis of Two and Single Variable Using R
#Descriptive Analysis of a Single Variable Using R

# Importing Data from CSV
salary = read.csv("salary.csv")

#Quick Overview
head(salary, 10)
tail(salary, 10)

#Names of Variables/Columns
names(salary)

#Before the analysis
summary(salary)
str(salary)

#Defining and converting data types
salary <- as.data.frame(salary)
salary$id<- as.character(salary$id)
salary$sex <- as.factor(salary$sex)
salary$jobcat <- as.factor(salary$jobcat)
salary$edlevel<-as.factor(salary$edlevel)
salary$minority<-as.factor(salary$minority)
salary$sexrace<-as.factor(salary$sexrace)
levels(salary$ sex) <- c( "male", "female")
levels(salary$edlevel) <- c("level_1", "level_2", "level_3", "level_4", "level_5", "level_6", "level_7", "level_8", "level_9", "level_10")
levels(salary$sexrace) <-c("white_males", "minority_males", "white_females", "minority_females")
levels(salary$minority)<-c("white","nonwhite")
levels(salary$jobcat)<-c("clerical","office_trainee","security_officer", "college_trainee", "exempt_employee", "MBA_trainee", "technical_employee")
levels(salary$minority)<-c("white","nonwhite")

#Missing values
any(is.na(salary)) 

# Adding variables
salary$new_salary_beg = salary$salbeg +12.15 * sqrt(2010)
salary$new_salary_beg


#((IF/ELSE))
salary$age_class<-ifelse(salary$age<30,"young",ifelse(salary$age>60,"elderly","middle-aged"))
salary$age_class = as.factor(salary$age_class)
salary$salnow_class<-ifelse(salary$salnow>=13768, "high","low")
salary$salnow_class<-as.factor(salary$salnow_class)

#subset
sub <- subset(salary, age>35)
sub2 <- subset(salary, sex =="male")
sub3 <- subset(salary, sex =="male" & salbeg<7000 & age>35)
sub4 <- salary[salary$age>34 & salary$salbeg<7000,]

# Qualitative variable
#Frequency table - Relative frequency table
freq.sex = table(salary$sex)
freq.sex
class(freq.sex)
names(freq.sex)
freq.sex.df = as.data.frame(freq.sex)
freq.sex.df
names(freq.sex.df)=c("Gender","Frequency")
freq.sex.df
rel.freq.sex = round(rel.freq.sex,3)
rel.freq.sex.df = as.data.frame(rel.freq.sex)
names(rel.freq.sex.df) = c("Gender","Relative Frequency")
rel.freq.sex.df
sex.df = cbind.data.frame(freq.sex.df, rel.freq.sex.df[2])
sex.df
colnames(sex.df)=c("gender","frequency","Rel Frequency")

freq.minority = table(salary$minority)
freq.minority.df=as.data.frame(freq.minority)
freq.minority
rel.freq.minority =prop.table(freq.minority)
rel.freq.minority=round(rel.freq.minority,2)
rel.freq.minority.df=as.data.frame(rel.freq.minority) 
rel.freq.minority
minority.df = cbind(freq.minority.df,rel.freq.minority.df[2])
minority.df
colnames(minority.df)<-c("Minority","Frequency","Rel.Frequency")
minority.df

freq.jobcat =table(salary$jobcat)
freq.jobcat.df<-as.data.frame(freq.jobcat)
rel.freq.jobcat = prop.table(freq.jobcat)
rel.freq.jobcat=round(rel.freq.jobcat,3)
rel.freq.jobcat.df=as.data.frame(rel.freq.jobcat)
rel.freq.jobcat
jobcat.df =cbind(freq.jobcat.df,rel.freq.jobcat.df[2])
colnames(jobcat.df)<-c("Jobcat","Frequncy","Rel.Frequency")
jobcat.df

# barplot
bar.freq.sex<-
  barplot(
    freq.sex, 
    main="Gender Distribution", 
    xlab="Gender", 
    ylab="Frequency",
    horiz=F, 
    cex.names=1.9,cex.axis = 1.9,col=c("blue","pink"))

bar.freq.sex<-
  barplot(
    rel.freq.sex, 
    main="Gender Distribution", 
    xlab="Gender", 
    ylab="Relative frequency",
    horiz=F, 
    cex.names=1.9,cex.axis = 1.9,col=c("blue","pink"))


bar.freq.minority<-
  barplot(
    freq.minority, 
    main="minority Distribution", 
    xlab="minority", 
    ylab="Frequency",
    horiz=F, 
    cex.names=1.9,cex.axis = 1.9,col=c("yellow","black"))


bar.freq.jobcat<-
  barplot(
    freq.jobcat, 
    main="jobcat Distribution", 
    xlab="job category", 
    ylab="Frequency",
    horiz=F, 
    cex.names=1.3,cex.axis = 1.2,col = rainbow(length(levels(salary$jobcat))))

#pie chart

piepercent<- paste( 100* rel.freq.sex, "%",sep="")

pie.freq.sex <- pie(
  freq.sex,
  labels= piepercent,
  col=rainbow(length(freq.sex)),
  main="Gender")
legend(
  locator(1), 
  legend=names(freq.sex),
  fill=rainbow(length(freq.sex)))

piepercent<- paste( 100* rel.freq.minority, "%",sep="")

pie.freq.minority <- pie(
  freq.minority,
  labels= piepercent,
  col=rainbow(length(freq.minority)),
  main="Gender")
legend(
  locator(1), 
  legend=names(freq.minority),
  fill=rainbow(length(freq.minority)))

piepercent3<- paste(names(freq.jobcat),"=", 100* rel.freq.jobcat,"%",sep="")
pie.freq.jobcat <-pie(
  freq.jobcat,
  col=rainbow(length(freq.jobcat)),
  main="jobcat")

#  Quantitative variable

mean(salary$salbeg)
median(salary$salbeg)
quantile(salary$salbeg,c(0.25,0.75))
library(DescTools)
Mode(salary$salbeg)
Range(salary$salbeg) 
max(salary$salbeg)
min(salary$salbeg)
IQR(salary$salbeg)
var(salary$salbeg)
sd(salary$salbeg)
ds.skewness(salary$salbeg)
ds.kurtosis(salary$salbeg)

#boxplot

b1<-boxplot( 
  x=salary$salbeg,
  main = "Boxplot of Beginning Salary", 
  col = "red")
b1

b2 <- boxplot( 
  x=salary$salnow,
  main = "Boxplot of Beginning Salary", 
  col = "red")

b3 <-boxplot( 
  x=salary$age,
  main = "age", 
  col = "red")


#histogram

h <- hist( 
  salary$salbeg,
  breaks = "scott", 
  main = "Histogram of Beginning Salary", 
  xlab = "salbeg",
  col = "blue",
  freq = TRUE)

points(x = h$mids, y = h$counts, col="red", pch=20)
lines(x= h$mids, y= h$counts, col="black")

h2 <-hist( 
  salary$salnow,
  breaks = "FD", 
  main = "Histogram of  Salary", 
  xlab = "salbeg",
  col = "blue",
  freq = TRUE)


h3  <-hist( 
  salary$age,
  breaks = "Sturges", 
  main = "Histogram of age", 
  xlab = "age",
  col = "blue",
  freq = TRUE)

################################################################################################

#Descriptive Statistics for Two  VariablesUsing R

# (Crosstabulation Matrix-Cross tabs)


jobcat_sex.freq = table(salary$jobcat,salary$sex)
jobcat_sex.freq

jobcat_sex.freq2 = addmargins(jobcat_sex.freq)
jobcat_sex.freq2

jobcat_minority.freq<-table(salary$jobcat,salary$minority)
jobcat_minority.freq

jobcat_minority.freq2<-addmargins(jobcat_minority.freq)
jobcat_minority.freq2

jobcat_sex.relfreq = prop.table(jobcat_sex.freq)
jobcat_sex.relfreq
jobcat_sex.relfreq = round(jobcat_sex.relfreq, 4)
jobcat_sex.relfreq


jobcat_sex.relfreq2 = addmargins(jobcat_sex.relfreq)
jobcat_sex.relfreq2

jobcat_minority.relfreq<-prop.table(jobcat_minority.freq)
jobcat_minority.relfreq
jobcat_minority.relfreq<-round(jobcat_minority.relfreq,4)


jobcat_sex_minor.freq = table(salary$jobcat,salary$sex,salary$minority)
jobcat_sex_minor.freq
ftable(jobcat_sex_minor.freq) 

jobcat_sex_minor.freq2 = addmargins(jobcat_sex_minor.freq)
jobcat_sex_minor.freq2 = ftable(jobcat_sex_minor.freq2)
jobcat_sex_minor.freq2

jobcat_minority_sexrace.freq<-table(salary$jobcat,salary$minority,salary$sexrace)
jobcat_minority_sexrace.freq

jobcat_sex_minor.relfreq = prop.table(jobcat_sex_minor.freq)
jobcat_sex_minor.relfreq

ftable(jobcat_sex_minor.relfreq)

jobcat_sex_minor.relfreq2 = addmargins(jobcat_sex_minor.relfreq)
jobcat_sex_minor.relfreq2
jobcat_sex_minor.relfreq2 = ftable(jobcat_sex_minor.relfreq2)
jobcat_sex_minor.relfreq2

jobcat_minority_sexrace.relfreq<-prop.table(jobcat_minority_sexrace.freq)
jobcat_minority_sexrace.relfreq

#barplot
jobcat_sex.bar<-
  barplot(
    height = t(jobcat_sex.freq), 
    beside = TRUE,
    horiz = F, 
    col = c("blue","pink"),
    main = "Job Category - Gender Distribution", 
    xlab = "Job Category", 
    ylab = "Frequency", 
    ylim = c(0,130),
    cex.names = 0.8,
    legend.text = levels(salary$sex))

text( 
  x = jobcat_sex.bar, 
  y = t(jobcat_sex.freq), 
  labels = t(jobcat_sex.freq), 
  pos = 3)

jobcat_sex.relbar<-
  barplot(
    height = t(jobcat_sex.relfreq), 
    beside = TRUE,
    horiz = FALSE, 
    col = c("blue","pink"),
    main = "Job Category - Gender Distribution", 
    xlab = "Job Category", 
    ylab = "Frequency",
    ylim=c(0,0.3),
    cex.names = 0.8,
    legend.text = levels(salary$sex))

text( 
  x = jobcat_sex.relbar, 
  y = t(jobcat_sex.relfreq), 
  labels = paste0(t(jobcat_sex.relfreq)*100, "%"), 
  pos = 3)

paste(t(jobcat_sex.relfreq)*100, "%",sep="")

jobcat_sex.bar<-
  barplot(
    height = t(jobcat_sex.freq), 
    beside = FALSE,
    horiz = FALSE, 
    col = c("blue","pink"),
    main = "Job Category - Gender Distribution", 
    xlab = "Job Category", 
    ylab = "Frequency", 
    ylim = c(0,250),
    cex.names = 0.8,
    legend.text = levels(salary$sex))

text(
  x = jobcat_sex.bar, 
  y = jobcat_sex.freq[,1]/2, 
  labels = jobcat_sex.freq[,1],
  pos = 3)

# Descriptive Statistics for a Quantitative and a Categorical Variable
#boxplot

boxplot(
  formula = salbeg~sex,
  data = salary,
  main = "Employees Salary by Sex", 
  ylab = "Beginning Salary", 
  xlab = "Sex of Employee",
  col = c("blue","pink"),range=3)

boxplot(
  formula = salnow~jobcat,
  data = salary,
  main = "Employees Salary by jobcat", 
  ylab = "now Salary", 
  xlab = "jobcat of Employee",
  col = rainbow(length(levels(salary$jobcat))),range=3)

#histogram
library(FSA)
hist(
  formula = salbeg~sex,
  data=salary,
  breaks = "scott",
  xlab="Beginning Salary",
  col = "lightblue")

################################################################################################
