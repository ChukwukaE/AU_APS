library(ggplot2)        # for generating visualizations
library(dplyr)
library(forcats)
library(grid)
library(gridExtra)
#library factoMineR
library("FactoMineR")
library("factoextra")
#install.packages("writexl")
library("writexl")
library(GPArotation)
library(ordinal)
library(rcompanion)
library(brant)
library(MASS)
library(corrplot)
library(FSA)
library(readxl)


set.seed(12345)

######################Importing and Reading 2020 Survey Data File##################################


data_2020 <- read.csv("C:/Users/hp/Downloads/Term_2/Dana/2020-aps-employee-census-dataset.csv", header=TRUE, na.strings=c(" "), stringsAsFactors=T)
#data_2020 <- edit(data_2020)
#str(data_2020)
#View(data_2020)
head(data_2020)




dataframe_filtered_2020 <- select(data_2020,c('?..AS', 'q5.','q18a','q18b','q18c','q18d','q18e','q18f','q18g',
                                              'q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h',
                                              'q21a','q21b','q21c','q21d','q21e','q21f',
                                              'q22a','q22b','q22c', 'q22d',
                                              'q30','q31' ,
                                              'q32a','q32b','q32c','q32d','q32e','q32f','q32g','q32h','q32i','q32j',
                                              'q34a','q34b','q34c','q34d','q34e','q23d','q23f',
                                              'q47a','q47b','q47c','q47d','q47e','q47f','q47g',
                                              'q48a','q48b','q48c','q48d','q48e','q48f',
                                              'q49','q50','q51' ))

#considering the scale do not know as null values 
dataframe_filtered_2020 <- dataframe_filtered_2020 %>% mutate(q22a=replace(q22a, q22a=="Do not know", NA))
dataframe_filtered_2020 <- dataframe_filtered_2020 %>% mutate(q22d=replace(q22d, q22d=="Do not know", NA))
dataframe_filtered_2020 <- dataframe_filtered_2020 %>% mutate(q22a=replace(q22c, q22c=="Do not know", NA))
#dataframe_filtered_2020 <- dataframe_filtered_2020 %>% mutate(q28=replace(q28, q28=="Don't know", NA))

#converting questions to numerical using mutate_at
map_factor <- function(x) {
  x <- ifelse(x=='Strongly agree',5,ifelse(x=='Agree',4,
                                           ifelse(x=='Neither agree nor disagree',3,
                                                  ifelse(x=='Disagree',2,
                                                         ifelse(x=='Strongly disagree',1,x)))))
}


map_factor_2 <- function(x) {
  x <- ifelse(x=='Always',5,
              ifelse(x=='Often',4,
                     ifelse(x=='Sometimes',3, 
                            ifelse(x=='Rarely',2,
                                   ifelse(x=='Never',1,x)))))
}

map_factor_3 <- function(x) {
  x <- ifelse(x=='Significantly improved',5,
              ifelse(x=='Improved',4,
                     ifelse(x=='No change',3, 
                            ifelse(x=='Reduced',2,
                                   ifelse(x=='Significantly reduced',1,x)))))
}

map_factor_4 <- function(x) {
  x <- ifelse(x=='Not at all',5,
              ifelse(x=='Very little',4,
                     ifelse(x=='Somewhat',3, 
                            ifelse(x=='To a great extent',2,
                                   ifelse(x=='To a very great extent',1,x)))))  #please check this
}

map_factor_5 <- function(x) {
  x <- ifelse(x=='Well above capacity ????" too much work',5,
              ifelse(x=='Slightly above capacity ????" lots of work to do',4,
                     ifelse(x=='At capacity ????" about the right amount of work to do',3, 
                            ifelse(x=='Slightly below capacity ????" available for more work',2,
                                   ifelse(x=='Below capacity ????" not enough work',1,x)))))
}

map_factor_6 <- function(x) {
  x <- ifelse(x=='Very positive change',5,
              ifelse(x=='Positive change',4,
                     ifelse(x=='No change',3, 
                            ifelse(x=='Negative change',2,
                                   ifelse(x=='Very negative change',1,x)))))
}

map_factor_7 <- function(x) {
  x <- ifelse(x=='To a very large extent',5,
              ifelse(x=='To a large extent',4,
                     ifelse(x=='Somewhat',3, 
                            ifelse(x=='To a small extent',2,
                                   ifelse(x=='To a very small extent',1,x)))))
}

dataframe_filtered_2020_b <- dataframe_filtered_2020 %>% mutate_at(c('q18a','q18b','q18c','q18d','q18e','q18f','q18g',
                                                                     'q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h',
                                                                     'q21a','q21b','q21c','q21d','q21e','q21f',
                                                                     'q22a','q22b','q22c', 'q22d',
                                                                     'q34a','q34b','q34c','q34d','q34e',
                                                                     'q23d','q23f',
                                                                     'q48a','q48b','q48c','q48d','q48e','q48f',
                                                                     'q51')
                                                                   ,map_factor)

dataframe_filtered_2020_b  <- dataframe_filtered_2020_b  %>% mutate_at(c('q47a','q47b','q47c','q47d','q47f','q47e','q47g')
                                                                       , map_factor_2)
dataframe_filtered_2020_b  <- dataframe_filtered_2020_b  %>% mutate_at(c('q30')
                                                                       , map_factor_3)
dataframe_filtered_2020_b  <- dataframe_filtered_2020_b  %>% mutate_at(c('q32a','q32b','q32c','q32d','q32f','q32e','q32g', 'q32h', 'q32i', 'q32j')
                                                                       , map_factor_4)
dataframe_filtered_2020_b  <- dataframe_filtered_2020_b  %>% mutate_at(c('q31')
                                                                       , map_factor_5) 
dataframe_filtered_2020_b  <- dataframe_filtered_2020_b  %>% mutate_at(c('q49')
                                                                       , map_factor_6) 
dataframe_filtered_2020_b  <- dataframe_filtered_2020_b  %>% mutate_at(c('q50')
                                                                       , map_factor_7) 




str(dataframe_filtered_2020_b)
View(dataframe_filtered_2020_b)
dim(dataframe_filtered_2020)
summary(dataframe_filtered_2020_b)

#Dropping NA values
dataframe_2020_clean <- na.omit(dataframe_filtered_2020_b)
sum(is.na(dataframe_2020_clean))
View(dataframe_2020_clean)
summary(dataframe_2020_clean)
str(dataframe_2020_clean)

#Creating dataframe
write_xlsx(dataframe_2020_clean,"C:/Users/hp/Downloads/DATA__2020.xlsx")

###Q4  -	Is this statement, "Australia departments recruit supervisors who have transformation and consideration styles still valid.#################
## Question1 
library(readxl)
dataframe_2020_clean <- read_excel("C:/Users/hp/Downloads/DATA__2020.xlsx")
summary(dataframe_2020_clean)

questions_2020 <-  select(dataframe_2020_clean,c(           'q5.','q19a','q19b','q19c','q19d','q19e','q19f','q19g','q19h',
                                                            'q21a','q21b','q21c','q21d','q21e','q21f',
                                                            'q22a','q22b','q22c','q22d'))
summary(questions_2020)
#Subset by classification
EL_2020 <- subset(questions_2020, q5.=='EL')
SES_2020 <- subset(questions_2020, q5.=='SES')
GRAD_2020 <- subset(questions_2020, q5.=='Trainee/Graduate/APS')

dim(EL_2020)
dim(SES_2020)
dim(GRAD_2020)


#Corrplot
correlate20 <- cor(GRAD_2020[-1])
corrplot(correlate20, method = 'number')

#Bartletts's Test
library(psych)
cortest.bartlett(correlate20, nrow(GRAD_2020[-1]))

#KMO
KMO(correlate20)

#PCA
Grad_2020.pca <- princomp(GRAD_2020[-1])
summary(Grad_2020.pca)
get_eig(Grad_2020.pca) # Eigen vales 
fviz_screeplot(Grad_2020.pca, addlabels = TRUE, ylim = c(0, 50))


#test with 2 factors
library(GPArotation)
Grad_model2020 <- fa(r = correlate20, nfactors = 2, rotate = "varimax", SMC = FALSE, fm= 'pa')
Grad_model2020
fa.diagram(Grad_model2020)

#######################################2014 data########################################################################
#import and select questions
data_2014 <- read.csv("C:/Users/hp/Downloads/Term_2/Dana/2014-aps-employee-census-5-point-dataset.csv", header=TRUE, na.strings=c(" "), stringsAsFactors=T)
#View(data_2014)
leadership2014 <- select(data_2014,c('?..AS', "q1","q2.","q6.",
                                     'q19a','q19b','q19c','q19d','q19e',
                                     'q20a','q20b','q20c','q20d','q20e','q20f','q20g',
                                     'q21a','q21b','q21c','q21d','q21e','q21f','q21g','q21h','q21i','q21j',
                                     'q24a', 'q24c', 'q24e', 'q24i', 'q24m', 'q24o', # remove
                                     'q55a','q55b','q55c','q55d','q55e','q55f','q55g','q55h', 'q55i', 'q55j',
                                     'q62c'))


#dropping missing data
leadership2014 <- na.omit(leadership2014)
dim(leadership2014)


#formatting to likert scale
summary(leadership2014)

map_factor_11 <- function(x) {
  x <- ifelse(x=='Very satisfied',5,
              ifelse(x=='Satisfied',4,
                     ifelse(x=='Neither satisfied nor dissatisfied',3, 
                            ifelse(x=='Dissatisfied',2,
                                   ifelse(x=='Very dissatisfied',1,x)))))
}

map_factor_12 <- function(x) {
  x <- ifelse(x=='Strongly agree',5,
              ifelse(x=='Agree',4,
                     ifelse(x=='Neither agree nor disagree',3,
                            ifelse(x=='Disagree',2,
                                   ifelse(x=='Strongly disagree',1,x)))))
}

map_factor_13 <- function(x) {
  x <- ifelse(x=='To a very great extent',5,ifelse(x=='Quite a lot',4,
                                                   ifelse(x=='Somewhat',3,
                                                          ifelse(x=='Hardly at all',2,
                                                                 ifelse(x=='Not at all',1,x)))))
}

leadership2014_num  <- leadership2014  %>% mutate_at(c('q55a','q55b','q55c','q55d','q55e','q55f','q55g','q55h', 'q55i', 'q55j'), map_factor_11)

leadership2014_num  <- leadership2014_num  %>% mutate_at(c(
  'q19a','q19b','q19c','q19d','q19e',
  'q20a','q20b','q20c','q20d','q20e','q20f','q20g',
  'q21a','q21b','q21c','q21d','q21e','q21f','q21g','q21h','q21i','q21j', 'q62c') 
  , map_factor_12)
leadership2014_num  <- leadership2014_num  %>% mutate_at(c('q24a', 'q24c', 'q24e', 'q24i', 'q24m', 'q24o') 
                                                         , map_factor_13)


#Checking data
dim(leadership2014_num)
summary(leadership2014_num)
View(leadership2014_num)
describe(leadership2014_num)

library(writexl)
write_xlsx(leadership2014_num,"C:/Users/hp/Downloads/leadership2014_num.xlsx")


#changing to factor
# leadership2014_factor<- leadership2014 
# cols <- c('q20a','q20b','q20c','q20d','q20e','q20f','q20g',
#           'q21a','q21b','q21c','q21d','q21e','q21f','q21g','q21h','q21i','q21j',
#           'q55a','q55b','q55c','q55d','q55e','q55f','q55g','q55h', 'q55i', 'q55j',
#           'q62c')
# leadership2014_factor[cols] <- lapply(leadership2014_factor[cols], factor)  #
# summary(leadership2014_factor)


######	What are the leadership styles that participants regard as significant for innovation in the public sector in Australia?#################
## Question1 
library(readxl)
leadership2014_num <- read_excel("C:/Users/hp/Downloads/leadership2014_num.xlsx")


#Selcted Questions
dataframe_2014 <- select(leadership2014_num,c(
  '?..AS', "q1","q2.","q6.",
  'q20c','q20d','q20e','q20f','q20g',
  'q21e','q21f','q21j',
  'q24m','q24o',
  'q55a','q55g','q55h','q55i', 'q55j',
  'q62c'))


frame_2014 <-dataframe_2014[-1][-1][-1]
dataframe_2014_Q1 <- frame_2014[-1]
names(frame_2014)
summary(frame_2014)
dim(frame_2014)

#Subset by classification
EL_2014_1_styles <- subset(frame_2014, q6.=='EL')
SES_2014_styles <- subset(frame_2014, q6.=='SES')
GRAD_2014_Q1_style <- subset(frame_2014, q6.=='Trainee/Graduate/APS')

dim(EL_2014_1_styles)
dim(SES_2014_styles)
dim(GRAD_2014_Q1_styles)

#####################################################GRAD#############################################################
# checking for leadership styles
GRAD_2014_Q1_styles <- GRAD_2014_Q1_style[-17]
#creating factored variant
Grad_2014_factor <- GRAD_2014_Q1_styles[-1]
colsss                                   <-c('q20c','q20d','q20e','q20f','q20g',
                                             'q21e','q21f','q21j',
                                             'q24m','q24o',
                                             'q55g','q55h','q55i')
Grad_2014_factor[colsss] <- lapply(Grad_2014_factor[colsss], factor)  
summary(Grad_2014_factor)

#Rename variables
name  <-c('A1', 'A2','A3','A4','A5',
          'B1','B2','B3',
          'C1','C2',
          'D1','D2','D3','D4','D5')

colnames(GRAD_2014_Q1_styles)[2:16] <- name
head(GRAD_2014_Q1_styles)


#Corrplot
correlate <- cor(GRAD_2014_Q1_styles[-1])
corrplot(correlate, method = 'number')

#Bartletts's Test
library(psych)
cortest.bartlett(correlate, nrow(GRAD_2014_Q1_styles[-1]))

#KMO
KMO(correlate)

#PCA
Grad_2014.pca <- princomp(GRAD_2014_Q1_styles[-1])
summary(Grad_2014.pca)
get_eig(Grad_2014.pca) # Eigen vales 
fviz_screeplot(Grad_2014.pca, addlabels = TRUE, ylim = c(0, 50))


#test one factor
Grad_model_one <- fa(GRAD_2014_Q1_styles[-1])
fa.diagram(Grad_model_one)

#test with 3 factors
library(GPArotation)
Grad_model2 <- fa(GRAD_2014_Q1_styles[-1], nfactors = 2, rotate = "oblimin")
fa.diagram(Grad_model2)
Grad_model2


#test with 2 factors
Grad_model1_14 <- fa(GRAD_2014_Q1_styles[-1], nfactors = 2, rotate = "oblimin")
fa.diagram(Grad_model1_14)
Grad_model1_14

#rotation
Grad_fit <- factanal(GRAD_2014_Q1_styles[-1], 2, rotation="oblimin")
print(Grad_fit, digits=2, cutoff=.3, sort=TRUE)


###Question 2 : -	Can leadership styles be used to predict employees that strive for creativity and innovation. / REGRESSION######

#creating dataframe with score and formatting
str(as.ordered(Grad_model1_14$q24m))
efa.data <- cbind.data.frame(q62c = as.factor(as.ordered(GRAD_2014_Q1_style$q62c)), Consideration = Grad_model1_14$scores[,2],
                             Transformational = Grad_model1_14$scores[,1])
str(efa.data)
head(efa.data)
summary(efa.data)

#logit regression

#split dataset
sample <- sample.int(n = nrow(efa.data), size = floor(.70*nrow(efa.data)), replace = F)
train <-efa.data[sample, ]
test  <- efa.data[-sample, ]
dim(train)
dim(test)


#logit regression
efa.glm <- polr(q62c ~ Consideration + Transformational, data = train, Hess = TRUE)
summary(efa.glm)

#pvalue
ctable <- coef(summary(efa.glm))
pval <-pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = pval))

#pred
pred <- predict(efa.glm, test[5:10,], type = 'prob')
print(pred, digits = 3)



#####Q5 :What are the differences among females' and males' impressions about their immediate workgroup and immediate supervisors?####
library(dplyr)
gender2014 <- select(leadership2014_num,c(    'q1', 'q6.',
                                     'q19a','q19b','q19c','q19d','q19e',
                                     'q20a','q20b','q20c','q20d','q20e','q20f','q20g'))

#Rename
namee  <-c('A1', 'A2','A3','A4','A5',
           'B1','B2','B3','B4','B5', 'B6','B7')

colnames(gender2014)[3:14] <- namee

# gender2014_num  <- gender2014  %>% mutate_at(c(
#   'A1', 'A2','A3','A4','A5',
#   'B1','B2','B3','B4','B5', 'B6','B7') 
#   , map_factor_12)

#gender2014_factor <-na.omit(gender2014)
# gender_2014_Q1 <- subset(gender2014_factor, q6.=='Trainee/Graduate/APS')
# female_factor<- subset(gender_2014_Q1, q1=='Female')
# male_factor<- subset(gender_2014_Q1, q1=='Male')
# summary(male_factor[-1][-1])


#summary(gender2014_num)
gender2014_num <- na.omit(gender2014)


library(writexl)
write_xlsx(gender2014_num,"C:/Users/hp/Downloads/gender2014_num.xlsx")


summary(dataframe_2014)


#splitting dataset among gender

GRAD_2014_Q1 <- subset(gender2014_num, q6.=='Trainee/Graduate/APS')
female<- subset(GRAD_2014_Q1[-2], q1=='Female')
male<- subset(GRAD_2014_Q1[-2], q1=='Male')
dim(male)
summary(male)

library(factoextra)
library(FactoMineR)
library(psych)
corrplot(cor(male[-1]), method = 'number')
head(male)
#PCA
man.pca <-princomp(male[-1])
get_eig(man.pca)
summary(man.pca)
fviz_screeplot(man.pca, addlabels = TRUE, ylim = c(0, 50))


EFA_modem <- fa(male[-1])
fa.diagram(EFA_modem)

#test with 2 factors not neccesary
EFA_modelm <- fa(male[-1], nfactors = 2, rotate = "varimax")
fa.diagram(EFA_modelm)
EFA_modelm


library(GPArotation)
#rotation
rotman <- factanal(male[-1], 2, rotation="varimax")
print(rotman, digits=2, cutoff=.3, sort=TRUE)

#####Female
dim(male[-1])
dim(female[-1])
EFA_modef <- fa(female[-1])
fa.diagram(EFA_modef)

#test with 2 factors
EFA_modelf <- fa(female[-1], nfactors = 2, rotate = "varimax")
fa.diagram(EFA_modelf)
EFA_modelf


library(GPArotation)
#rotation
rotf <- factanal(female[-1], 2, rotation="varimax")
print(rotf, digits=2, cutoff=.3, sort=TRUE)
fa.diagram(fit)

#Observe the loadings
rotgen <-cbind('female' = rotf$loadings, 'male' = rotman$loadings)
rotgen

library(writexl)
write_xlsx(female_factor[-1][-1],"C:/Users/hp/Downloads/female_factor.xlsx")

##CA : What are the differences among females' and males' impressions about their immediate workgroup and immediate supervisors?###############################################
male_CA <- read.delim("C:/Users/hp/Downloads/male_CA.txt", row.names=1) # read file 
female_CA <- read.delim("C:/Users/hp/Downloads/female_CA.txt", row.names=1) # read file 
View(cac)
cac2[6]


#In our example, the row and the column variables are statistically significantly associated (p-value < 0.05).
chisq <- chisq.test(male_CA)
chisq
chisq1 <- chisq.test(female_CA)
chisq1

res.ca.male <- CA(male_CA[-6], graph = TRUE)#symetric plot shows a global pattern within the data
summary(res.ca.male)

fviz_ca_biplot(res.ca.male, 
               map ="symmetric", arrow = c(TRUE, TRUE),
               title = "Biplot of Male responses",
               repel = TRUE)
res.ca.female <- CA(female_CA[-6], graph = TRUE)#symetric plot shows a global pattern within the data
summary(res.ca.female)

fviz_ca_biplot(res.ca.female, 
               title = "Biplot of Female responses",
               map ="symmetric", arrow = c(TRUE, TRUE),
               repel = TRUE)
##################################END###########################################################################
###################################################################################################################################

#######################Further Analysis######################################

#dataframe_filtered_2020_b
#Running Kruskal wallis test to see the effect of covid within the classification groups

#select question
krus <- select( dataframe_2020_clean,c('q5.', 'q49','q50','q51' ))
summary(krus)

str(krus)

#Sampling
Krus_sample <- sample_n(krus,3100)
str(Krus_sample)

#CHecking assumptions 
shapiro.test(Krus_sample$q50)


bartlett.test(q50~ q5., data = Krus_sample)

summary(krus$q5.)

group1 <- subset(krus,  q5. == "EL")
group2 <- subset(krus,  q5. == "SES")
group3 <- subset(krus,  q5. == "Trainee/Graduate/APS")
hist(group2$q50)
hist(group1$q50)
hist(group3$q50)
barplot(group3$q50)
summary(group1)
summary(group2)
summary(group3)

qqnorm(group1$q50)
qqline(group1$q50)

#install.packages('FSA')
library(FSA)
kruskal.test(q5.~ q50, data = Krus_sample)
dunnTest(q50~ q5., data = Krus_sample, method = 'bonferroni')


ggplot(Krus_sample,aes(x= q5., y = q50))+
  geom_boxplot(fill = 'grey80', color = 'black')+
  scale_x_discrete() + xlab("Classification")+
  ylab("to what extent is your work emotionally draining")


