BookRequests <- read_excel("C:/Users/cynth/Desktop/StThomas/SEIS 631 Data Analysis/BookRequests.xlsx")
View(BookRequests)
dim(BookRequests)
table(BookRequests)
str(BookRequests)
BookRequests$Days_until_on_Hold<-as.numeric(BookRequests$Days_until_on_Hold)

## q_daily is the measurement of movement of all book through the queue per day
BookRequests$q_daily <- (BookRequests$Starting_position_in_Queue/BookRequests$No._of_Copies)/BookRequests$Days_until_on_Hold
summary(BookRequests$q_daily)
BookRequests$q_weekly <- (7 * BookRequests$q_daily) ## weekly movement
summary(BookRequests$q_weekly)

##movement of adult nonfiction, adult fiction, non-adult
q_adult_nf <- BookRequests$q_daily[BookRequests$Audience_Refined == 'adult_nonfiction']
adult_nf <-(BookRequests$Audience_Refined == 'adult_nonfiction')
q_adult_fic <- BookRequests$q_daily[BookRequests$Audience_Refined == 'adult_fiction']
q_non_adult <- BookRequests$q_daily[BookRequests$Audience_Refined == 'non-adult']
summary(q_adult_nf)
summary(q_adult_fic)
summary(q_non_adult)

##movement of large print, regular print
##large print only has three values, one of which is NA since it doesn't have a
##value for Days_until_on_hold
##create variables for the change from start to three weeks, so I can have third value
q_reg_print <- BookRequests$q_daily[BookRequests$Regular_or_Large_Print == 'regular']
q_large_print <- BookRequests$q_daily[BookRequests$Regular_or_Large_Print == 'large']
summary(q_reg_print)
summary(q_large_print)

BookRequests$start_three_wks <- BookRequests$Starting_position_in_Queue - BookRequests$Position_After_3_Weeks
BookRequests$q_start_three_wks <-(BookRequests$start_three_wks/BookRequests$No._of_Copies)/ 21
q_start_threewks_large_print <- BookRequests$q_start_three_wks[BookRequests$Regular_or_Large_Print == 'large']
q_start_threewks_reg_print <- BookRequests$q_start_three_wks[BookRequests$Regular_or_Large_Print == 'regular']


##create variable to hold titles of books where there's a regular and large print
BookRequests$bks_duplicates <- BookRequests$Title[duplicated(BookRequests$Title)]
##separate regular print from large print
##TODO
##bks_reg_print <-bks_duplicates[BookRequests$Regular_or_Large_Print == 'regular']
##bks_lg_print <-BookRequests$bks_duplicates[BookRequests$Regular_or_Large_Print == 'large']
table(bks_duplicates)

##histograms
hist(BookRequests$Days_until_on_Hold)
hist(BookRequests$q_weekly)
hist(BookRequests$q_start_three_wks)
##The second histogram has a negative value, meaning some values went backwards, checked data and two items had moved back, these are possibly correct

summary(BookRequests$q_start_three_wks)
min(BookRequests$q_start_three_wks)

##frequency tables
table(BookRequests$Regular_or_Large_Print)
table(BookRequests$Audience_Refined)
table(BookRequests$`Fiction_or_Non-Fiction`)

##proportions
prop.table(table(BookRequests$Regular_or_Large_Print))
prop.table(table(BookRequests$Audience_Refined))
prop.table(table(BookRequests$`Fiction_or_Non-Fiction`))

##scatterplots
plot(BookRequests$Starting_position_in_Queue, BookRequests$Position_After_3_Weeks)
plot(BookRequests$No._of_Copies, BookRequests$Days_until_on_Hold)
##The ones with the shortest and longest days on hold are the ones with the fewest no of copies
plot(BookRequests$q_weekly, BookRequests$q_start_three_wks)
plot(BookRequests$q_weekly, BookRequests$No._of_Copies)
plot(BookRequests$Starting_position_in_Queue, BookRequests$No._of_Copies)
##I like this next version better
plot(BookRequests$Days_until_on_Hold, BookRequests$No._of_Copies)
plot(BookRequests$Days_until_on_Hold, BookRequests$q_weekly)
##This next one is better
plot( BookRequests$q_weekly, BookRequests$Days_until_on_Hold)

##boxplots
boxplot(BookRequests$q_start_three_wks ~ BookRequests$Regular_or_Large_Print)
boxplot(BookRequests$q_daily ~ BookRequests$Audience_Refined)
boxplot(BookRequests$q_daily ~ BookRequests$`Fiction_or_Non-Fiction`)

summary(BookRequests$`Fiction_or_Non-Fiction`)
##bks_reg_print <-BookRequests$Regular_or_Large_Print != 'large'[bks_duplicates]
##BookRequests$bks_reg_print <- BookRequests$Regular_or_Large_Print == 'regular'[BookRequests$bks_duplicates]
##table(BookRequests$bks_reg_print)
##still trying to get large, regular print variable<- where(there are two titles the same and it is in large and regular)
bks_reg_print <- subset(BookRequests,BookRequests$Regular_or_Large_Print == 'regular', BookRequests$Title [bks_duplicates])

#finding mean for fiction, non-fiction speed dataset
q_fiction <-(BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'fiction'])
summary(q_fiction)
table(BookRequests$`Fiction_or_Non-Fiction`)
q_nonfiction <-(BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'non-fiction'])
summary(q_nonfiction)
hist(BookRequests$`Fiction_or_Non-Fiction`== 'fiction')
##side by side histograms
par(mfrow = c(1,2))
hist(BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'fiction'], main = 'Speed of Fiction', xlab = 'speed through queue')
hist(BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'non-fiction'], main = 'Speed of Non-fiction', xlab = 'speed through queue')

##qqnorm
##data is skewed, trying log transformation
par(mfrow = c(1,1))
hist(log(BookRequests$q_daily))
par(mfrow = c(1,2))
hist(log(BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'fiction']))
hist(log(BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'non-fiction']))
##Fiction still looks very skewed, but non-fiction looks more normal
par(mfrow = c(1,2))
qqnorm((BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'fiction']))
qqnorm((BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'non-fiction']))

##t-test
t.test(x =BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'fiction'], y =  BookRequests$q_daily[BookRequests$`Fiction_or_Non-Fiction`== 'non-fiction'], alternative = 'two.sided')

##start of ANOVA
boxplot(BookRequests$q_daily ~ BookRequests$Audience_Refined)

##plots to assess normality, created variables earlier
par(mfrow = c(1,3))
hist(q_adult_fic, main = 'Adult Fiction', xlab = 'speed through queue')
hist(q_adult_nf, main = 'Adult Non-fiction', xlab = 'speed through queue')
hist(q_non_adult, main = 'Non-adult', xlab = 'speed through queue')

##qqplot
qqnorm(q_adult_fic, main = 'Adult Fiction')
qqnorm(q_adult_nf, main = 'Adult Non-fiction')
qqnorm(q_non_adult, main = 'Non-adult')

##standard deviations, remove NAs
sd(q_adult_fic, na.rm = T)
sd(q_adult_nf, na.rm = T)
sd(q_non_adult)

##ANOVA
fit <- lm(BookRequests$q_daily ~ BookRequests$Audience_Refined)
anova(fit)
TukeyHSD(aov(fit))

##create new variables
##slow_q binary variable, less than mean speed(0.05176), true is slow
BookRequests$slow_q <- (BookRequests$q_daily < 0.05176)
summary(BookRequests$q_daily)
table(BookRequests$slow_q)
##is_popular variable, binary, estimate popularity by starting position/number of copies, the greater the number the more popular
BookRequests$popular <- (BookRequests$Starting_position_in_Queue)/(BookRequests$No._of_Copies)
summary(BookRequests$popular)
##mean value for popular is 3.340, so > 3.340 is true
BookRequests$is_popular <- (BookRequests$popular > 3.340)
summary(BookRequests$is_popular)
summary(BookRequests$is_popular[BookRequests$`Fiction_or_Non-Fiction`== 'fiction'])
summary(BookRequests$is_popular[BookRequests$`Fiction_or_Non-Fiction`!= 'fiction'])

##new variable, low_copies, T or F, no._of_copies<=30
BookRequests$low_copies <- BookRequests$No._of_Copies <= 80
summary(BookRequests$low_copies[BookRequests$`Fiction_or_Non-Fiction`== 'fiction'])
summary(BookRequests$low_copies[BookRequests$`Fiction_or_Non-Fiction`== 'non-fiction'])

##another variable
BookRequests$Extremes <- BookRequests$No._of_Copies < 20 | BookRequests$No._of_Copies >150
summary(BookRequests$Extremes[BookRequests$`Fiction_or_Non-Fiction`== 'fiction'])
summary(BookRequests$Extremes[BookRequests$`Fiction_or_Non-Fiction`!= 'fiction'])

##Test of two proportions
table(BookRequests$Extremes, BookRequests$`Fiction_or_Non-Fiction`)
prop.table(table(BookRequests$Extremes, BookRequests$`Fiction_or_Non-Fiction`), 2)
#prop.test(table(BookRequests$Extremes, BookRequests$`Fiction_or_Non-Fiction`))
prop.test(table(BookRequests$`Fiction_or_Non-Fiction`, BookRequests$Extremes))
chisq.test(table(BookRequests$Extremes, BookRequests$`Fiction_or_Non-Fiction`))

##Chi qs test of goodness of fit
table(BookRequests$`Fiction_or_Non-Fiction`)
prop.table(table(BookRequests$`Fiction_or_Non-Fiction`))
chisq.test(table(BookRequests$`Fiction_or_Non-Fiction`), p = c(1/2, 1/2))

##Chi sq test for independence
table(BookRequests$is_popular, BookRequests$Audience_Refined)
table(BookRequests$is_popular, BookRequests$`Fiction_or_Non-Fiction`)

chisq.test(table(BookRequests$is_popular, BookRequests$Audience_Refined))
chisq.test(table(BookRequests$is_popular, BookRequests$`Fiction_or_Non-Fiction`))
