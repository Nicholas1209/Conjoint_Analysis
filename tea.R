# BUS 421 analysis of tea conjoint
# 03/02/2023

# import conjoint library
#install.packages("conjoint")
library(conjoint)

######################################################################
### Step 1 experimental design

# define four ice cream attributes for conjoint study         <---------------------define attributes here
price = c("$40", "$55", "$70")
variety = c("black", "green", "red")
kind = c("bags", "granulated", "leafy")
aroma = c("yes", "no")

#create level names
levelnames = data.frame("levels" = c(price, variety, kind, aroma))

# full factorial design, total 3x3x2x2=36 combinations
experiment = expand.grid(price=price, variety=variety, kind=kind, aroma=aroma)
surveycards = caFactorialDesign(data=experiment,cards=13, type="fractional") #<--------change num of cards here


#view experiment and carddesign
head(experiment)
print(surveycards)
# QC design cards
cor(caEncodedDesign(surveycards))
# encode the design
profiles=caEncodedDesign(design=surveycards)

# export survey cards for data collection
# reset survey cards index
rownames(surveycards) <- NULL 
file.choose()
write.csv(surveycards, "/Users/nicholas/Desktop/tea_conjointcards.csv") #<-----change dir here

### end of design #####

### step 2 collect survey data

# import survey respondent data from csv file
file.choose()
preferences = read.csv(file="/Users/nicholas/Desktop/CSUMB/Spring 23/BUS421/03 conjoint/assignment/tea_pref.csv")   #<----change file here

# import conjoint cards/profile
print(profiles)

### step 3 main conjoint model

# find the utilities
u1 = caUtilities(y=preferences,x=profiles,z=levelnames)
prod_utility = data.frame(u1,row.names = c("intercept",as.list(levelnames)$levels))
print(prod_utility)

# find the feature importance & print
u2 = caImportance(y=preferences,x=profiles)
barplot(u2, names.arg = c("price","variety","kind", "aroma"))

### step 5 optimal price
#which price point maximizes revenue?
# 1. find the most favorable product profile

prod_utility = data.frame(u1,row.names = c("intercept",as.list(levelnames)$levels))
print(prod_utility)
# by looking at utilities, we identify most favorable features flavor='vanilla', container='cone', topping='no'

#price1 demand
d1 = prod_utility['intercept',] + prod_utility['granulated',] + prod_utility['black',] + prod_utility['yes',] + prod_utility['$40',] 
print(d1)
#The alternative way to calculate demand at price 1 is to copy the partUtility in formular below
#d1 = 5.347 + 0.722 + 0.917 + 0.125 + 0.833
r1 = d1*40

#price 2 demand 
d2 = prod_utility['intercept',] + prod_utility['granulated',] + prod_utility['black',] + prod_utility['yes',] + prod_utility['$55',] 
r2 = d2*55

#price 3 demand intercept + vanilla + cone + no + price3
d3 = prod_utility['intercept',] + prod_utility['granulated',] + prod_utility['black',] + prod_utility['yes',] + prod_utility['$70',] 
r3 = d3*70

#create the dataframe for plot
est_demand = c(d1,d2,d3)
est_rev = c(r1,r2,r3)
price_level = c(40, 55, 70)

revenue = data.frame(est_demand, est_rev, price_level)

#plot demand curve
plot(price_level, est_demand, type='o',xlab="price", ylab="demand")

#plot revenue curve
plot(price_level, est_rev, type='o',xlab="price", ylab="revenue")











