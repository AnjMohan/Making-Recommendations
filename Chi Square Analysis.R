library(MASS)
View(carsdatabase$Cylinders)
View(carsdatabase$Type)

# Creating Table
Tbl = table(carsdatabase$Cylinders, carsdatabase$Type)
Tbl
# Chi-squared statistics
chisq.test(Tbl)

# Chi-squared test on new table
newT = cbind(Tbl[,"Compact"], Tbl[,"Large"], Tbl[,"Midsize"], Tbl[,"Small"], Tbl[,"Sporty"] + Tbl[,"Van"])
print(newT)
chisq.test(newT)

newT1 = cbind(Tbl[,"Compact"], Tbl[,"Midsize"], Tbl[,"Small"], Tbl[,"Sporty"]+Tbl[,"Large"],Tbl[,"Van"])
print(newT1)
chisq.test(newT1)

newT2 = cbind(Tbl[,"Compact"]+ Tbl[,"Small"], Tbl[,"Large"], Tbl[,"Midsize"], Tbl[,"Sporty"], Tbl[,"Van"])
print(newT2)
chisq.test(newT2)

newT3 = cbind(Tbl[,"Compact"] + Tbl[,"Large"], Tbl[,"Midsize"], Tbl[,"Small"], Tbl[,"Sporty"] , Tbl[,"Van"])
print(newT3)
chisq.test(newT3)

newT4 = cbind(Tbl[,"Compact"], Tbl[,"Large"]+Tbl[,"Midsize"], Tbl[,"Small"], Tbl[,"Sporty"] , Tbl[,"Van"])
print(newT4)
chisq.test(newT4)

newT5 = cbind(Tbl[,"Compact"], Tbl[,"Large"], Tbl[,"Midsize"]+ Tbl[,"Small"], Tbl[,"Sporty"] , Tbl[,"Van"])
print(newT5)
chisq.test(newT5)

newT6 = cbind(Tbl[,"Compact"], Tbl[,"Large"], Tbl[,"Midsize"], Tbl[,"Small"]+Tbl[,"Sporty"], Tbl[,"Van"])
print(newT6)
chisq.test(newT6)

newT7 = cbind(Tbl[,"Compact"] + Tbl[,"Van"], Tbl[,"Large"], Tbl[,"Midsize"], Tbl[,"Small"], Tbl[,"Sporty"])
print(newT7)
chisq.test(newT7)

newT8 = cbind(Tbl[,"Compact"], Tbl[,"Large"]+Tbl[,"Midsize"], Tbl[,"Small"]+ Tbl[,"Sporty"] + Tbl[,"Van"])
print(newT8)
chisq.test(newT8)


print(str(carsdatabase))
car_data = data.frame(carsdatabase$Cylinders, carsdatabase$Type)
car_data = table(carsdatabase$Cylinders, carsdatabase$Type)
print(car_data)
print(chisq.test(car_data))

library(descr)

# Alternative method
crosstab(carsdatabase$Cylinders,carsdatabase$Type, prop.r=TRUE,
         prop.c = TRUE, prop.chisq = TRUE, chisq = TRUE, row.labels = TRUE)
