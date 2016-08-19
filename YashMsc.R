#Yashika MSc code
#March 2016 - Kolombangara Solomon Islands
library(ggplot2)
library(ggmap)   
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(reshape2)
library(car)
library(reports)
library(lme4)
library(stringr)
??lmer

#rawest data for emily


### CO-OCCURENCE ANALYSIS
##4. Import cleaned categories data
#no healthy colonies
#partially dead, pd, removed from all
setwd ("/Users/emilydarling/Documents/Work/GitHub/Misc-R-code/YashR/data from Yash")           
d <- read.csv("cooccurence RAW_26Mar2016.csv", header = TRUE, stringsAsFactors = TRUE) 
head(d)
nrow(d)
names(d)

hist(d$n_cat)

unique(d$cat1)

#subset to co-occurence data
d2 <- subset(d, n_cat > 1)

## HERE - make replicate rows for colonies with NumberperTransect > 0 

##but then, have a dataset with row = colony, and co-occurence in separate rows 






##3. THIRD PASS TO SEPARATE TYPES AND CODES INTO CATEGORIES
setwd ("/Users/emilydarling/Documents/Work/GitHub/Misc-R-code/YashR/data from Yash/csv")           
d <- read.csv("test_bind_update_v0.csv", header = TRUE, stringsAsFactors = TRUE) 
head(d)
nrow(d)
names(d)  

#types are CLEAN - no typos
levels(as.factor(d$type1))
levels(as.factor(d$type2))
levels(as.factor(d$type3))
levels(as.factor(d$type4))
levels(as.factor(d$type5))

head(d)

test <- d %>%
  group_by(SiteName) %>%
  summarize(totalcols = sum(NumberperTransect))





##HERE, CAN FIND TOTAL COUNT OF HEALTHY and NON-HEALTHY COLONIES LATER
#Use SignsAbnormal to remove healthy and PD colonies
#remove healthy colonies for now
d <- d[-which(d$SignsAbnormal == 0),]
#remove PD
d <- d[-which(d$SignsAbnormal == 3),]
head(d)
unique(d$SignsAbnormal)

head(d)
#for loop to turn types into categories
#remove type5, total types
d <- d[,-c(12:13)]
names(d)

for (i in 8:11) {
  type = d[,i]
  category = recode(type, "'FB' = 'B';
                        'E' = 'GA';
                        'CLOD' = 'other';
                        'C' = 'other';
                        'ESE' = 'GA';
                        'IG' = 'GA';
                        'SD' = 'other';
                        'AO' = 'other';
                        'TR' = 'GA'")
  k = i + 4
  d[,k] = category  
}

head(d)
names(d)
names(d)[12:15] <- c("cat1", "cat2", "cat3", "cat4")

write.csv(d, "category_check2.csv", row.names = FALSE)



#recode categories from types
unique(d2$variable)
unique(d2$value)

d2$category <- recode(d2$value, 
                      " 'FB' = 'B';
                        'E' = 'GA';
                        'CLOD' = 'other';
                        'C' = 'other';
                        'ESE' = 'GA';
                        'IG' = 'GA';
                        'SD' = 'other';
                        'AO' = 'other';
                        'TR' = 'GA'")
unique(d2$category)
head(d2)
#other disease
#UWS - ulcerative white spot syndrome
#PLD - pink line disease
#BBD - black band disease
#BRB - brown band disease
#TLS - tissue loss syndrome

head(d2)
write.csv(d2, "category_check.csv", row.names= FALSE)

##sum across different types of growthforms
##will sum all counts to a per genus level
d3 <- d2 %>%
  select(Location,SiteName,Transect,Genus,NumberperTransect,variable,category) %>%
  group_by(Location,SiteName,Transect,Genus,variable,category) %>%
  summarise(no = sum(NumberperTransect))           

head(d3)


##NOW, cast data back out to each type, with category information
head(d2)
d3 <- dcast(d2, Location+SiteName+Transect+Genus+NumberperTransect)



#EXAMPLE OF DPLYR!!
#read Introduction to DPLYR
#THIS IS PIVOT TABLES!!
#We are eating pivot tables for breakfast
head(d2)

d3 <- d2 %>%
  group_by(Location,SiteName,Transect,Genus,)
  
  
  
  




#recode types into categories
#check there is no duplication in categories




##2. SECOND PASS TO CLEAN TYPES
setwd ("/Users/emilydarling/Documents/Work/GitHub/Misc-R-code/YashR/data from Yash/csv")           
d <- read.csv("test_bind_update.csv", header = TRUE, stringsAsFactors = TRUE) 
head(d)
nrow(d)
names(d)      

#clean types
levels(as.factor(d$type1))
d$type1 <- toupper(d$type1)

levels(as.factor(d$type1))
d$type1 <- recode(d$type1, 
                  "'FD' = 'FB';
                  'USWD' = 'UWS';
                  'UWD' = 'UWS';
                  'UWSD' = 'UWS';
                  'WSPT' = 'UWS'")

levels(as.factor(d$type2))
d$type2 <- toupper(d$type2)
d$type2 <- recode(d$type2, 
                  "'FD' = 'FB';
                  'USWD' = 'UWS';
                  'UWD' = 'UWS';
                  'UWSD' = 'UWS';
                  'WSPT' = 'UWS';
                  'PFB' = 'FB';
                  'PPD' = 'PD'")

levels(as.factor(d$type3))
d$type3 <- toupper(d$type3)
d$type3 <- recode(d$type3, 
                  "'FD' = 'FB';
                  'USWD' = 'UWS';
                  'UWD' = 'UWS';
                  'UWSD' = 'UWS';
                  'WSPT' = 'UWS'")

levels(as.factor(d$type4))
d$type4 <- toupper(d$type4)
d$type4 <- recode(d$type4, 
                  "'FD' = 'FB';
                  'USWD' = 'UWS';
                  'UWD' = 'UWS';
                  'UWSD' = 'UWS';
                  'WSPT' = 'UWS'")

levels(as.factor(d$type5))
d$type5 <- toupper(d$type5)
d$type5 <- recode(d$type5, 
                  "'FD' = 'FB';
                  'USWD' = 'UWS';
                  'UWD' = 'UWS';
                  'UWSD' = 'UWS';
                  'WSPT' = 'UWS'")

write.csv(d, "test_bind_update_v0.csv", row.names = FALSE)

#2. FIRST PASS OF RAW CSV FILES
##Genus co-occurrence data
##pull in raw csv files and bind together
getwd()
setwd ("/Users/emilydarling/Documents/Work/GitHub/Misc-R-code/YashR/data from Yash/csv")  

#read in .csv files and bind together with unique id
csv_files <- dir(pattern='*.csv$', recursive = T)
csv_files
yash_bind <- rbind_all(lapply(csv_files, read.csv))    
head(yash_bind)

#use first 10 columns
#remove date and time with NAs, and growthform
names(yash_bind)
d <- yash_bind[,c(1,4:10)]
#remove NA rows
d <- na.omit(d)
head(d)
names(d)
nrow(d)

#check that bind worked, and clean some
unique(d$Location)
d$Location <- str_trim(d$Location)
d$Location <- recode(d$Location, "'Nanuyalailai' = 'NanuyaLailai'")

unique(d$Site)
table(d$Location, d$Site)

#add in Reef type, management LATER
#unique(d$Reef.Type)
#d$factorReef.Type <- recode(d$Reef.Type,
                            "'Shallow Terrace' = 'Reef flat';
                             'Diffused fringing' = 'Reef flat';
                             'Reef' = 'Reef flat'")
#unique(d$factorReef.Type)
#unique(d$Management)

unique(d$Transect.)
table(d$Site, d$Transect.)

#clean up Genus a little bit
d$Genus <- str_trim(d$Genus)
#CA capitalizes only first letter, e.g., fixed PAvona to Pavona
d$Genus <- CA(d$Genus)
d$Genus <- recode(d$Genus, "'Coscinarea' = 'Coscinaraea'")

#drop Milleopora
d <- d[-which(d$Genus == "Millepora"),]

levels(as.factor(d$Genus))

#cleanup growthforms, could separate Porites massive from Porites branching later
#str_trim is remove any white spaces on start or end
d$Growthforms <- str_trim(d$Growthforms)
#toupper converts all to upper case
d$Growthforms <- toupper(d$Growthforms)
levels(as.factor(d$Growthforms))
#5 missing Growthform, no biggie for now
subset(d, Growthforms == "")

#check Signs of abnormality
levels(as.factor(d$Signs.of.abornmality))

#clean up types and separate into individual columns
#cheat in Excel
levels(as.factor(d$Types))
d$Types <- str_trim(d$Types)
write.csv(d, "test_bind.csv", row.names = FALSE)





table(d$site, d$transect)

head(d)
names(d)

#calculate total colonies
d$Total <- rowSums(d[,4:14])
head(d$Total)
hist(d$Total)

subset(d, Total > 200)

#calculate bleaching prevalence
d$pbleach <- d$Total.Bleaching / d$Total
d$pdisease <- d$TotalD / d$Total

#take average of datasets using DPLYR and PIPES
head(d)
d_avg <- d %>%
  group_by(genus,site) %>%
  summarize(mean.pbleach = mean(pbleach),
            mean.pdisease = mean(pdisease),
            n = n()) 

d_avg2 <- d_avg %>%
  group_by(genus) %>%
  summarize(mean.pbleach = mean(mean.pbleach),
            mean.pdisease = mean(mean.pdisease),
            n = n())

arrange(d_avg2, desc(mean.pbleach))

head(d_avg2)
ggplot(data = d_avg2, aes(x = mean.pbleach, y = mean.pdisease)) + 
  geom_point(aes(colour = genus), size = 2) +
  geom_text(label = abbreviate(d_avg2$genus)) +
  theme_bw(base_size = 16)

##Daily progression data
#set working directory
#read in .csv data
getwd()
setwd ( "/Users/emilydarling/Documents/Work/GitHub/Misc-R-code/YashR")  
d <- read.csv("ProgressiveLesion_4day.csv", header = TRUE, stringsAsFactors = TRUE)  
head(d)
names(d)
nrow(d)

#checked out data properties
str(d)
levels(d$Coral)
levels(d$Time)

#histogram of response variable
hist(d$ProgRate_cm_hr)

#summary values
mean(d$ProgRate_cm_hr)
sd(d$ProgRate_cm_hr)

#check for outliers
#uh oh, outliers! 
#see Zuur et al. Methods Ecol Evol 2010
boxplot(d$ProgRate_cm_hr,  ylab = "Progression rate, cm/hour")
dotchart(d$ProgRate_cm_hr,  xlab = "Progression rate, cm/hour",
         ylab = "Order of the data")      

#plot progression rates separately for each coral
#ggplot is a great graphing package
head(d)
ggplot(data = d, aes(x = Time, y = ProgRate_cm_hr, group = Coral)) +
  geom_point(aes(colour = Coral), size = 3) +
  geom_line(aes(colour = Coral), size = 1) +
  facet_wrap(~Coral) +
  theme_bw(base_size = 16) +
  ylab("Progression rate, cm/hour") + 
  theme(legend.key = element_blank()) + 
  scale_colour_brewer(palette = "Set1")

#save plot as PDF
getwd()
ggsave("Yash plot.pdf", width = 8, height =5)

#plots of progression length, cm
head(d)
ggplot(data = d, aes(x = Time, y = ProgDistance_cm, group = Coral)) +
  geom_point(aes(colour = Coral)) +
  geom_line(aes(colour = Coral)) +
  facet_wrap(~Coral) +
  theme_bw(base_size = 16)

#model repeated measures using a random effect of coral colony
model <- lme(ProgRate_cm_hr ~ Time, 
             random = ~1 | Coral, 
             data = d)
summary(model)

#save residuals, E, and check for normal distribution of residuals 
E <- resid(model)
hist(E)

#remove outliers and recheck model
head(d)
d2 <- subset(d, ProgRate_cm_hr < 0.15)

model <- lme(ProgRate_cm_hr ~ Time, 
             random = ~1 | Coral, 
             data = d2)
summary(model)


#check gamma distribution for right-skewed continuous response variable 

