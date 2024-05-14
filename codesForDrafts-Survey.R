
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in libraries and data ------------------------------------------------------------
#set working directory

library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(skimr)
library(descr)
library(srvyr)
library(survey)
library(weights)
library(anesrake)
library(stargazer)
library(pollster)
library(kableExtra)
library(labelled)
library(janitor)
library(rstatix)
library(flextable)
library(knitr)
library(poliscidata)
library(data.table)

#Import Data
omni <- read_sav("omni_final.sav")
names(omni) #Use this to quickly see what your variables are named 
skim(omni) #Important to review your data before working with it
omni<-clean_names(omni) #Cleans up the names in your data - janitor package 
detach(package:janitor, unload = TRUE)
  
# clean data ----------------------------------------------------------

#Creates weight variable placeholder 
omni$weight<-1

#Filter out respondents who failed attention check  
###Removes attention check failures 
omni$check<-omni$q9_3+omni$q9_4
freq(omni$check) #Checks number who failed 

omni_pass<-omni %>% #New Data frame to work from  
  filter(check==2)

# Weighting ----------------------------------------------------------

# run as a data frame
omni_pass <- as.data.frame(omni_pass)

# Set some values to numeric instead of characters
omni_pass$age = as.numeric(omni_pass$age)
omni_pass$ethnicity = as.numeric(omni_pass$ethnicity)
omni_pass$hispanic = as.numeric(omni_pass$hispanic)
omni_pass$gender = as.numeric(omni_pass$gender)

# create numeric variables to match demographic weights
omni_pass <- omni_pass %>%
  mutate(age_group_demo = case_when(
    age < 26 ~ 1,
    age > 25 & age < 35 ~ 2,
    age > 34 & age < 55 ~ 3,
    age > 54 & age < 65 ~ 4,
    age > 64 ~ 5,
  )) %>%
  mutate(race_demo = case_when(
    ethnicity==1 ~ 1,
    ethnicity==2 ~ 2,
    ethnicity==3 ~ 4,
    ethnicity==4 ~ 3,
    ethnicity==5 ~ 3,
    ethnicity==6 ~ 3,
    ethnicity==7 ~ 3,
    ethnicity==8 ~ 3,
    ethnicity==9 ~ 3,
    ethnicity==10 ~ 3,
    ethnicity==11 ~ 5,
    ethnicity==12 ~ 5,
    ethnicity==13 ~ 5,
    ethnicity==14 ~ 5,
    ethnicity==15 ~ 5,
  )) %>%
  mutate(hispanic_demo = case_when(
    hispanic==1 ~ 2, 
    hispanic==2 ~ 1,
    hispanic==3 ~ 1,
    hispanic==4 ~ 1,
    hispanic==5 ~ 1,
    hispanic==6 ~ 1,
    hispanic==7 ~ 1,
    hispanic==8 ~ 1,
    hispanic==9 ~ 1,
    hispanic==10 ~ 1,
    hispanic==11 ~ 1,
    hispanic==12 ~ 1,
    hispanic==13 ~ 1,
    hispanic==14 ~ 1
  )) %>%
  mutate(college_demo = if_else(education<6, 1, 2))

# Establish Vectors for Weight Proportions
# Hispanic proportions: Hispanic gets 16.8% and Non-Hispanic gets 83.2%
hispanic_demo <- c(0.168, 0.832)

# College Degree gets 37.7% and No College Degree gets 62.3%
college_demo <- c(0.623, 0.377)

# Race demos: white is 64.1, black is 12.0, asian is 6.1, native is 1.3, and other is 16.5
race_demo <- c(0.641, 0.120, 0.061, 0.013, 0.165)

age_group_demo <- c(0.113, 0.159, 0.334, 0.168, 0.226)

# Sex demo: Males are 48.3% and Females are 51.7%
gender <- c(0.483, 0.517)

# Establish target list
targets <- list(gender, age_group_demo, race_demo, hispanic_demo, college_demo)
names(targets) <- c("gender", "age_group_demo", "race_demo", "hispanic_demo", "college_demo")


# Calculate the weights -------------------------------------------------------

# Calc a new dataframe called myweights where we input targets list, etc. 
#anesrake(target values, dataframe, caseid, weightvec = NULL, cap = 5,
#verbose = FALSE, maxit = 1000, type = "pctlim", pctlim = 5,
#nlim = 5, filter = 1, choosemethod = "total", iterate = TRUE)

#Step 2 - Calculate the Rake Weight
set.seed(1599385) #Set the seed for replication  
myweights <- anesrake(targets, omni_pass, 
                      caseid = omni_pass$rid, cap = 8, type = "nolim", pctlim=.05)

# Save Weights in Data - save rake weight at end of data
omni_pass$weight <- unlist(myweights[1])

# Review Weights -------------------------------------------------

#Displays summary of the weight size to see range
summary(omni_pass$weight)

#Shows the weight size by demographic groups used in the weighting scheme
omni_pass %>% 
  as_survey(weights = c(weight)) %>%
  group_by(gender, age_group_demo) %>% 
  summarise(weight = survey_mean(weight, na.rm = T))

# Review -------------------------------------------------------------

###Review the variable names, value labels and values for your key variable
attributes(omni_pass$q25_1) #Tells you the value labels for the variable if they exist - USE  
freq(omni_pass$q25_1) #Tells you frequency that each response option was selected


#get_summary_stats from the rstatix package works better for a few variables
#Only works on numeric variables 
omni_pass %>%  #Manually enter the variable names
  get_summary_stats(     
    q25_1, q25_2, q25_3,  # columns to calculate for
    type = "common")

# Calculate Margin of Error --------------------------------------

#Create a function to calculate our margin of error 
moe_fun <- function(p, n, cv) {
  step1<-p*(1-p)
  step2<-step1/n
  se<-(step2)^.5
  moe<-cv*se
  print(moe)
}

moe_fun(.5, 281, 1.96)

# Mutate variables ---------------------------------------------------------------

omni_pass <- omni_pass %>%   #Recode with labels for easy table display
  mutate(ESP = case_when(
    q25_1==1 ~ 'Agree',
    q25_1==2 ~ 'Agree',
    q25_1==3 ~ 'Do not Agree',
    q25_1==4 ~ 'Do not Agree',
    q25_1==5 ~ 'Do not Agree',))

omni_pass <- omni_pass %>%   #Recode as value for easy graphical display
  mutate(Psychic = case_when(
    q25_1==1 ~ 1,
    q25_1==2 ~ 1,
    q25_1==3 ~ 0,
    q25_1==4 ~ 0,
    q25_1==5 ~ 0,))

omni_pass <- omni_pass %>%   #Recode with labels for easy table display
  mutate(Sasquatch = case_when(
    q25_2==1 ~ 'Agree',
    q25_2==2 ~ 'Agree',
    q25_2==3 ~ 'Do not Agree',
    q25_2==4 ~ 'Do not Agree',
    q25_2==5 ~ 'Do not Agree',))

omni_pass <- omni_pass %>%   #Recode as value for easy graphical display
  mutate(B2ft = case_when(
    q25_2==1 ~ 1,
    q25_2==2 ~ 1,
    q25_2==3 ~ 0,
    q25_2==4 ~ 0,
    q25_2==5 ~ 0,))

omni_pass <- omni_pass %>%   #Recode with labels for easy table display
  mutate(Aliens = case_when(
    q25_3==1 ~ 'Agree',
    q25_3==2 ~ 'Agree',
    q25_3==3 ~ 'Do not Agree',
    q25_3==4 ~ 'Do not Agree',
    q25_3==5 ~ 'Do not Agree',))

omni_pass <- omni_pass %>%   #Recode as value for easy graphical display
  mutate(Space = case_when(
    q25_3==1 ~ 1,
    q25_3==2 ~ 1,
    q25_3==3 ~ 0,
    q25_3==4 ~ 0,
    q25_3==5 ~ 0,))

omni_pass <- omni_pass %>%   
  mutate(female = case_when(
    gender==1 ~ 'Male',
    gender==2 ~ 'Female'))

omni_pass <- omni_pass %>%
  mutate(region = case_when(
    region==1 ~ "Northeast",
    region==2 ~ "Midwest",
    region==3 ~ "South",
    region==4 ~ "West",
  ))

omni_pass <- omni_pass %>%
  mutate(religion = case_when(
    q44==1 ~ "Evangelical/Born Again",
    q44==2 ~ "Other Protestant",
    q44==3 ~ "Catholic",
    q44==4 ~ "Other Christian",
    q44==5 ~ "Jewish",
    q44==6 ~ "Muslim",
    q44==7 ~ "Other",
    q44==8 ~ "Elements of Multiple Religions",
    q44==9 ~ "No Religion",
  ))

omni_pass <- omni_pass %>%
  mutate(rlgn = case_when(
    q44==1 ~ "Christian (non-Catholic)",
    q44==2 ~ "Christian (non-Catholic)",
    q44==3 ~ "Catholic",
    q44==4 ~ "Christian (non-Catholic)",
    q44==5 ~ "Jewish",
    q44==6 ~ "Muslim",
    q44==7 ~ "Other",
    q44==8 ~ "Elements of Multiple Religions",
    q44==9 ~ "No Religion",
  ))

omni_pass <- omni_pass %>%
  mutate(beliefs = (B2ft + Space + Psychic))
freq(omni_pass$beliefs)

# Space, B2ft, and Psychic are the column names with binary data

omni_pass <- omni_pass %>%
  mutate(one_belief = case_when(
    beliefs==1 & Space==1 ~ "Aliens", 
    beliefs==1 & B2ft==1 ~ "Bigfoot",
    beliefs==1 & Psychic==1 ~ "ESP")
  ) %>%
  mutate(two_beliefs = case_when(
    beliefs==2 & Space==0 ~ "ESP & Bigfoot",
    beliefs==2 & B2ft==0 ~ "ESP & Aliens",
    beliefs==2 & Psychic==0 ~ "Aliens & Bigfoot",
  ))

omni_pass <- omni_pass %>%
  mutate(race = case_when(
    ethnicity==1 ~ "White",
    ethnicity==2 ~ "Black",
    ethnicity==3 ~ "American Indian/Alaska Native",
    ethnicity==4 ~ "Asian",
    ethnicity==5 ~ "Asian",
    ethnicity==6 ~ "Asian",
    ethnicity==7 ~ "Asian",
    ethnicity==8 ~ "Asian",
    ethnicity==9 ~ "Asian",
    ethnicity==10 ~ "Asian",
    ethnicity==11 ~ "Pacific Islander",
    ethnicity==12 ~ "Pacific Islander",
    ethnicity==13 ~ "Pacific Islander",
    ethnicity==14 ~ "Pacific Islander",
    ethnicity==15 ~ "Other"
  ))

omni_pass <- omni_pass %>%
  mutate(hispanic = case_when(
    hispanic==1 ~ "Not Hispanic",
    hispanic==c(2:14) ~ "Hispanic",
    hispanic==15 ~ "Prefer Not to Answer"
  ))

omni_pass <- omni_pass %>%
  mutate(political_lean = case_when(
    political_party==c(1:3, 6) ~ "Democrat or Dem-leaning",
    political_party==c(5, 8:10) ~ "Republican or Rep-leaning",
    political_party==c(4, 7) ~ "Other"
  ))

omni_pass <- omni_pass %>%
  mutate(political_party = case_when(
    political_party==c(1:2) ~ "Democrat",
    political_party==c(9:10) ~ "Republican",
    political_party==c(3:5) ~ "Independent",
    political_party==c(6:8) ~ "Other"
  ))

# Recode other variables -------------------------------------------------------------

class(omni_pass$education) #Tells you the value labels for the variable if they exist - USE. Lucid demos import as character but should be numeric so needs  changing
omni_pass$education<-as.numeric(omni_pass$education) #Changes to numeric 
class(omni_pass$education) 
freq(omni_pass$education) #Notice how there is a value = -3105 in here. That must be removed

omni_pass <- omni_pass %>% #Explicitly making the values -1/-9 NA 
  mutate(education = replace(education, education <= -1, NA)) #Recodes all values in education variable <0 to NA since all negative values represent non-analyzable data 

freq(omni_pass$education) 
#Easily Collapse into dichotomous variable with if_else command
omni_pass <- omni_pass %>% 
  mutate(college = if_else(education<6, 'No College Degree', 'College Degree'))
# omni_pass <- omni_pass %>% 
#   mutate(college_n = if_else(education<6, 0, 1))
freq(omni_pass$college) #Always check your newly created variable to make sure it looks right
CrossTable(omni_pass$education, omni_pass$college, prop.r=TRUE, prop.c=FALSE,
           prop.t = FALSE, prop.chisq = FALSE)

#Collapse into 4 groups - notice new name is educ4 to indicate number of educ groups
omni_pass <- omni_pass %>%   
  mutate(educ4 = case_when(
    education==1 ~ 'HS or Less',
    education==2 ~ 'HS or Less',
    education==3 ~ 'Some College',
    education==4 ~ 'Some College',
    education==5 ~ 'Some College',
    education==6  ~ 'Bachelors',
    education==7 ~ 'Advanced Degree',
    education==8 ~ 'Advanced Degree',))

freq(omni_pass$educ4)
CrossTable(omni_pass$education, omni_pass$college, prop.r=TRUE, prop.c=FALSE,
           prop.t = FALSE, prop.chisq = FALSE)

#Creates Age Group Buckets using the cut function from base r 
omni_pass$age<-as.numeric(omni_pass$age)
omni_pass$age_group <- cut(omni_pass$age, breaks = c(18, 26, 35, 55, 65, Inf), labels = c("18-25", "26-34", "35-54", "55-64", "65+"), right = FALSE)

CrossTable(omni_pass$age, omni_pass$age_group, prop.r=TRUE, prop.c=FALSE,
           prop.t = FALSE, prop.chisq = FALSE)


# Topline Tables ------------------------------------------------------------

#topline code creates a table with frequency distributions of the variable you choose
ESP_topline = topline(df = omni_pass, variable = q25_1, 
        weight = weight, pct = FALSE, cum_pct=FALSE) %>%
  kable(digits=c(0,0,1), #We set the decimal places to 0 for first two columns and 1 for third 
        col.names = c('Response', 'N', 
                      'Percent <br> Responding'),#Changes names of the columns to make them more readable for people 
        escape=FALSE, align=('lcc'),
        caption = "Q: Sometimes people can know future events without explanation before they happen.", ) %>% #Use align to chance the alignment of each column 
  kable_classic(lightable_options = "striped",
                full_width = F, 
                html_font = "Helvetica") %>% #Controls font and width of table 
  kableExtra::footnote(alphabet = ("Margin of Error +/-5.8%"))

ESP_topline

#topline code creates a table with frequency distributions of the variable you choose
BF_topline = topline(df = omni_pass, variable = q25_2, 
        weight = weight, pct = FALSE, cum_pct=FALSE) %>%
  kable(digits=c(0,0,1), #We set the decimal places to 0 for first two columns and 1 for third 
        col.names = c('Response', 'N', 
                      'Percent <br> Responding'),#Changes names of the columns to make them more readable for people 
        escape=FALSE, align=('lcc'),
        caption = "Q: Bigfoot (Sasquatch) is a real animal species.", ) %>% #Use align to chance the alignment of each column 
  kable_classic(lightable_options = "striped",
                full_width = F, 
                html_font = "Helvetica") %>% #Controls font and width of table 
  kableExtra::footnote(alphabet = ("Margin of Error +/-5.8%"))

BF_topline

#topline code creates a table with frequency distributions of the variable you choose
ET_topline = topline(df = omni_pass, variable = q25_3, 
        weight = weight, pct = FALSE, cum_pct=FALSE) %>%
  kable(digits=c(0,0,1), #We set the decimal places to 0 for first two columns and 1 for third 
        col.names = c('Response', 'N', 
                      'Percent <br> Responding'),#Changes names of the columns to make them more readable for people 
        escape=FALSE, align=('lcc'),
        caption = "Q: Extraterrestrials (aliens) have visited Earth.", ) %>% #Use align to chance the alignment of each column 
  kable_classic(lightable_options = "striped",
                full_width = F, 
                html_font = "Helvetica") %>% #Controls font and width of table 
  kableExtra::footnote(alphabet = ("Margin of Error +/-5.8%"))

ET_topline

# save toplines --------------------------------------------------------

saveRDS(ESP_topline, file = "ESP_topline.rds")

saveRDS(BF_topline, file = "BF_topline.rds")

saveRDS(ET_topline, file = "ET_topline.rds")


# Respondents who believe in 1 of the 3 phenomena -----------------------------------------

One_Phenom = ggplot(subset(omni_pass, !is.na(one_belief))) + 
  geom_bar(aes(x = one_belief, fill=female), position = "dodge") + 
  scale_fill_manual(values = c("darkorange2", "mediumpurple4")) + 
  labs(title = "ESP is Queen Bee Among Paranormal Beliefs", subtitle = "Respondents with only one paranormal belief, divided by sex", x="Paranormal Belief", y="Count") +
  theme(legend.position = c(0.12, 0.85), legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size = 18), plot.subtitle = element_text(hjust = 0.5, size = 13), legend.background = element_rect(fill='transparent'), panel.grid.major.x = element_blank(), legend.box.background = element_rect("white"), panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "lightgray"))

One_Phenom

# save One_Phenom Draft ----------------------------------------------------------
saveRDS(One_Phenom, file = "One_Phenom.rds")

# Respondents who believe in 2 of the 3  ------------------------------------------------
Two_Phenom = ggplot(subset(omni_pass %>% filter(!is.na(female)), !is.na(two_beliefs))) + 
  geom_bar(aes(x = two_beliefs, fill=female), position = "dodge") + 
  scale_fill_manual(values = c("darkorange2", "mediumpurple4"), labels=c("Female", "Male")) + 
  labs(title = "ESP & Aliens Most Common Paranormal Beliefs", subtitle = "Respondents with only two of three paranormal beliefs, divided by sex", x="Paranormal Belief", y="Count") +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "lightgray"), legend.position = c(0.12, 0.85), legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size = 18), plot.subtitle = element_text(hjust = 0.5, size = 13), legend.background = element_rect(fill='transparent'), panel.grid.major.x = element_blank(), legend.box.background = element_rect("white"))

Two_Phenom 

# save Two_Phenom Draft ----------------------------------------------------------
saveRDS(Two_Phenom, file = "Two_Phenom.rds")

# Bigfoot Belief by Region & Sex -------------------------------------------------------

#Step 1: Save the mean value you want to graph by the demographic variable
mean_values <- omni_pass %>%
  group_by(region, female) %>%
  summarise(mean_B2ft = mean(B2ft, na.rm = TRUE),
            n_count = n())

#Step 2: Filter out data you do not want to graph  
mean_values <- mean_values %>% 
  filter(region!="NA")

#Step 3: Get the MOE 
mean_values$mov<-moe_fun(.5, mean_values$n_count, 1.96)

#Step 4: Plot 
BFRegion = ggplot(mean_values, aes(x = region, y = mean_B2ft, fill = female)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_B2ft - mov, ymax = mean_B2ft + mov), 
                width = 0.2, position = position_dodge(0.9), color = "red") +
  scale_fill_manual(values = c("darkorange2", "mediumpurple4"), labels=c("Female", "Male")) +
  labs(title = "Bigfoot Belief by Region & Sex",
       x = "Region",
       y = "% Believes Bigfoot is a Real Animal Species") +
  # theme_minimal() + 
  theme(legend.position = c(0.10, 0.87), legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size = 18), legend.background = element_rect(fill='transparent'), panel.grid.major.x = element_blank(), legend.box.background = element_rect("white"), panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "lightgray"))

BFRegion

# save BFRegion ----------------------------------------------------------
saveRDS(BFRegion, file = "BFRegion.rds")


# Bigfoot belief by Age and Sex --------------------------------------------------------

#Step 1: Save the mean value you want to graph by the demographic variable
mean_values <- omni_pass %>%
  group_by(age_group, female) %>%
  summarise(mean_B2ft = mean(B2ft, na.rm = TRUE),
            n_count = n())

#Step 2: Filter out data you do not want to graph  
mean_values <- mean_values %>% 
  filter(age_group!="NA")

#Step 3: Get the MOE 
mean_values$mov<-moe_fun(.5, mean_values$n_count, 1.96)

#Step 4: Plot 
BFAge = ggplot(mean_values, aes(x = age_group, y = mean_B2ft, fill = female)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean_B2ft - mov, ymax = mean_B2ft + mov), 
                width = 0.2, position = position_dodge(0.9), color = "red") +
  scale_fill_manual(values = c("darkorange2", "mediumpurple4"), labels=c("Female", "Male")) +
  labs(title = "Bigfoot Belief by Age Group & Sex",
       x = "Age Group",
       y = "% Believes Bigfoot is a Real Animal Species") +
  # theme_minimal() + 
  theme(legend.position = c(0.87, 0.85), legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size = 18), legend.background = element_rect(fill='transparent'), panel.grid.major.x = element_blank(), legend.box.background = element_rect("white"), panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "lightgray"))

BFAge

# save BFAge ----------------------------------------------------------
saveRDS(BFAge, file = "BFAge.rds")


# Bigfoot Belief and Race -------------------------------------------------------

moe_fun <- function(p, n, cv) {
  step1<-p*(1-p)
  step2<-step1/n
  se<-(step2)^.5
  moe<-cv*se
  print(moe)
}

moe_fun(.5, 201, 1.96)  

#Step 1: Save the mean value you want to graph by the demographic variable
mean_values <- omni_pass %>% 
  filter(!is.na(race)) %>%
  filter(race != "Pacific Islander") %>%
  group_by(race) %>%
  summarise(mean_B2ft = mean(B2ft, na.rm = TRUE),
            n_count = n())

#Step 2: Filter out data you do not want to graph  
mean_values <- mean_values %>% 
  filter(race!="NA")

#Step 3: Get the MOE 
mean_values$mov<-moe_fun(.5, mean_values$n_count, 1.96)

#Step 4: Plot 
BFRace = ggplot(mean_values, aes(x = race, y = mean_B2ft)) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkgreen") +
  geom_errorbar(aes(ymin = mean_B2ft - mov, ymax = mean_B2ft + mov), 
                width = 0.2, position = position_dodge(0.9), color = "red") +
  labs(title = "Bigfoot Belief by Race",
       x = "Race",
       y = "% Believes Bigfoot is a Real Animal Species") +
  theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "lightgray"), legend.position = "none", legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size = 18), plot.subtitle = element_text(hjust = 0.5, size = 13), panel.grid.major.x = element_blank())

BFRace

# save BFRace ----------------------------------------------------------
saveRDS(BFRace, file = "BFRace.rds")
