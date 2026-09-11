#Introduction to the labs#

# I mentioned that the study of participation involves looking at evidence.
# Most of the questions are about: levels; styles; and factors that influence participation
# In these labs, we"ll look at the evidence.


#To do that, we need data and software to analyse it. These labs use RStudio to analyse the data.
# This is a programming-based environment, but I've written all of the code for you.
#All you need to do is run it.

#Before we look at the data, we need to do a few things to set up the lab project:

install.packages("tidyverse")
install.packages("tidytext")
install.packages("kableExtra")
install.packages("shiny")
install.packages("bslib")

ess <- foreign::read.dta("data/ess.dta", convert.factors=TRUE)
ess_minor <- foreign::read.dta("data/ess_minor.dta", convert.factors=TRUE)
library(tidyverse)
library(tidytext)
options(warn = -1)

#Introduction to the data#

#In the labs we'll be using the European Social Survey (ESS) -
#a big cross-sectional survey conducted in 30 countries in Europe.


#Coverage
# To make the file more manageable, I've cut it down quite a bit.

#Basic Terminology#

                  #1. Dependent variable: something we are looking to explain, e.g. one (or more) forms of participation:

                  #2. Independent variable : something that influences our dependent variable (age, for example, may influence voting).

                  #3. Observation: someone we ask about their participation and age.

# Overall, then, this file contains :

                      # 8 different forms of participation;

                        # 8 different variables that may influence participation (9 if we include country)

                        # In total we have 78473 observations - or 78473 people gave us answers to our questions.


# We also have time which enables us to compare participation is changing. We have data from five different time points covering a 16 year period: 

                      # 2002;

                      # 2006;

                      # 2010;

                      # 2014; and

                      # 2018.

# Let's look at the data file and see what forms of participation it contains#

View(ess)
#We have:

          #1. Voting;

          #2. Contacting politicians;

          #3 working for a political party;

          #4 wearing political paraphenalia;

          #5. signing a petition;

          #6. demonstrating;

          #7. boycotting;

          #8 voting for the radical right


#And we also have variables that might influence these forms of participation, including:

                #1. Age; 2. Education; 3. gender; 4. unemployed. These are *structural* characteristics.

                # And we can also look at the attitudes people hold shapes their participation. We have attitudes on:

                # Economic satisfaction; political interest; trust in politics; trust in European parliament.


#Let's finish today by looking at some forms of participation:

#How many people said they voted at the last national election?


ess %>% 
filter(!is.na(vote)) %>% 
count(vote) %>% 
mutate('%' = round(n/sum(n)*100, digits=1))

#Let's look at contacting politicians

ess %>% 
filter(!is.na(contact)) %>% 
count(contact) %>% 
mutate('%' = round(n/sum(n)*100, digits=1))

#Lets look at demonstrating

ess %>% 
filter(!is.na(demo)) %>% 
count(demo) %>% 
mutate('%' = round(n/sum(n)*100, digits=1))

# Let's look at voting for the radical right

ess %>% 
filter(!is.na(right)) %>% 
count(right) %>% 
mutate('%' = round(n/sum(n)*100, digits=1))

# What's the general picture across the countries?

# Let's take the average and graph it for each country

ess_minor %>% 
  group_by(country) %>% 
  summarise(mean = mean(total)) %>% 
  ggplot(aes(reorder(country, -mean), mean))+
  labs(x = "", 
       y = "mean",
       title = "Average Participation in Europe",
       subtitle = "(maximum of 7)",
       caption = "Source: European Social Survey")+
  geom_col(fill = "steelblue")+
  theme_light()

# Let's take the average and graph it for each country

ess_minor %>% 
  group_by(country) %>% 
  summarise(mean = mean(total)) %>% 
  ggplot(aes(reorder(country, -mean), mean))+
  labs(x = "", 
       y = "mean",
       title = "Average Participation in Europe",
       subtitle = "(maximum of 7)",
       caption = "Source: European Social Survey")+
  geom_col(fill = "steelblue")+
  theme_light()

# Let's take a more detailed look: What percentage of respondents participate

ess_minor %>% 
  group_by(country) %>% 
  count(total) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  ggplot(aes(total, perc))+
  geom_col(fill = "steelblue")+
  labs(x = "", 
       y = "%",
       title = "Participation in Europe",
       subtitle = "(maximum of 7)",
       caption = "Source: European Social Survey")+
  facet_wrap(~country)+
  theme_light()+
  scale_x_continuous(breaks = seq(0,7, by=1))+
  scale_y_continuous(breaks = seq(0,60, by=10))

# What about the inactives: those who do not participate?

ess_minor %>% 
  group_by(country) %>% 
  count(total) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  filter(total ==0) %>% 
  ggplot(aes(reorder(country, -perc), perc))+
  labs(x = "", 
       y = "%",
       title = "Which country has the most inactives?",
       caption = "Source: European Social Survey")+
  geom_col(fill = "steelblue")+
  theme_light()

# What about those who only vote?

a <- ess_minor %>% 
  group_by(country) %>% 
  count() %>% rename("pop" = "n")

b <- ess_minor %>% 
  group_by(country) %>% 
  filter(vote ==1 & total == 1) %>% 
  count(total)

left_join(a,b, by = "country") %>% 
  group_by(country) %>% 
  mutate(perc = n/pop*100) %>% 
  ggplot(aes(reorder(country, -perc), perc))+
  labs(x = "", 
       y = "%",
       title = "Which country has the highest percentage that vote only?",
       caption = "Source: European Social Survey")+
  geom_col(fill = "steelblue")+
  theme_light()

rm(a,b)



