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

ess <- foreign::read.dta("data/ess.dta", convert.factors=TRUE)
library(tidyverse)
library(tidytext)
options(warn = -1)

#Introduction to the data#

#In the labs we'll be using the ESS. This is the European Social Survey -
#a big cross-sectional survey conducted in 30 countries in Europe.


#Coverage
# To make the file more manageable, I've cut it down quite a bit.

#Basic Terminology#

                  #1. Dependent variable: something we are looking to explain, e.g. one (or more) forms of participation:

                  #2. Independent variable : something that influences our dependent variable (age, for example, may influence voting).

                  #3. Observation: someone we ask about their participation and age.

# Overall, then, this file contains :

                      # 8 different forms of participation;

                        # 8 different variables that may influence participation (or 9 if we include country)

                        # In total we have 78473 observations - or 78473 people gave us answers to our questions.

ess %>% count(country, year)

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

          ess %>% filter(!is.na(vote)) %>% count(vote)

#Percentages are easier, so let's add them in:

          ess %>% filter(!is.na(vote)) %>% count(vote) %>% mutate('%' = round(n/sum(n)*100, digits=1))

#Let's look at contacting politicians

          ess %>% filter(!is.na(contact)) %>% count(contact) %>% mutate('%' = round(n/sum(n)*100, digits=1))

#Lets look at demonstrating

          ess %>% filter(!is.na(demo)) %>% count(demo) %>% mutate('%' = round(n/sum(n)*100, digits=1))

# Let's look at voting for the radical right

          ess %>% filter(!is.na(right)) %>% count(right) %>% mutate('%' = round(n/sum(n)*100, digits=1))

# What if we compared a lot of forms across countries:

ess %>% 
  drop_na() %>% 
  pivot_longer((vote:petit), 
               names_to = "mode", 
               values_to = "value") %>% 
  group_by(country, mode) %>% 
  count(value) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  filter(value == str_remove(value, "not")) %>% 
  mutate(value=str_replace(value, "have", ""), 
         value = reorder_within(value, percent, country)) %>% 
  ggplot(aes(reorder(value, percent), percent))+
  geom_col(fill= "steelblue")+
  facet_wrap(~country, scales = "free")+
  coord_flip()+
  theme_minimal()+
  scale_x_reordered()+
  labs(x = "", y = "%", 
       title = "Figure 1:Comparing Political Participation in European Democracies",
       subtitle = "(Political Styles that Differ)",
       caption = "Source: ESS 2002-2018")



