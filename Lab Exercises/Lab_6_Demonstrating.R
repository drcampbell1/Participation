# In the lecture I argued that demonstrating has expanded and diversified as a form of participation.
# Whilst it was once relatively uncommon, it has now grown in frequency.
# Whilst it was once the preserve of younger and more ideologically extreme males, it is now
# used by a wide demographic.

# This will become clear as we look at the data.

library(tidyverse)
library(kableExtra)
options(warn = -1)

# Question 1: How common is demonstrating in European democracies?

ess %>% filter(!is.na(demo)) %>% count(demo) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl(caption = "Demonstrating in European Democracies",
  col.names = c('Demonstrating', 'N', '%'), align="lcc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")


# Let's break this down by the six countries for which we have data:

ess %>% filter(!is.na(demo)) %>% group_by(country) %>% count(demo) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl(caption = "Demonstrating in European Democracies",
  col.names = c('Country', 'Demonstrating', 'N', '%'), align="lccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# We can graph this to make it clear

ess %>% group_by(country) %>% filter(!is.na(demo)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="not demonstrated") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Demonstrating by Country", caption="Source: ESS 2002-2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What is the age pattern of the demonstrators?

ess %>% group_by(agecat) %>% filter(!is.na(demo), !is.na(agecat)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="not demonstrated") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 2: Demonstrating by Age Category", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What is the education profile of demonstrators?

ess %>% group_by(educat) %>% filter(!is.na(demo), !is.na(educat)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="not demonstrated") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 3: Demonstrating by Education", caption="ess 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Males only?

ess %>% group_by(gender) %>% filter(!is.na(demo), !is.na(gender)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="not demonstrated") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Demonstrating by Gender", caption="ess 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ess %>% group_by(unemp) %>% filter(!is.na(demo), !is.na(unemp)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="not demonstrated") %>%
  ggplot(aes(x=unemp, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 5: Demonstrating by Unemployed", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What does all of the above tell us about demonstrators?


# Question 2: Do demonstrators participate?

ess %>% filter(!is.na(demo), !is.na(vote)) %>%
  group_by(demo) %>% count(vote) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote == "did not vote") %>%
  kbl(caption = "Demonstrating and Voting",
  col.names = c('Demonstrating', 'Voting', 'N', 'Percentage'),
  digits=2, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

ess %>% filter(!is.na(demo), !is.na(vote)) %>%
  group_by(demo, country) %>% count(vote) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote == "did not vote") %>%
  ggplot(aes(x=demo, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 6: Demonstrating and Voting by Country", caption="Source: ESS 2002-2018")+
  theme_bw()+
  facet_wrap(~country, nrow = 3)


 # Petitioning 

ess %>% filter(!is.na(demo), !is.na(petit)) %>%
  group_by(demo) %>% count(petit) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petit == "not signed") %>%
  kbl(caption = "Demonstrating and Petition",
  col.names = c('Demonstrating', 'Petition', 'N', 'Percentage'),
  digits=2, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Petitioning across countries by demonstrating

ess %>% filter(!is.na(demo), !is.na(petit)) %>%
  group_by(demo, country) %>% count(petit) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petition == "not signed") %>%
  ggplot(aes(x=demo, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 7: Petitioning by Socialising and Country", caption="ess 2016")+
  theme_bw()+
  facet_wrap(~country, nrow = 3)
  

# Contacting by Demonstrating

ess %>% filter(!is.na(demo), !is.na(contact)) %>%
  group_by(demo) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "have not contacted") %>%
  kbl(caption = "Contacting and Petition",
  col.names = c('Demonstrating', 'Contacting', 'N', 'Percentage'),
  digits=2, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Contacting across countries by demonstrating

ess %>% filter(!is.na(demo), !is.na(contact)) %>%
  group_by(demo, country) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "not contacted") %>%
  ggplot(aes(x=demo, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 9: Contacting by Socialising and Country", caption="Source: ESS 2002-2016")+
  theme_bw()+
  facet_wrap(~country, nrow = 3)


# So what can we conclude overall about demonstrators? Does it look like it is a small group of people who demonstrate
# or a more homogenous section of the population? 
