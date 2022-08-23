# As we've discussed in the lectures, there are other forms of protest politics.
# In this lecture, we're going to focus on petiting and boycotting.
# These are important because people may show their support for a group or cause by signing a public
# petit supporting it, or they may boycott products for political or ethical reasons.

library(tidyverse)
library(kableExtra)
options(warn = -1)

# Question 1: How common is petiting in European democracies?

ess %>% filter(!is.na(petit)) %>% count(petit) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl(caption = "petiting in European Democracies",
               col.names = c('Petitioning', 'N', '%'), align="lcc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Let's break this down by the six countries for which we have data:

ess %>% filter(!is.na(petit)) %>% group_by(country) %>% count(petit) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl(caption = "petiting in European Democracies",
               col.names = c('Country', 'petiting', 'N', '%'), align="lccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# We can graph this to make it clear

ess %>% group_by(country) %>% filter(!is.na(petit)) %>%
  count(petit) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petit=="not signed") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: petiting by Country", caption="Source: ESS 2002-2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()

# Question 2: How common is boycotting in European democracies?

ess %>% filter(!is.na(boyct)) %>% count(boyct) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl(caption = "Boycotting in European Democracies",
               col.names = c('Boycotting', 'N', '%'), align="lcc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Let's break this down by the six countries for which we have data:

ess %>% filter(!is.na(boyct)) %>% group_by(country) %>% count(boyct) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl("pandoc", caption = "Boycotting in European Democracies",
               col.names = c('Country', 'Boycotting', 'N', '%'), align="lccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# We can graph this to make it clear

ess %>% group_by(country) %>% filter(!is.na(boyct)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="not boycotted") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 2: Boycotting by Country", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()


# Section 2: Who takes part in these forms of participation?

# What is the age profile of petitioners?

ess %>% group_by(agecat) %>% filter(!is.na(petit), !is.na(agecat)) %>%
  count(petit) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petit=="not signed") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 3: petiting by Age Category", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
 
# What is the age profile of people who boycott?

ess %>% group_by(agecat) %>% filter(!is.na(boyct), !is.na(agecat)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="not boycotted") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Boycotting by Age Category", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()


# What is the education profile of petitioners?

ess %>% group_by(educat) %>% filter(!is.na(petit), !is.na(educat)) %>%
  count(petit) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petit=="not signed") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 5: Petitioning by Education", caption="Source: ESS 2002-2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
 
# What is the education profile of people who boycott?

ess %>% group_by(educat) %>% filter(!is.na(boyct), !is.na(educat)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="not boycotted") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 6: Boycotting by Education", caption="Source: ESS 2002-2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
  
# What is the gender profile of petiters?

# Males only?

ess %>% group_by(gender) %>% filter(!is.na(petit), !is.na(gender)) %>%
  count(petit) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petit=="not signed") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 7: petiting by Gender", caption="ess 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()

ess %>% group_by(gender) %>% filter(!is.na(boyct), !is.na(gender)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="not boycotted") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 8: Boycotting by Gender", caption="Source: ESS 2002-2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()

# What, then, might you conclude about protest politics on the basis of what we've looked at here?

# Do very few people take part?

# Are they concentrated in one age group?

# Are they better educated?

# Is there gender imbalance?
