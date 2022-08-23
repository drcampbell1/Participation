# In this lab, we're going to follow-up and examine the trends and consequences of social capital.
# Of all the forms of participation, this is by far the hardest to measure. So we need
# remember that our findings are limited.

# Let's start by looking at the data:

library(tidyverse)
library(kableExtra)
options(warn = -1)

# Question 1: How trusting are we?

# We're going to start by looking at the variable measuring social trust. This is a question asking how
# trusting we are of other people and is perhaps the most widely used indicator of social capital.

ess %>% filter(!is.na(strust)) %>% count(strust) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  kbl(caption = "Socialise with Friends in European Democracies",
  col.names = c('Social Trust', 'N', '%'), align="lccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# The pattern varies, but it's possible that a lot of the variation comes from countries with particularly low levels of social trust.
# We need to compare the different countries within our sample. Lets take a look:

ess %>% filter(!is.na(strust)) %>% group_by(country) %>%
              count(strust) %>% mutate(percent = round(n/sum(n)*100, digits=1)) %>%
              kbl(caption = "Socialise with Friends in European Democracies",
              col.names = c('Country', 'Socialise', 'N', '%'), align="lcccc") %>%
              kable_classic_2(full_width=F, position= "left")%>%
              footnote(general = "Source: ESS 2002-2018")

# And we can graph this to make it clear

ess %>% group_by(country) %>% filter(!is.na(strust)) %>%
  count(strust) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!strust=="low" & !strust == "medium") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Social Trust by Country", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Has anything changed over time?

ess %>% group_by(year, country) %>% filter(!is.na(strust)) %>%
  count(strust) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!strust=="low" & !strust == "medium") %>%
  ggplot(aes(year, prop))+
  geom_line()+
  facet_wrap(~country, nrow = 3)+
  labs(x="", y="", title="Figure 2: Social Trust Over Time", caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  scale_x_continuous(breaks = c(2002, 2006, 2010, 2014, 2018))


# Do the figures differ by age category?

ess %>% group_by(agecat) %>% filter(!is.na(strust), !is.na(agecat)) %>%
  count(strust) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!strust=="low" & !strust == "medium") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 3: Social Trust by Age Category", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)


# Do the figures differ by Education

ess %>% group_by(educat) %>% filter(!is.na(strust), !is.na(educat)) %>%
  count(strust) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!strust=="low" & !strust == "medium") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Social Trust and Education", caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# And Gender
ess %>% group_by(gender) %>% filter(!is.na(strust), !is.na(gender)) %>%
  count(strust) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!strust=="low" & !strust == "medium") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 5: Social Trust and Gender", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Section 2: Does this influence participation?

# Although we have established the people are relatively social, one might ask: does it matter?

# Does the propensity to be socially active spur participation? And does it vary across countries?

# Voting

ess %>% filter(!is.na(strust), !is.na(vote)) %>%
  group_by(strust) %>% count(vote) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote == "did not vote") %>%
  kbl(caption = "Social Trust and Electoral Turnout",
  col.names = c('Socialising', 'Voting', 'N', 'Percentage'),
  digits=2, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

ess %>% filter(!is.na(strust), !is.na(vote)) %>%
  group_by(strust, country) %>% count(vote) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote == "did not vote") %>%
  ggplot(aes(x=strust, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 6: Voting by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_wrap(~country, nrow= 3)

# Contacting

ess %>% filter(!is.na(strust), !is.na(contact)) %>%
  group_by(strust) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "not contacted") %>%
  kbl(caption = "Socialising and Contacting Politicians",
  col.names = c('Socialising', 'Contacting', 'N', 'Percentage'),
  digits=2, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")


# Contacting across countries

ess %>% filter(!is.na(strust), !is.na(contact)) %>%
  group_by(strust, country) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "not contacted") %>%
  ggplot(aes(x=strust, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 7: Contacting by Socialising and Country", caption="Source: ESS 2002-2018")+
  theme_bw()+
  facet_wrap(~country, nrow = 3)

# Petition

ess %>% filter(!is.na(strust), !is.na(petit)) %>%
  group_by(strust) %>% count(petit) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petit == "not signed") %>%
  kbl(caption = "Socialising and Petition",
  col.names = c('Socialising', 'Petition', 'N', 'Percentage'),
  digits=2, align="cccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Petitioning across countries by socialising

ess %>% filter(!is.na(strust), !is.na(petit)) %>%
  group_by(strust, country) %>% count(petit) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petit == "not signed") %>%
  ggplot(aes(x=strust, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 8: Petitioning by Socialising and Country", caption="Source: ESS 2002-2018")+
  theme_bw()+
  facet_wrap(~country, nrow = 3)


# Demonstrating

ess %>% filter(!is.na(strust), !is.na(demo)) %>%
  group_by(strust) %>% count(demo) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!demo == "not demonstrated") %>%
  kbl(caption = "Socialising and Demonstrating",
  col.names = c('Socialising', 'Demonstrating', 'N', 'Percentage'),
  digits=2, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Demonstrating Across Countries

ess %>% filter(!is.na(strust), !is.na(demo)) %>%
  group_by(strust, country) %>% count(demo) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!demo == "not demonstrated") %>%
  ggplot(aes(x=strust, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 9: Demonstrating by Social Trust and Country", caption="Source: ESS 2002-2018")+
  theme_bw()+
  facet_wrap(~country, nrow = 3)

# So, time to draw some conclusions.
# Does social capital matter?

