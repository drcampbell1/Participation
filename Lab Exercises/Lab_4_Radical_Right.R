# In the lecture, we introduced theories about who votes for the radical right

# In this lab, we're going to test some aspects of those theories with evidence from the ESS

library(tidyverse)
library(kableExtra)
options(warn = -1)

ess %>% filter(!is.na(right)) %>% count(right) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  kbl(caption = "Voted for the Radical Right in European Democracies",
               col.names = c('Voted Radical Right', 'N', '%'), align="lccc")%>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

#How does this vary by country?#

ess %>% group_by(country) %>% filter(!is.na(right)) %>%
     count(right) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>% filter(!right =="did not vote") %>% 
     kbl(caption = "Comparing Voting for Radical Right in European Democracies",
     col.names = c('Country', 'Voted Radical Right', 'N', '%'), align="cccc")%>%
     kable_classic_2(full_width=F, position= "left")%>%
     footnote(general = "Source: ESS 2002-2018")


# Let's graph this to make the findings stand out a bit better

ess %>% group_by(country) %>% filter(!is.na(right)) %>%
  count(right) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=reorder(country, -prop), y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 1: Voted for Radical Right Party by Country", caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()


# What is the age profile of voters for the radical right?

a <- ess %>% group_by(country) %>% 
             summarise(mean= mean(age, na.rm=T))
b <- ess %>% group_by(country, right) %>% 
             summarise(avg_age_vt_rr= mean(age, na.rm=T)) %>% 
             filter(!is.na(right), !right=="did not vote")

left_join(a, b, by = "country") %>% select(-right) %>%
          kbl(caption = "Average Age of Population and Voters of Radical Right Parties in European Democracies",
          col.names = c('Country', 
                        'Average Age Population', 
                        'Average Age Voter Radical Right Party'), 
              digits =1, align="cccc")%>%
          kable_classic_2(full_width=F, position= "left")%>%
          footnote(general = "Source: ESS 2002-2018")

# What does this tell us about the age profile of voters for radical right parties?



# Let's look at the gender profile of radical right voters


c <- ess %>%
  group_by(country) %>%
  filter(!is.na(gender))%>%
  count(gender) %>%
  mutate(perc_pop=n/sum(n)*100)


d <- ess %>%
  filter(!is.na(right)) %>%
  group_by(country, gender) %>% 
  filter(!right=="did not vote") %>% count(right) %>% 
  group_by(country) %>% 
  mutate(perc_vt_pp=n/sum(n)*100) %>% 
  select(-right)

left_join(c, d, by=c("country", "gender")) %>% select(-n.x, -n.y) %>%
  kbl(caption = "Gender Balance in Population and Voters of Radical Right Parties in European Democracies",
               col.names = c('Country', 'Gender', 'Percentage Population', 'Percentage Voter of Radical Right Political Party'),
               digits =1, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

rm(a,b,c,d)


#Let's look at Education

ess %>% group_by(educat) %>% filter(!is.na(right), !is.na(educat)) %>%
  count(right) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 2: Voting for the Radical Right by Education", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
 

ess %>% group_by(educat, country) %>% 
filter(!is.na(right), !is.na(educat)) %>%
  count(right) %>% 
  mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_col()+
  facet_wrap(~country, nrow=3)+
  labs(x="", y="", title="Figure 3: Voting for the Radical Right by Education and Country", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
 
#Let's look at what voters of the radical right think about government?

#Do voters of the radical right trust government?

ess %>% group_by(ptrust) %>% 
  filter(!is.na(right), !is.na(ptrust)) %>%
  count(right) %>% 
  mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=ptrust, y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 4: Political Trust and Voting for the Radical Right", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
  
ess %>% group_by(ptrust, country) %>% 
  filter(!is.na(right), !is.na(ptrust)) %>%
  count(right) %>% 
  mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=ptrust, y=prop)) +
  geom_col()+
  facet_wrap(~country, nrow=3)+
  labs(x="", y="", title="Figure 5: Political Trust and Voting for the Radical Right", caption="Source: ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()
  

# Attitudes to Immigrants of a Different Ethnicity

ess %>% group_by(imdfetn) %>% filter(!is.na(right), !is.na(imdfetn)) %>%
  count(right) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=imdfetn, y=prop)) +
  geom_col()+
  labs(x="", y="", 
       title="Figure 6: Attitudes to Immigration and Voting for the Radical Right",
       subtitle = "(different ethnicity)",
       caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = c(1,2,3,4), labels=c("s_disagree", "disagree", "agree", "s_agree"))+
  theme_bw()
  
ess %>% group_by(imdfetn, country) %>% filter(!is.na(right), !is.na(imdfetn)) %>%
  count(right) %>% 
  mutate(prop=prop.table(n*100)) %>%
  filter(!right=="did not vote") %>%
  ggplot(aes(x=imdfetn, y=prop)) +
  geom_col()+
  facet_wrap(~country, nrow=3)+
  labs(x="", y="", title="Figure 7: Attitudes to Immigration and Voting for the Radical Right", caption="ESS 2002-2018")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = c(1,2,3,4), labels=c("s_disagree", "disagree", "agree", "s_agree"))+
  theme_bw()
  

rm(a,b,c,d)

# What can we conclude about voters for the radical right?
