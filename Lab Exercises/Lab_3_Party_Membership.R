# In the lecture, we found a particularly challenging picture of political parties. We found:

                  # (1) weakening attachments

                  # (2) falling (and concentrating) membership; and

                  # (3) Negative public images.

# In this lab, we're going to look at members of political parties,
# examining their socio-economic background to see if there is inequality.

library(tidyverse)
library(kableExtra)
options(warn = -1)

ess_mem <- foreign::read.dta("data/ess_mem.dta", convert.factors=TRUE)

# Where are the party members?

ess_mem %>% 
  filter(!is.na(prtymbr)) %>% 
  group_by(country) %>% 
  count(prtymbr) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  filter(prtymbr == "member") %>% 
  ggplot(aes(reorder(country, percent), percent))+
  labs(x = "", 
       y = "%",
       caption = "Source: European Social Survey, 2002, 2006 and 2010")+
  geom_text(aes(label = round(percent, digits=1)), vjust = -0.2)+
  geom_col(fill = "steelblue")+
  theme_light()

# Are the party members representative of the general public?

a <- ess_mem %>% 
  group_by(country) %>% 
  summarise(mean= mean(age, na.rm=T))

b <- ess_mem %>%
  group_by(country, prtymbr) %>% 
  summarise(avg_age_pp= mean(age, na.rm=T)) %>% 
  filter(!is.na(prtymbr), !prtymbr=="not member")

left_join(a, b, by = "country") %>% 
  select(-prtymbr) %>%
  kbl(caption = "Table 1: Age and Party Membership in European Democracies",
      col.names = c('Country', 
                    'Average Age Population', 
                    'Average Age Member Political Party'), 
      digits =1, 
      align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: European Social Survey 2002, 2006, 2010")

# Is there gender balance amongst party members?#

c <- ess_mem %>% 
  group_by(country) %>%
  filter(!is.na(gender))%>%
  count(gender) %>%
  mutate(perc_pop=n/sum(n)*100)

d <- ess_mem %>% 
  filter(prtymbr=="member" & !is.na(gender)) %>% 
  group_by(country, gender) %>%
  count(prtymbr) %>% 
  group_by(country) %>% 
  mutate(perc_wk_pp=n/sum(n)*100) %>%
  select(-prtymbr)

left_join(c, d, by=c("country", "gender")) %>% 
  select(country, gender, perc_pop, perc_wk_pp)%>%
  kbl(caption = "Table 2: Gender Balance and Party Membership in European Democracies",
      col.names = c('Country', 'Gender', 'Population', 'Party Members'),
      digits =1, align="cccc") %>%
  kable_classic_2(full_width=F, position= "left")%>%
  footnote(general = "Source: ESS 2002-2018")

# Are those with more income over represented amongst party members?

e <- ess_mem %>% 
  filter(!is.na(quin)) %>% 
  count(quin) %>% 
  mutate(perc_pop=n/sum(n)*100) %>% 
  select(-n)

f <- ess_mem %>% 
  filter(prtymbr=="member" & !is.na(quin)) %>% 
  group_by(quin) %>%
  count(prtymbr) %>%
  ungroup() %>% 
  mutate(perc_mbr_pp=n/sum(n)*100) %>%
  select(-c(prtymbr, n))


left_join(e, f, by="quin") %>% 
  kbl(caption = "Table 3: Income and Party Membership in European Democracies",
      col.names = c('Income Quintile', 'Population', 'Party Members'),
      digits =1, align="ccc") %>%
  kable_classic_2(full_width=F, 
                  position= "left")%>%
  footnote(general = "Source: European Social Survey 2002, 2006 and 2010")


# Are the more educated more likely to be party members?

g <- ess_mem %>% 
  filter(!is.na(educat)) %>% 
  count(educat) %>% 
  mutate(perc_pop=n/sum(n)*100) %>% 
  select(-n)


h <- ess_mem %>% 
  filter(prtymbr=="member" & !is.na(educat)) %>% 
  group_by(educat) %>%
  count(prtymbr) %>%
  ungroup() %>% 
  mutate(perc_mbr_pp=n/sum(n)*100) %>%
  select(-c(prtymbr, n))

left_join(g, h, by="educat") %>% 
  kbl(caption = "Table 4: Education and Party Membership in European Democracies",
      col.names = c('Education', 'Population', 'Party Members'),
      digits =1, align="ccc") %>%
  kable_classic_2(full_width=F, 
                  position= "left")%>%
  footnote(general = "Source: European Social Survey 2002, 2006 and 2010")


# What about the motivation of party members?


i <- ess_mem %>% 
  filter(!is.na(pinterest)) %>% 
  count(pinterest) %>% 
  mutate(perc_pop=n/sum(n)*100) %>% 
  select(-n)

j <- ess_mem %>% 
  filter(prtymbr=="member" & !is.na(pinterest)) %>% 
  group_by(pinterest) %>%
  count(prtymbr) %>%
  ungroup() %>% 
  mutate(perc_mbr_pp=n/sum(n)*100) %>%
  select(-c(prtymbr, n))


left_join(i, j, by="pinterest") %>% 
  kbl(caption = "Table 5: Political Interest and Party Membership in European Democracies",
      col.names = c('Political Interest', 'Population', 'Party Members'),
      digits =1, align="ccc") %>%
  kable_classic_2(full_width=F, 
                  position= "left")%>%
  footnote(general = "Source: European Social Survey 2002, 2006 and 2010")

# Ideology (0-10 scale, left = 0, right = 10)


ess_mem %>% 
  filter(!is.na(lrscale), !is.na(prtymbr)) %>%
  group_by(prtymbr) %>% 
  summarise(mean = mean(lrscale)) %>% 
  ggplot(aes(mean, prtymbr, colour = prtymbr))+
  geom_point(size = 2.5)+
  labs(x = "Mean",
       y = "",
       caption = "Source: European Social Survey, 2002, 2006 and 2010")+
  theme_light()+
  theme(legend.title = element_blank())+
  scale_color_manual(values = c("darkred", "steelblue"),
                     guide = guide_legend(reverse = T))+
  scale_x_continuous(limits = c(0,10), 
                     breaks = c(0,2,4,6,8,10),
                     labels = c("left", "2", "4", "6", "8", "right"))


# Do party members participate in other ways more than non-members?

ess_mem %>% 
  select(vote, contplt, badge, pbldmn, bctprd, prtymbr) %>% 
  pivot_longer(cols = vote:bctprd, 
               names_to = "participation", 
               values_to= "value") %>% 
  filter(!is.na(value), !is.na(prtymbr)) %>% 
  group_by(prtymbr, participation) %>% 
  count(value) %>% 
  mutate(pct = n /sum(n)*100) %>% 
  filter(!str_detect(value, "not")) %>% 
  ggplot(aes(value, pct, fill = prtymbr))+
  geom_col(position = "dodge")+
  labs(x = "",
       y = "%",
       caption = "Source: European Social Survey, 2002, 2006 and 2010")+
  theme_light()+
  scale_fill_manual(values = c("darkred", "steelblue"))+
  theme(legend.title = element_blank())


rm(a,b,c,d, e,f, g, h, i, j)

# What can we conclude about people who work for parties? Are they a model of equality and diversity?
