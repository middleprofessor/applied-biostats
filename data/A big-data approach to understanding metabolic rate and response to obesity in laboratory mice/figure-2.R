library(tidyverse)
library(cowplot)
library(janitor)
library(broom)
library(modelr)
library(viridis)

mmpc_sites <- read.csv("MMPC_sites.csv")
mmpc_12wk <- read.csv("MMPC_12WK_overall_avg.csv") %>% filter(acclimation==TRUE) %>% 
  inner_join(mmpc_sites, by = c("Subject.ID"="subject.id")) %>% rename(Site=institution) %>% distinct() %>%
  mutate(Group=factor(Group, levels= c("LFD", "HFD")), Site=factor(Site, levels= c("UC Davis", "UMass", "Yale","Vanderbilt")),
         Energy.Expenditure = Energy.Expenditure * 4.184,
         Hourly.Food.Intake = Hourly.Food.Intake * 4.184,
         Energy.Balance = Energy.Balance * 4.184)
MMPC_colors<-c("#CC79A7","#E69F00","#009E73","#56B4E9")
#2A
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  ggplot(., aes(y=Energy.Expenditure, x=Total.Mass, fill=Site, linetype=Group, shape=Group)) +
  geom_point(colour="black",size=2)+stat_smooth(method = "lm", se=F, color="black", size = 1) +
  theme_bw() +  ylab("energy expenditure (kJ/hr)") + scale_shape_manual(values = c(21,24)) +
  xlab("body mass (g)") +scale_fill_manual(values=MMPC_colors) + facet_wrap(~ Site, ncol = 4)+theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)"))
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  group_by(Site)%>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Energy.Expenditure ~ Total.Mass + Group, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(glanced)

mmpc_df <- mmpc_12wk %>% clean_names() %>% filter(time_of_day == "Full day") %>%
  group_by(site) %>% mutate(loco_max = max(locomotor_activity)) %>% ungroup() %>% 
  mutate(loco_pct = locomotor_activity/loco_max)
mmpc_lf <- mmpc_df %>% filter(group=="LFD")
mmpc_hf <- mmpc_df %>% filter(group=="HFD")
mmpc_ee <- lm(energy_expenditure ~ site + total_mass + loco_pct, data = mmpc_lf)
mmpc_ee_res <- mmpc_hf %>% modelr::add_residuals(model=mmpc_ee) %>% 
  group_by(site) %>% summarize_if(is.numeric, mean, na.rm=T)
#2B
ggplot(mmpc_ee_res, aes(y=resid, x=total_mass, color=institution)) +
  geom_hline(yintercept=0, linetype=3, color="red") +
  geom_point(pch=24, color = "black",size=5, aes(fill=site))+
  scale_fill_manual(values=MMPC_colors)+
  ylab("residual of\nenergy expenditure (kJ/hr)") + xlab("body mass (g)")+ 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = c(30,32.5,35.0,37.5,40), limits = c(29.5,40)) +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "residual of\nenergy expenditure (kcal/hr)"))
#2C
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  ggplot(., aes(y=Hourly.Food.Intake, x=Total.Mass, fill=Site, linetype=Group, shape=Group)) +
  geom_point(colour="black",size=2)+stat_smooth(method = "lm", se=F, color="black", size = 1) +
  theme_bw() +  ylab("energy intake (kJ/hr)") + scale_shape_manual(values = c(21,24)) +
  xlab("body mass (g)") +scale_fill_manual(values=MMPC_colors) + facet_wrap(~ Site, ncol = 4)+theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy intake (kcal/hr)"))
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>%
  group_by(Site)%>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Hourly.Food.Intake ~ Total.Mass + Group, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)

mmpc_ei <- lm(hourly_food_intake ~ site + total_mass + loco_pct, data = mmpc_lf)
mmpc_ei_res <- mmpc_hf %>% add_residuals(model=mmpc_ei) %>% 
  group_by(site) %>% summarize_if(is.numeric, mean, na.rm=T)
#2D
ggplot(mmpc_ei_res, aes(y=resid, x=total_mass, color=institution)) +
  geom_hline(yintercept=0, linetype=3, color="red") +
  geom_point(pch=24, color = "black",size=5, aes(fill=site))+
  scale_fill_manual(values=MMPC_colors)+
  ylab("residual of\nenergy intake (kJ/hr)") + xlab("body mass (g)")+ 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = c(30,32.5,35.0,37.5,40), limits = c(29.5,40))  +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "residual of\nenergy intake (kcal/hr)"))
#2E
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  ggplot(., aes(y=Energy.Balance, x=Total.Mass, fill=Site, linetype=Group, shape=Group)) +
  geom_point(colour="black",size=2)+stat_smooth(method = "lm", se=F, color="black", size = 1) +
  theme_bw() +  ylab("energy balance (kJ/hr)") + scale_shape_manual(values = c(21,24)) +
  xlab("body mass (g)") +scale_fill_manual(values=MMPC_colors) + facet_wrap(~ Site, ncol = 4)+theme(legend.position = "none")+
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy balance (kcal/hr)"))
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>%
  group_by(Site)%>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Energy.Balance ~ Total.Mass + Group, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)

mmpc_eb <- lm(energy_balance ~ site + total_mass + loco_pct, data = mmpc_lf)
mmpc_eb_res <- mmpc_hf %>% modelr::add_residuals(model=mmpc_eb) %>% 
  group_by(site) %>% summarize_if(is.numeric, mean, na.rm=T)
#2F
ggplot(mmpc_eb_res, aes(y=resid, x=total_mass, color=institution)) +
  geom_hline(yintercept=0, linetype=3, color="red") +
  geom_point(pch=24, color = "black",size=5, aes(fill=site))+
  scale_fill_manual(values=MMPC_colors)+
  ylab("residual of\nenergy balance (kJ/hr)") + xlab("body mass (g)")+ 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = c(30,32.5,35.0,37.5,40), limits = c(29.5,40)) +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "residual of\nenergy balance (kcal/hr)"))

### figure 2 - figure supplement 1
#s1A
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  ggplot(., aes(y=Energy.Expenditure, x=Lean.Mass, fill=Site, linetype=Group, shape=Group)) +
  geom_point(colour="black",size=2)+stat_smooth(method = "lm", se=F, color="black", size = 1) +
  theme_bw() +  ylab("energy expenditure (kJ/hr)") + scale_shape_manual(values = c(21,24)) +
  xlab("lean mass (g)") +scale_fill_manual(values=MMPC_colors) + facet_wrap(~ Site, ncol = 4)+theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)")) + 
  scale_x_continuous(breaks = c(16,18,20,22,24), limits = c(15.5,24.7))
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>%
  group_by(Site)%>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Energy.Expenditure ~ Lean.Mass + Group, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)
#s1B
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  ggplot(., aes(y=Hourly.Food.Intake, x=Lean.Mass, fill=Site, linetype=Group, shape=Group)) +
  geom_point(colour="black",size=2)+stat_smooth(method = "lm", se=F, color="black", size = 1) +
  theme_bw() +  ylab("energy intake (kJ/hr)") + scale_shape_manual(values = c(21,24)) +
  xlab("lean mass (g)") +scale_fill_manual(values=MMPC_colors) + facet_wrap(~ Site, ncol = 4)+theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy intake (kcal/hr)")) + 
  scale_x_continuous(breaks = c(16,18,20,22,24), limits = c(15.5,24.7))
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>%
  group_by(Site)%>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Hourly.Food.Intake ~ Lean.Mass + Group, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)
#s1C
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% 
  ggplot(., aes(y=Energy.Balance, x=Lean.Mass, fill=Site, linetype=Group, shape=Group)) +
  geom_point(colour="black",size=2)+stat_smooth(method = "lm", se=F, color="black", size = 1) +
  theme_bw() +  ylab("energy balance (kJ/hr)") + scale_shape_manual(values = c(21,24)) +
  xlab("lean mass (g)") +scale_fill_manual(values=MMPC_colors) + facet_wrap(~ Site, ncol = 4)+theme(legend.position = "none")+ 
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy balance (kcal/hr)")) + 
  scale_x_continuous(breaks = c(16,18,20,22,24), limits = c(15.5,24.7))
mmpc_12wk %>% filter(Time.of.Day=="Full day") %>%
  group_by(Site)%>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Energy.Balance ~ Lean.Mass + Group, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)

mmpc_fat<-mmpc_12wk %>% filter(Time.of.Day=="Full day") %>% group_by(Site) %>%
  nest(-Site) %>%
  mutate(fit = map(data, ~lm(Energy.Expenditure~Lean.Mass + Fat.Mass + Group, data = .x)),tidied = map(fit, tidy),glanced = map(fit, glance)) %>%
  unnest(tidied) %>% dplyr::select(-c("std.error","statistic","p.value")) %>%
  spread(term, estimate) %>% group_by(Site) %>% mutate(pct_fat = Fat.Mass/(Lean.Mass+Fat.Mass))
#s1D
mmpc_fat %>% ungroup(Site) %>% 
  mutate(Site = factor(Site, levels = c("UC Davis","UMass","Yale","Vanderbilt"))) %>%
  ggplot(., aes(x=Site, y=pct_fat, fill=Site ))+geom_col()+
  xlab("")+ ylab("fat mass\ncontribution to EE")+ scale_fill_manual(values=MMPC_colors)+
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

### figure 2 - figure supplement 2
mmpc_eb <- read_csv("mmpc_lfd_full.csv") %>% clean_names() %>%
  filter(time_of_day == "Full day" & hourly_food_intake > 0.1) %>% 
  select(subject_id,respiratory_exchange_ratio,energy_balance) %>%
  left_join(read_csv("mmpc_lfd_masses.csv")) %>%
  mutate(eb_day = energy_balance*24*4.184, mb_day = (mass_2 - mass_1)/days)
#s2A
ggplot(mmpc_eb, aes(y=eb_day,x=mb_day)) +
  scale_fill_viridis(limits=c(.7,1.05))+labs(fill="RER")+ 
  xlab("change in body mass (g/day)") + theme(legend.position = c(.8,.3)) +
  ylab ("change in energy balance (kJ/day)") + 
  geom_point(aes(fill=respiratory_exchange_ratio),colour="black",pch=21, size = 2)+ 
  geom_smooth(method = "lm", se = FALSE, color = "black", size =2) +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "change in energy balance (kcal/day)")) 

impc_wt <- read.csv("impc_df.csv") %>% filter(group=="control")
impc_eb <- impc_wt %>% select(energy_intake,ee,duration,total_mass_1,total_mass_2,rer, sex) %>% 
  mutate(energy_balance = energy_intake - ee, mass_balance = total_mass_2 - total_mass_1) %>% filter(complete.cases(.) & rer <1.05) %>%
  mutate(eb_day = energy_balance*24, mb_day = mass_balance/duration*24)
#s2B
ggplot(impc_eb, aes(y=eb_day,x=mb_day)) +
  scale_fill_viridis(limits=c(.7,1.05))+labs(fill="RER")+ 
  xlab("change in body mass (g/day)") +theme(legend.position = c(.8,.3)) +
  ylab ("change in energy balance (kJ/day)") +xlim(-5.5,4) +
  geom_point(aes(fill=rer),colour="black",pch=21, size = 2)+ geom_smooth(method = "lm", se = FALSE, color = "black", size =2) +
  scale_y_continuous(limits = c(-12*4.184, 15*4.184), sec.axis = sec_axis(~./4.184, name = "change in energy balance (kcal/day)")) +
  theme(legend.position = "none")

