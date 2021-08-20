library(tidyverse)
library(cowplot)
library(relaimpo)
library(broom)

ww <- read.csv("mmpc_weekly_weights.csv") %>% mutate(date = as.Date(as.character(date),"%m/%d/%y")) %>%  
  filter(phase == 1) %>% group_by(run) %>% mutate(start = min(date)) %>% 
  mutate(week = as.numeric(difftime(date,start,units = "weeks"))) %>%
  mutate(institution = factor(institution, levels = c("UC Davis","UMass","Yale","Vanderbilt")))
MMPC_colors<-c("#CC79A7","#E69F00","#009E73","#56B4E9")
#1A
ww %>% filter(diet=="LF" ) %>% group_by(institution, week) %>% add_tally() %>%
  summarise(m_mass = mean(mass), n = mean(n), se = sd(mass)/sqrt(n)) %>%
  ggplot(aes(x=week, y=m_mass, color=institution, fill = institution)) + geom_line() + 
  geom_errorbar(aes_string(group="institution",ymin="m_mass-se",ymax="m_mass+se"),width=.2) +
  geom_point(colour="black",pch=21, size=2) + scale_color_manual(values=MMPC_colors)+ 
  scale_fill_manual(values=MMPC_colors)+ 
  theme(legend.title = element_blank(),legend.position = c(0.05,0.85), plot.title = element_text(size = 14))+
  ylab("body mass (g)") +ylim(c(17.5,43)) + scale_x_continuous(breaks = 0:12)
#1B
ww %>% filter(diet=="HF" ) %>% group_by(institution, week) %>% add_tally() %>%
  summarise(m_mass = mean(mass), n = mean(n), se = sd(mass)/sqrt(n)) %>%
  ggplot(aes(x=week, y=m_mass, color=institution, fill = institution)) + geom_line() + 
  geom_errorbar(aes_string(group="institution",ymin="m_mass-se",ymax="m_mass+se"),width=.2) +
  geom_point(colour="black",pch=21, size=2) + scale_color_manual(values=MMPC_colors)+ 
  scale_fill_manual(values=MMPC_colors)+ 
  theme(legend.position = "none") +
  ylab("body mass (g)")  +ylim(c(17.5,43)) + scale_x_continuous(breaks = 0:12)

mmpc_all_phases<-read.csv("mmpc_all_phases.csv") %>%
  mutate(institution = factor(institution, levels = c("UC Davis","UMass","Yale","Vanderbilt")),
         ee = ee * 4.184)
lf_group <- mmpc_all_phases %>% filter(phase=="2"&diet=="LF"&acclimation==TRUE)
lf_mice <- mmpc_all_phases %>% filter(diet == "LF", acclimation == TRUE, subject.id %in% lf_group$subject.id)
hf_mice <- mmpc_all_phases %>% filter(diet == "HF", acclimation == TRUE)
#1C
lf_mice %>%
  ggplot(.,aes(y = ee, x = total_mass)) + 
  scale_shape_manual(values = c(21,24,22)) +
  geom_smooth(method = "lm", aes(color=institution), se=F, size = 2) +
  geom_point(aes(fill = institution,shape=as.factor(phase)), color="black", size = 2) +
  theme(legend.position = "none") +  xlim(c(16,46)) +
  xlab("body mass (g)") + ylab("energy expenditure (kJ/hr)")+
  guides(color = guide_legend(override.aes = list(size = 10))) + scale_color_manual(values = MMPC_colors) +
  scale_fill_manual(values = MMPC_colors) + 
  scale_y_continuous(limits = c(1.4,3.1), sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)"))
lf_mice %>% 
  group_by(institution) %>%
  nest(-institution) %>% 
  mutate(fit = map(data, ~lm(ee~total_mass, data = .x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)
#1D
hf_mice %>%
  ggplot(.,aes(y = ee, x = total_mass)) + scale_shape_manual(values = c(24,22)) +
  geom_smooth(method = "lm", aes(color=institution), se=F, size = 2) +
  geom_point(aes(fill = institution,shape=as.factor(phase)), color="black", size = 2) +
  theme(legend.position = "none") +  xlim(c(16,46)) +
  xlab("body mass (g)") + ylab("energy expenditure (kJ/hr)") + scale_color_manual(values = MMPC_colors) +
  scale_fill_manual(values = MMPC_colors) +
  scale_y_continuous(limits = c(1.4,3.1),sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)"))
hf_mice %>% 
  group_by(institution)%>%
  nest(-institution) %>%
  mutate(fit = map(data, ~lm(ee~total_mass, data = .x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)

bc <- mmpc_all_phases %>% filter(phase==3, acclimation==T) %>% dplyr::select("subject.id","institution","diet","total_mass":"fat_mass")
bc_mean <- bc %>% dplyr::select(-"subject.id") %>% group_by(institution,diet) %>% summarise_all(funs(mean(.,na.rm=T))) %>%
  gather(key = "mass", value = "mean", total_mass, lean_mass, fat_mass)
bc_se <- bc %>% dplyr::select(-"subject.id") %>% group_by(institution,diet) %>% summarise_all(funs(sd(.,na.rm=T)/sqrt(n()))) %>%
  gather(key = "mass", value = "se", total_mass, lean_mass, fat_mass)
bc2 <- left_join(bc_mean, bc_se) %>% 
  mutate(diet=factor(diet,levels=c("LF","HF"), labels = c("LFD","HFD")),
         mass=factor(mass, levels = c("lean_mass","fat_mass","total_mass"), labels = c("lean mass","fat mass","total mass")))
#1E
bc2 %>% ggplot(aes(diet,mean))+
  geom_bar(aes(fill=institution),stat="identity",position="dodge",width=.75)+
  geom_errorbar(aes_string(group="institution",ymin="mean-se",ymax="mean+se"),width=.2,position=position_dodge(0.75))+
  xlab(NULL)+ylab("body composition (g)") + facet_wrap("mass") + theme(legend.position = "none") +
  scale_fill_manual(values = MMPC_colors)

mmpc_sites <- read.csv("MMPC_sites.csv")
hfd_avg <- read.csv("MMPC_12WK_overall_avg.csv") %>% right_join(mmpc_sites, by= c("Subject.ID"="subject.id")) %>% distinct() %>%
  filter(Time.of.Day!="Full day") %>% group_by(institution) %>% mutate(Locomotor.Activity = Locomotor.Activity/max(Locomotor.Activity))
m_var <- lm(Energy.Expenditure~Time.of.Day+acclimation+Group+Locomotor.Activity+Lean.Mass+Fat.Mass, data=hfd_avg)
m_calc <- calc.relimp(m_var)
round(m_calc@R2 * 100,digits=2)
m_variance <-rownames_to_column(as.data.frame(m_calc$lmg)) %>% 'colnames<-' (c("var","pct")) %>% mutate(pct = pct*100) %>% 
  arrange(desc(pct))
#1F
m_variance %>% ggplot(aes(reorder(var,-pct),pct))+
  geom_bar(stat="identity",aes(fill=-pct))+
  xlab(NULL)+ylab("% of energy expenditure variance") + 
  theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  annotate("text",label=expression(R^{2}~"= 72.41%"), x = 4.5, y=22, size = 8) +
  scale_x_discrete(labels=c("lean mass","fat mass", "locomotor\nactivity","time of day","diet","acclimation"))
#1G
data.frame("pct"=c(83.7,16.3),"var"=c("other","institution"),"total"="total") %>% 
  mutate(var = factor(var, levels=c("other","institution"))) %>%
  ggplot(aes(x=total,y=pct,fill=var))+geom_bar(position="stack",stat="identity") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        legend.position = "none") + 
  ylab("% of residual variance\nexplained by institution") +
  scale_fill_manual(values=c("gray","#132c43"))

### figure 1 - figure supplement 1

mmpc_2wk<-read.csv("MMPC_2WK_LFD_overall_avg.csv") %>%
  mutate(Group = factor(Group, levels = c("UC Davis", "Umass", "Yale", "Vanderbilt"), labels = c("UC Davis","UMass","Yale","Vanderbilt")),
         Energy.Expenditure  = Energy.Expenditure * 4.184,
         Hourly.Food.Intake = Hourly.Food.Intake * 4.184)
MMPC_colors<-c("#CC79A7","#E69F00","#009E73","#56B4E9")

#s1B
mmpc_2wk %>% filter(Time.of.Day =="Dark") %>% 
  mutate(Acclimated = factor(Acclimated, labels = c("non-\nacclim.","acclim."))) %>%
  ggplot(.,aes(x= Acclimated, y = Energy.Expenditure)) +
  geom_boxplot(aes(fill=Group), width=0.25) +facet_wrap(~ Group, ncol = 4) +
  geom_jitter(position=position_jitter(width=.02, height=0),size=1) +
  theme_bw() +  geom_line(aes(group=Subject.ID), alpha=0.20, color="black")+
  scale_fill_manual(values=MMPC_colors) + #ylim(c(0.317,0.6)) + 
  xlab("") + ylab("energy expenditure (kJ/hr)")+ 
  theme(legend.position = "none",axis.text.x = element_text(color=c("darkgreen","purple"))) +
  scale_y_continuous(limits = c(0.317*4.184,0.6*4.184),sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)"))

mmpc_2wk %>% filter(Time.of.Day=="Dark") %>%
  group_by(Group)%>%
  nest(-Group) %>%
  mutate(fit = map(data, ~lm(Energy.Expenditure ~ Acclimated, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)
#s1D
mmpc_2wk %>% filter(Time.of.Day =="Dark" & Hourly.Food.Intake > 0.01) %>% 
  mutate(Acclimated = factor(Acclimated, labels = c("non-\nacclim.","acclim."))) %>%
  ggplot(.,aes(x= Acclimated, y = Hourly.Food.Intake)) +
  geom_boxplot(aes(fill=Group), width=0.25)  +facet_wrap(~ Group, ncol = 4) +
  geom_jitter(position=position_jitter(width=.02, height=0),size=1) +
  theme_bw() +  geom_line(aes(group=Subject.ID), alpha=0.20, color="black")+
  scale_fill_manual(values=MMPC_colors) + 
  xlab("") + ylab("energy intake (kJ/hr)")+ 
  theme(legend.position = "none",axis.text.x = element_text(color=c("darkgreen","purple"))) +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy intake (kcal/hr)")) 
mmpc_2wk %>% filter(Time.of.Day=="Dark") %>%
  group_by(Group)%>%
  nest(-Group) %>%
  mutate(fit = map(data, ~lm(Hourly.Food.Intake ~ Acclimated, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)
#s1F
mmpc_2wk %>% filter(Time.of.Day =="Dark") %>% 
  mutate(Acclimated = factor(Acclimated, labels = c("non-\nacclim.","acclim."))) %>%
  ggplot(.,aes(x= Acclimated, y = Respiratory.Exchange.Ratio)) +
  geom_boxplot(aes(fill=Group), width=0.25)  +facet_wrap(~ Group, ncol = 4) +
  geom_jitter(position=position_jitter(width=.02, height=0),size=1) +
  theme_bw() +  geom_line(aes(group=Subject.ID), alpha=0.20, color="black")+
  scale_fill_manual(values=MMPC_colors) + 
  xlab("") + ylab("respiratory exchange ratio")+ 
  theme(legend.position = "none",axis.text.x = element_text(color=c("darkgreen","purple")))
mmpc_2wk %>% filter(Time.of.Day=="Dark") %>%
  group_by(Group)%>%
  nest(-Group) %>%
  mutate(fit = map(data, ~lm(Respiratory.Exchange.Ratio ~ Acclimated, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)
#s1H
mmpc_2wk %>% filter(Time.of.Day =="Dark") %>% 
  mutate(Acclimated = factor(Acclimated, labels = c("non-\nacclim.","acclim."))) %>%
  ggplot(.,aes(x= Acclimated, y = Locomotor.Activity)) +
  geom_boxplot(aes(fill=Group), width=0.25)  +facet_wrap(~ Group, ncol = 4) +
  geom_jitter(position=position_jitter(width=.02, height=0),size=1) +
  theme_bw() +  geom_line(aes(group=Subject.ID), alpha=0.20, color="black")+
  scale_fill_manual(values=MMPC_colors) + 
  xlab("") + ylab("locomotor activity (fraction of max)")+ 
  theme(legend.position = "none",axis.text.x = element_text(color=c("darkgreen","purple")))
mmpc_2wk %>% filter(Time.of.Day=="Dark") %>%
  group_by(Group)%>%
  nest(-Group) %>%
  mutate(fit = map(data, ~lm(Locomotor.Activity ~ Acclimated, data=.x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  unnest(tidied)