library(tidyverse)
library(cowplot)
library(relaimpo)
library(viridis)
library(broom)
library(lsmeans)

theme_set(theme_cowplot())

impc_df <- read.csv("impc_df.csv")
impc_wt <- impc_df %>% filter(group=="control") %>% mutate(sex=factor(sex, levels = c("male","female")))

#3A
impc_wt %>% ggplot(aes(x = total_mass_1, fill=sex)) + geom_histogram(bins=40) +ylab(NULL) +
  xlab("body mass (g)") +ylab("number of mice")+
  theme(legend.title = element_blank(),legend.position = c(0.8,0.85)) +
  scale_fill_manual(values = c("#00B0F6","#FF67A4"))
#3B
impc_wt %>% ggplot(aes(y = ee, x = total_mass_1)) +
  geom_point(alpha=0.15, aes(color=sex))+
  scale_color_manual(values = c("#00B0F6","#FF67A4")) +
  scale_fill_manual(values = c("#00B0F6","#FF67A4")) +
  geom_point(aes(fill=sex),colour="black",pch=21, size=2)+
  theme(legend.position = "none") + #ylim(c(.3, .7)) + 
  geom_smooth(method = "lm",size=2,aes(color=sex,fill=sex), se = F) +
  xlab("body mass (g)") + ylab("energy expenditure\n(kJ/hr)") +
  scale_y_continuous(limits = c(.3*4.184,.7*4.184), sec.axis = sec_axis(~./4.184, name = "energy expenditure\n(kcal/hr)"))

impc_wt %>%
  group_by(sex) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied)
sex_lm <- lm(ee ~ total_mass_1*sex, data = impc_wt)
anova(sex_lm)
sex_ls <- lstrends(sex_lm,"sex", var="total_mass_1")
pairs(sex_ls)

impc_wt %>% filter(total_mass_1 <= 23) %>% #skinnier mice  
  group_by(sex) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied)
skinny_lm <- lm(ee ~ total_mass_1*sex, data = (impc_wt %>% filter(total_mass_1 <= 23)))
anova(skinny_lm)
skinny_ls <- lstrends(skinny_lm,"sex", var="total_mass_1")
pairs(skinny_ls)

impc_wt %>% filter(total_mass_1 > 23) %>% #fatter mice
  group_by(sex) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied)
fat_lm <- lm(ee ~ total_mass_1*sex, data = (impc_wt %>% filter(total_mass_1 > 23)))
anova(fat_lm)
fat_ls <- lstrends(fat_lm,"sex", var="total_mass_1")
pairs(fat_ls)

male_wt <- impc_wt %>% filter(sex=="male")
loc_freq <- male_wt$location %>% fct_infreq() %>% levels()
#3C
male_wt %>% 
  mutate(location = factor(location, levels = loc_freq, labels = c("Cambridge-\nshire,UK","Germany","Oxfordshire,\nUK","California,\nUSA","France","China","Japan","Canada","Korea","Texas,\nUSA"))) %>%
  ggplot(aes(location, fill =location)) + geom_bar() +xlab(NULL) +
  scale_fill_brewer(palette = "Paired") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position = "none")+ylab("number of mice")

#3D
male_wt %>% 
  mutate(location = factor(location, levels = loc_freq)) %>%
  ggplot(., aes(y=ee, x=total_mass_1, color=location, fill = location)) +
  geom_point(colour="black",pch=21, size=2)+
  geom_smooth(method = "lm", se=FALSE, size=2) +
  ylab("energy expenditure\n(kJ/hr)") + scale_color_brewer(palette="Paired") +
  scale_fill_brewer(palette="Paired") +theme(legend.position = "none") +
  xlab("body mass (g)")+  xlim(c(17.5,40)) +
  scale_y_continuous(limits = c(.3*4.184,.7*4.184), sec.axis = sec_axis(~./4.184, name = "energy expenditure\n(kcal/hr)"))
male_wt %>%
  group_by(location) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) %>% filter(term == "total_mass_1") %>% arrange(estimate)
site_lm <- lm(ee ~ total_mass_1*location, data = male_wt)
anova(site_lm)
site_ls <- lstrends(site_lm,"location", var="total_mass_1")
pairs(site_ls)
#3E
male_wt %>% mutate(location = factor(location, levels = loc_freq)) %>%
  ggplot(aes(room_temp, fill = location)) + geom_histogram() +xlab("room temperature (°C)") + ylab(NULL) + 
  scale_fill_brewer(palette = "Paired") + ylab("number of mice") +
  theme(legend.title=element_blank(),legend.position = c(.6,.7),legend.key.size = unit(.5, 'lines')) 
#3F
male_wt$temp_range <- as.character(cut(male_wt$room_temp, breaks=c(18,20,22,24,26,28) ,labels=c("18-20","20-22","22-24","24-26","26-28")))
male_wt %>% mutate(temp_range = if_else(is.na(temp_range), "26-28", temp_range),
                      room_temp = if_else(room_temp >= 26, 26, room_temp)) %>%
  ggplot(., aes(y=ee, x=total_mass_1)) +
  ylab("energy expenditure\n(kJ/hr)") +  xlab("body mass (g)")+
  geom_point(pch=21, color="black", size=2, aes(fill=room_temp))+
  stat_smooth(method = "lm", se=FALSE, size=2,aes(color=temp_range) )+
  scale_color_brewer(palette="RdYlBu", direction=-1)+ labs(fill = "ambient\ntemp (°C)", color = NULL) +
  scale_fill_gradient2(low = "#4575b4", mid = "#ffffbf", high = "red",midpoint = 22, limits=c(18,26))+
  xlim(c(17.5,40)) + guides(fill = guide_colorbar(reverse = TRUE), color=F) +
  scale_y_continuous(limits = c(.3*4.184,.7*4.184), sec.axis = sec_axis(~./4.184, name = "energy expenditure\n(kcal/hr)"))

male_wt %>% mutate(temp_range = if_else(is.na(temp_range), "26-28", temp_range)) %>%
  group_by(temp_range) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)
  ) %>% 
  unnest(tidied) %>% arrange(temp_range)
#3G
male_wt %>% 
  mutate(season = factor(season, levels = c("winter","spring","summer","autumn"))) %>%
  ggplot(., aes(y=ee, x=total_mass_1, color=season,fill=season)) +
  ylab("energy expenditure\n(kcal/hr)") + 
  geom_point(pch=21, stroke= 0.5,color="black", size=2, aes(fill=season))+
  scale_color_manual(values=c("dodgerblue3", "forestgreen", "firebrick4", "goldenrod3")) +
  stat_smooth(method = "lm",se=F,  size=2) +
  scale_fill_manual(values=c("dodgerblue3", "forestgreen", "firebrick4", "goldenrod3")) + guides(fill=F) +
  xlab("body mass (g)")+ xlim(c(17.5,40)) +
  theme(legend.title=element_blank(),legend.position = c(.73,.9),legend.key.size = unit(.5, 'lines')) +
  scale_y_continuous(limits = c(.3*4.184,.7*4.184), sec.axis = sec_axis(~./4.184, name = "energy expenditure\n(kcal/hr)"))
#3H
male_wt %>% filter(rer > .69 & rer < 1.01 ) %>% 
  ggplot(., aes(y=ee, x=(amb_act))) +
  geom_point(pch=21, stroke= 0.5,color="black", size=2, aes(fill=rer))+
  stat_smooth(method = "lm", color="black",se =F, size = 2) +  
  scale_fill_viridis()+ labs(fill = "RER") + theme(legend.position = c(.8,.34)) +
  ylab("energy expenditure\n(kJ/hr)") + xlab("ambulatory activity (count/hr)") + 
  scale_y_continuous(limits = c(.3*4.184,.7*4.184), sec.axis = sec_axis(~./4.184, name = "energy expenditure\n(kcal/hr)"))

i_var <- lm(ee~sex+lean_mass+fat_mass+room_temp+loc_act+season, data=impc_wt)
i_calc <- calc.relimp(i_var)
round(i_calc@R2 * 100,digits=2) #r2
i_variance <-rownames_to_column(as.data.frame(i_calc$lmg)) %>% 'colnames<-' (c("var","pct")) %>% mutate(pct = pct*100) %>% 
  arrange(desc(pct))
#3I
ggplot(i_variance,aes(reorder(var,-pct),pct))+
  geom_bar(stat="identity",aes(fill=-pct))+ 
  xlab(NULL)+ylab("% of energy\nexpenditure variance") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  annotate("text",label=expression(R^{2} ~ "= 38.82%"), x = 4, y=17, size = 8) +
  scale_x_discrete(labels=c("ambient\ntemp","lean mass","locomotor\nactivity","sex","fat mass","season"))

fat_cont<-male_wt %>% group_by(location) %>%
  nest(-location) %>%
  mutate(fit = map(data, ~lm(ee~lean_mass + fat_mass, data = .x)),tidied = map(fit, tidy),glanced = map(fit, glance)) %>%
  unnest(tidied) %>% dplyr::select(-c("std.error","statistic","p.value")) %>%
  spread(term, estimate) %>% group_by(location) %>% mutate(pct_fat = fat_mass/(lean_mass + fat_mass)) %>% arrange(pct_fat)
#3J
data.frame("pct"=c(90.4,9.6),"var"=c("other","institution"),"total"="total") %>% 
  mutate(var = factor(var, levels=c("other","institution"))) %>%
  ggplot(aes(x=total,y=pct,fill=var))+geom_bar(position="stack",stat="identity") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        legend.position = "none") + 
  ylab("% of residual variance\nexplained by institution") +
  scale_fill_manual(values=c("gray","#132c43"))

#3K
fat_cont %>% ungroup(location) %>% 
  mutate(location = factor(location, levels = loc_freq, labels = c("Cambridge-\nshire,UK","Germany","Oxfordshire,\nUK","California,\nUSA","France","China","Japan","Canada","Korea","Texas,\nUSA"))) %>%
  ggplot(., aes(x=location, y=pct_fat, fill=location ))+geom_col()+
  xlab("")+ ylab("fat mass\ncontribution to EE")+ scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1))

### figure 3 - figure supplement 1
impc_size <- impc_wt %>% mutate(size = case_when(total_mass_1 < 20.75 ~ "small",
                                                 total_mass_1 >= 20.75 & total_mass_1 < 27.5 ~ "medium",
                                                 total_mass_1 >= 27.5 & total_mass_1 < 34.25 ~ "large",
                                                 TRUE ~ "largest")) %>%
  mutate(lean_size = case_when(lean_mass < 13.92 ~ "small",
                               lean_mass >= 13.92 & lean_mass < 19.14 ~ "medium",
                               lean_mass >= 19.14 & lean_mass < 24.36 ~ "large",
                               TRUE ~ "largest"))
#s1A
impc_size %>% mutate(size = factor(size, levels=c("small","medium","large","largest"))) %>%
  ggplot(aes(total_mass_1, ee)) + geom_point(alpha=0.5, aes(color=size)) + 
  geom_smooth(method="lm", color="black", aes(fill=size)) + ylab("energy expenditure (kJ/hr)") +
  xlab("body mass (g)") + guides(fill=F) + theme(legend.title = element_blank(), legend.position = c(.8,.85))

#s1B
impc_size %>% mutate(lean_size = factor(lean_size, levels=c("small","medium","large","largest"))) %>%
  ggplot(aes(lean_mass, ee)) + geom_point(alpha=0.5, aes(color=lean_size)) + 
  geom_smooth(method="lm", color="black", aes(fill=lean_size)) + ylab("energy expenditure (kJ/hr)") +
  xlab("lean mass (g)") + guides(fill=F) + theme(legend.title = element_blank(), legend.position = c(.8,.85))

### figure 3 - figure supplement 2
impc_act <- impc_wt %>% filter(!is.na(loc_act)) %>% filter(location != "California, USA")
impc_no_act <- impc_wt %>% filter(location %in% c("Germany","Texas, USA","China", "California, USA"))

act_model <- impc_act %>%
  group_by(location, sex) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1 + room_temp + loc_act, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )
no_act_model <- impc_no_act %>%
  group_by(location, sex) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ lm(ee ~ total_mass_1 + room_temp, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )
act_res <- act_model %>% unnest(augmented)
no_act_res <- no_act_model %>% unnest(augmented)
all_res <- bind_rows(act_res, no_act_res) 
#s2A
all_res %>% ungroup() %>% filter(location %in% c("Canada","France","Germany","Japan","Korea","Oxfordshire, UK", "Texas, USA")) %>%
  mutate(location=fct_relevel(location, c("Canada","Oxfordshire, UK","Germany", "Japan","France","Korea", "Texas, USA")),
         sex = factor(sex, levels= c("female", "male")) ) %>%
  ggplot(., aes(x=total_mass_1, y=ee))+ labs(fill = "residual") +
  scale_fill_viridis(option="magma", direction = -1, limits=c(-0.35*4.184,0.5*4.184)) + 
  geom_point(aes(fill=.resid), pch=21, colour="black", size=2)+
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  facet_grid(location~sex) + xlab("body mass (g)") + 
  ylab("energy expenditure (kJ/hr)") + theme(legend.position = "none") + theme_classic() + 
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)"))
#s2B
all_res %>% ungroup() %>% filter(location %in% c("Cambridgeshire, UK","China","California, USA")) %>%
  mutate(location = factor(location, levels = c("Cambridgeshire, UK","China","California, USA"))) %>%
  ggplot(., aes(x=total_mass_1, y=ee))+ labs(fill = "residual") +
  scale_fill_viridis(option="magma", direction = -1,limits=c(-0.35*4.184,0.5*4.184)) +
  geom_point(aes(fill=.resid), pch=21, colour="black", size=2)+
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  facet_grid(location~sex) + xlab("body mass (g)") + 
  ylab("energy expenditure (kJ/hr)") + theme(legend.position = "none") + theme_classic() +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)"))+
  theme(legend.position = "none")

act_r2 <- act_model %>% unnest(glanced)
no_act_r2 <- no_act_model %>% unnest(glanced)
all_r2 <- bind_rows(act_r2, no_act_r2) %>% select(location,sex,r.squared) %>% arrange(location, sex)
#s2C
all_res %>% 
  ggplot(., aes(sample=ee))+
  scale_color_manual(values = c("#00B0F6","#FF67A4"))+
  geom_qq_line(aes(color=sex)) +
  xlab("theoretical") + ylab("energy expenditure (kJ/hr)") +
  theme(legend.position = c(0.1, 0.8), legend.title = element_blank()) +
  geom_qq( aes(color=sex)) +
  scale_y_continuous(sec.axis = sec_axis(~./4.184, name = "energy expenditure (kcal/hr)")) 

### figure 3 - figure supplement 3
impc_unfixed <- read_csv("impc_unfixed.csv") %>% mutate_at(vars(total_mass_1:rer), as.numeric)

#s3A
impc_unfixed %>% 
  mutate(inst_id = factor(institution,labels = c("institution A","institution B","institution C","institution D","institution E",
                                                 "institution F","institution G","institution H","institution I","institution J"))) %>%
  filter(!is.na(vo2) & !is.na(total_mass_1)) %>%
  ggplot(.,aes(total_mass_1, vo2, color=inst_id)) + 
  geom_point(aes(fill=inst_id),color="black",pch=21, size=2)+
  geom_smooth(method = "lm",se=F, color="black", size=2) +
  facet_wrap(vars(inst_id), nrow = 2) +
  ylab("reported oxygen consumption (ml/hr)") + xlab("body mass (g)") +
  theme(legend.position = "none")
#s3B
impc_unfixed %>% 
  mutate(inst_id = factor(institution, labels = c("A","B","C","D","E","F","G","H","I","J"))) %>%
  mutate(food_intake = food_intake/duration) %>% 
  filter(!is.na(food_intake)) %>% 
  ggplot(.,aes(food_intake, fill = inst_id)) + geom_histogram() + 
  labs(fill = "institution") + xlab("reported food intake (g/hr)") + ylab("number of mice") +
  scale_fill_manual(values=c("#d89000", "#a3a500","#39b600","#00bf7d","#00b0f6","#9590ff","#ff62bc")) +
  theme(legend.position = "none")
#s3C
impc_unfixed %>% 
  mutate(inst_id = factor(institution, labels = c("A","B","C","D","E","F","G","H","I","J"))) %>%
  filter(!is.na(vo2) & !is.na(total_mass_1)) %>%
  mutate(duration = as.numeric(duration),
         food_intake = food_intake/duration,
         ee_old = ee/4.184,
         ee_weir = (3.941*vo2/60 + 1.106*vco2/60)*0.06, #weir eq
         ee_diff = (ee_old - ee_weir)*4.184) %>% 
  ggplot(.,aes(ee_diff*4.184, fill = inst_id)) + geom_histogram() + 
  labs(fill = "institution") + xlab("difference in reported and calculated EE (kJ/hr)") +
  theme(legend.position = c(0.1,0.6)) + ylab("number of mice")
#s3D
impc_unfixed %>% 
  mutate(inst_id = factor(institution, labels = c("A","B","C","D","E","F","G","H","I","J"))) %>%
  ggplot(aes(hours, fill = inst_id)) + geom_histogram() + ylab("number of mice") + xlab("duration of calorimeter run (hrs)") +
  theme(legend.position = "none")
