# Clear memory
rm(list=ls())

# Load libraries
library("cowplot")
library("tidyverse")
library("haven")
library("estimatr")
library("stargazer")

# Import data
marijuana_data <- read_dta("~/Documents/GitHub/evali_and_recreational_marijuana/data_for_R.dta")

# Make a variable equal to 1 if non-marijuana, 2 if medical
marijuana_data <- marijuana_data %>%
    mutate(mj_policy = ifelse(marijuana_data$mm== 1, 1,2))

# Make the variable equal to 3 if recreational marijuana
marijuana_data$mj_policy <- ifelse(marijuana_data$rm_disp == 1, 3, marijuana_data$mj_policy)

# Calculate group means of EVALI by MJ policy
means <- marijuana_data %>%
  group_by(mj_policy) %>%
  summarise(
    Avg = mean(cases_per_million)
  )

# Difference between Rec and Medical
model_rm_mm <- lm_robust(cases_per_million ~ rm_disp, data = subset(marijuana_data, mm == 1), se_type = "stata")
summary(model_rm_mm)

# Difference between Rec and prohibition
model_rm_prohib <- lm_robust(cases_per_million ~ rm_disp, data = subset(marijuana_data, mm == 0 | rm_disp ==1), se_type = "stata")
summary(model_rm_prohib)

# Difference between medical and prohibition
model_mm_prohib <- lm_robust(cases_per_million ~ mm, data = subset(marijuana_data, mm == 1 & rm_disp == 0 | mm == 0), se_type = "stata")
summary(model_mm_prohib)

# Calculate group means of e-cigarette use by MJ policy
means_ecig <- marijuana_data %>%
  group_by(mj_policy) %>%
  summarise(
    Avg = mean(ecigarette_use)
  )
# Difference between Rec and Medical
model_rm_mm_ecig <- lm_robust(ecigarette_use ~ rm_disp, data = subset(marijuana_data, mm == 1), se_type = "stata")
summary(model_rm_mm_ecig)

# Difference between Rec and prohibition
model_rm_prohib_ecig <- lm_robust(ecigarette_use ~ rm_disp, data = subset(marijuana_data, mm == 0 | rm_disp ==1), se_type = "stata")
summary(model_rm_prohib_ecig)

# Difference between medical and prohibition
model_mm_prohib_ecig <- lm_robust(ecigarette_use ~ mm, data = subset(marijuana_data, mm == 1 & rm_disp == 0 | mm == 0), se_type = "stata")
summary(model_mm_prohib_ecig)

# Count states by MJ policy
count <- marijuana_data %>%
  group_by(mj_policy) %>%
  tally()

# Main regression
model_robust <- lm_robust(cases_per_million ~ ecigarette_use + mm + rm_disp, data = marijuana_data, se_type = "stata")
summary(model_robust)

model_mot_robust <- lm(cases_per_million ~ ecigarette_use + mm + rm_disp, data = marijuana_data)
stargazer(model_mot_robust, 
          se=starprep(model_mot_robust, se_type = "stata"),
          covariate.labels=c("Prevalence of e-cigarette use (0-100%)",
                             "Medical marijuana", 
                             "Recreational dispensary open"),
          column.labels=c(''))

# Make a histogram of EVALI case rate
hist <- ggplot(marijuana_data, aes(x = cases_per_million)) +
  geom_histogram(fill = "white", color = "black") +
  theme_classic() +
  labs(x="Cases per million population", y = "Count", title = "Histogram of state EVALI case rate")
hist 
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/histogram_evali.pdf", plot = hist,dpi = 1200, width = 6, height = 6, units = "in")
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/histogram_evali.png", plot = hist,dpi = 1200, width = 6, height = 6, units = "in")

# Make a histogram of e-cigarette case rate
hist_ecig <- ggplot(marijuana_data, aes(x = ecigarette_use)) +
  geom_histogram(fill = "white", color = "black") +
  theme_classic() +
  labs(x="Cases per million population", y = "Count", title = "Histogram of e-cigarette prevalence")
hist_ecig
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/histogram_ecigarette.pdf", plot = hist_ecig,dpi = 1200, width = 6, height = 6, units = "in")
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/histogram_ecigarette.png", plot = hist_ecig,dpi = 1200, width = 6, height = 6, units = "in")

# Make a custom color palette 
cbPalette <- c("#3F3DCB", 
               "#d6d4d3", 
               "#0CC693")

# Make a dot plot of EVALI case rate, highlighting state MJ policy
dot_plot <- ggplot(marijuana_data, aes(x = cases_per_million, y = reorder(state, -cases_per_million))) +
  geom_vline(data=filter(marijuana_data, mj_policy==3), aes(xintercept=mean(cases_per_million)), colour=cbPalette[3], size = 1.5, linetype = 'longdash') + 
  geom_vline(data=filter(marijuana_data, mj_policy==2), aes(xintercept=mean(cases_per_million)), colour=cbPalette[2], size = 1.5, linetype = 'longdash') + 
  geom_vline(data=filter(marijuana_data, mj_policy==1), aes(xintercept=mean(cases_per_million)), colour=cbPalette[1], size = 1.5, linetype = 'longdash') + 
  geom_point(aes(fill=as.factor(mj_policy),shape=as.factor(mj_policy)), size=4) + 
  theme_classic() + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                     values=cbPalette,
                     name = "State marijuana policy",
                     labels = c("Medicial", "Prohibition", "Recreational")) +
  scale_shape_manual(breaks = c("1", "2", "3"),
                    values=c(21,22,23),
                    name = "State marijuana policy",
                    labels = c("Medicial","Prohibition",  "Recreational")) +
  theme(legend.position = c(0.7, 0.9)) +
  scale_x_continuous(limits = c(0,40)) +
  labs(x="", 
       y = "",
       title = "States with recreational marijuana have lower\n rates of e-cigarette/vaping associated lung\n injury (EVALI)") +
  theme(axis.text.x = element_text(size = 20),
       axis.text.y = element_text(size = 10),
       legend.title=element_text(size=15), 
       legend.text=element_text(size=15),
       axis.title.x=element_text(size = 18),
       title=element_text(size = 14))

dot_plot

# Make a bar plot of mean EVALI case rate by state MJ policy
bar_plot <- ggplot(marijuana_data, aes(x = as.factor(mj_policy), y = cases_per_million, fill=as.factor(mj_policy),color=as.factor(mj_policy))) + 
  theme_bw() + 
  geom_bar(stat = "summary", fun.y = "mean",aes(fill=as.factor(mj_policy))) +
  coord_flip() +
  theme_classic() + 
  scale_color_manual(breaks = c("1", "2", "3"),
                     values=c("black", "black", "black")) + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=cbPalette) +
  scale_y_continuous(limits = c(0,40)) +
  theme(legend.position = "none") +
  labs(y="Cases per million population", x = "") +
  scale_x_discrete(labels = c("Medicial\n (N = 26)", "Prohibition \n (N = 18)",  " Recreational\n (N = 7)")) +
  annotate("text", label = "    6.4**", x = 2.55, y = 12.5, color = "black", size = 5) +
  geom_segment(x = 3, xend = 2.1, 
               y = 10.5, yend = 10.5,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 9.5, yend =10.5 ,
               colour = "black") +
  geom_segment(x = 2.1, xend = 2.1, 
               y = 9.5, yend = 10.5,
               colour = "black") +
  annotate("text", label = "0.7", x = 1.5, y = 12.5, color = "black", size = 5) +
  geom_segment(x = 1, xend = 1.9, 
               y = 10.5, yend = 10.5,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 9.5, yend =10.5 ,
               colour = "black") +
  geom_segment(x = 1.9, xend = 1.9, 
               y = 9.5, yend = 10.5,
               colour = "black")  +
  annotate("text", label = "   8.8 - 1.7 = 7.1***", x = 2, y = 25, color = "black", size = 5) +
  geom_segment(x = 1, xend = 3, 
               y = 19, yend = 19,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 18, yend =19 ,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 18, yend = 19,
               colour = "black")  +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 12, angle = 90, hjust = .5)) +
  labs(title="Average case rate by state marijuana policy",
       caption=
         "Note: * p < 0.05, ** p < 0.01, *** p < 0.001. We consider a state
        to be a recreational marijuana state if it had at least one 
        recreational dispensary open in January 2019. Results
        are robust to considering any state with an effective
        recreational marijuana law as of 2019 (AK, CA, CO, DC,
        ME, MA, MI, NV, OR, VT, WA) to be a recreational state.") +
  theme(axis.title.x=element_text(size = 18),
        title=element_text(size = 15),
        plot.caption = element_text(hjust = 0))

bar_plot

# Combine the two plots
combined_plot <- plot_grid(dot_plot, bar_plot, ncol = 1, align = 'v', rel_heights = c(2,1))
combined_plot

ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/exhibit_1.pdf", plot = combined_plot,dpi = 1200, width = 6, height = 15, units = "in")
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/exhibit_1.png", plot = combined_plot,dpi = 1200, width = 6, height = 15, units = "in")

# Make a dot plot of e-cigarette use, highlighting state MJ policy
dot_plot_ecig <- ggplot(marijuana_data, aes(x = ecigarette_use, y = reorder(state, -ecigarette_use))) +
  geom_vline(data=filter(marijuana_data, mj_policy==3), aes(xintercept=mean(ecigarette_use)), colour=cbPalette[3], size = 1.5, linetype = 'longdash') + 
  geom_vline(data=filter(marijuana_data, mj_policy==2), aes(xintercept=mean(ecigarette_use)), colour=cbPalette[2], size = 1.5, linetype = 'longdash') + 
  geom_vline(data=filter(marijuana_data, mj_policy==1), aes(xintercept=mean(ecigarette_use)), colour=cbPalette[1], size = 1.5, linetype = 'longdash') + 
  geom_point(aes(fill=as.factor(mj_policy),shape=as.factor(mj_policy)), size=4) + 
  theme_classic() + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=cbPalette,
                    name = "State marijuana policy",
                    labels = c("Medicial", "Prohibition", "Recreational")) +
  scale_shape_manual(breaks = c("1", "2", "3"),
                     values=c(21,22,23),
                     name = "State marijuana policy",
                     labels = c("Medicial","Prohibition",  "Recreational")) +
  theme(legend.position = c(.8, 0.9)) +
  scale_x_continuous(limits = c(0,9)) +
  labs(x="", 
       y = "",
       title = "Prevalence of e-cigarette use is similar across\n states with different marijuana policies.\n") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.title.x=element_text(size = 18),
        title=element_text(size = 14))

dot_plot_ecig

# Make a bar plot of mean e-cigarete use by state MJ policy
bar_plot_ecig <- ggplot(marijuana_data, aes(x = as.factor(mj_policy), y = ecigarette_use, fill=as.factor(mj_policy),color=as.factor(mj_policy))) + 
  geom_bar(stat = "summary", fun.y = "mean",aes(fill=as.factor(mj_policy))) +
  coord_flip() +
  theme_classic() + 
  scale_color_manual(breaks = c("1", "2", "3"),
                     values=c("black", "black", "black")) + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=cbPalette) +
  scale_y_continuous(limits = c(0,9)) +
  theme(legend.position = "none") +
  labs(y="Prevalence of e-cigarette use (0-100%)", x = "") +
  scale_x_discrete(labels = c("Medicial\n (N = 26)", "Prohibition \n (N = 18)",  " Recreational\n (N = 7)")) +
  annotate("text", label = "  0.4", x = 2.55, y = 5.8 , color = "black", size = 5) +
  geom_segment(x = 3, xend = 2.1, 
               y = 5.5, yend = 5.5,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 5, yend =5.5 ,
               colour = "black") +
  geom_segment(x = 2.1, xend = 2.1, 
               y = 5, yend = 5.5,
               colour = "black") +
  annotate("text", label = " -0.7", x = 1.5, y = 5.8, color = "black", size = 5) +
  geom_segment(x = 1, xend = 1.9, 
               y = 5.5, yend = 5.5,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 5, yend =5.5 ,
               colour = "black") +
  geom_segment(x = 1.9, xend = 1.9, 
               y = 5, yend = 5.5,
               colour = "black")  +
  annotate("text", label = "0.3", x = 2, y = 7.5, color = "black", size = 5) +
  geom_segment(x = 1, xend = 3, 
               y = 7, yend = 7,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 6.5, yend =7 ,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 6.5, yend = 7,
               colour = "black")  +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 12, angle = 90, hjust = .5)) +
  labs(title="Average e-cig use by state marijuana policy",
       caption=
         "Note: * p < 0.05, ** p < 0.01, *** p < 0.001. We consider a state
       to be a recreational marijuana state if it had at least one 
       recreational dispensary open in January 2019. Results
       are robust to considering any state with an effective
       recreational marijuana law as of 2019 (AK, CA, CO, DC,
       ME, MA, MI, NV, OR, VT, WA) to be a recreational state.") +
  theme(axis.title.x=element_text(size = 18),
        title=element_text(size = 15),
        plot.caption = element_text(hjust = 0))

bar_plot_ecig

# Combine the two plots
combined_plot_ecig <- plot_grid(dot_plot_ecig, bar_plot_ecig, ncol = 1, align = 'v', rel_heights = c(2,1))
combined_plot_ecig
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/exhibit_2.pdf", plot = combined_plot_ecig,dpi = 1200, width = 6, height = 15, units = "in")
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/exhibit_2.png", plot = combined_plot_ecig,dpi = 1200, width = 6, height = 15, units = "in")

# Combine the two combined plots
combine_both_plot <- plot_grid(combined_plot, combined_plot_ecig, 
                               ncol = 2, 
                               labels = c('A', 'B'),
                               rel_heights = c(1,1))
combine_both_plot
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/main_exhibit.pdf", plot = combine_both_plot,dpi = 1200, width = 12, height = 15, units = "in")
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/main_exhibit.png", plot = combine_both_plot,dpi = 300, width = 12, height = 15, units = "in")


#  EVALI case  rate v e-cigarette use
model_evali_ecig <- lm_robust(cases_per_million ~ ecigarette_use, data = marijuana_data, se_type = "stata")
summary(model_evali_ecig)

## Clean up a little bit (optional)
colnames(pred_df) <- gsub("fit.", "", colnames(pred_df))

#  EVALI case  rate v e-cigarette use. Weighted by population.
model_evali_ecig_weighted <- lm_robust(cases_per_million ~ ecigarette_use, data = marijuana_data, se_type = "stata", weights = population)
summary(model_evali_ecig_weighted)

#   geom_point(fill = "white", color = "white", size = 3) +

library(ggrepel)

#geom_text(aes(label = state),hjust= -.5, vjust=-.25, size = 3) +
  

# Scatter EVALI case  rate against e-cigarette use
scatterplot <- ggplot(marijuana_data, aes(y = cases_per_million, x = ecigarette_use)) +
  geom_point(aes(fill=as.factor(mj_policy),shape=as.factor(mj_policy)), size=4) + 
  geom_smooth(method='lm_robust') +
  theme_classic() +
  labs(y="EVALI cases per million population", 
       x = "Prevalence of e-cigarette use (0-100%)", 
       title = "There is no discernable relationship between EVALI case rate\n and e-cigarette use",
       caption=
         "Note: Best fit line is displayed in blue with a slope of -0.8 and a robust standard error of 0.9 
          (p-value of 0.36). 95% confidence interval is denoted by gray shaded area. Results
          are robust to weighting by state population.") +
  theme(axis.title.x=element_text(size = 18),
        title=element_text(size = 15),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=cbPalette,
                    name = "State marijuana policy",
                    labels = c("Medicial", "Prohibition", "Recreational")) +
  scale_shape_manual(breaks = c("1", "2", "3"),
                     values=c(21,22,23),
                     name = "State marijuana policy",
                     labels = c("Medicial","Prohibition",  "Recreational")) +
  theme(legend.position =  "none")

p <- scatterplot +  geom_text_repel(data=marijuana_data, aes(label=state),  box.padding = 0.5)
p
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/exhibit_3.pdf", plot = p, dpi = 1200, width = 8, height = 6, units = "in")
ggsave("~/Documents/GitHub/evali_and_recreational_marijuana/exhibit_3.png", plot = p, dpi = 300, width = 8, height = 6, units = "in")

#reg cases_per_million i.rm_disp if rm_disp == 1 | mm == 0, robust
#reg cases_per_million i.rm_disp if rm_disp == 1 | mm == 1, robust
#reg cases_per_million i.mm if rm_disp == 1 | mm == 1, robust
#reg cases_per_million i.mm if rm_disp == 0, robust
