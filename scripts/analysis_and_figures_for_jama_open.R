###################################################################
# Replication code for Wing, Bradford, Carroll, Hollingsworth (2020)

###################################################################
# Clear memory
rm(list=ls())

###################################################################
# Load required packages using package manager (pacman)

# First make sure that pacman is installed
if (!require("pacman")) install.packages("pacman")

# Second, load all the packages that we will use 
pacman::p_load(plyr, cowplot, tidyverse, haven, estimatr, tidylog, huxtable,
               flextable, ggrepel, mfx, openxlsx)

###################################################################
# Set display options for tidylog
options("tidylog.display" = NULL)

###################################################################
# Import marijuana data
marijuana_data <- read_dta("data/data_for_R.dta")

###################################################################
# Recode marijuana policy variables

# Make a variable equal to 2 if non-marijuana, 1 if medical
marijuana_data <- marijuana_data %>%
    mutate(mj_policy = ifelse(marijuana_data$mm== 1, 1,2))

# Make the variable equal to 3 if recreational marijuana
marijuana_data$mj_policy <- ifelse(marijuana_data$rm_disp == 1, 3, marijuana_data$mj_policy)

###################################################################
# Calculate group means of EVALI case rate by MJ policy

mean_evali_by_mj_use <- marijuana_data %>%
  group_by(mj_policy) %>%
  summarise(
    mean = mean(cases_per_million), 
    sd = sd(cases_per_million),
    n = n()
  )  %>%
  mutate(se = sd/sqrt(n),
         lower_ci = mean - qt(0.975, n -1)*se,
         upper_ci = mean + qt(0.975, n -1)*se)

# Use regression  so we can easily calculate the 95% CI and compare groups using huxreg package
evali_no_mj_mean <- lm_robust(cases_per_million ~ 1, data = subset(marijuana_data, mj_policy == 2), se_type = "stata")
evali_mm_mean <- lm_robust(cases_per_million ~ 1, data = subset(marijuana_data, mj_policy == 1), se_type = "stata")
evali_rm_mean <- lm_robust(cases_per_million ~ 1, data = subset(marijuana_data, mj_policy == 3), se_type = "stata")

# Store results in table 
TableOfMeans.EVALI <- huxreg("Prohibition" = evali_no_mj_mean,  "Medical Only" = evali_mm_mean, "Recreational" = evali_rm_mean,
       align = "center",
       number_format = "%.2f",
       error_format = '({conf.low} to {conf.high})', 
       statistics = c(N = "nobs"), 
       stars = NULL, 
       coefs = c("Mean" = "(Intercept)"), 
       note = "Note: 95% confidence interval calculated using robust standard errors in parantheses." )

# Add title
caption(TableOfMeans.EVALI) <- "Table 1: Mean EVALI case rate per million by marijuana policy"

# Display table
TableOfMeans.EVALI

# Export table to word document
TableOfMeansForWord.EVALI <- as_FlexTable(TableOfMeans.EVALI)
TableOfMeansForWord.EVALI <-  fontsize(TableOfMeansForWord.EVALI, size = 12)
TableOfMeansForWord.EVALI <- width(TableOfMeansForWord.EVALI, width = 1.25)
my_doc <- officer::read_docx()
my_doc <- flextable::body_add_flextable(
  my_doc, TableOfMeansForWord.EVALI)
print(my_doc, target =
        "output/evali_means.docx")

###################################################################
# Calculate difference between group means of EVALI case rate by MJ policy

# Difference between Rec and Medical
compare_rm_mm <- lm_robust(cases_per_million ~ rm_disp, data = subset(marijuana_data, mm == 1), se_type = "stata")

# Difference between Rec and prohibition
compare_rm_prohib <- lm_robust(cases_per_million ~ rm_disp, data = subset(marijuana_data, mm == 0 | rm_disp ==1), se_type = "stata")

# Difference between medical and prohibition
compare_mm_prohib <- lm_robust(cases_per_million ~ mm, data = subset(marijuana_data, mm == 1 & rm_disp == 0 | mm == 0), se_type = "stata")


TableOfDifferences.EVALI <- huxreg("Medical v\n Prohibition" = compare_mm_prohib,  "Prohibition v\n Recreational" = compare_rm_prohib, "Medical v\n Recreational" = compare_rm_mm,
       align = "center",
       number_format = "%.3f",
       error_format = '({std.error}) \n [{p.value}] \n ({conf.low} to {conf.high})', 
       statistics = c(N = "nobs"), 
       coefs = c("Difference" = "mm", "Difference" = "rm_disp"), 
       note = "Note: Difference between means of groups reported with the robust standard error of the difference reported in parantheses below. P-values reported in brackets. P-values also represented by stars with * p < 0.05, ** p < 0.01, *** p < 0.001. 95% confidence interval of difference in means calculated using robust standard errors in parantheses.")

# Add title
caption(TableOfDifferences.EVALI) <- "Table 2: Difference in mean EVALI case rate per million by marijuana policy"

# Display table
TableOfDifferences.EVALI

# Export table to word document
TableOfDifferencesForWord.EVALI <- as_FlexTable(TableOfDifferences.EVALI)
TableOfDifferencesForWord.EVALI <-  fontsize(TableOfDifferencesForWord.EVALI, size = 12)
TableOfDifferencesForWord.EVALI <- width(TableOfDifferencesForWord.EVALI, width = 1.25)
my_doc <- officer::read_docx()
my_doc <- flextable::body_add_flextable(
  my_doc, TableOfDifferencesForWord.EVALI)
print(my_doc, target =
        "output/evali_difference_in_means.docx")

##########################################################################################################
# Control for both at the same time (have to put results in a data frame then export as a huxtable since there is no clear way to link mfx for poisson to table)

# Initialize table
common_results_table <- data.frame(matrix(ncol = 2, nrow = 16 ))
names(common_results_table) <- c("OLS", "Poisson (mfx)")


# Using OLS
compare_all_ols <- lm_robust(cases_per_million ~ I(mm == 1 & rm == 0) +  I(rm == 1) , data = marijuana_data, se_type = "stata")


sym <-""
  if (is.na(compare_all_ols$p.value[1])==FALSE){
    if (compare_all_ols$p.value[1]<=.05) {
      sym <- "*"
    } 
    if (compare_all_ols$p.value[1]<=.01) {
      sym <- "**"
    } 
    if (compare_all_ols$p.value[1]<=.001) {
      sym <- "***"
    }
  }
common_results_table[1,1] <- paste0(round(compare_all_ols$coefficients[1],digits = 2), sym)
common_results_table[2,1] <- paste0("(",round(compare_all_ols$std.error[1],digits = 2),")")
common_results_table[3,1] <- paste0("[",round(compare_all_ols$p.value[1],digits = 2),"]")
common_results_table[4,1] <- paste0("(",round(compare_all_ols$conf.low[1],digits = 2)," to ",round(compare_all_ols$conf.high[1],digits = 2),")")


sym <-""
if (is.na(compare_all_ols$p.value[2])==FALSE){
  if (compare_all_ols$p.value[2]<=.05) {
    sym <- "*"
  } 
  if (compare_all_ols$p.value[2]<=.01) {
    sym <- "**"
  } 
  if (compare_all_ols$p.value[2]<=.001) {
    sym <- "***"
  }
}
common_results_table[6,1] <- paste0(round(compare_all_ols$coefficients[2],digits = 2), sym)
common_results_table[7,1] <- paste0("(",round(compare_all_ols$std.error[2],digits = 2),")")
common_results_table[8,1] <- paste0("[",round(compare_all_ols$p.value[2],digits = 2),"]")
common_results_table[9,1] <- paste0("(",round(compare_all_ols$conf.low[2],digits = 2)," to ",round(compare_all_ols$conf.high[2],digits = 2),")")

sym <-""
if (is.na(compare_all_ols$p.value[3])==FALSE){
  if (compare_all_ols$p.value[3]<=.05) {
    sym <- "*"
  } 
  if (compare_all_ols$p.value[3]<=.01) {
    sym <- "**"
  } 
  if (compare_all_ols$p.value[3]<=.001) {
    sym <- "***"
  }
}
common_results_table[11,1] <- paste0(round(compare_all_ols$coefficients[3],digits = 2), sym)
common_results_table[12,1] <- paste0("(",round(compare_all_ols$std.error[3],digits = 2),")")
common_results_table[13,1] <- paste0("[",round(compare_all_ols$p.value[3],digits = 2),"]")
common_results_table[14,1] <- paste0("(",round(compare_all_ols$conf.low[3],digits = 2)," to ",round(compare_all_ols$conf.high[3],digits = 3),")")

common_results_table[16,1] <- nrow(marijuana_data)

# Using Poisson count model
compare_all_poisson <-poissonmfx(round(mid_point) ~  I(mm == 1 & rm == 0) + I(rm == 1) + offset(log(pop_total/1000000)), data = marijuana_data)
# Recreational marijuana adoption is associated with -3.05 per million (roughly 40% of mean) 


sym <-""
if (is.na(compare_all_poisson$mfxest[1,4])==FALSE){
  if (compare_all_poisson$mfxest[1,4]<=.05) {
    sym <- "*"
  } 
  if (compare_all_poisson$mfxest[1,4]<=.01) {
    sym <- "**"
  } 
  if (compare_all_poisson$mfxest[1,4]<=.001) {
    sym <- "***"
  }
}
common_results_table[6,2] <- paste0(round(compare_all_poisson$mfxest[1,1],digits = 2), sym)
common_results_table[7,2] <- paste0("(",round(compare_all_poisson$mfxest[1,2],digits = 2),")")
common_results_table[8,2] <- paste0("[",round(compare_all_poisson$mfxest[1,4],digits = 2),"]")
common_results_table[9,2] <- paste0("(",
                                    round(compare_all_poisson$mfxest[1,1] - qt(0.975, compare_all_poisson[["fit"]][["df.residual"]] -1)*compare_all_poisson$mfxest[1,2],digits = 2),
                                    " to ",
                                    round(compare_all_poisson$mfxest[1,1] + qt(0.975, compare_all_poisson[["fit"]][["df.residual"]] -1)*compare_all_poisson$mfxest[1,2],digits = 2),
                                    ")")

sym <-""
if (is.na(compare_all_poisson$mfxest[2,4])==FALSE){
  if (compare_all_poisson$mfxest[2,4]<=.05) {
    sym <- "*"
  } 
  if (compare_all_poisson$mfxest[2,4]<=.01) {
    sym <- "**"
  } 
  if (compare_all_poisson$mfxest[2,4]<=.001) {
    sym <- "***"
  }
}
common_results_table[11,2] <- paste0(round(compare_all_poisson$mfxest[2,1],digits = 2), sym)
common_results_table[12,2] <- paste0("(",round(compare_all_poisson$mfxest[2,2],digits = 2),")")
common_results_table[13,2] <- paste0("[",round(compare_all_poisson$mfxest[2,4],digits = 2),"]")
common_results_table[14,2] <- paste0("(",
                                    round(compare_all_poisson$mfxest[2,1] - qt(0.975, compare_all_poisson[["fit"]][["df.residual"]] -1)*compare_all_poisson$mfxest[2,2],digits = 2),
                                    " to ",
                                    round(compare_all_poisson$mfxest[2,1] + qt(0.975, compare_all_poisson[["fit"]][["df.residual"]] -1)*compare_all_poisson$mfxest[2,2],digits = 2),
                                    ")")

common_results_table[16,2] <- nrow(marijuana_data)

hux_common_results_table <- as_hux(common_results_table, add_colnames = TRUE)


hux_common_results_table <- insert_row(hux_common_results_table,
                      c("(1)", "(2)"),
                      after = 0
)

hux_common_results_table <- insert_column(hux_common_results_table,
                                       c("", "", 
                                         "Intercept", "",  "",  "",  "",
                                         "Medical marijuana only", "",  "",  "",  "",
                                         "Recreational marijuana", "",  "",  "",  "", 
                                         "N"
                                         ),
                                       after = 0
)


hux_common_results_table <- hux_common_results_table %>%
  set_top_border(1, 1:3, 1)     %>%
  set_bottom_border(2, 2:3, 1)     %>%
  set_top_border(18, 2:3, 1)     %>%
  set_bottom_border(18, 1:3, 1)   %>%
  set_align(1:18, 2:3, "center")  

# Add title
caption(hux_common_results_table) <- "Table 5: The association between the EVALI case rate per million and marijuana policy while controling for both medical and recreational"

# Display table
hux_common_results_table



wb <- as_Workbook(hux_common_results_table)

openxlsx::saveWorkbook(wb,
                       "output/evali_common_regression.xlsx")




##########################################################################################################
###################################################################
# Calculate group means of e-cigarette use by MJ policy

mean_ecig_by_mj_use <- marijuana_data %>%
  group_by(mj_policy) %>%
  summarise(
    mean = mean(ecigarette_use), 
    sd = sd(ecigarette_use),
    n = n()
  )  %>%
  mutate(se = sd/sqrt(n),
         lower_ci = mean - qt(0.975, n -1)*se,
         upper_ci = mean + qt(0.975, n -1)*se)


# Use regression  so we can easily calculate the 95% CI and compare groups using huxreg package
ecig_no_mj_mean <- lm_robust(ecigarette_use ~ 1, data = subset(marijuana_data, mj_policy == 2), se_type = "stata")
ecig_mm_mean <- lm_robust(ecigarette_use ~ 1, data = subset(marijuana_data, mj_policy == 1), se_type = "stata")
ecig_rm_mean <- lm_robust(ecigarette_use ~ 1, data = subset(marijuana_data, mj_policy == 3), se_type = "stata")

# Store results in table 
TableOfMeans.ecig <- huxreg("Prohibition" = ecig_no_mj_mean,  "Medical Only" = ecig_mm_mean, "Recreational" = ecig_rm_mean,
                       align = "center",
                       number_format = "%.2f",
                       error_format = '({conf.low} to {conf.high})', 
                       statistics = c(N = "nobs"), 
                       stars = NULL, 
                       coefs = c("Mean" = "(Intercept)"), 
                       note = "Note: 95% confidence interval calculated using robust standard errors in parantheses.")
# Add title
caption(TableOfMeans.ecig) <- "Table 3: Mean e-cigarette use prevalence by marijuana policy"

# Display table
TableOfMeans.ecig

# Export table to word document
TableOfMeansForWord.ecig <- as_FlexTable(TableOfMeans.ecig)
TableOfMeansForWord.ecig <-  fontsize(TableOfMeansForWord.ecig, size = 12)
TableOfMeansForWord.ecig <- width(TableOfMeansForWord.ecig, width = 1.25)
my_doc <- officer::read_docx()
my_doc <- flextable::body_add_flextable(
  my_doc, TableOfMeansForWord.ecig)
print(my_doc, target =
        "output/ecigarette_use_means.docx")

###################################################################
# Calculate difference between group means of EVALI case rate by MJ policy

# Difference between Rec and Medical
ecig_compare_rm_mm <- lm_robust(ecigarette_use ~ rm_disp, data = subset(marijuana_data, mm == 1), se_type = "stata")

# Difference between Rec and prohibition
ecig_compare_rm_prohib <- lm_robust(ecigarette_use ~ rm_disp, data = subset(marijuana_data, mm == 0 | rm_disp ==1), se_type = "stata")

# Difference between medical and prohibition
ecig_compare_mm_prohib <- lm_robust(ecigarette_use ~ mm, data = subset(marijuana_data, mm == 1 & rm_disp == 0 | mm == 0), se_type = "stata")


TableOfDifferences.ecig <- huxreg("Medical v\n Prohibition" = ecig_compare_mm_prohib,  "Prohibition v\n Recreational" = ecig_compare_rm_prohib, "Medical v\n Recreational" = ecig_compare_rm_mm,
                             align = "center",
                             number_format = "%.3f",
                             error_format = '({std.error}) \n [{p.value}] \n ({conf.low} to {conf.high})', 
                             statistics = c(N = "nobs"), 
                             coefs = c("Difference" = "mm", "Difference" = "rm_disp"), 
                             note = "Note: Difference between means of groups reported with the robust standard error of the difference reported in parantheses below. P-values reported in brackets. P-values also represented by stars with * p < 0.05, ** p < 0.01, *** p < 0.001. 95% confidence interval of difference in means calculated using robust standard errors in parantheses.")

# Add title
caption(TableOfDifferences.ecig) <- "Table 4: Difference in mean e-cigarette use prevalence by marijuana policy"

# Display table
TableOfDifferences.ecig

# Export table to word document
TableOfDifferencesForWord.ecig <- as_FlexTable(TableOfDifferences.ecig)
TableOfDifferencesForWord.ecig <-  fontsize(TableOfDifferencesForWord.ecig, size = 12)
TableOfDifferencesForWord.ecig <- width(TableOfDifferencesForWord.ecig, width = 1.25)
my_doc <- officer::read_docx()
my_doc <- flextable::body_add_flextable(
  my_doc, TableOfDifferencesForWord.ecig)
print(my_doc, target =
        "output/ecigarette_use_difference_in_means.docx")




#################################################################################
#################################################################################
#################################################################################
# Plot data 
#################################################################################
#################################################################################
#################################################################################

# Make a custom color palette 
cbPalette <- c("#3F3DCB", 
               "#d6d4d3", 
               "#0CC693")


#################################################################################
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
  scale_x_discrete(labels = c(paste0("Medicial\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                    filter(mj_policy == 1) %>%
                                                                    dplyr::select(c(n)),
                                                                  digits = 3),1),")"), 
                              paste0("Prohibition\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                       filter(mj_policy == 2) %>%
                                                                       dplyr::select(c(n)),
                                                                     digits = 3),1),")"), 
                              paste0("Recreational\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                        filter(mj_policy == 3) %>%
                                                                        dplyr::select(c(n)),
                                                                      digits = 3),1),")"))) +
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

ggsave("output/exhibit_1a.pdf", plot = combined_plot,dpi = 1200, width = 6, height = 15, units = "in")
ggsave("output/exhibit_1a.png", plot = combined_plot,dpi = 1200, width = 6, height = 15, units = "in")

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
  scale_x_discrete(labels = c(paste0("Medicial\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                    filter(mj_policy == 1) %>%
                                                                    dplyr::select(c(n)),
                                                                  digits = 3),1),")"), 
                              paste0("Prohibition\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                       filter(mj_policy == 2) %>%
                                                                       dplyr::select(c(n)),
                                                                     digits = 3),1),")"), 
                              paste0("Recreational\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                        filter(mj_policy == 3) %>%
                                                                        dplyr::select(c(n)),
                                                                      digits = 3),1),")"))) +
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
ggsave("output/exhibit_1b.pdf", plot = combined_plot_ecig,dpi = 1200, width = 6, height = 15, units = "in")
ggsave("output/exhibit_1b.png", plot = combined_plot_ecig,dpi = 1200, width = 6, height = 15, units = "in")

# Combine the two combined plots
combine_both_plot <- plot_grid(combined_plot, combined_plot_ecig, 
                               ncol = 2, 
                               labels = c('A', 'B'),
                               rel_heights = c(1,1))
combine_both_plot
ggsave("output/main_exhibit.pdf", plot = combine_both_plot,dpi = 1200, width = 12, height = 15, units = "in")
ggsave("output/main_exhibit.png", plot = combine_both_plot,dpi = 300, width = 12, height = 15, units = "in")


##########################################################################################################
# Code that I ran in stata. Confirming that we are using the robust standard errors we think we are. 

#reg cases_per_million i.rm_disp if rm_disp == 1 | mm == 0, robust
#reg cases_per_million i.rm_disp if rm_disp == 1 | mm == 1, robust
#reg cases_per_million i.mm if rm_disp == 1 | mm == 1, robust
#reg cases_per_million i.mm if rm_disp == 0, robust

##########################################################################################################
# Redo the above but add in bounds for case rates and 95% CI

# Make sure upper and lower bounds are in cases per million 
marijuana_data$lower_bound <- (marijuana_data$lower_bound/marijuana_data$pop_total)*1000000
marijuana_data$upper_bound <- (marijuana_data$upper_bound/marijuana_data$pop_total)*1000000

  #################################################################################
# Make a dot plot of EVALI case rate, highlighting state MJ policy
dot_plot_with_bounds <- ggplot(marijuana_data, aes(x = cases_per_million, y = reorder(state, -cases_per_million))) +
  geom_errorbarh(aes(y = reorder(state, -cases_per_million), xmax = upper_bound, xmin = lower_bound, height = .2)) +
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
  scale_x_continuous(limits = c(0,65)) +
  labs(x="", 
       y = "",
       title = "States with recreational marijuana have lower\n rates of e-cigarette/vaping associated lung\n injury (EVALI)") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=15),
        axis.title.x=element_text(size = 18),
        title=element_text(size = 14))

dot_plot_with_bounds

# From: https://rstudio-pubs-static.s3.amazonaws.com/52698_50ff69eef1524ec5a3f082a99da4c704.html
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

dt <- summarySE(data = marijuana_data, measurevar = "cases_per_million", groupvars = c("mj_policy"))


# Make a bar plot of mean EVALI case rate by state MJ policy
bar_plot_with_ci <- ggplot(dt, aes(x = as.factor(mj_policy), y = cases_per_million, fill=as.factor(mj_policy),color=as.factor(mj_policy))) + 
  theme_bw() + 
  geom_bar(stat="identity", position = position_dodge()) + # adding bar plo
  geom_errorbar(aes(ymin = cases_per_million - se, ymax = cases_per_million + se),  
                width=.1, size = 1,                  # Width of the error bars
                position=position_dodge(.9), color = "white") + 
  geom_errorbar(aes(ymin = cases_per_million - se, ymax = cases_per_million + se),  
    width=.1,       size = .5,                # Width of the error bars
    position=position_dodge(.9), color = "black") +
  coord_flip() +
  theme_classic() + 
  scale_color_manual(breaks = c("1", "2", "3"),
                     values=c("black", "black", "black")) + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=cbPalette) +
  scale_y_continuous(limits = c(0,65)) +
  theme(legend.position = "none") +
  labs(y="Cases per million population", x = "") +
  scale_x_discrete(labels = c(paste0("Medicial\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                    filter(mj_policy == 1) %>%
                                                                    dplyr::select(c(n)),
                                                                  digits = 3),1),")"), 
                              paste0("Prohibition\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                    filter(mj_policy == 2) %>%
                                                                      dplyr::select(c(n)),
                                                                  digits = 3),1),")"), 
                              paste0("Recreational\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                    filter(mj_policy == 3) %>%
                                                                      dplyr::select(c(n)),
                                                                  digits = 3),1),")"))) +
  annotate("text", label = "    6.4**", x = 2.55, y = 14.5, color = "black", size = 5) +
  geom_segment(x = 3, xend = 2.1, 
               y = 12.5, yend = 12.5,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 11.5, yend =12.5 ,
               colour = "black") +
  geom_segment(x = 2.1, xend = 2.1, 
               y = 11.5, yend = 12.5,
               colour = "black") +
  annotate("text", label = "  0.7", x = 1.5, y = 14.5, color = "black", size = 5) +
  geom_segment(x = 1, xend = 1.9, 
               y = 12.5, yend = 12.5,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 11.5, yend =12.5 ,
               colour = "black") +
  geom_segment(x = 1.9, xend = 1.9, 
               y = 11.5, yend = 12.5,
               colour = "black")  +
  annotate("text", label = "      8.8 - 1.7 = 7.1***", x = 2, y = 30, color = "black", size = 5) +
  geom_segment(x = 1, xend = 3, 
               y = 21, yend = 21,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 20, yend =21 ,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 20, yend = 21,
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
       ME, MA, MI, NV, OR, VT, WA) to be a recreational state.
       Bars in top panel reflect range of EVALI cases per million. 
       Brackets in the bottom panel display 95% confidence interval
       of each group mean.") +
  theme(axis.title.x=element_text(size = 18),
        title=element_text(size = 15),
        plot.caption = element_text(hjust = 0))

bar_plot_with_ci

# Combine the two plots
combined_plot_with_bars <- plot_grid(dot_plot_with_bounds, bar_plot_with_ci, ncol = 1, align = 'v', rel_heights = c(2,1))
combined_plot_with_bars

ggsave("output/exhibit_1a_with_bars.pdf", plot = combined_plot_with_bars,dpi = 1200, width = 6, height = 15, units = "in")
ggsave("output/exhibit_1a_with_bars.png", plot = combined_plot_with_bars,dpi = 1200, width = 6, height = 15, units = "in")


dt_ecig <- summarySE(data = marijuana_data, measurevar = "ecigarette_use", groupvars = c("mj_policy"))


# Make a bar plot of mean EVALI case rate by state MJ policy
bar_plot_ecig_with_ci <- ggplot(dt_ecig, aes(x = as.factor(mj_policy), y = ecigarette_use, fill=as.factor(mj_policy),color=as.factor(mj_policy))) + 
  theme_bw() + 
  geom_bar(stat="identity", position = position_dodge()) + # adding bar plo
  geom_errorbar(aes(ymin = ecigarette_use - se, ymax = ecigarette_use + se),  
                width=.1, size = 1,                  # Width of the error bars
                position=position_dodge(.9), color = "white") + 
  geom_errorbar(aes(ymin = ecigarette_use - se, ymax = ecigarette_use + se),  
                width=.1,       size = .5,                # Width of the error bars
                position=position_dodge(.9), color = "black") +
  coord_flip() +
  theme_classic() + 
  scale_color_manual(breaks = c("1", "2", "3"),
                     values=c("black", "black", "black")) + 
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=cbPalette) +
  scale_y_continuous(limits = c(0,9)) +
  theme(legend.position = "none") +
  labs(y="Prevalence of e-cigarette use (0-100%)", x = "") +
  scale_x_discrete(labels = c(paste0("Medicial\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                    filter(mj_policy == 1) %>%
                                                                    dplyr::select(c(n)),
                                                                  digits = 3),1),")"), 
                              paste0("Prohibition\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                       filter(mj_policy == 2) %>%
                                                                       dplyr::select(c(n)),
                                                                     digits = 3),1),")"), 
                              paste0("Recreational\n (N = ",nth(round(mean_evali_by_mj_use %>% 
                                                                        filter(mj_policy == 3) %>%
                                                                        dplyr::select(c(n)),
                                                                      digits = 3),1),")"))) +
  annotate("text", label = "  0.4", x = 2.55, y = 5.8 , color = "black", size = 5) +
  geom_segment(x = 3, xend = 2.1, 
               y = 5.5, yend = 5.5,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 5.25, yend =5.5 ,
               colour = "black") +
  geom_segment(x = 2.1, xend = 2.1, 
               y = 5.25, yend = 5.5,
               colour = "black") +
  annotate("text", label = " -0.7", x = 1.5, y = 5.8, color = "black", size = 5) +
  geom_segment(x = 1, xend = 1.9, 
               y = 5.5, yend = 5.5,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 5.25, yend =5.5 ,
               colour = "black") +
  geom_segment(x = 1.9, xend = 1.9, 
               y = 5.25, yend = 5.5,
               colour = "black")  +
  annotate("text", label = "0.3", x = 2, y = 7.5, color = "black", size = 5) +
  geom_segment(x = 1, xend = 3, 
               y = 7, yend = 7,
               colour = "black") +
  geom_segment(x = 1, xend = 1, 
               y = 6.75, yend =7 ,
               colour = "black") +
  geom_segment(x = 3, xend = 3, 
               y = 6.75, yend = 7,
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
       ME, MA, MI, NV, OR, VT, WA) to be a recreational state.
       Brackets in the bottom panel display 95% confidence interval
       of each group mean.
       ") +
  theme(axis.title.x=element_text(size = 18),
        title=element_text(size = 15),
        plot.caption = element_text(hjust = 0))

bar_plot_ecig_with_ci

# Combine the two plots
combined_plot_ecig_with_bars <- plot_grid(dot_plot_ecig, bar_plot_ecig_with_ci, ncol = 1, align = 'v', rel_heights = c(2,1))
combined_plot_ecig_with_bars
ggsave("output/exhibit_1b_with_bars.pdf", plot = combined_plot_ecig_with_bars,dpi = 1200, width = 6, height = 15, units = "in")
ggsave("output/exhibit_1b_with_bars.png", plot = combined_plot_ecig_with_bars,dpi = 1200, width = 6, height = 15, units = "in")

# Combine the two combined plots
combine_both_plot_with_bars <- plot_grid(combined_plot_with_bars, combined_plot_ecig_with_bars, 
                               ncol = 2, 
                               labels = c('A', 'B'),
                               rel_heights = c(1,1))
combine_both_plot
ggsave("output/main_exhibit_with_bars.pdf", plot = combine_both_plot_with_bars,dpi = 1200, width = 12, height = 15, units = "in")
ggsave("output/main_exhibit_with_bars.png", plot = combine_both_plot_with_bars,dpi = 300, width = 12, height = 15, units = "in")


##########################################################################################################
##########################################################################################################
##########################################################################################################
# Examine relationship between evali and e-cig use
##########################################################################################################
##########################################################################################################
##########################################################################################################

##########################################################################################################
# Using regression

#  EVALI case  rate v e-cigarette use
model_evali_ecig <- lm_robust(cases_per_million ~ ecigarette_use, data = marijuana_data, se_type = "stata")

#  EVALI case  rate v e-cigarette use. Weighted by population.
model_evali_ecig_weighted <- lm_robust(cases_per_million ~ ecigarette_use, data = marijuana_data, se_type = "stata", weights = pop_total)

#  EVALI case  rate v e-cigarette use; controlling for MJ policy 
model_evali_ecig_and_mj_policies <- lm_robust(cases_per_million ~ ecigarette_use + I(rm == 1) + I(mm == 1 & rm == 0), data = marijuana_data, se_type = "stata")

#  EVALI case  rate v e-cigarette use; controlling for MJ policy. Weighted by population. 
model_evali_ecig_and_mj_policies_weighted <- lm_robust(cases_per_million ~ ecigarette_use + I(rm == 1) + I(mm == 1 & rm == 0), data = marijuana_data, se_type = "stata", weights = pop_total)
huxreg(model_evali_ecig_and_mj_policies_weighted)

Table.EVALIvEcig <- huxreg( model_evali_ecig, model_evali_ecig_and_mj_policies,
                                  align = "center",
                                  number_format = "%.3f",
                                  error_format = '({std.error}) \n [{p.value}] \n ({conf.low} to {conf.high})', 
                                  statistics = c(N = "nobs"), 
                                  coefs = c("Intercept" = "(Intercept)", 
                                            "E-cigarette use (0-100%)" = "ecigarette_use", 
                                            "Medical marijuana only" = "I(mm == 1 & rm == 0)TRUE", 
                                            "Recreational marijuana" = "I(rm == 1)TRUE"), 
                                  note = "Note: Robust standard error reported in parantheses below. P-values reported in brackets. P-values also represented by stars with * p < 0.05, ** p < 0.01, *** p < 0.001. 95% confidence interval calculated using robust standard errors in parantheses.")

# Add title
caption(Table.EVALIvEcig) <- "Table 5: The association of lower EVALI case rates and recreational marijuana is robust to controlling for prevalence of e-cigarette use and
there is no clear relationship between EVALI and e-cigarette use"

# Display table
Table.EVALIvEcig

# Export table to word document
TableForWord.EVALIvEcig <- as_FlexTable(Table.EVALIvEcig)
TableForWord.EVALIvEcig <-  fontsize(TableForWord.EVALIvEcig, size = 12)
TableForWord.EVALIvEcig <- width(TableForWord.EVALIvEcig, width = 2)
my_doc <- officer::read_docx()
my_doc <- flextable::body_add_flextable(
  my_doc, TableForWord.EVALIvEcig)
print(my_doc, target =
        "output/evali_and_ecig.docx")




##########################################################################################################
# Using figure

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
ggsave("output/exhibit_3.pdf", plot = p, dpi = 1200, width = 8, height = 6, units = "in")
ggsave("output/exhibit_3.png", plot = p, dpi = 300, width = 8, height = 6, units = "in")
