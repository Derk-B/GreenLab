install.packages("dplyr")
install.packages("tidyverse")
install.packages('ggplot2')
install.packages('hmisc')
install.packages('ARTool')
install.packages('ggpubr')
install.packages('xtable')
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(xtable)

rawruntable <- read.csv("~/Documents/vrije_universiteit/Greenlab/runtable.csv", stringsAsFactors = TRUE)

datasetsizes <-data.frame(dataset = as.factor(c('low_spar_TPM.csv', 'low_spar_CPM.csv', 'high_spar_TPM.csv', 'high_spar_CPM.csv', 'GDS3900_TPM.csv', 'GDS3900_CPM.csv')),
                          size_.B. = c(4800000, 4800000, 4800000, 4800000, 132900, 132900),
                          sparsity = c(0.6993, 0.6999, 0.7999, 0.7995, 0.81, 0.29))

runtable <- rawruntable %>%  
  left_join(datasetsizes) %>% 
  transform(preprocessing = as_factor(with(rawruntable, ifelse(grepl('CPM', dataset), 'CPM', 'TPM')))) %>% 
  transform(dataset = as_factor(with(rawruntable, ifelse(grepl('low_spar', dataset), 'low_spar', ifelse(grepl('high_spar', dataset), 'high_spar', 'GDS3900')))))  %>% 
  filter(energy_consumption_.J. > 0)  %>% 
  filter(!(dataset != "GDS3900" & energy_consumption_.J. >= 1200))

summary_stats_CPU <- runtable %>% 
  subset(hardware == "CPU") %>%
  group_by(dataset, preprocessing) %>%
  summarise(mean_energy = mean(energy_consumption_.J.), mean_exec = mean(execution_time_.ms.), mean_mem = mean(peak_memory_.B.), .groups = "drop")

summary_table_CPU <- xtable(summary_stats_CPU,
                        caption = "Summary Statistics by Dataset and Preprocessing type on CPU",
                        digits = c(0, 0, 0, 2, 2, 0))

print(summary_table_CPU, 
      include.rownames = TRUE,
      floating = FALSE)

summary_stats_GPU <- runtable %>% 
  subset(hardware == "GPU") %>%
  group_by(dataset, preprocessing) %>%
  summarise(mean_energy = mean(energy_consumption_.J.), mean_exec = mean(execution_time_.ms.), mean_mem = mean(peak_memory_.B.), .groups = "drop")

summary_table_GPU <- xtable(summary_stats_GPU,
                            caption = "Summary Statistics by Dataset and Preprocessing type on GPU",
                            digits = c(0, 0, 0, 2, 2, 0))

print(summary_table_GPU, 
      include.rownames = TRUE,
      floating = FALSE)

R1_CPU_box_exec_per_data <- ggplot(subset(runtable, hardware =="CPU" ), aes(x = preprocessing, y = execution_time_.ms., fill = preprocessing)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = "free") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               size = 2,
               color = "blue") +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Execution time per Dataset on CPU",
    x = "Preprocessing",
    y = "Execution time (in Milliseconds)",
    fill = "Preprocessing"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none"
  )

R1_CPU_box_exec_per_data

R1_GPU_box_exec_per_data <- ggplot(subset(runtable, hardware =="GPU" ), aes(x = preprocessing, y = execution_time_.ms., fill = preprocessing)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = "free") +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               size = 2,
               color = "blue") +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Execution time per Dataset on GPU",
    x = "Preprocessing",
    y = "Execution time (in Milliseconds)",
    fill = "Preprocessing"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none"
  )

R1_GPU_box_exec_per_data

R1_energy <- ggplot(subset(runtable, dataset!="GDS3900"), aes(x = sparsity, y=energy_consumption_.J., color = hardware)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~hardware, scales="free") +
  scale_colour_brewer(palette="Dark2") +
  labs(
    title = "Energy Usage by Dataset size",
    x = "Sparsity",
    y = "Energy Usage (in Joules)",
    color = "Hardware"
  )  +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
  )

R1_energy

R1_memory <- ggplot(subset(runtable, dataset!="GDS3900"), aes(x = sparsity, y=peak_memory_.B., color = hardware)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~hardware, scales="free") +
  scale_colour_brewer(palette="Dark2") +
  labs(
    title = "Peak memory usage by Dataset size",
    x = "Sparsity",
    y = "Peak memory usage (in Bytes)",
    color = "Hardware"
  )  +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
  )

R1_memory

R1_exec <- ggplot(subset(runtable, dataset!="GDS3900"), aes(x = sparsity, y=execution_time_.ms., color = hardware)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~hardware, scales="free") +
  scale_colour_brewer(palette="Dark2") +
  labs(
    title = "Execution time by Dataset size",
    x = "Sparsity",
    y = "Execution time (in Milliseconds)",
    color = "Hardware"
  )  +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
  )

R1_exec

R2_bar_exec <- ggplot(runtable, aes(x = preprocessing, y = execution_time_.ms., fill = preprocessing)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3) +
  facet_wrap(~hardware, scales="free") +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Mean execution time per preprocessing type on CPU and GPU",
    x = "Preprocessing",
    y = "Execution time (in Milliseconds)",
    fill = "Preprocessing"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none"
  ) 

R2_bar_exec

R2_bar_energy <- ggplot(runtable, aes(x = preprocessing, y = energy_consumption_.J., fill = preprocessing)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3) +
  facet_wrap(~hardware, scales="free") +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Mean energy usage per preprocessing type on CPU and GPU",
    x = "Preprocessing",
    y = "Energy usage (in Joules)",
    fill = "Preprocessing"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none"
  ) 

R2_bar_energy

R3_bar_energy <- ggplot(runtable, aes(x = hardware, y = energy_consumption_.J., fill = hardware)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3) +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Mean energy usage per hardware type",
    x = "Preprocessing",
    y = "Energy usage (in Joules)",
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none",
    aspect.ratio=1
  ) 

R3_bar_energy

run_table_syn <- runtable[runtable$dataset != 'GDS3900',]

# Plot energy consumtion by hardware and dataset
runtable %>%
  group_by(hardware, preprocessing, dataset) %>%
  group_split() %>%
  lapply(function(group_data) {
    hist(group_data$peak_memory_.B., 
         main = unique(group_data$hardware), 
         xlab = paste("Energy consumption and algo", group_data$dataset))
  })

# Normality by hardware and dataset for the metric: energy consumption
runtable %>%
  group_by(hardware, preprocessing, dataset) %>%
  dplyr::summarize(stest = shapiro.test(energy_consumption_.J.)$p.value)


# Normality by hardware and dataset for the metric: execution time
runtable %>%
  group_by(hardware, preprocessing, dataset) %>%
  dplyr::summarize(stest = shapiro.test(execution_time_.ms.)$p.value)


# Normality by hardware and dataset for the metric: peak memory
runtable %>%
  group_by(hardware, preprocessing, dataset) %>%
  dplyr::summarize(n = n(), stest = shapiro.test(peak_memory_.B.)$p.value)

library(bestNormalize)
runtable$norm_peak_memory_.B. = bestNormalize(runtable$peak_memory_.B.)$x.t
runtable %>%
  group_by(hardware, dataset) %>%
  group_split() %>%
  lapply(function(group_data) {
    hist(group_data$norm_peak_memory_.B., 
         main = unique(group_data$hardware), 
         xlab = paste("Energy consumption", group_data$dataset))
  })
runtable %>%
  group_by(hardware, dataset) %>%
  dplyr::summarize(n = n(), stest = shapiro.test(norm_peak_memory_.B.)$p.value)

shapiro.test(runtable$execution_time_.ms.)
library(ARTool)

# Wilcoxon test: GPU vs CPU by energy consumed
cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'low_spar',]$energy_consumption_.J.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'low_spar',]$energy_consumption_.J.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'high_spar',]$energy_consumption_.J.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'high_spar',]$energy_consumption_.J.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'GDS3900',]$energy_consumption_.J.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'GDS3900',]$energy_consumption_.J.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

# Wilcoxon test: TPM vs CPM by energy consumed
cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'low_spar',]$energy_consumption_.J.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'low_spar',]$energy_consumption_.J.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'high_spar',]$energy_consumption_.J.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'high_spar',]$energy_consumption_.J.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'GDS3900',]$energy_consumption_.J.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'GDS3900',]$energy_consumption_.J.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

# ART test: Sparsity by energy consumed
model = art(energy_consumption_.J. ~ factor(sparsity), data = runtable)
anova(model)

summary(model)

# Wilcoxon test: GPU vs CPU by exeuction time
cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'low_spar',]$execution_time_.ms.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'low_spar',]$execution_time_.ms.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'high_spar',]$execution_time_.ms.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'high_spar',]$execution_time_.ms.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'GDS3900',]$execution_time_.ms.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'GDS3900',]$execution_time_.ms.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

# Wilcoxon test: TPM vs CPM by exeuction time
cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'low_spar',]$execution_time_.ms.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'low_spar',]$execution_time_.ms.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'high_spar',]$execution_time_.ms.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'high_spar',]$execution_time_.ms.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'GDS3900',]$execution_time_.ms.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'GDS3900',]$execution_time_.ms.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

# ART test: Sparsity by exeuction time
model = art(execution_time_.ms. ~ factor(sparsity), data = runtable)
anova(model)

summary(model)

# Wilcoxon test: GPU vs CPU by peak memory
cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'low_spar',]$peak_memory_.B.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'low_spar',]$peak_memory_.B.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'high_spar',]$peak_memory_.B.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'high_spar',]$peak_memory_.B.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

cpu_energy <- runtable[runtable$hardware == 'CPU' & runtable$dataset == 'GDS3900',]$peak_memory_.B.
gpu_energy <- runtable[runtable$hardware == 'GPU' & runtable$dataset == 'GDS3900',]$peak_memory_.B.
wilcox.test(head(gpu_energy, length(cpu_energy)), cpu_energy, paired = TRUE)

# Wilcoxon test: TPM vs CPM by peak memory
cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'low_spar',]$peak_memory_.B.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'low_spar',]$peak_memory_.B.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'high_spar',]$peak_memory_.B.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'high_spar',]$peak_memory_.B.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

cpm <- runtable[runtable$preprocessing == 'CPM' & runtable$dataset == 'GDS3900',]$peak_memory_.B.
tpm <- runtable[runtable$preprocessing == 'TPM' & runtable$dataset == 'GDS3900',]$peak_memory_.B.
wilcox.test(head(cpm, length(tpm)), tpm, paired = TRUE)

# ART test: Sparsity by peak memory
model = art(peak_memory_.B. ~ factor(sparsity), data = runtable[runtable$dataset == 'low_spar',])
anova(model)

summary(model)

result <- runtable %>%
  group_by(sparsity) %>%
  summarise(
    median_value = median(peak_memory_.B.),
    wilcox_statistic = list(wilcox.test(peak_memory_.B.)$statistic),
    p_value = list(wilcox.test(peak_memory_.B.)$p.value)
  )
result$p_value

library(lmerTest)
model = lmer(peak_memory_.B. ~ factor(sparsity), data = runtable)
anova(model)
model2 = lmer(peak_memory_.B. ~ factor(sparsity), data = runtable)



#runtable[runtable$hardware == 'CPU',] %>%
runtable %>%
  group_by(hardware) %>%
  summarise(
    M_statistic = shapiro.test(energy_consumption_.J.)$statistic,
    p_value = shapiro.test(energy_consumption_.J.)$p.value
  )

#runtable %>%

# results <- apply(df[, c("hardware", "preprocessing", "energy_consumption_.J.")], 2, shapiro.test)
  
R3_bar_exec <- ggplot(runtable, aes(x = hardware, y = execution_time_.ms., fill = hardware)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3) +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Mean execution time usage per hardware type",
    x = "Preprocessing",
    y = "Execution time (in Milliseconds)",
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none",
    aspect.ratio=1
  ) 

R3_bar_exec

R3_bar_mem <- ggplot(runtable, aes(x = hardware, y = peak_memory_.B., fill = hardware)) +
  geom_bar(stat = "summary", fun = "mean") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.3) +
  scale_fill_brewer(palette="Dark2") +
  labs(
    title = "Peak memory usage per hardware type",
    x = "Preprocessing",
    y = "Peak memory usage (in Bytes)",
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position="none",
    aspect.ratio=1
  ) 

R3_bar_mem
