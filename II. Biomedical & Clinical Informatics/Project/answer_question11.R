data.frame(pneumoniaAnalyzeDemo$`Death rate for pneumonia patients`,
           imputedPneumoniaAnalyzeDemo$`Death rate for pneumonia patients`) %>% 
  rename(Before = `pneumoniaAnalyzeDemo..Death.rate.for.pneumonia.patients.`,
         After = `imputedPneumoniaAnalyzeDemo..Death.rate.for.pneumonia.patients.`) %>% 
  pivot_longer(1:2, values_to = "Value", names_to = "Variable") %>% 
  ggplot(aes(x = Value, fill = Variable)) +
  geom_histogram(alpha = 0.8, 
                 bins = 20,
                 color = prettyPurples[3]) + 
  scale_fill_manual(values = c(prettyPurples[3], prettyPurples[8])) +
  labs(title = "Pneumonia-related Death Rate",
       y = "Frequency",
       fill = "Imputation", 
       x = "Pneumonia-related Death Rate") +
  theme_minimal()

data.frame(pneumoniaAnalyzeDemo$`ComparedToNational_Hospital return days for pneumonia patients`,
           imputedPneumoniaAnalyzeDemo$`ComparedToNational_Hospital return days for pneumonia patients`) %>% 
  rename(Before = `pneumoniaAnalyzeDemo..ComparedToNational_Hospital.return.days.for.pneumonia.patients.`,
         After = `imputedPneumoniaAnalyzeDemo..ComparedToNational_Hospital.return.days.for.pneumonia.patients.`) %>% 
  pivot_longer(1:2, values_to = "Value", names_to = "Variable") %>% 
  ggplot(aes(x = Value, fill = Variable)) +
  geom_bar(alpha = 0.8,
           color = prettyPurples[3]) +
  scale_fill_manual(values = c(prettyPurples[3], prettyPurples[8])) +
  labs(title = "Compared to National: Pneumonia-related Hospital Return Days",
       y = "Frequency",
       fill = "Imputation", 
       x = "Compared to National: Pneumonia-related Hospital Return Days") +
  theme_minimal()
