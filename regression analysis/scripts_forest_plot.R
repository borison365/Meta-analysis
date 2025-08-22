
library(readxl) 
library(meta)
#Read the excel, prepared in wide format (1 row only per study)
data <- read_excel("C:\\Users\\Utente\\OneDrive\\Humanitas\\ACP\\ACP final_all studies.xlsx", sheet = "reshaped")
data <- as.data.frame(data)
#convert all required variables in numeric (all-cause-mortality)
data$ACM_low <- as.numeric(data$ACM_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$ACM_control <- as.numeric(data$ACM_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (CV mortality)
data$CVM_low <- as.numeric(data$CVM_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$CVM_control <- as.numeric(data$CVM_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (Major CV events)
data$MCVE_low <- as.numeric(data$MCVE_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$MCVE_control <- as.numeric(data$MCVE_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (Stroke)
data$stroke_low <- as.numeric(data$stroke_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$stroke_control <- as.numeric(data$stroke_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (Loss of kidney function)
data$kidney_low <- as.numeric(data$kidney_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$kidney_control <- as.numeric(data$kidney_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (symptomatic hypotension)
data$hypo_low <- as.numeric(data$hypo_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$hypo_control <- as.numeric(data$hypo_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (syncope)
data$syncope_low <- as.numeric(data$syncope_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$syncope_control <- as.numeric(data$syncope_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric (orthostatic hypotension)
data$ortho_low <- as.numeric(data$ortho_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$ortho_control <- as.numeric(data$ortho_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#convert all required variables in numeric
data$falls_low <- as.numeric(data$falls_low)
data$nTOT_low <- as.numeric(data$nTOT_low)
data$falls_control <- as.numeric(data$falls_control)
data$nTOT_control <- as.numeric(data$nTOT_control)
#Prepare the subset in which the target for sbp is known
data_sbp <- subset(data, !is.na(target))

#ALL CAUSE MORTALITY
#subset the data to studies with non-missing values
data_clean <- subset(data, !is.na(ACM_low) & !is.na(ACM_control))
#perform the meta
meta_rr <- metabin(event.e = ACM_low, n.e = nTOT_low,
                   event.c = ACM_control, n.c = nTOT_control,
                   studlab = Study,
                   data = data_clean,
                   sm = "RR",
                   method = "Inverse",
                   common = FALSE,
                   random = TRUE,
                   method.tau = "REML", incr = "TACC")
forest(meta_rr)
print(meta_rr)
#save the meta in pdf
pdf("forest_metaACM_rr.pdf", width = 10, height = 7)  # Increase height as needed
print(meta_rr)
forest(meta_rr)
dev.off()

#Funnel & Egger
pdf("funnel_ACM.pdf", width = 10, height = 10) 
funnel_ACM <- funnel(meta_rr, 
                     common = FALSE, # Match random-effects model
                     xlab = "Risk Ratio", # Label for x-axis (effect size)
                     studlab = TRUE,      # Show study labels
                     contour = c(0.9, 0.95, 0.99), # Contour lines for significance
                     col.contour = c("gray50", "gray75", "gray90"))
dev.off()

metabias(meta_rr) # Requires ≥10 studies

#Metaregression for all cause mortality and target of BP
meta_sbp <- metabin(event.e = ACM_low, n.e = nTOT_low,
                    event.c = ACM_control, n.c = nTOT_control,
                    studlab = Study,
                    data = data_sbp,
                    sm = "RR",
                    method = "Inverse",
                    random = TRUE,
                    method.tau = "REML", incr = "TACC")
metareg_sbp <- metareg(meta_sbp, ~ target)
summary(metareg_sbp)


A more detailed account of the design, planning, conduct, analysis, and reporting of IPD meta-analysis projects can be found in the book edited by Riley, Tierney, and Stewart [2].