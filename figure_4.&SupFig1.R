library(ggplot2)
library(dplyr)

data(ToothGrowth)

# Supplementary Figure 1. Projected visual acuity change over time among patients forecasted to initiate anti-VEGF therapy and not matched to the MARINA sham arm.
# i.e. , this is This figure for Matched vs. Non-matched

ToothGrowth %>% 
  ggplot(aes(x = factor(dose), y = len, color = supp, fill = factor(supp,
                                                                    levels = c("OJ", "VC"),
                                                                    labels = c("Matched", "Non-Matched")))) + 
  geom_boxplot() + 
  theme_bw() +
  labs(
    color= "",
    fill = "",
    x = "Months post-baseline",
    y = "Mean change in visual acuity (ETDRS letters)") + 
  scale_color_manual(values = c("black","#1380A1")) +
  scale_fill_manual(values = c("#1380A1","white")) 



## Use this color for UHB cohort please
scale_color_manual(values = c("black","#588300")) +
  scale_fill_manual(values = c("#588300","white")) 


#Figure 4. Projected visual acuity change over time among patients forecasted to initiate anti-VEGF therapy and matched to the MARINA sham arm.
# i.e. Figure is for natched patients (MEH vs. UHB)

ToothGrowth %>% 
  ggplot(aes(x = factor(dose), y = len, color = supp, fill = factor(supp,
                                                                    levels = c("OJ", "VC"),
                                                                    labels = c("MEH", "UHB")))) + 
  geom_boxplot() + 
  theme_bw() +
  labs(
    color= "",
    fill = "",
    x = "Months post-baseline",
    y = "Mean change in visual acuity (ETDRS letters)") + 
  scale_color_manual(values = c("black","black")) +
  scale_fill_manual(values = c("#1380A1","#588300")) 

#Please ggsave as PNG in a couple of different dimeansion (square, flat, tall)
ggsave(x, file=".png", width = , height =)
