#Necessary packages
library(xlsx)
library(ggplot2)
library(cowplot)
library(grid)
library(patchwork)
library(jpeg)
library(ggpmisc)
library(ggpubr)
library(tidyr)

#Figure 1 data (Calibration curve for iron)
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 10)

#Calibration curve for iron
p5 <- ggplot(mart23, aes(x = x, y = conc)) + geom_point(size = 2, col = "gray27") +
  stat_poly_line(col = "gray65") +
  scale_color_manual(values = c("hotpink1", "goldenrod2"), labs(color='Группа' )) +
  theme(text = element_text(size = 18, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 18)) + 
  ylab(expression("OD"["390 nm"])) + 
  xlab("L-lactate nominal concentration, mM") +
  xlim(0, 15) +
  ylim(0, 2) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')) +
  theme(plot.title = element_text(hjust = 0.02)) +
  theme(title = ggtext::element_markdown()) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  annotate("text", x = c(3.5), y = c(1.8),          #Linear regression formula
           label = 'atop("y = 0.143x + 0.1")', cex=7, parse = TRUE, family = "Arial") +
  annotate("text", x = c(3.5), y = c(1.72), 
           label = "R² = 0.98", cex=7, family = "Arial") 



#Figure 1 data (Linear regression of the concentration of initial lactic acid solutions)
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 24)

#Linear regression of the concentration of initial lactic acid solutions
p6 <- ggplot(mart23, aes(x = NomCONC, y = RealCONC)) + geom_point(size = 2, col = "gray27") +
  stat_poly_line( col = "gray65") +
  theme(legend.position="bottom", legend.title=element_blank()) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, col = "tomato1", linetype = "dashed") +
  theme(text = element_text(size = 18, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 18)) + 
  ylab('L-lactate by LOx-LD, mM') + 
  xlab('L-lactate nominal concentration, mM') +
  expand_limits(y = 0, x = 0) +
  xlim(0, 15) +
  ylim(0,15) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')) +
  theme(plot.title = element_text(hjust = 0.02)) +
  theme(title = ggtext::element_markdown()) +
  annotate("text", x = c(3.5), y = c(13.2),          #Linear regression formula  
           label = 'atop("y = 1.03x - 0.392")', cex=7, parse = TRUE, family = "Arial") +
  annotate("text", x = c(3.5), y = c(12.7), 
           label = "R² = 0.99", cex=7, family = "Arial") +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97'))


#Creates Fig.1
ggarrange(p6, p5, ncol = 2, labels = c("A", "B"), font.label=list(size=20))

#Save Fig.1
ggsave("Fig1.JPG", width = 27, height = 12, units = "cm", dpi = 300)



#Figure2
#Loading experimental data
allPL <- read.csv("exp_data_fig2.csv")
#Loading experimental data
allPL1 <- read.csv("exp_data_fig2.csv")
#Loading experimental data
allPL2 <- read.csv("exp_data_fig2.csv")
#Conversion of experimental data
allPL1 <- allPL1[, -c(2, 4, 7, 8, 9, 10)]
#Conversion of experimental data
allPL2 <- allPL2[, -c(3, 5, 11, 12, 13, 14)]

#Conversion of experimental data
(f1 = allPL1 %>% 
    pivot_longer(cols =	PVK05mM: lactateD05mM: FeCl11mM: lactateD1mM:	lactateD01mM: PVK1mM:	PVK01mM,
                 names_to = 'Group', 
                 values_to = 'T'))
#Conversion of experimental data
(f2 = allPL2 %>% 
    pivot_longer(cols = PVK2_5mM:	lactateD2_5mM: FeCl11mM: lactateD10mM:	PVK10mM: PVK5.0mM: lactateD5mM,
                 names_to = 'Group', 
                 values_to = 'T'))
#Conversion of experimental data
(f3 = allPL %>% 
    pivot_longer(cols = PVK2_5mM:	PVK05mM:	lactateD2_5mM:	lactateD05mM:	FeCl11mM: lactateD10mM:	PVK10mM: PVK5.0mM:	
                   lactateD5mM: lactateD1mM:	lactateD01mM:	PVK1mM:	PVK01mM,
                 names_to = 'Group', 
                 values_to = 'T'))
#Changing the order of experimental data
f1$Group <- factor(f1$Group, levels = c("FeCl11mM", "PVK01mM", 
                                        "lactateD01mM", "PVK05mM", 
                                        "lactateD05mM", "lactateD1mM", "PVK1mM"))

#Changing the order of experimental data
f2$Group <- factor(f2$Group, levels = c("FeCl11mM","PVK2_5mM", 
                                        "lactateD2_5mM", "PVK5.0mM",
                                        "lactateD5mM", "PVK10mM", "lactateD10mM"))
#Creating a color vector 1
cvet1 <-  c(FeCl11mM = "black", 
            PVK01mM = "#00C2FF", lactateD01mM = "#00C2FF", 
            PVK05mM = "#52B69A", lactateD05mM = "#52B69A", 
            PVK1mM = "#279324", lactateD1mM = "#279324")
#Creating a color vector 2
cvet2 <-  c(FeCl11mM = "black", 
            PVK2_5mM = "#B7D132", lactateD2_5mM =  "#B7D132", 
            PVK5.0mM = "#FFB800", lactateD5mM = "#FFB800",
            PVK10mM = "#E45C01", lactateD10mM = "#E45C01")
#Creating a color vector 3
cvet3 <- c(FeCl11mM = "black", 
           PVK01mM = "#00C2FF", lactateD01mM = "#00C2FF", 
           PVK05mM = "#52B69A", lactateD05mM = "#52B69A", 
           PVK1mM = "#279324", lactateD1mM = "#279324", 
           PVK2_5mM = "#B7D132", lactateD2_5mM =  "#B7D132", 
           PVK5.0mM = "#FFB800", lactateD5mM = "#FFB800",
           PVK10mM = "#E45C01", lactateD10mM = "#E45C01")
#Creating a line type vector 1
tipline1 <- c(FeCl11mM = "twodash", 
              PVK01mM = "dashed", lactateD01mM = "solid", 
              PVK05mM = "dashed", lactateD05mM = "solid", 
              PVK1mM = "dashed", lactateD1mM = "solid")
#Creating a line type vector 2
tipline2 <- c(FeCl11mM = "twodash",
              PVK2_5mM = "dashed",lactateD2_5mM = "solid", 
              PVK5.0mM = "dashed",lactateD5mM = "solid", 
              PVK10mM = "dashed", lactateD10mM = "solid")
#Creating a line type vector 3
tipline3 <- c(FeCl11mM = "twodash", 
              PVK01mM = "dashed", lactateD01mM = "solid", 
              PVK05mM = "dashed", lactateD05mM = "solid", 
              PVK1mM = "dashed", lactateD1mM = "solid", 
              PVK2_5mM = "dashed",lactateD2_5mM = "solid", 
              PVK5.0mM = "dashed",lactateD5mM = "solid", 
              PVK10mM = "dashed", lactateD10mM = "solid")


#Creating a graph for a panel from a single graph (Fig1 v.1)
ggplot(f3, aes(x = nm, y = T, group = Group)) + 
  geom_line(aes(color = Group, linetype = Group)) +
  labs(color = 'Concentration', linetype = 'Substance') +
  scale_linetype_manual(values = tipline3, 
                        breaks = c("PVK01mM", "lactateD01mM"),
                        labels = c("Pyruvate", "D-lactate")) +
  scale_color_manual(values = cvet3,
                     breaks = c("FeCl11mM",  
                                "PVK01mM",
                                "PVK05mM",
                                "lactateD1mM",
                                "PVK2_5mM",
                                "PVK5.0mM",
                                "PVK10mM"),
                     labels = c("0 mM", 
                                "0.2 mM", 
                                "1 mM",
                                "2 mM",
                                "5 mM",
                                "10 mM", 
                                "20 mM")) +
  ylab('Transmission, %') + 
  xlim(375, 600) +
  xlab("Wavelength, nm") + 
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  theme(text = element_text(size = 15, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 15))  +
  theme(legend.position="right", legend.box="vertical", legend.margin=margin()) +
  guides(linetype=guide_legend(ncol=1,byrow=TRUE)) +
  guides(color=guide_legend(ncol=1,byrow=TRUE)) +
  guides(color = guide_legend(order=1,),
         linetype = guide_legend(order=2))
#Save Fig2 main version
ggsave("fig2.JPG", width = 17, height = 12, units = "cm", dpi = 300)


#Figure 3
#data for fig.3
amino <- read.xlsx("aminoandcarboacid390.xlsx", 1)
carbo <- read.xlsx("aminoandcarboacid390.xlsx", 2)
#data for fig.3
names(amino)[1] <- "Thr"
names(amino)[2] <- "Val"
names(amino)[3] <- "Glu"
names(amino)[4] <- "Leu"
names(amino)[5] <- "Ile"
names(amino)[6] <- "Ala"
names(amino)[7] <- "Met"
names(amino)[8] <- "Gln"
names(amino)[9] <- "Gly"
names(amino)[10] <- "Phe"
names(amino)[11] <- "Arg"
names(amino)[12] <- "Lys"
(amino1 = amino %>% 
    pivot_longer(cols = Thr: Val:	Glu:	Leu:
                   Ile:	Ala:	Met:
                   Gln:	Gly:	Phe:	Arg:	Lys,
                 names_to = 'Aminoacids', 
                 values_to = 'ABS'))
#Histogram for amino acids
amino1$Aminoacids <- as.factor(amino1$Aminoacids)
amino1$ABS <- as.numeric(amino1$ABS)
p1 <-ggplot(amino1, aes(y = ABS, reorder(x = Aminoacids, -ABS))) + 
  geom_bar(stat="identity", col = "grey25", fill = "grey55", alpha = 0.7, size = 0.7, width = 0.65) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  ylab(expression("OD" ["390 nm"])) + 
  xlab(NULL) +
  theme(text = element_text(size = 40, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 35)) +
  ggtitle("Amino acid")


#Histogram for carboxylic acids
carbo$Group <- as.factor(carbo$Group)
carbo$ABS <- as.numeric(carbo$ABS)
p2 <-ggplot(carbo, aes(y = ABS, reorder(x = Group, -ABS))) + 
  geom_bar(stat="identity", col = "grey25", fill = "grey55", alpha = 0.7, size = 0.7, width = 0.25) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  ylab(expression("OD" ["390 nm"])) + 
  xlab(NULL) +
  theme(text = element_text(size = 40, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 35)) +
  ggtitle("Carboxylic acids")

#fig.3
ggarrange(p2, p1, ncol = 1, nrow = 2, labels = c("A", "B"), font.label=list(size=40),
          label.x = -0.01,
          label.y = 1.01)
#save fig.3
ggsave("fig3.JPG", width = 40, height = 45, units = "cm", dpi = 300)



#Figure4
#Figure 4 data (NaCl)
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 20)

#Graph of the influence of NaCl on the measurement
p1 <- ggplot(mart23, aes(x = xЛВ2, y = yFeCl2, col = "gray65")) + geom_point(size = 2, col = "gray27") + 
  geom_errorbar(aes(ymin=yFeCl2-yFeCl2so, ymax=yFeCl2+yFeCl2so, xmin=xЛВ2-xЛВ2so, xmax=xЛВ2+xЛВ2so), width=.2,  
                position=position_dodge(0.05), col = "gray1", size = 0.1) +        #Creates a standard deviation along the Y axis
  geom_errorbar(aes(xmin=xЛВ2-xЛВ2so, xmax=xЛВ2+xЛВ2so), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +        #Creates a standard deviation along the X axis
  stat_poly_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, col = "tomato1", linetype = "dashed") +      #Creates an etalon straight line
  scale_color_manual(values = c("purple1", "gray65"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) + 
  scale_linetype_manual(values = c("dashed", "solid"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) +
  theme(legend.position="none", legend.title=element_blank()) +
  theme(text = element_text(size = 15, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 15)) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  ylab('L-lactate by FI-LD, mM') + 
  xlab('L-lactate by LOx-LD mM') +
  ggtitle("NaCl (2.5 g/L)") +
  theme(title = ggtext::element_markdown()) +
  ylim(-1, 16) +
  xlim(-1, 16) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')) +   
  theme(plot.title = element_text(hjust = 0.02)) +
  annotate("text", x = c(3.5), y = c(13.1),                                #Linear regression formula
           label = 'atop("y = 0.857x + 1.43")', cex=5, parse = TRUE, family = "Arial") +
  annotate("text", x = c(3.5), y = c(12.4), 
           label = "R² = 0.93", cex=5, family = "Arial")

#Figure 4 data (Lysogeny broth)
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 21)

#Graph of the influence of Lysogeny broth on the measurement
p2 <- ggplot(mart23, aes(x = xЛВср, y = yFeClср, col = "gray65")) + geom_point(size = 2, col = "gray27") +
  geom_errorbar(aes(ymin=yFeClср-yFeClст, ymax=yFeClср+yFeClст), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +         #Creates a standard deviation along the Y axis
  geom_errorbar(aes(xmin=xЛВср-xЛВст, xmax=xЛВср+xЛВст), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +         #Creates a standard deviation along the X axis
  stat_poly_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, col = "tomato1", linetype = "dashed") +       #Creates an etalon straight line
  scale_color_manual(values = c("purple1", "gray65"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) + 
  scale_linetype_manual(values = c("dashed", "solid"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) +
  theme(legend.position="none", legend.title=element_blank()) +
  theme(text = element_text(size = 15, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 15)) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  ylab('L-lactate by FI-LD, mM') + 
  xlab('L-lactate by LOx-LD mM') +
  ggtitle("Lysogeny broth")+
  theme(title = ggtext::element_markdown()) +
  ylim(-1, 16) +
  xlim(-1, 16) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')) +  
  theme(plot.title = element_text(hjust = 0.02)) +
  annotate("text", x = c(3.5), y = c(13.1),          #Linear regression formula
           label = 'atop("y = 0.689x + 4.18")', cex=5, parse = TRUE, family = "Arial") +
  annotate("text", x = c(3.5), y = c(12.4), 
           label = "R² = 0.96", cex=5, family = "Arial") +
  annotate("text", x = c(0), y = c(5.2), 
           label = "*", cex=12, family = "Arial")

#Figure 4 data (Peptone)
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 22)

#Graph of the influence of Peptone on the measurement
p3 <- ggplot(mart23, aes(x = xЛВср, y = yFeClср, col = "gray65")) + geom_point(size = 2, col = "gray27") +
  geom_errorbar(aes(ymin=yFeClср-yFeClст, ymax=yFeClср+yFeClст), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +         #Creates a standard deviation along the Y axis
  geom_errorbar(aes(xmin=xЛВср-xЛВст, xmax=xЛВср+xЛВст), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +         #Creates a standard deviation along the X axis
  stat_poly_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, col = "tomato1", linetype = "dashed") +       #Creates an etalon straight line
  scale_color_manual(values = c("purple1", "gray65"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) + 
  scale_linetype_manual(values = c("dashed", "solid"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) +
  theme(legend.position="none", legend.title=element_blank()) +
  theme(text = element_text(size = 15, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 15)) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  ylab('L-lactate by FI-LD, mM') + 
  xlab('L-lactate by LOx-LD mM') +
  ggtitle("Peptone (5 g/L)") +
  theme(title = ggtext::element_markdown()) +
  ylim(-1, 16) +
  xlim(-1, 16) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')) +
  theme(plot.title = element_text(hjust = 0.02)) +
  annotate("text", x = c(3.5), y = c(13.1),          #Linear regression formula
           label = 'atop("y = 0.841x + 0.542")', cex=5, parse = TRUE, family = "Arial") +
  annotate("text", x = c(3.5), y = c(12.4), 
           label = "R² = 0.98", cex=5, family = "Arial")

#Figure 4 data (Yeast extract)
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 23)

#Graph of the influence of Yeast extract on the measurement
p4 <- ggplot(mart23, aes(x = xЛВср, y = yFeClср, col = "gray65")) + geom_point(size = 2, col = "gray27") +
  geom_errorbar(aes(ymin=yFeClср-yFeClст, ymax=yFeClср+yFeClст), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +         #Creates a standard deviation along the Y axis
  geom_errorbar(aes(xmin=xЛВср-xЛВст, xmax=xЛВср+xЛВст), width=.2,
                position=position_dodge(0.05), col = "gray1", size = 0.1) +         #Creates a standard deviation along the X axis
  stat_poly_line(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, col = "tomato1", linetype = "dashed") +       #Creates an etalon straight line
  scale_color_manual(values = c("purple1", "gray65"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) + 
  scale_linetype_manual(values = c("dashed", "solid"), breaks = c("Reference", "Lactic acid in solution dd H₂O with NaCl")) +
  theme(legend.position="none", legend.title=element_blank()) +
  theme(text = element_text(size = 15, family = "Arial", colour = "black"),
        axis.text=element_text(color="black", size = 15)) +
  theme(panel.background = element_rect(fill = 'gray97', colour = 'gray97')) +
  ylab('L-lactate by FI-LD, mM') + 
  xlab('L-lactate by LOx-LD mM') +
  ggtitle("Yeast extract (2.5 g/L)") +
  theme(title = ggtext::element_markdown()) +
  ylim(-1, 16) +
  xlim(-1, 16) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), 'lines')) +
  theme(plot.title = element_text(hjust = 0.02)) +
  annotate("text", x = c(3.5), y = c(13.1),          #Linear regression formula
           label = 'atop("y = 0.941x + 0.863")', cex=5, parse = TRUE, family = "Arial") +
  annotate("text", x = c(3.5), y = c(12.4), 
           label = "R² = 0.96", cex=5, family = "Arial") +
  annotate("text", x = c(0), y = c(2), 
           label = "*", cex=12, family = "Arial")

#Creates Fig.4
ggarrange(p2,
          p4,
          p1,
          p3,
          nrow = 2,
          ncol = 2,
          labels = c("A","B", "C", "D"),
          font.label = list(size = 20))

#Save Fig.4
ggsave("Fig4.JPG", width = 2362, height = 2244, units = "px", dpi = 300)


#Statistics for Yeas extract
mart11 <- read.xlsx("2023-03-14_exp_data.xlsx", 23)
lmHeight <- lm(mart11$yFeClср~mart11$xЛВср, data=mart11)
summary(lmHeight)
#Statistics for LB
mart23 <- read.xlsx("2023-03-14_exp_data.xlsx", 21)
lmHeight <- lm(mart23$yFeClср~mart23$xЛВср, data=mart23)
summary(lmHeight)
