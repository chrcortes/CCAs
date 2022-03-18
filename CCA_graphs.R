setwd("G:/Mi unidad/GRO/1_Artículo_Resiliencia_GC/CCAs")
library(ggplot2)

theme_set(theme_bw() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.title.x = element_text(size=16),
                  axis.title.y = element_text(size=16), 
                  axis.text.x = element_text(size=14, color="gray30"),
                  axis.text.y = element_text(size=14, color="gray30"),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16)))

#strip.background = element_rect(fill="white"),
#strip.text.x = element_text(size=13),

#Size= 1220*715

#________________________________________________________________________________________
#### PNCP DENSITY LOSERS ####
CP_D_L_cca <- read.csv("data/PNCP.df_species_loser_den1.csv")
CP_D_L_arrows <- read.csv("data/PNCP.df_arrows_loser_den.csv", row.names = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#png("plots/CP_D_L.png", width = 5, height = 4, units = "in", res = 300)
cca.cp <- ggplot(CP_D_L_cca, aes(CCA1, CCA2), fill=Category) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_point(color="#FF0033", size=6) +
  annotate("text", x=CP_D_L_cca[1,2], y=CP_D_L_cca[1,3], 
           label="H. chierchiae", fontface="italic", size=4, vjust=1.1, hjust=1.1) +
  annotate("text", x=CP_D_L_cca[2,2], y=CP_D_L_cca[2,3], 
           label="H. passer", fontface="italic", size=4, vjust=-.55, hjust=-.15) +
  geom_segment(data=CP_D_L_arrows, aes(x = 0, y = 0, xend = x, yend = y), 
               size = 1.5, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=CP_D_L_arrows[1,1], y=CP_D_L_arrows[1,2], 
           label="H. population", size=5, vjust=-1, hjust=.9) +
  annotate("text", x=CP_D_L_arrows[2,1], y=CP_D_L_arrows[2,2], 
           label="Visitors", size=5, vjust=1.5, hjust=1) +
  guides(fill=FALSE)
cca.cp
#dev.off()

#________________________________________________________________________________________
#### PNAES OCCURRENCE WINNERS ####
ES_O_W_cca <- read.csv("data/PNAES_df_species_winner_occ.csv")
ES_O_W_arrows <- read.csv("data/PNAES_df_arrows_winner_occ.csv", row.names = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#png("plots/ES_O_W.png", width = 5, height = 4, units = "in", res = 300)
cca.es.o.w <- ggplot(ES_O_W_cca, aes(CCA1, CCA2), fill=Category) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_point(color="#2166AC", size=6) +
  annotate("text", x=ES_O_W_cca[1,2], y=ES_O_W_cca[1,3], 
           label="H. chierchiae", fontface="italic", size=4, vjust=1.5, hjust=-.1) +
  annotate("text", x=ES_O_W_cca[2,2], y=ES_O_W_cca[2,3], 
           label="H. dispilus", fontface="italic", size=4, vjust=-1.1, hjust=-.1) +
  geom_segment(data=ES_O_W_arrows, aes(x = 0, y = 0, xend = x, yend = y), 
               size = 1.5, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=ES_O_W_arrows[1,1], y=ES_O_W_arrows[1,2], 
           label="CHL_a", size=5, vjust=-1, hjust=.9) +
  guides(fill=FALSE)
cca.es.o.w
#dev.off()

#________________________________________________________________________________________
#### PNAES OCCURRENCE LOSERS ####
ES_O_L_cca <- read.csv("data/PNAES_df_species_loser_occ.csv")
ES_O_L_arrows <- read.csv("data/PNAES_df_arrows_loser_occ.csv", row.names = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#png("plots/ES_O_L.png", width = 5, height = 4, units = "in", res = 300)
cca.es.o.l <- ggplot(ES_O_L_cca, aes(CCA1, CCA2), fill=Category) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_point(color="#FF0033", size=6) +
  annotate("text", x=ES_O_L_cca[1,2], y=ES_O_L_cca[1,3], 
           label="C. limbaughi", fontface="italic", size=4, vjust=.5, hjust=-.15) +
  annotate("text", x=ES_O_L_cca[2,2], y=ES_O_L_cca[2,3], 
           label="D. holocanthus", fontface="italic", size=4, vjust=.5, hjust=-.15) +
  annotate("text", x=ES_O_L_cca[3,2], y=ES_O_L_cca[3,3], 
           label="M. dorsalis", fontface="italic", size=4, vjust=.5, hjust=-.2) +
  annotate("text", x=ES_O_L_cca[4,2], y=ES_O_L_cca[4,3], 
           label="M. rosacea", fontface="italic", size=4, vjust=.5, hjust=-.15) +
  annotate("text", x=ES_O_L_cca[5,2], y=ES_O_L_cca[5,3], 
           label="S. ghobban", fontface="italic", size=4, vjust=.5, hjust=-.15) +
  annotate("text", x=ES_O_L_cca[6,2], y=ES_O_L_cca[6,3], 
           label="S. rubroviolaceus", fontface="italic", size=4, vjust=.5, hjust=-.1) +
  annotate("text", x=ES_O_L_cca[7,2], y=ES_O_L_cca[7,3], 
           label="S. verres", fontface="italic", size=4, vjust=.5, hjust=-.15) +
  geom_segment(data=ES_O_L_arrows, aes(x = 0, y = 0, xend = x, yend = y), 
               size = 1.5, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=ES_O_L_arrows[1,1], y=ES_O_L_arrows[1,2], 
           label="SST", size=5, vjust=-1, hjust=.9) +
  guides(fill=FALSE)
cca.es.o.l
#dev.off()

#________________________________________________________________________________________
#### PNAES DENSITY LOSERS ####
#theme_set(old)
ES_D_L_cca <- read.csv("data/PNAES_df_species_loser_den.csv")
ES_D_L_arrows <- read.csv("data/PNAES_df_arrows_loser_den.csv", row.names = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#png("plots/ES_D_L.png", width = 5, height = 4, units = "in", res = 300)
cca.es.d.l <- ggplot(ES_D_L_cca, aes(CCA1, CCA2), fill=Category) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_point(color="#FF0033", size=6) +
  annotate("text", x=ES_D_L_cca[1,2], y=ES_D_L_cca[1,3], 
           label="C. punctatissima", fontface="italic", size=4, vjust=.4, hjust=.9) +
  annotate("text", x=ES_D_L_cca[2,2], y=ES_D_L_cca[2,3], 
           label="T. lucasanum", fontface="italic", size=4, vjust=.3, hjust=-.1) +
  annotate("text", x=ES_D_L_cca[3,2], y=ES_D_L_cca[3,3], 
           label="A. troschelii", fontface="italic", size=4, vjust=1.5, hjust=1.05) +
  annotate("text", x=ES_D_L_cca[4,2], y=ES_D_L_cca[4,3], 
           label="J. nigrirostris", fontface="italic", size=4, vjust=-1, hjust=0.5) +
  annotate("text", x=ES_D_L_cca[5,2], y=ES_D_L_cca[5,3], 
           label="M. dorsalis", fontface="italic", size=4, vjust=.2, hjust=-.15) +
  annotate("text", x=ES_D_L_cca[6,2], y=ES_D_L_cca[6,3], 
           label="S. rectifraenum", fontface="italic", size=4, vjust=-.15, hjust=0) +
  annotate("text", x=ES_D_L_cca[7,2], y=ES_D_L_cca[7,3], 
           label="S. rubroviolaceus", fontface="italic", size=4, vjust=.5, hjust=-.1) +
  annotate("text", x=ES_D_L_cca[8,2], y=ES_D_L_cca[8,3], 
           label="C. limbaughi", fontface="italic", size=4, vjust=.5, hjust=-.15) +
  annotate("text", x=ES_D_L_cca[9,2], y=ES_D_L_cca[9,3], 
           label="S. ghobban", fontface="italic", size=4, vjust=.5, hjust=1.15) +
  annotate("text", x=ES_D_L_cca[10,2], y=ES_D_L_cca[10,3], 
           label="C. atrilobata", fontface="italic", size=4, vjust=-.9, hjust=0) +
  annotate("text", x=ES_D_L_cca[11,2], y=ES_D_L_cca[11,3], 
           label="D. holocanthus", fontface="italic", size=4, vjust=.9, hjust=-.07) +
  annotate("text", x=ES_D_L_cca[12,2], y=ES_D_L_cca[12,3], 
           label="C. oxycephalus", fontface="italic", size=4, vjust=.5, hjust=-.1) +
  geom_segment(data=ES_D_L_arrows, aes(x = 0, y = 0, xend = x, yend = y), 
               size = 1.5, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=ES_D_L_arrows[1,1], y=ES_D_L_arrows[1,2], 
           label="CHL_a", size=5, vjust=-1, hjust=.9) +
  annotate("text", x=ES_D_L_arrows[2,1], y=ES_D_L_arrows[2,2], 
           label="H.population", size=5, vjust=-.5, hjust=-.15, angle=-35) +
  annotate("text", x=ES_D_L_arrows[3,1], y=ES_D_L_arrows[3,2], 
           label="Coral (%)", size=5, vjust=.5, hjust=-.15) +
  annotate("text", x=ES_D_L_arrows[4,1], y=ES_D_L_arrows[4,2], 
           label="Hurricanes", size=5, vjust=1, hjust=.9, angle=30) +
  annotate("text", x=ES_D_L_arrows[5,1], y=ES_D_L_arrows[5,2], 
           label="Visitors", size=5, vjust=-.5, hjust=.1) +
  annotate("text", x=ES_D_L_arrows[6,1], y=ES_D_L_arrows[6,2], 
           label="PAR", size=5, vjust=-.5, hjust=.1) +
  guides(fill=FALSE)
cca.es.d.l
#dev.off()

#________________________________________________________________________________________
#### PNBL OCCURRENCE WINNERS ####
BL_O_W_cca <- read.csv("data/PNBL_df_species_WO.csv")
BL_O_W_arrows <- read.csv("data/PNBL_df_arrows_WO.csv", row.names = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#png("plots/BL_O_W.png", width = 5, height = 4, units = "in", res = 300)
cca.lor.o.w <- ggplot(BL_O_W_cca, aes(CCA1, CCA2), fill=Category) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_point(color="#2166AC", size=6) +
  annotate("text", x=BL_O_W_cca[1,2], y=BL_O_W_cca[1,3], 
           label="P. laticlavius", fontface="italic", size=4, vjust=1.5, hjust=-.1) +
  annotate("text", x=BL_O_W_cca[2,2], y=BL_O_W_cca[2,3], 
           label="B. polylepis", fontface="italic", size=4, vjust=-1.1, hjust=0) +
  geom_segment(data=BL_O_W_arrows, aes(x = 0, y = 0, xend = x, yend = y), 
               size = 1.5, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=BL_O_W_arrows[1,1], y=BL_O_W_arrows[1,2], 
           label="PAR", size=5, vjust=-1, hjust=.9) +
  annotate("text", x=BL_O_W_arrows[2,1], y=BL_O_W_arrows[2,2], 
           label="SST", size=5, vjust=-1, hjust=.9) +
  annotate("text", x=BL_O_W_arrows[3,1], y=BL_O_W_arrows[2,2], 
           label="H. population", size=5, vjust=-1, hjust=.1) +
  annotate("text", x=BL_O_W_arrows[4,1], y=BL_O_W_arrows[2,2], 
           label="Visitors", size=5, vjust=1.75, hjust=-.1) +
  guides(fill=FALSE)
cca.lor.o.w
#dev.off()

#________________________________________________________________________________________
#### PNBL DENSITY LOSERS ####
BL_D_L_cca <- read.csv("data/PNBL_df_species_LD.csv")
BL_D_L_arrows <- read.csv("data/PNBL_df_arrows_LD.csv", row.names = 1)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#png("plots/BL_D_L.png", width = 5, height = 4, units = "in", res = 300)
cca.lor.d.l <- ggplot(BL_D_L_cca, aes(CCA1, CCA2), fill=Category) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") + 
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_point(color="#FF0033", size=6) +
  annotate("text", x=BL_D_L_cca[1,2], y=BL_D_L_cca[1,3], 
           label="M. rosacea", fontface="italic", size=4, vjust=1.7, hjust=.5) +
  annotate("text", x=BL_D_L_cca[2,2], y=BL_D_L_cca[2,3], 
           label="A. troschelii", fontface="italic", size=4, vjust=1.6, hjust=.5) +
  annotate("text", x=BL_D_L_cca[3,2], y=BL_D_L_cca[3,3], 
           label="C. punctatissima", fontface="italic", size=4, vjust=1.7, hjust=.45) +
  annotate("text", x=BL_D_L_cca[4,2], y=BL_D_L_cca[4,3], 
           label="S. psittacinus", fontface="italic", size=4, vjust=1.9, hjust=.5) +
  annotate("text", x=BL_D_L_cca[5,2], y=BL_D_L_cca[5,3], 
           label="H. dispilus", fontface="italic", size=4, vjust=-1.1, hjust=.2) +
  annotate("text", x=BL_D_L_cca[6,2], y=BL_D_L_cca[6,3], 
           label="T. lucasanum", fontface="italic", size=4, vjust=-1.1, hjust=.5) +
  annotate("text", x=BL_D_L_cca[7,2], y=BL_D_L_cca[7,3], 
           label="S. rectifraenum", fontface="italic", size=4, vjust=1.6, hjust=.5) +
  annotate("text", x=BL_D_L_cca[8,2], y=BL_D_L_cca[8,3], 
           label="M. dorsalis", fontface="italic", size=4, vjust=1.6, hjust=.5) +
  annotate("text", x=BL_D_L_cca[9,2], y=BL_D_L_cca[9,3], 
           label="B. diplotaenia", fontface="italic", size=4, vjust=1.6, hjust=.5) +
  annotate("text", x=BL_D_L_cca[10,2], y=BL_D_L_cca[10,3], 
           label="D. holocanthus", fontface="italic", size=4, vjust=.7, hjust=-.1) +
  annotate("text", x=BL_D_L_cca[11,2], y=BL_D_L_cca[11,3], 
           label="S. perrico", fontface="italic", size=4, vjust=-1.1, hjust=.5) +
  annotate("text", x=BL_D_L_cca[12,2], y=BL_D_L_cca[12,3], 
           label="S. ghobban", fontface="italic", size=4, vjust=1.6, hjust=.5) +
  annotate("text", x=BL_D_L_cca[13,2], y=BL_D_L_cca[13,3], 
           label="H. passer", fontface="italic", size=4, vjust=1.5, hjust=.5) +
  annotate("text", x=BL_D_L_cca[14,2], y=BL_D_L_cca[14,3], 
           label="C. atrilobata", fontface="italic", size=4, vjust=1.6, hjust=.45) +
  annotate("text", x=BL_D_L_cca[15,2], y=BL_D_L_cca[15,3], 
           label="S. compressus", fontface="italic", size=4, vjust=1.6, hjust=.5) +
  annotate("text", x=BL_D_L_cca[16,2], y=BL_D_L_cca[16,3], 
           label="C. oxycephalus", fontface="italic", size=4, vjust=-1.1, hjust=.85) +
  annotate("text", x=BL_D_L_cca[17,2], y=BL_D_L_cca[17,3], 
           label="S. rubroviolaceus", fontface="italic", size=4, vjust=-1.1, hjust=.5) +
  geom_segment(data=BL_D_L_arrows, aes(x = 0, y = 0, xend = x, yend = y), 
               size = 1.5, arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text", x=BL_D_L_arrows[1,1], y=BL_D_L_arrows[1,2], 
           label="SST", size=5, vjust=-.5, hjust=.9) +
  annotate("text", x=BL_D_L_arrows[2,1], y=BL_D_L_arrows[2,2], 
           label="H. population", size=5, vjust=.3, hjust=-.07) +
  annotate("text", x=BL_D_L_arrows[3,1], y=BL_D_L_arrows[3,2], 
           label="Fisheries", size=5, vjust=-.5, hjust=1) +
  guides(fill=FALSE)
cca.lor.d.l 
#dev.off()


#________________________________________________________________________________________
#### Plot FINAL ####
library(ggpubr)
cca.final <- ggarrange(cca.cp, cca.lor.o.w, cca.lor.d.l,
                       cca.es.o.w, cca.es.o.l, cca.es.d.l,
                       ncol = 3, nrow = 2, labels = c("a)","b)", "c)", "d)", "e)", "f)"))

tiff("plots/4_ccas.tiff", width = 13.5, height = 7.5, units = "in", res = 300)
cca.final
dev.off()

png("plots/4_ccas.png", width = 13.5, height = 7.5, units = "in", res = 300)
cca.final
dev.off()

ggsave("plots/4_ccas.pdf", width = 13.5, height = 7.5, units = "in")



#________________________________________________________________________________________
#### CORR MATRIX ####
library(corrplot)
C1_ES_D <- read.csv("data/Datos_para_CCA/PNAES_CCA_den.csv")
C1_ES_D <- C1_ES_D[,-12]
#BL_D_L_arrows <- read.csv("data/PNBL_df_arrows_LD.csv", row.names = 1)

mcor_C1_ES_D <- cor(C1_ES_D)
# Print mcor and round to 2 digits
mcor_C1_ES_D <- round(mcor_C1_ES_D, digits=2)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor_C1_ES_D, type="lower", order="hclust")


corrplot(mcor_C1_ES_D, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(20), addCoef.col="black", addcolorlabel="no", order="AOE")
dimnames(mcor_C1_ES_D)
pairs(mcor_C1_ES_D[,c(2,3,4,7,18,19,21)])



