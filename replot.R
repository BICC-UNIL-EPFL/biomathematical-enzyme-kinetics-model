library(tidyverse)
# The palette with grey:
res1 <- read_tsv('Model_1_res.tsv.gz')
res2.1 <- read_tsv('Model_2.1_res.tsv.gz')
res2.2 <- read_tsv('Model_2.2_res.tsv.gz')
res3.0.1 <- read_tsv('Model_3.0.1_res.tsv.gz')
res3.0.2 <- read_tsv('Model_3.0.2_res.tsv.gz')
res3.1 <- read_tsv('Model_3.1_res.tsv.gz')
res3.2 <- read_tsv('Model_3.2_res.tsv.gz')

fig1 <- ggplot(res1) +
        geom_point(aes(t, R1, color = 'R1')) +
        geom_point(aes(t, R2, color = 'R2')) +
	scale_colour_manual(values=c("red", "chartreuse"))

fig2.1 <- ggplot(res2.1) +
          geom_point(aes(t, R1, color = 'R1')) +
          geom_point(aes(t, R2, color = 'R2')) +
	  geom_point(aes(t, R3, color = 'R3')) +
	  scale_colour_manual(values=c("red", "chartreuse", "blue"))
fig2.2 <- ggplot(res2.2) +
          geom_point(aes(t, R1, color = 'R1')) +
          geom_point(aes(t, R2, color = 'R2')) +
	  geom_point(aes(t, R3, color = 'R3')) +
	  scale_colour_manual(values=c("red", "chartreuse", "blue"))

fig3.0.1 <- ggplot(res3.0.1) +
            geom_point(aes(t, R1, color = 'R1')) +
            geom_point(aes(t, R2, color = 'R2')) +
	    geom_point(aes(t, R3, color = 'R3')) +
	    geom_point(aes(t, R4, color = 'R4')) +
	    scale_colour_manual(values=c("red", "chartreuse", "blue", "darkgreen"))
fig3.0.2 <- ggplot(res3.0.2) +
            geom_point(aes(t, R1, color = 'R1')) +
	    geom_point(aes(t, R2, color = 'R2')) +
	    geom_point(aes(t, R3, color = 'R3')) +
	    geom_point(aes(t, R4, color = 'R4')) +
	    scale_colour_manual(values=c("red", "chartreuse", "blue", "darkgreen"))
fig3.1 <- ggplot(res3.1) +
          geom_point(aes(t, R1, color = 'R1')) +
	  geom_point(aes(t, R2, color = 'R2')) +
	  geom_point(aes(t, R3, color = 'R3')) +
	  geom_point(aes(t, R4, color = 'R4')) +
	  scale_colour_manual(values=c("red", "chartreuse", "blue", "darkgreen"))
fig3.2 <- ggplot(res3.2) +
          geom_point(aes(t, R1, color = 'R1')) +
	  geom_point(aes(t, R2, color = 'R2')) +
	  geom_point(aes(t, R3, color = 'R3')) +
	  geom_point(aes(t, R4, color = 'R4')) +
	  scale_colour_manual(values=c("red", "chartreuse", "blue", "darkgreen"))

pdf('Model_1.pdf',paper="a4r",width=0,height=0)
print(fig1)
dev.off()

pdf('Model_2.1.pdf',paper="a4r",width=0,height=0)
print(fig2.1)
dev.off()
pdf('Model_2.2.pdf',paper="a4r",width=0,height=0)
print(fig2.2)
dev.off()

pdf('Model_3.0.1.pdf',paper="a4r",width=0,height=0)
print(fig3.0.1)
dev.off()
pdf('Model_3.0.2.pdf',paper="a4r",width=0,height=0)
print(fig3.0.2)
dev.off()
pdf('Model_3.1.pdf',paper="a4r",width=0,height=0)
print(fig3.1)
dev.off()
pdf('Model_3.2.pdf',paper="a4r",width=0,height=0)
print(fig3.2)
dev.off()

###
