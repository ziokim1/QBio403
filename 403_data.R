library("ggplot2")

df_r <- read.csv("nbent_data_rates.csv")
df1 <- read.csv("nbent_1.csv")


ggplot(df1, aes(x=day, y=length, group=trt, colour=factor(trt))) +
  geom_point() + geom_line(size=.75) + ggtitle("A.1") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Day") +
  ylab("E[Length] (cm)") +
  theme_classic() +
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 

ggplot(df1, aes(x=day, y=width, group=trt, colour=factor(trt))) +
  geom_point() + geom_line(size=.75) + ggtitle("B.1") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Day") +
  ylab("E[Width] (cm)") +
  theme_classic() +
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 

ggplot(df1, aes(x=day, y=height, group=trt, colour=factor(trt))) +
  geom_point() + geom_line(size=.75) +  ggtitle("C.1") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Day") +
  ylab("E[Height] (cm)") +
  theme_classic() +  
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 

ggplot(df1, aes(x=day, y=spad, group=trt, colour=factor(trt))) +
  geom_point() + geom_line(size=.75) +  ggtitle("D.1") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Day") +
  ylab("E[SPAD] (a.u.)") +
  theme_classic() +  
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 


p1 <- ggplot(df_r, aes(x=trt, y=length_rate, group=trt, colour=factor(trt))) +
  geom_boxplot() + 
  ggtitle("A.2") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Treatment") +
  ylab("Growth Rate - Length (cm/day)") +
  scale_x_discrete(labels=c("a" = "Acidic", "b" = "Basic",
                            "c" = "Control")) +
  theme_classic() +
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 

k1 <- kruskal.test(length_rate ~ trt, data = df_r)

p1 <- p1 + annotate("text", size = 6, x = 2, y = 0.24, label = paste("Kruskal-Wallis p =", format.pval(k1$p.value, digits = 3)))

p1


p2 <- ggplot(df_r, aes(x=trt, y=width_rate, group=trt, colour=factor(trt))) +
  geom_boxplot() + 
  ggtitle("B.2") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Treatment") +
  ylab("Growth Rate - Width (cm/day)") +
  scale_x_discrete(labels=c("a" = "Acidic", "b" = "Basic",
                            "c" = "Control")) +
  theme_classic() +
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 

k2 <- kruskal.test(width_rate ~ trt, data = df_r)

p2 <- p2 + annotate("text", size = 6, x = 2, y = 0.24, label = paste("Kruskal-Wallis p =", format.pval(k2$p.value, digits = 3)))

p2


p4 <- ggplot(df_r, aes(x=trt, y=spad_rate, group=trt, colour=factor(trt))) +
  geom_boxplot() + 
  ggtitle("D.2") +
  scale_colour_manual(values=c("red", "blue", "green"), name="Treatment", labels=c("Acidic", "Basic","Control")) +
  xlab("Treatment") +
  ylab("Degradation Rate - SPAD (a.u./day)") +
  scale_x_discrete(labels=c("a" = "Acidic", "b" = "Basic",
                            "c" = "Control")) +
  theme_classic() +
  theme(
    plot.title = element_text(size=rel(3)),
    legend.title = element_text(size=rel(1.5)),
    legend.text = element_text(size=rel(1.5)),
    axis.text.x = element_text(size=rel(1.5)),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5)),
    axis.title.y = element_text(size=rel(1.5))
  ) 

k4 <- kruskal.test(spad_rate ~ trt, data = df_r)

p4 <- p4 + annotate("text", size = 6, x = 2, y = 0.1, label = paste("Kruskal-Wallis p =", format.pval(k4$p.value, digits = 3)))

p4
