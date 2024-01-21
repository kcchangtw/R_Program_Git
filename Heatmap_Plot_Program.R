rm(list = ls())
library("dplyr")
library("tidyr")
library("ggplot2")
#library("ggthemes")


load(file = "array_Go_NoGo.RData")

#Plot the graph preprocess
mut <- array_Go_NoGo#array_Go_NoG0
mut <- as.data.frame(mut)
names(mut) <- c("rc", "rt", "decision")

mut$rc <- factor(mut$rc, levels = unique(mut$rc))
mut$rt <- factor(mut$rt, levels = unique(mut$rt))
mut$decision <- factor(mut$decision, levels = unique(mut$decision))

gg <- ggplot(mut, aes(x = rc, y = rt, fill = decision)) +
  geom_tile(color= "white", size = 0.5, show.legend = FALSE) +
  #scale_fill_manual(values = c("#fffa8c", "#6dc700", "#ff0000")) +
  scale_fill_manual(values = c("#ff0000", "#fffa8c", "#6dc700")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 12)) +
  #scale_x_discrete(position = "top") +
  #scale_y_discrete(name = "", labels = row_labels) +
  labs(x=NULL, y=NULL) +
  geom_text(aes(label=decision), size=2)

show(gg)

#Reverse
mut$rt <- rev(mut$rt)
row_labels <- rev(levels(mut$rt))
gg <- ggplot(mut, aes(x = rc, y = rt, fill = decision)) +
  geom_tile(color= "white", size = 0.5, show.legend = FALSE) +
  #scale_fill_manual(values = c("#fffa8c", "#6dc700", "#ff0000")) +
  scale_fill_manual(values = c("#ff0000", "#fffa8c", "#6dc700")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, size = 12), axis.text.y = element_text(size = 12)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(name = "", labels = row_labels) +
  labs(x=NULL, y=NULL) +
  geom_text(aes(label=decision), size=2)

show(gg)

