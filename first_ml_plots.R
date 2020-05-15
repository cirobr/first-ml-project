library(ggthemes)
#options(digits = 3)

# histograma da população
p <- heights %>%
  ggplot(aes(x = height_cm)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = mean(heights$height_cm), colour = "green", size = 1.2)

p <- p +
  ggtitle("Distribuição da População") +
  xlab("Altura (cm)") +
  ylab("Contagem") +
  theme_economist(base_size = 15)

p

# boxplot
q <- heights %>%
  ggplot(aes(x = sex, y = height_cm)) +
  geom_boxplot()

q <- q +
  ggtitle("Distribuição por Gênero") +
  xlab("") +
  ylab("Altura (cm)") +
  theme_economist(base_size = 15)

q

# boxplot -2sigma
q1 <- q +
  geom_hline(yintercept = 156, colour = "red", size = 1.2)
q1
  
# boxplot max accuracy
q2 <- q1 +
  geom_hline(yintercept = cutoff[which.max(accuracy)], colour = "yellow", size = 1.2)
q2

# boxplot max f-score
q3 <- q2 +
  geom_hline(yintercept = cutoff[which.max(F_1)], colour = "blue", size = 1.2)
q3

# accuracy
accuracy <- round(accuracy, digits = 3)
r1 <- data.frame(cutoff, accuracy)
r1 %>% ggplot(aes(x = cutoff, y = accuracy)) +
  geom_point() +
  geom_text(label = accuracy, nudge_y = .005) +
  ggtitle("Cutoff por Precisão") +
  xlab("Cutoff (cm)") +
  ylab("Accuracy") +
  theme_economist(base_size = 15)

F_1 <- round(F_1, digits = 3)
r2 <- data.frame(cutoff, F_1)
r2 %>% ggplot(aes(x = cutoff, y = F_1)) +
  geom_point() +
  geom_text(label = F_1, nudge_y = .02) +
  ggtitle("Cutoff por F-Score") +
  xlab("Cutoff (cm)") +
  ylab("F-Score") +
  theme_economist(base_size = 15)
