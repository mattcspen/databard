
# Linear Regression and RMSD
# Matt Spencer - DataBard
# www.databard.blog
# 12/20/2017
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)

theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

# Just a smidge of data cleaning
statdata <- read.csv('stature-hand-foot.csv')
colnames(statdata) <- c('gender', 'height', 'hand', 'foot')
data <- statdata %>%
    mutate(sex    = ifelse(gender == 1, 'male', 'female'),
           height = height / 304.8,
           hand   = hand / 25.4) %>%
    select(-gender, -foot)

g <- ggplot(data = data, aes(hand, height)) + 
    geom_point(aes(color = sex), size = 0.5) +

    scale_y_continuous(breaks = seq(4, 7, 0.5), labels = c("4'0\"", "4'6\"", "5'0\"", "5'6\"", "6'0\"", "6'6\"", "7'0\"")) +
    labs(x = 'Hand Size (in)', y = 'Height') +
    scale_color_manual(values = c("#ED58BD", "#34B0C2")) +

    theme_Publication() +
    theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))
g
#ggsave(file= "figures/1.initial.plot.jpeg", g, width = 5, height = 5, dpi = 200)

# Calculate regression line of best fit
hmod <- train(height ~ hand, data, method = "lm")
summary(hmod) 
# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.4805     0.1819   8.141 1.27e-13 ***
# hand          0.5122     0.0231  22.172  < 2e-16 ***

bestfit.int <- summary(hmod)$coefficients['(Intercept)', 'Estimate']
bestfit.slope <- summary(hmod)$coefficients['hand', 'Estimate']

g2 <- g + 
    geom_abline(slope = bestfit.slope, intercept = bestfit.int) +
    geom_point(aes(color = sex), size = 0.5) 
g2
#ggsave(file = "figures/2.fit.jpeg", g2, width = 5, height = 5, dpi = 200)

data$predicted <- predict(hmod, newdata = data)
coords <- data %>% 
    mutate(grp = row_number()) %>% 
    select(grp, hand, height, predicted) %>%
    rename(actual = height) %>%
    gather(h.type, height, actual, predicted) 

g3 <- g + geom_line(data=coords, aes(hand, height, group = grp), size = 0.25) +
    geom_abline(slope = bestfit.slope, intercept = bestfit.int) +
    geom_point(aes(color = sex), size = 0.5) 
g3
#ggsave(file = "figures/3.fit.errors.jpeg", g3, width = 5, height = 5, dpi = 200)

# Predictions for the outliers
data %>% arrange(predicted) %>% head(1)
data %>% arrange(-predicted) %>% head(1)

g4 <- ggplot(data = data, aes(hand, height)) +
    geom_line(data = coords, aes(hand, height, group = grp), size = 0.25) +
    geom_abline(slope = bestfit.slope, intercept = bestfit.int) +
    geom_point(aes(color = sex), size = 0.5) +

    # Reduced figure limits
    scale_x_continuous(limits = c(6.75, 9), expand = c(0, 0)) +
    scale_y_continuous(limits = c(4.75, 6.25), expand = c(0, 0), 
                       breaks=seq(4, 7, 0.5), labels=c("4'0\"", "4'6\"", "5'0\"", "5'6\"", "6'0\"", "6'6\"", "7'0\"")) +
    labs(x = 'Hand Size (in)', y = 'Height') +
    scale_color_manual(values = c("#ED58BD", "#34B0C2")) +

    theme_Publication() +
    theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))
g4
#ggsave(file = "figures/4.fit.errors.zoom.jpeg", g4, width = 5, height = 5, dpi = 200)

# Calculate the average error over the core population
data %>% filter(hand > 6, hand < 9) %>%
    mutate(diff = abs(height - predicted)) %>%
    summarize(avg = mean(diff))
# 0.098 feet ~ 1.2 inches

# Generate training and testing dataset for the ".no" (no outlier) attempt
# Train another model.
data.no <- data[data$hand > 6.3 & data$hand < 9.4,]
outliers <- data[data$hand <= 6.3 | data$hand >= 9.4,]

set.seed(123)
train_idx <- sample(seq_len(nrow(data.no)), nrow(data.no) * .8)
train <- data.no[train_idx,]
test <- rbind(data.no[-train_idx,], outliers)
paste("Train size:", nrow(train), "Test size:", nrow(test))
hmod.no <- train(height ~ hand , train, method = "lm")
summary(hmod.no) 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.42384    0.14167   2.992  0.00337 ** 
# hand         0.64692    0.01808  35.788  < 2e-16 ***

bestfit.int <- summary(hmod.no)$coefficients['(Intercept)', 'Estimate']
bestfit.slope <- summary(hmod.no)$coefficients['hand', 'Estimate']

data$predicted <- predict(hmod.no, newdata = data)

# Calculate the average error over the core population
data %>% filter(hand > 6, hand < 9) %>%
    mutate(diff = abs(height - predicted)) %>%
    summarize(avg = mean(diff))
# 0.05 feet ~ 0.6 inches

coords <- data %>% 
    mutate(grp = row_number()) %>% 
    select(grp, hand, height, predicted) %>%
    rename(actual = height) %>%
    gather(h.type, height, actual, predicted) 

g5 <- ggplot(data = data, aes(hand, height)) +
    geom_line(data = coords, aes(hand, height, group = grp), size = 0.25) +
    geom_abline(slope = bestfit.slope, intercept = bestfit.int) +
    geom_point(aes(color = sex), size = 0.5) +

    # Reduced figure limits
    scale_x_continuous(limits = c(6.75, 9), expand = c(0, 0)) +
    scale_y_continuous(limits = c(4.75, 6.25), expand = c(0, 0), 
                       breaks = seq(4, 7, 0.5), labels = c("4'0\"", "4'6\"", "5'0\"", "5'6\"", "6'0\"", "6'6\"", "7'0\"")) +
    labs(x = 'Hand Size (in)', y = 'Height') +
    scale_color_manual(values = c("#ED58BD", "#34B0C2")) +

    theme_Publication() +
    theme(legend.justification = c(1, 0), legend.position = c(0.95, 0.05))
g5
#ggsave(file = "figures/5.nofit.errors.zoom.jpeg", g5, width = 5, height = 5, dpi = 200)

plot.errors <- function(alldata, data.no, model, rmsd, t, filename) {
    rmsd <- round(rmsd, 5)
    bestfit.int <- summary(model)$coefficients['(Intercept)', 'Estimate']
    bestfit.slope <- summary(model)$coefficients['hand', 'Estimate']
    
    data.no$predicted <- predict(model, newdata = data.no)
    coords <- data.no %>% 
        mutate(grp = row_number()) %>% 
        select(grp, hand, height, predicted) %>%
        rename(actual = height) %>%
        gather(h.type, height, actual, predicted) 
    
    g <- ggplot(data = alldata, aes(hand, height)) + 
        geom_line(data = coords, aes(hand, height, group = grp), size = 0.25) +
        geom_abline(slope = bestfit.slope, intercept = bestfit.int) +
        geom_point(aes(color = sex)) +
        geom_label(data = data.frame(), aes(x = 7.75, y = 4.9, label = paste("RMSD:", rmsd)), size = 5, hjust = 0) +
        scale_x_continuous(limits = c(6.75, 9), expand = c(0, 0)) +
        scale_y_continuous(limits = c(4.75, 6.25), expand = c(0, 0), breaks = seq(4, 7, 0.5), 
                           labels = c("4'0\"", "4'6\"", "5'0\"", "5'6\"", "6'0\"", "6'6\"", "7'0\"")) +
        labs(x = 'Hand Size (in)', y = 'Height') +
        scale_color_manual(values=c("#ED58BD","#34B0C2")) +
        annotation_custom(grob = ggplotGrob(t), xmin = 6.9, xmax = 7.9, ymin = 5.75, ymax = 6.25) +
        theme_Publication() +
        theme(
            legend.justification = c(1, 0), legend.position = c(0.95, 0.05)
        )
    
    #ggsave(file = filename, g, dpi = 100)
    return(g)
}

plot.timeline <- function(series) {
    t <- ggplot(data = series, aes(x = outliers, y = RMSD)) +
        geom_line(color = 'red', size = 0.75) +
        scale_x_continuous(limits = c(0, 80), expand = c(0, 0), breaks = c(0, 20, 40, 60, 80)) +
        scale_y_continuous(limits = c(0.045, 0.123), expand = c(0, 0)) +
        labs(x = 'Outliers', y = 'RMSD') +
        theme_Publication() +
        theme(
            panel.grid.major = element_blank()
            )
    return(t)
}

RMSD <- function(actual, pred, M=mean) {
    data.frame('actual' = actual, 'pred' = pred) %>%
        mutate(se = (actual - pred) ^ 2) %>%
        summarize(RMSD = sqrt(M(se)))
}

model.core <- function(alldata, train, test, n.outliers, timeline) {
    meanhand <- mean(train$hand)
    train.dist <- abs(meanhand - train$hand)
    train.no <- train[order(-train.dist),][(n.outliers+1):nrow(train),]
    cat(paste("Train:", min(train.no$hand), "->", max(train.no$hand), "\n"))

    hmod.no <- train(height ~ hand, train.no, method = "lm")
    rmsd <- RMSD(test$height, predict(hmod.no, newdata = test))
    timeline <- rbind(timeline, data.frame(outliers = n.outliers, rmsd = rmsd))
    
    t <- plot.timeline(timeline)
    plot.errors(alldata, rbind(train.no, test), hmod.no, rmsd, t, paste("BestFit", n.outliers, ".png", sep = ''))
    
    return(timeline)
}

set.seed(12345)
train_idx <- sample(seq_len(nrow(data)), nrow(data) * 0.8)
train <- data[train_idx,]
test <- data[-train_idx,]
cat(paste("Test:", min(test$hand), "->", max(test$hand), "\n"))

paste("Train size:", nrow(train), "Test size:", nrow(test))

timeline <- data.frame(outliers = numeric(), rmsd = numeric())
for (i in 0:80) {
    timeline <- model.core(data, train, test, i, timeline)
}
