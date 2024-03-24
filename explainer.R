require(caret)
require(lime)
require(caret)
require(MBMethPred)
require(ggplot2)
require(gridExtra)
require(caTools)

# by @biomedical_informatics Edris Sharif Rahmani

explainer <- function(Data, 
                      SplitRatio, 
                      CV, 
                      N_test, 
                      Top) {
  set.seed(1235) 
  fac <- ncol(Data)
  split <- sample.split(Data[, fac], 
                        SplitRatio = SplitRatio)
  training_set <- subset(Data, 
                         split == TRUE)
  test_set <- subset(Data, 
                     split == FALSE)
  ctrl <- trainControl(method = "cv",
                       number = CV)
  classifier <- train(class ~ ., 
                      data = training_set, 
                      method = "rf", 
                      trControl = ctrl)
  y_pred <- predict(classifier, 
                    newdata = test_set[-fac])
  ConfusionMatrix(test_set[, fac],
                             y_pred)
  samples_test <- sample(seq_len(nrow(test_set)),
                         N_test)
  explainer <- lime(training_set[,-fac], 
                    classifier, 
                    bin_continuous = TRUE, 
                    quantile_bins = FALSE)
  explanation <- explain(test_set[samples_test,-fac],
                         explainer, 
                         labels = test_set[,fac][samples_test],
                         n_features = Top)
  plots <- list()
  for (label in levels(factor(explanation$label))) {
    lavel <- explanation[which(explanation[,"label"] == label), ]
    lavel$color <- ifelse(lavel$feature_value > 0,
                          "positive",
                          "negative")
    lavel <- lavel[order(abs(lavel$feature_value), 
                         decreasing = T),]
    lavel <- lavel[!duplicated(lavel$feature_value),]
    lavel <- lavel[1:Top,]
    my_pal <- c("#7570B3", "#E7298A")
    g <- ggplot(lavel, 
                aes(reorder(feature, 
                            feature_value, 
                            sum),
                           feature_value)) +
      geom_col(aes(color = color, 
                   fill = color), 
               width = 0.4) +
      xlab("") + 
      ylab("Feature value contribution") + 
      scale_fill_manual(values=my_pal) +
      scale_color_manual(values= my_pal) +
      theme_classic() + 
      coord_flip() +
      theme(axis.text.x = element_text(family = "Times",
                                       colour = "black", 
                                       size = 10, 
                                       angle = 90, 
                                       hjust = 1, 
                                       vjust = 0.5),
            axis.text.y = element_text(family = "Times",
                                       colour = "black",
                                       size = 10),
            plot.subtitle = element_text(family = "Times",
                                         size = 10, 
                                         colour = "black",
                                         hjust = 0.5),
            axis.title.y = element_text(family = "Times",
                                        size = 10,
                                        angle = 90,
                                        hjust = 0.5),
            axis.title.x = element_text(family = "Times", 
                                        size = 12,
                                        angle = 00),
            legend.position="none") +
      labs(subtitle = label)
    plots[[label]] <- g
  }
  return(grid.arrange(grobs = plots, nrow = 2))
}

# Your data : a data frame
# The last column should be class variable

load("Data1.RData")

explainer(Data = Data1, 
          SplitRatio = 0.8, 
          CV = 5,
          N_test = 10,
          Top = 20)
