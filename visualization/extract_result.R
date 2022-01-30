setwd("C:\\Users\\mehdi\\OneDrive\\TU_Wien_Uni\\PycharmProjects\\ED\\CollaborativeMemoryNetwork\\result\\")

folder_names = list.files()
all_experimental_results = data.frame()

for(folder in folder_names){
  print(folder)
  final_results=t(read.csv(paste0(folder, "\\final_results"), header = F))
  colnames(final_results) <- final_results[1,]
  final_results <- final_results[-1,]
  final_results <- apply(final_results, 2, as.numeric)
  final_results
  
  configuration <- jsonlite::read_json(paste0(folder, "\\config.json"))
  dataset <- gsub(".npz", "", gsub("data/","", configuration$filename))
  configuration <- data.frame(
    dataset=rep(dataset, 10),
    embedding_size=rep(configuration$embed_size, 10),
    hops=rep(configuration$hops, 10)
  )
  
  experimental_results = cbind(configuration, final_results)
  print(experimental_results)
  
  all_experimental_results = rbind(all_experimental_results, experimental_results)
}

all_experimental_results$hops <- as.factor(all_experimental_results$hops)



citeulike <- all_experimental_results[which(all_experimental_results$dataset == "citeulike-a"),]
citeulike

library(ggplot2)
citeulike$hops <- as.factor(citeulike$hops)

citeulike_ndcg_10 <- ggplot(data=citeulike[which(citeulike$metric == 10),], 
       aes(x=embedding_size, 
           y=ndcg, 
           group = hops)) + 
  geom_line(aes(color=hops)) +
  geom_point(aes(shape = hops, color=hops)) +
  scale_y_continuous(breaks=seq(0.45,0.85,0.05)) +
  scale_color_manual(values=c("blue", "green", "red", "purple")) +
  scale_shape_manual(values = c(18, 20, 8, 15)) +
  ylab(label = "NDCG@10") + 
  xlab(label = "Embedding Size") + 
  theme(axis.text = element_text(size=5),
        axis.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.title = element_text(size=7))
  
citeulike_ndcg_10

citeulike_hr_10 <- ggplot(data=citeulike[which(citeulike$metric == 10),], 
                            aes(x=embedding_size, 
                                y=hr, 
                                group = hops)) + 
  geom_line(aes(color=hops)) +
  geom_point(aes(shape = hops, color=hops)) +
  scale_y_continuous(breaks=seq(0.55,0.95,0.05)) +
  scale_color_manual(values=c("blue", "green", "red", "purple")) +
  scale_shape_manual(values = c(18, 20, 8, 15)) +
  ylab(label = "HR@10") + 
  xlab(label = "Embedding Size") + 
  theme(axis.text = element_text(size=5),
        axis.title = element_text(size=7),
        legend.text = element_text(size=6),
        legend.title = element_text(size=7))

citeulike_hr_10 

library(cowplot)
new_leg <- get_legend(citeulike_hr_10 + 
                        guides(color = guide_legend(nrow = 1)) +
                        theme(legend.position = "bottom") 
                      )
combined_plot <- plot_grid(citeulike_hr_10 + theme(legend.position="none"), 
          citeulike_ndcg_10 + theme(legend.position="none"),
          align = 'vh',
          labels = c("A", "B"),
          label_size = 7,
          hjust = -1,
          nrow = 2)
full_plot <- plot_grid(combined_plot, new_leg,
                       ncol = 1, rel_heights = c(1, .1))
full_plot
ggsave("Comparison_Citeulike.png", width = 3, height = 4)






pinterest <- all_experimental_results[which(all_experimental_results$dataset == "pinterest"),]
pinterest <- pinterest[which(pinterest$metric %in% c(5,10)),]


pytorch <- read.csv("../pytorch-results")
pytorch$dataset <- ifelse(pytorch$dataset == "citeulike", "citeulike-a", "pinterest")
pytorch <- pytorch[which(pytorch$pretraining == 0),]
pytorch <- reshape2::melt(pytorch, id.vars=c("dataset", "hops", "pretraining"))

pytorch <- cbind(pytorch,
      do.call(rbind, 
        strsplit(as.character(pytorch$variable), '.', fixed=TRUE)))
pytorch

colnames(pytorch) <- c("dataset", "hops", 
                       "pretraining", "variable", "value",
                       "type", "metric")
pytorch <- pytorch[!colnames(pytorch) %in% c("pretraining", "variable")]


pytorch <- reshape(pytorch, idvar = c("dataset", "hops", "metric"), timevar = "type", direction = "wide")
colnames(pytorch) <- c("dataset", "hops", "metric", "hr", "ndcg")
pytorch <- cbind(implementation="pytorch", pytorch)

tensorflow <- all_experimental_results[all_experimental_results$metric %in% c(5,10) & 
                           ((all_experimental_results$embedding_size == 100 & 
                               all_experimental_results$dataset == "citeulike-a") | 
                              (all_experimental_results$embedding_size == 20 & 
                                 all_experimental_results$dataset == "pinterest"))
                           ,]
tensorflow <- tensorflow[!colnames(tensorflow) %in% c("embedding_size")]
tensorflow <- cbind(implementation="tensorflow", tensorflow)
tensorflow
pytorch


paper_citeulike <- data.frame(implementation = rep("paper", 3),
                     dataset = rep("citeulike-a", 3),
                     hops = c(1,2,3),
                     "HR@5"=c(0.6692, 0.7959, 0.7932),
                     "HR@10"=c(0.7809, 0.8921, 0.8901),
                     "NDCG@5"=c(0.5213, 0.6185, 0.6234),
                     "NDCG@10"=c(0.5575, 0.6500, 0.6551)
)
colnames(paper_citeulike) <- c("implementation", "dataset", 
                               "hops", "HR@5", "HR@10",
                               "NDCG@5", "NDCG@10")




tensorflow_citeulike <- tensorflow[which(tensorflow$dataset == "citeulike-a"),]
tensorflow_citeulike <- tensorflow_citeulike[with(tensorflow_citeulike, order(metric)),]

tensorflow_citeulike_5 <- tensorflow_citeulike[which(tensorflow_citeulike$hops %in% 1:3 & 
                             tensorflow_citeulike$metric == 5),c("implementation", "dataset", "hops", "ndcg", "hr")]
colnames(tensorflow_citeulike_5) <- c("implementation", "dataset", "hops", "NDCG@5", "HR@5")

tensorflow_citeulike_10 <- tensorflow_citeulike[which(tensorflow_citeulike$hops %in% 1:3 & 
                                                       tensorflow_citeulike$metric == 10),c("implementation", "dataset", "hops", "ndcg", "hr")]
colnames(tensorflow_citeulike_10) <- c("implementation", "dataset", "hops", "NDCG@10", "HR@10")
tensorflow_citeulike_10

tensorflow_citeulike <- cbind(tensorflow_citeulike_5, tensorflow_citeulike_10[,c("NDCG@10", "HR@10")])
tensorflow_citeulike



pytorch_citeulike <- pytorch[which(pytorch$dataset == "citeulike-a"),]

pytorch_citeulike_5 <- pytorch_citeulike[which(pytorch_citeulike$hops %in% 1:3 & 
                                                       pytorch_citeulike$metric == 5),c("implementation", "dataset", "hops", "ndcg", "hr")]
colnames(pytorch_citeulike_5) <- c("implementation", "dataset", "hops", "NDCG@5", "HR@5")

pytorch_citeulike_10 <- pytorch_citeulike[which(pytorch_citeulike$hops %in% 1:3 & 
                                                        pytorch_citeulike$metric == 10),c("implementation", "dataset", "hops", "ndcg", "hr")]
colnames(pytorch_citeulike_10) <- c("implementation", "dataset", "hops", "NDCG@10", "HR@10")
pytorch_citeulike_10
pytorch_citeulike <- cbind(pytorch_citeulike_5, pytorch_citeulike_10[,c("NDCG@10", "HR@10")])
pytorch_citeulike


write.csv2(pytorch_citeulike, "../pytorch_citeulike.csv")

write.csv2(tensorflow_citeulike, "../tensorflow_citeulike.csv")

write.csv2(paper_citeulike, "../paper_citeulike.csv")



# Pinterest
paper_pinterest <- data.frame(implementation = rep("paper", 3),
                              dataset = rep("pinterest", 3),
                              hops = c(1,2,3),
                              "HR@5"=c(0.6984, 0.7267, 0.7277),
                              "HR@10"=c(0.8662, 0.8904, 0.8931),
                              "NDCG@5"=c(0.4960, 0.5180, 0.5175),
                              "NDCG@10"=c(0.5507, 0.5714, 0.5715)
)
colnames(paper_pinterest) <- c("implementation", "dataset", 
                               "hops", "HR@5", "HR@10",
                               "NDCG@5", "NDCG@10")


tensorflow_pinterest <- data.frame(implementation = rep("tensorflow", 3),
                                   dataset = rep("pinterest", 3),
                                   hops = c(1),
                                   "HR@5"=c(0.7319477),
                                   "HR@10"=c(0.8874916),
                                   "NDCG@5"=c(0.5281756),
                                   "NDCG@10"=c(0.5789300)
)
colnames(tensorflow_pinterest) <- c("implementation", "dataset", 
                               "hops", "HR@5", "HR@10",
                               "NDCG@5", "NDCG@10")

pytorch_pinterest <- data.frame(implementation = rep("pytorch", 3),
                                   dataset = rep("pinterest", 3),
                                   hops = c(1, 2, 3),
                                   "HR@5"=c(NA, 0.729103, 0.732220),
                                   "HR@10"=c(NA, 0.890264, 0.891840),
                                   "NDCG@5"=c(NA, 0.523141, 0.525794),
                                   "NDCG@10"=c(NA, 0.575743, 0.577961)
)
colnames(pytorch_pinterest) <- c("implementation", "dataset", 
                                    "hops", "HR@5", "HR@10",
                                    "NDCG@5", "NDCG@10")
pytorch_pinterest

write.csv2(pytorch_pinterest, "../pytorch_pinterest.csv")

write.csv2(tensorflow_pinterest, "../tensorflow_pinterest.csv")

write.csv2(paper_pinterest, "../paper_pinterest.csv")








