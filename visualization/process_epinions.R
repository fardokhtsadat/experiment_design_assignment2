library(ggplot2)

setwd("C:\\Users\\mehdi\\OneDrive\\TU_Wien_Uni\\PycharmProjects\\ED\\CollaborativeMemoryNetwork")


epinions <- read.csv("epinions_comparison.csv")
epinions$hops <- as.factor(epinions$hops)
# 
# epinions_ndcg_5 <- ggplot(data=epinions[which(epinions$metric == 5),], 
#                            aes(x=embedding_size, 
#                                y=ndcg, 
#                                group = hops)) + 
#   geom_line(aes(color=hops)) +
#   geom_point(aes(shape = hops, color=hops)) +
#   scale_y_continuous(breaks=seq(0.45,0.85,0.05)) +
#   scale_color_manual(values=c("blue", "green", "red", "purple")) +
#   scale_shape_manual(values = c(18, 20, 8, 15)) +
#   ylab(label = "NDCG@5") + 
#   xlab(label = "Embedding Size")+ 
#   theme(axis.text = element_text(size=14),
#         axis.title = element_text(size=16),
#         legend.text = element_text(size=15),
#         legend.title = element_text(size=16))
# epinions_ndcg_5

epinions_ndcg_10 <- ggplot(data=epinions[which(epinions$metric == 10),], 
                            aes(x=embedding_size, 
                                y=ndcg, 
                                group = hops)) + 
  geom_line(aes(color=hops)) +
  geom_point(aes(shape = hops, color=hops)) +
  scale_y_continuous(breaks=seq(0.45,0.85,0.05)) +
  scale_color_manual(values=c("blue", "green", "red", "purple")) +
  scale_shape_manual(values = c(18, 20, 8, 15)) +
  ylab(label = "NDCG@10") + 
  xlab(label = "Embedding Size")+ 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))
epinions_ndcg_10


# epinions_hr_5 <- ggplot(data=epinions[which(epinions$metric == 5),], 
#                           aes(x=embedding_size, 
#                               y=hr, 
#                               group = hops)) + 
#   geom_line(aes(color=hops)) +
#   geom_point(aes(shape = hops, color=hops)) +
#   scale_y_continuous(breaks=seq(0.45,0.85,0.05)) +
#   scale_color_manual(values=c("blue", "green", "red", "purple")) +
#   scale_shape_manual(values = c(18, 20, 8, 15)) +
#   ylab(label = "HR@5") + 
#   xlab(label = "Embedding Size")
# epinions_hr_5

epinions_hr_10 <- ggplot(data=epinions[which(epinions$metric == 10),], 
                           aes(x=embedding_size, 
                               y=hr, 
                               group = hops)) + 
  geom_line(aes(color=hops)) +
  geom_point(aes(shape = hops, color=hops)) +
  scale_y_continuous(breaks=seq(0.45,0.85,0.05)) +
  scale_color_manual(values=c("blue", "green", "red", "purple")) +
  scale_shape_manual(values = c(18, 20, 8, 15)) +
  ylab(label = "HR@10") + 
  xlab(label = "Embedding Size")+ 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size=15),
        legend.title = element_text(size=16))
epinions_hr_10


library(cowplot)
new_leg <- get_legend(epinions_hr_10 + 
                        guides(color = guide_legend(nrow = 1)) +
                        theme(legend.position = "bottom") 
)
combined_plot <- plot_grid(epinions_hr_10 + theme(legend.position="none"), 
                           epinions_ndcg_10 + theme(legend.position="none"),
                           align = 'vh',
                           labels = c("A", "B"),
                           hjust = -1,
                           ncol = 1)
full_plot <- plot_grid(combined_plot, new_leg,ncol = 1, rel_heights = c(1, .1))
full_plot
ggsave("Comparison_Epinions.png", width = 3, height = 4)



#write.csv(reshape(tensorflow_epinions, idvar = c("hops"), timevar = "variable", direction = "wide"),"epinions_comparison.csv")
