setwd("C:\\Users\\mehdi\\OneDrive\\TU_Wien_Uni\\PycharmProjects\\ED\\CollaborativeMemoryNetwork")


# Citeulike
citeulike <- read.csv2("citeulike_comparison.csv", header = F)

paper <- citeulike[2:5, 1:6]
colnames(paper) <- t(paper[1,])
paper <- paper[-1,]
paper <- reshape2::melt(paper, id.vars=c("dataset", "hops"))
paper$value <- as.numeric(stringr::str_replace_all(paper$value, ",", "."))

tensorflow <- citeulike[2:5, c(1:2, 7:10)]
colnames(tensorflow) <- t(tensorflow[1,])
tensorflow <- tensorflow[-1,]
tensorflow <- reshape2::melt(tensorflow, id.vars=c("dataset", "hops"))
tensorflow$value <- as.numeric(stringr::str_replace_all(tensorflow$value, ",", "."))


pytorch <- citeulike[2:5, c(1:2, 11:14)]
colnames(pytorch) <- t(pytorch[1,])
pytorch <- pytorch[-1,]
pytorch <- reshape2::melt(pytorch, id.vars=c("dataset", "hops"))
pytorch$value <- as.numeric(stringr::str_replace_all(pytorch$value, ",", "."))


# Significance test on the difference between group means
t.test(paper$value, tensorflow$value, paired = TRUE)
t.test(paper[paper$hops != 1,]$value, pytorch[paper$hops != 1,]$value, paired = TRUE)


# Pinterest
pinterest <- read.csv2("pinterest_comparison.csv", header = F)

paper <- pinterest[2:5, 1:5]
colnames(paper) <- t(paper[1,])
paper <- paper[-1,]
paper <- reshape2::melt(paper, id.vars=c("hops"))
paper$value <- as.numeric(stringr::str_replace_all(paper$value, ",", "."))

tensorflow <- pinterest[2:5, c(1, 6:9)]
colnames(tensorflow) <- t(tensorflow[1,])
tensorflow <- tensorflow[-1,]
tensorflow <- reshape2::melt(tensorflow, id.vars=c("hops"))
tensorflow$value <- as.numeric(stringr::str_replace_all(tensorflow$value, ",", "."))


pytorch <- pinterest[2:5, c(1, 10:13)]
colnames(pytorch) <- t(pytorch[1,])
pytorch <- pytorch[-1,]
pytorch <- reshape2::melt(pytorch, id.vars=c("hops"))
pytorch$value <- as.numeric(stringr::str_replace_all(pytorch$value, ",", "."))


# Significance test on the difference between group means
t.test(paper[paper$hops == 1,]$value, tensorflow[tensorflow$hops == 1,]$value, paired = TRUE)
t.test(paper[paper$hops != 1,]$value, pytorch[paper$hops != 1,]$value, paired = TRUE)


# Epinions
epinions <- read.csv("epinions_comparison.csv")


library(dplyr)
epinions <- epinions %>%
  group_by(hops, metric) %>%
  filter(embedding_size == max(embedding_size, na.rm=TRUE)) %>%
  filter(hops != 3)


tensorflow_epinions <- cbind(epinions[epinions$metric==5,3],
      hr5=epinions[epinions$metric==5,]$hr,
      hr10=epinions[epinions$metric==10,]$hr,
      ndcg5=epinions[epinions$metric==5,]$ndcg,
      ndcg10=epinions[epinions$metric==10,]$ndcg
)
tensorflow_epinions <- as.data.frame(tensorflow_epinions)

colnames(tensorflow_epinions) <- c("hops", 
                                   "HR@5", "HR@10", 
                                   "NDCG@5", "NDCG@10")
tensorflow_epinions
tensorflow_epinions <- reshape2::melt(tensorflow_epinions, id.vars=c("hops"))


paper_epinions <- data.frame(implementation = rep("paper", 3),
                             dataset = rep("epinions", 3),
                             hops = c(1,2,3),
                             "HR@5"=c(0.5962, 0.6017, 0.6020),
                             "HR@10"=c(0.6943, 0.7007, 0.6985),
                             "NDCG@5"=c(0.4684, 0.4724, 0.4748),
                             "NDCG@10"=c(0.5003, 0.5045, 0.5062)
)
colnames(paper_epinions) <- c("implementation", "dataset",
                              "hops", "HR@5", "HR@10",
                              "NDCG@5", "NDCG@10")
paper_epinions <- paper_epinions[,3:7]
paper_epinions <- reshape2::melt(paper_epinions, id.vars=c("hops"))
paper_epinions <- paper_epinions[paper_epinions$hops != 3,]

write.csv(reshape(paper_epinions, idvar = c("hops"), timevar = "variable", direction = "wide"),"paper_epinions.csv")

# Significance test on the difference between group means
t.test(paper_epinions$value, tensorflow_epinions$value, paired = TRUE)



