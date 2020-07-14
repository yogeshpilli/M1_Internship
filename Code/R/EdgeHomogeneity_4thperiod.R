source('./library_loader.R')
source("./utils.R")

brexit1<-  read.csv('./Data/DataPeriodwise/Reddit_brexit_period4.csv')

brexit1 <- brexit1 %>%
  mutate(EntryID = if_else(condition = is.na(CommentID), true = SubmissionID, false = CommentID) ) 


brexit1 <- select(brexit1, Author, SubmissionID, ParentID, CommentID, polarization_prob, polarization_class,EntryID)

colnames(brexit1) <- c("Author", "SubmissionID", "ParentID", "CommentID", "Score", "Stance","EntryID")

brexit1 <- select(brexit1, Author,SubmissionID,ParentID,CommentID, Score, Stance,EntryID )


df4 <- brexit1 %>%
  mutate(k = match(ParentID,EntryID),
         Reply = Author[k]) %>%
  filter(!is.na(k)) %>%
  select(-k) %>%
  select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID, Reply) %>%
  left_join(brexit1 %>% select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df5 <- subset(df4,ParentID.x == EntryID.y)


df5 <- select(df5, Author, Score.x,Stance.x,ParentID.x, Reply, Score.y, Stance.y,EntryID.y )

colnames(df5) <- c("Target","TScore","TStance","TargetID","Source","SScore","SStance","SourceID")

## reorder columns
df5 <- df5 %>%
  select(Source, SScore, SStance,SourceID,TargetID, Target,TScore,TStance)



df5 <- df5 %>%
  mutate(EdgeHomogeneity = (SScore * TScore))


df3 <- brexit1 %>%
  mutate(p = match(ParentID,CommentID),
         Reply = Author[p]) %>%
  filter(!is.na(p)) %>%
  select(-p) %>%
  select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID, Reply) %>%
  left_join(brexit1 %>% select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df6 <- subset(df3,ParentID.x == CommentID.y)


df6 <- select(df6, Author, Score.x,Stance.x,ParentID.x, Reply, Score.y, Stance.y,CommentID.y )

colnames(df6) <- c("Target","TScore","TStance","TargetID","Source","SScore","SStance","SourceID")

## reorder columns
df6 <- df6 %>%
  select(Source, SScore, SStance,SourceID,TargetID, Target,TScore,TStance)



df6<-df6 %>%
  mutate(EdgeHomogeneity = (SScore * TScore))




data_grouped <- rbind(df6,df5)



data_S_N <- subset(data_grouped,SStance == "Neutral")

data_S_A <- subset(data_grouped,SStance == "Against")

data_S_P <- subset(data_grouped,SStance == "Brexit")



data_T_N <- subset(data_grouped,TStance == "Neutral")

data_T_A <- subset(data_grouped,TStance == "Against")

data_T_P <- subset(data_grouped,TStance == "Brexit")






Data_Case_Against_Case1 <- select(data_S_A,TStance,SStance,EdgeHomogeneity)

Data_Case_Against_Case2 <- select(data_T_A,SStance,TStance,EdgeHomogeneity)

length(Data_Case_Against_Case1) = length(Data_Case_Against_Case2)


Case_Against <- cbind(Data_Case_Against_Case1$EdgeHomogeneity, Data_Case_Against_Case2$EdgeHomogeneity )


Case_Against <- data.frame(Case_Against)

colnames(Data_Case_Against_Case1) <- c('Target','Source','EdgeHomogeneity')


colnames(Data_Case_Against_Case2) <- c('Target','Source','EdgeHomogeneity')


length(Data_Case_Against_Case1) = length(Data_Case_Against_Case2)

Case_Against <- cbind(Data_Case_Against_Case1$EdgeHomogeneity,Data_Case_Against_Case2$EdgeHomogeneity ,fill =TRUE )

Case_Against <- data.frame(Case_Against)

colnames(Case_Against) = c("Case1","Case2")

plot.data <- data.frame(x = c(Case_Against$Case1, Case_Against$Case2), column = c(rep("Case1", nrow(Case_Against)), rep("Case2", nrow(Case_Against))))


png("PDF_Edge_Grouped_Case1_Case2_Against_period4.png", width = 3200, height = 1800, res = 300)
ggplot(plot.data, aes(x = x, fill = column)) + geom_density(lwd=1.5,alpha = 0.2)+
  xlab("EdgeHomogeneity") + ylab("PDF") +
  scale_fill_manual(name = "Cases", values = c("rosybrown", "tan1"))+theme_classic()+
  ggtitle("PDF of Edge Homogeneity of Case1and Case2 with Source as AgainstBrexit for Time period 4") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()



Data_Neutral_Case1 <- select(data_S_N,TStance,SStance,EdgeHomogeneity)

Data_Neutral_Case2 <- select(data_T_N,SStance,TStance,EdgeHomogeneity)


colnames(Data_Neutral_Case1) <- c('Target','Source','EdgeHomogeneity')

colnames(Data_Neutral_Case2) <- c('Target','Source','EdgeHomogeneity')

length(Data_Neutral_Case1) = length(Data_Neutral_Case2)

Case_Neutral <- cbind(Data_Neutral_Case1$EdgeHomogeneity, Data_Neutral_Case2$EdgeHomogeneity )

Case_Neutral <- data.frame(Case_Neutral)

colnames(Case_Neutral) = c("Case1","Case2")


plot.data1 <- data.frame(x = c(Case_Neutral$Case1, Case_Neutral$Case2), column = c(rep("Case1", nrow(Case_Neutral)), rep("Case2", nrow(Case_Neutral))))

png("PDF_Edge_Grouped_Case1_Case2_Neutral_period4.png", width = 3200, height = 1800, res = 300)
ggplot(plot.data1, aes(x = x, fill = column)) + geom_density(lwd=1.5,alpha = 0.2)+
  xlab("EdgeHomogeneity") + ylab("PDF") +
  scale_fill_manual(name = "Cases", values = c(" blue", "lightskyblue"))+theme_classic()+
  ggtitle("PDF of Edge Homogeneity of Case1and Case2 with Source as Neutral for Time period 4 ") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


Data_PRO_Case1 <- select(data_S_P,TStance,SStance,EdgeHomogeneity)

Data_PRO_Case2 <- select(data_T_P,SStance,TStance,EdgeHomogeneity)


colnames(Data_PRO_Case1) <- c('Target','Source','EdgeHomogeneity')

colnames(Data_PRO_Case2) <- c('Target','Source','EdgeHomogeneity')

length(Data_PRO_Case1) = length(Data_PRO_Case2)

Case_PRO <- cbind(Data_PRO_Case1$EdgeHomogeneity, Data_PRO_Case2$EdgeHomogeneity )

Case_PRO <- data.frame(Case_PRO)

colnames(Case_PRO) = c("Case1","Case2")


plot.data2 <- data.frame(x = c(Case_PRO$Case1, Case_PRO$Case2), column = c(rep("Case1", nrow(Case_PRO)), rep("Case2", nrow(Case_PRO))))

png("PDF_Edge_Grouped_Case1_Case2_PRO_period4.png", width = 3200, height = 1800, res = 300)
ggplot(plot.data2, aes(x = x, fill = column)) + geom_density(lwd=1.5,alpha = 0.2)+
  xlab("EdgeHomogeneity") + ylab("PDF") +
  scale_fill_manual(name = "Cases", values = c("green", "greenyellow"))+theme_classic()+
  ggtitle("PDF of Edge Homogeneity of Case1and Case2 with Source as PRO BREXIT for time period 4 ") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()





png("PDF_Edge_Grouped_Case1_ALL_period4.png", width = 3200, height = 1800, res = 300)
ggplot(data_grouped,aes (EdgeHomogeneity,color=factor(TStance), ..scaled..)) + 
  geom_density(lwd = 2) +
  theme_classic()+
  ggtitle("Scaled Density of Edge Homogeneity of Case1 for all Stances Together for Time Period 4")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("TargetStances",values = c("red","green", "blue"))
dev.off()




png("PDF_Edge_Grouped_Case2_ALL_period4.png", width = 3200, height = 1800, res = 300)
ggplot(data_grouped,aes (EdgeHomogeneity,color=factor(SStance), ..scaled..)) + 
  geom_density(lwd = 2) +
  theme_classic()+
  ggtitle("Scaled Density of Edge Homogeneity of Case2 for all Stances Together for Time Period 4")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("SourceStances",values = c("red","green", "blue"))
dev.off()

