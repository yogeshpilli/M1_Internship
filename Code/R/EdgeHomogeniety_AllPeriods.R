source('./library_loader.R')
source("./utils.R")


#Calculation from Scratch, if you want to go to results you can load the data from which is already filtered 

brexit<-  read.csv('./Data/Reddit_Brexit_All.csv')

brexit$polarization_class = factor(brexit$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))


plot(density(brexit$polarization_class))


brexit <- brexit %>%
  mutate(EntryID = if_else(condition = is.na(CommentID), true = SubmissionID, false = CommentID) ) 


brexit <- select(brexit, Author, SubmissionID, ParentID, CommentID, polarization_prob, polarization_class,EntryID)

colnames(brexit) <- c("Author", "SubmissionID", "ParentID", "CommentID", "Score", "Stance","EntryID")

brexit <- select(brexit, Author,SubmissionID,ParentID,CommentID, Score, Stance,EntryID )

#Checking if our calculation is correct on small thread
dt1<- subset(brexit, SubmissionID == "476nsv")

dt11 <- dt1 %>%
  mutate(j = match(ParentID,EntryID),
         Reply = Author[j]) %>%
  filter(!is.na(j)) %>%
  select(-j) %>%
  select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID, Reply) %>%
  left_join(brexit %>% select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


dt111 <- subset(dt11,ParentID.x == EntryID.y)

dt111<- subset(dt111,ParentID.x == "476nsv")

dt111 <- select(dt111, Author, Score.x,Stance.x,ParentID.x, Reply, Score.y, Stance.y,EntryID.y )

colnames(dt111) <- c("Target","TScore","TStance","TargetID","Source","SScore","SStance","SourceID")

## reorder columns
dt111<- dt111 %>%
  select(Source, SScore, SStance,SourceID,TargetID, Target,TScore,TStance)




df4 <- brexit %>%
  mutate(k = match(ParentID,EntryID),
         Reply = Author[k]) %>%
  filter(!is.na(k)) %>%
  select(-k) %>%
  select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID, Reply) %>%
  left_join(brexit %>% select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df5 <- subset(df4,ParentID.x == EntryID.y)


df5 <- select(df5, Author, Score.x,Stance.x,ParentID.x, Reply, Score.y, Stance.y,EntryID.y )

colnames(df5) <- c("Target","TScore","TStance","TargetID","Source","SScore","SStance","SourceID")

## reorder columns
df5 <- df5 %>%
  select(Source, SScore, SStance,SourceID,TargetID, Target,TScore,TStance)



df5 <- df5 %>%
  mutate(EdgeHomogeneity = (SScore * TScore))


df_source_N <- subset(df5, SStance == "Neutral")

df_source_A <- subset(df5, SStance == "AgainstBrexit")

df_source_P <- subset(df5, SStance ==  "ProBrexit")


# png("PDF_Edge_Root_FirstCommenter_Neutral.png", width = 3200, height = 1800, res = 300)
# ggplot(data =df_source_N,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5)+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Neutral and Target Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# png("PDF_Edge_Root_FirstCommenter_SA.png", width = 3200, height = 1800, res = 300)
# ggplot(data =df_source_A,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5)+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Against and Target Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# 
# png("PDF_Edge_Root_FirstCommenter_P.png", width = 3200, height = 1800, res = 300)
# ggplot(data =df_source_P,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5)+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as ProBrexit and Target Node other stances ") +
#   theme_classic()
# dev.off()


df_Target_N <- subset(df5, TStance == "Neutral")

df_Target_A <- subset(df5, TStance == "AgainstBrexit")

df_Target_P <- subset(df5, TStance ==  "ProBrexit")


# png("PDF_Edge_Root_FirstCommenter_TNN.png", width = 3200, height = 1800, res = 300)
# ggplot(data =df_Target_N,aes(EdgeHomogeneity))+
#   geom_density()+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Neutral and Source Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# png("PDF_Edge_Root_FirstCommenter_TAA.png", width = 3200, height = 1800, res = 300)
# ggplot(data =df_Target_A,aes(EdgeHomogeneity))+
#   geom_density()+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Against and Source Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# 
# png("PDF_Edge_Root_FirstCommenter_TPP.png", width = 3200, height = 1800, res = 300)
# ggplot(data =df_Target_P,aes(EdgeHomogeneity))+
#   geom_density()+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as ProBrexit and Source Node other stances ") +
#   theme_classic()
# dev.off()




write.csv(df_source_A, './Data/Root_FirstCommenters_Source_Target_Data_Against.csv')


write.csv(df_source_N, './Data/Root_FirstCommenters_Source_Target_Data_Neutral.csv')


write.csv(df_source_P, './Data/Root_FirstCommenters_Source_Target_Data_ProBrexit.csv')


write.csv(df_Target_N, './Data/Root_FirstCommenters_Target_Source_Data_Neutral.csv')


write.csv(df_Target_A, './Data/Root_FirstCommenters_Target_Source_Data_Against.csv')


write.csv(df_Target_P, './Data/Root_FirstCommenters_Target_Source__Data_ProBrexit.csv')



#Checking if our calculation is correct on small thread on second Commenter
dtt1<- subset(brexit, SubmissionID == "476nsv")

dtt11 <- dtt1 %>%
  mutate(j = match(ParentID,CommentID),
         Reply = Author[j]) %>%
  filter(!is.na(j)) %>%
  select(-j) %>%
  select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID, Reply) %>%
  left_join(brexit %>% select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


dtt111 <- subset(dtt11,ParentID.x == CommentID.y)



df3 <- brexit %>%
  mutate(p = match(ParentID,CommentID),
         Reply = Author[p]) %>%
  filter(!is.na(p)) %>%
  select(-p) %>%
  select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID, Reply) %>%
  left_join(brexit %>% select(Author, Score, Stance,ParentID,CommentID,SubmissionID,EntryID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df6 <- subset(df3,ParentID.x == CommentID.y)


df6 <- select(df6, Author, Score.x,Stance.x,ParentID.x, Reply, Score.y, Stance.y,CommentID.y )

colnames(df6) <- c("Target","TScore","TStance","TargetID","Source","SScore","SStance","SourceID")

## reorder columns
df6 <- df6 %>%
  select(Source, SScore, SStance,SourceID,TargetID, Target,TScore,TStance)



df6<-df6 %>%
  mutate(EdgeHomogeneity = (SScore * TScore))



dff_source_N <- subset(df6, SStance == "Neutral")

dff_source_A <- subset(df6, SStance == "AgainstBrexit")

dff_source_P <- subset(df6, SStance ==  "ProBrexit")


# png("PDF_Edge_SecondCommenter_Neutral.png", width = 3200, height = 1800, res = 300)
# ggplot(data =dff_source_N,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5)+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Neutral and Target Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# png("PDF_Edge_SecondCommenter_Against.png", width = 3200, height = 1800, res = 300)
# ggplot(data =dff_source_A,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5)+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Against and Target Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# 
# png("PDF_Edge_SecondCommenter_ProBrexit.png", width = 3200, height = 1800, res = 300)
# ggplot(data =dff_source_P,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5)+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as ProBrexit and Target Node other stances ") +
#   theme_classic()
# dev.off()


dff_Target_N <- subset(df6, TStance == "Neutral")

dff_Target_A <- subset(df6, TStance == "AgainstBrexit")

dff_Target_P <- subset(df6, TStance ==  "ProBrexit")


# png("PDF_Edge_SecondCommenter_TNN.png", width = 3200, height = 1800, res = 300)
# ggplot(data =dff_Target_N,aes(EdgeHomogeneity))+
#   geom_density()+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Neutral and Source Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# png("PDF_Edge_SecondCommenter_TAA.png", width = 3200, height = 1800, res = 300)
# ggplot(data =dff_Target_A,aes(EdgeHomogeneity))+
#   geom_density()+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Against and Source Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# 
# png("PDF_Edge_SecondCommenter_TPP.png", width = 3200, height = 1800, res = 300)
# ggplot(data =dff_Target_P,aes(EdgeHomogeneity))+
#   geom_density()+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as ProBrexit and Source Node other stances ") +
#   theme_classic()
# dev.off()


write.csv(dff_source_A, './Data/Other_Commenters_Source_Target_Data_Against.csv')


write.csv(dff_source_N, './Data/Other_Commenters_Source_Target_Data_Neutral.csv')


write.csv(dff_source_P, './Data/Other_Commenters_Source_Target_Data_ProBrexit.csv')


write.csv(dff_Target_N, './Data/Other_Commenters_Target_Source_Data_Neutral.csv')


write.csv(dff_Target_A, './Data/Other_Commenters_Target_Source_Data_Against.csv')


write.csv(dff_Target_P, './Data/Other_Commenters_Target_Source__Data_ProBrexit.csv')


dff_source_A <- read.csv('./Data/Other_Commenters_Source_Target_Data_Against.csv')
dff_source_N <- read.csv('./Data/Other_Commenters_Source_Target_Data_Neutral.csv')
dff_source_P <- read.csv('./Data/Other_Commenters_Source_Target_Data_ProBrexit.csv')

dff_Target_A <- read.csv('./Data/Other_Commenters_Target_Source_Data_Against.csv')
dff_Target_N <- read.csv('./Data/Other_Commenters_Target_Source_Data_Neutral.csv')
dff_Target_P <- read.csv('./Data/Other_Commenters_Target_Source__Data_ProBrexit.csv')


df_source_A <- read.csv('./Data/Root_FirstCommenters_Source_Target_Data_Against.csv')
df_source_N <- read.csv('./Data/Root_FirstCommenters_Source_Target_Data_Neutral.csv')
df_source_P <- read.csv('./Data/Root_FirstCommenters_Source_Target_Data_ProBrexit.csv')

df_Target_N <- read.csv('./Data/Root_FirstCommenters_Target_Source_Data_Neutral.csv')
df_Target_A <- read.csv('./Data/Root_FirstCommenters_Target_Source_Data_Against.csv')
df_Target_P <-read.csv('./Data/Root_FirstCommenters_Target_Source__Data_ProBrexit.csv')



#Combining Both the firstcommenters and other commenters


Data_Grouped_Source_Against <- rbind(df_source_A,dff_source_A)

Data_Grouped_Source_Neutral <- rbind(df_source_N,dff_source_N)

Data_Grouped_Source_ProBrexit <- rbind(df_source_P,dff_source_P)


Data_Grouped_Target_Against <- rbind(df_Target_A,dff_Target_A)

Data_Grouped_Target_Neutral <- rbind(df_Target_N,dff_Target_N)

Data_Grouped_Target_ProBrexit <- rbind(df_Target_P,dff_Target_P)



# #plotting the pdf of edge homogeneity
# 
# 
# png("PDF_Edge_Grouped_Source_Neutral.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Grouped_Source_Neutral,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5,colour="#000099")+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Neutral and Target Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# png("PDF_Edge_Grouped_Source_Against.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Grouped_Source_Against ,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5,colour ='red')+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Against and Target Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# 
# png("PDF_Edge_Grouped_Source_ProBrexit.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Grouped_Source_ProBrexit,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5,colour ='green')+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as ProBrexit and Target Node other stances ") +
#   theme_classic()
# dev.off()
# 
# 
# 
# 
# 
# 
# png("PDF_Edge_Grouped_Target_Neutral.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Grouped_Target_Neutral,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5,colour="#000099")+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Neutral and Source Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# png("PDF_Edge_Grouped_Target_Against.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Grouped_Target_Against,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5,colour ='red')+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Against and Source Node other stances") +
#   theme_classic()
# dev.off()
# 
# 
# 
# png("PDF_Edge_Grouped_Target_ProBrexit.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Grouped_Target_ProBrexit,aes(EdgeHomogeneity))+
#   geom_density(lwd=1.5,colour ='green')+
#   labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as ProBrexit and Source Node other stances ") +
#   theme_classic()
# dev.off()



write.csv(Data_Grouped_Source_Against, './Data/Data_Grouped_Source_Target_Data_Against.csv')


write.csv(Data_Grouped_Source_Neutral, './Data/Data_Grouped_Source_Target_Data_Neutral.csv')


write.csv(Data_Grouped_Source_ProBrexit, './Data/Data_Grouped_Source_Target_Data_ProBrexit.csv')


write.csv(Data_Grouped_Target_Against, './Data/Data_Grouped_Target_Source_Data_Neutral.csv')


write.csv(Data_Grouped_Target_Neutral, './Data/Data_Grouped_Target_Source_Data_Against.csv')


write.csv(Data_Grouped_Target_ProBrexit, './Data/Data_Grouped_Target_Source__Data_ProBrexit.csv')






Data_Grouped_Source_Against <- read.csv('./Data/Data_Grouped_Source_Target_Data_Against.csv')


Data_Grouped_Source_Neutral <- read.csv('./Data/Data_Grouped_Source_Target_Data_Neutral.csv')


Data_Grouped_Source_ProBrexit <- read.csv('./Data/Data_Grouped_Source_Target_Data_ProBrexit.csv')



Data_Grouped_Target_Against <- read.csv('./Data/Data_Grouped_Target_Source_Data_Neutral.csv')

Data_Grouped_Target_Neutral <- read.csv('./Data/Data_Grouped_Target_Source_Data_Against.csv')

Data_Grouped_Target_ProBrexit <- read.csv('./Data/Data_Grouped_Target_Source__Data_ProBrexit.csv')



# Pie Chart from data frame with Appended Sample Sizes
png("PieChartCase1_S1.png", width = 3200, height = 1800, res = 300)
mytable <- table(Data_Grouped_Source_Against$TStance)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of TargetNode when SourceNode is Against\n (with sample sizes)")
dev.off()

png("PieChartCase1_S2.png", width = 3200, height = 1800, res = 300)
mytable <- table(Data_Grouped_Source_Neutral$TStance)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of TargetNode when SourceNode is Neutral\n (with sample sizes)")
dev.off()

png("PieChartCase1_S3.png", width = 3200, height = 1800, res = 300)
mytable <- table(Data_Grouped_Source_ProBrexit$TStance)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of TargetNode when SourceNode is ProBrexit\n (with sample sizes)")
dev.off()
  
png("PieChartCase2_S1.png", width = 3200, height = 1800, res = 300)
mytable <- table(Data_Grouped_Target_Against$SStance)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of SourceNode when TargetNode is Against\n (with sample sizes)")
dev.off()

png("PieChartCase2_S2.png", width = 3200, height = 1800, res = 300)
mytable <- table(Data_Grouped_Target_Neutral$SStance)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of SourceNode when TargetNode is Neutral\n (with sample sizes)")
dev.off()

png("PieChartCase2_S3.png", width = 3200, height = 1800, res = 300)
mytable <- table(Data_Grouped_Target_ProBrexit$SStance)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart of SourceNode when TargetNode is ProBrexit\n (with sample sizes)")
dev.off()


Data_Case1 <- rbind(Data_Grouped_Source_Against,Data_Grouped_Source_Neutral,Data_Grouped_Source_ProBrexit)



library(scales)


# png("PDF_Edge_Grouped_CASE2.png", width = 3200, height = 1800, res = 300)
# ggplot(data =Data_Case1,aes(EdgeHomogeneity,color=factor(TStance)))+
#   geom_density(aes(linetype = factor(TStance)),lwd=1.5,adjust =25)
# labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 ") +theme_classic()+ 
#   scale_colour_manual("Stances",values = c("orange", "blue","red"))+
#   scale_y_log10()
# dev.off()
# 
# 
# 
# png("PDF_Edge_Grouped_CASE.png", width = 3200, height = 1800, res = 300)
# 
# ggplot(data =Data_Case11,aes(EdgeHomogeneity.rescaled,color = factor(TStance)))+
#   geom_density(aes(linetype = factor(TStance)),lwd=1.5)
# labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 ") +theme_classic()+ 
#   scale_colour_manual("Stances",values = c("orange", "blue","red"))+
#   scale_y_log10()
# dev.off()



Data_Case_Against_Case1 <- select(Data_Grouped_Source_Against,TStance,SStance,EdgeHomogeneity)

Data_Case_Against_Case2 <- select(Data_Grouped_Target_Against,SStance,TStance,EdgeHomogeneity)


colnames(Data_Case_Against_Case1) <- c('Target','Source','EdgeHomogeneity')


colnames(Data_Case_Against_Case2) <- c('Target','Source','EdgeHomogeneity')


length(Data_Case_Against_Case1) = length(Data_Case_Against_Case2)

Case_Against <- cbind.data.frame(Data_Case_Against_Case1$EdgeHomogeneity,Data_Case_Against_Case2$EdgeHomogeneity ,fill =TRUE )

Case_Against <- data.frame(Case_Against)

plot.data <- data.frame(x = c(Case_Against$Case1, Case_Against$Case2), column = c(rep("Case1", nrow(Case_Against)), rep("Case2", nrow(Case_Against))))


png("PDF_Edge_Grouped_Case1_Case2_Against.png", width = 3200, height = 1800, res = 300)
ggplot(plot.data, aes(x = x, fill = column)) + geom_density(lwd=1.5,alpha = 0.2)+
  xlab("EdgeHomogeneity") + ylab("PDF") +
  scale_fill_manual(name = "Cases", values = c("rosybrown", "tan1"))+theme_classic()+
  ggtitle("PDF of Edge Homogeneity of Case1and Case2 with Source as AgainstBrexit ") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()



Data_Neutral_Case1 <- select(Data_Grouped_Source_Neutral,TStance,SStance,EdgeHomogeneity)

Data_Neutral_Case2 <- select(Data_Grouped_Target_Neutral,SStance,TStance,EdgeHomogeneity)


colnames(Data_Neutral_Case1) <- c('Target','Source','EdgeHomogeneity')

colnames(Data_Neutral_Case2) <- c('Target','Source','EdgeHomogeneity')

length(Data_Neutral_Case1) = length(Data_Neutral_Case2)

Case_Neutral <- cbind(Data_Neutral_Case1$EdgeHomogeneity, Data_Neutral_Case2$EdgeHomogeneity )

Case_Neutral <- data.frame(Case_Neutral)

colnames(Case_Neutral) = c("Case1","Case2")


plot.data1 <- data.frame(x = c(Case_Neutral$Case1, Case_Neutral$Case2), column = c(rep("Case1", nrow(Case_Neutral)), rep("Case2", nrow(Case_Neutral))))

png("PDF_Edge_Grouped_Case1_Case2_Neutral.png", width = 3200, height = 1800, res = 300)
ggplot(plot.data1, aes(x = x, fill = column)) + geom_density(lwd=1.5,alpha = 0.2)+
  xlab("EdgeHomogeneity") + ylab("PDF") +
  scale_fill_manual(name = "Cases", values = c(" blue", "lightskyblue"))+theme_classic()+
  ggtitle("PDF of Edge Homogeneity of Case1and Case2 with Source as Neutral ") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


Data_PRO_Case1 <- select(Data_Grouped_Source_ProBrexit,TStance,SStance,EdgeHomogeneity)

Data_PRO_Case2 <- select(Data_Grouped_Target_ProBrexit,SStance,TStance,EdgeHomogeneity)


colnames(Data_PRO_Case1) <- c('Target','Source','EdgeHomogeneity')

colnames(Data_PRO_Case2) <- c('Target','Source','EdgeHomogeneity')

length(Data_PRO_Case1) = length(Data_PRO_Case2)

Case_PRO <- cbind(Data_PRO_Case1$EdgeHomogeneity, Data_PRO_Case2$EdgeHomogeneity )

Case_PRO <- data.frame(Case_PRO)

colnames(Case_PRO) = c("Case1","Case2")


plot.data2 <- data.frame(x = c(Case_PRO$Case1, Case_PRO$Case2), column = c(rep("Case1", nrow(Case_PRO)), rep("Case2", nrow(Case_PRO))))

png("PDF_Edge_Grouped_Case1_Case2_PRO.png", width = 3200, height = 1800, res = 300)
ggplot(plot.data2, aes(x = x, fill = column)) + geom_density(lwd=1.5,alpha = 0.2)+
  xlab("EdgeHomogeneity") + ylab("PDF") +
  scale_fill_manual(name = "Cases", values = c("green", "greenyellow"))+theme_classic()+
  ggtitle("PDF of Edge Homogeneity of Case1and Case2 with Source as PRO BREXIT ") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()



png("PDF_Edge_Grouped_Case1_ALL.png", width = 3200, height = 1800, res = 300)
ggplot(Data_Case1,aes (EdgeHomogeneity,color=factor(TStance), ..scaled..)) + 
  geom_density(lwd = 2) +
  theme_classic()+
  ggtitle("Scaled Density of Edge Homogeneity of Case1 for all Stances Together")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("TargetStances",values = c("red", "blue","green"))
dev.off()

Data_Case2<- rbind(Data_Grouped_Target_Against,Data_Grouped_Target_Neutral,Data_Grouped_Target_ProBrexit)


png("PDF_Edge_Grouped_Case2_ALL.png", width = 3200, height = 1800, res = 300)
ggplot(Data_Case2,aes (EdgeHomogeneity,color=factor(SStance), ..scaled..)) + 
  geom_density(lwd = 2) +
  theme_classic()+
  ggtitle("Scaled Density of Edge Homogeneity of Case2 for all Stances Together")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("SourceStances",values = c("red", "blue","green"))
dev.off()

Data_Case1_AP <- rbind(Data_Grouped_Source_Against,Data_Grouped_Source_ProBrexit)



png("PDF_Edge_Grouped_Case1_Against_ProBrexit_Stance.png", width = 3200, height = 1800, res = 300)
ggplot(Data_Case1_AP,aes (EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(lwd = 2) +
  theme_classic()+
  ggtitle(" Density of Edge Homogeneity of Case1 for AgainstBrexit and ProBrexit  Stances Together")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("Stances",values = c("red","green"))
dev.off()

Data_Case2_AP  <- rbind(Data_Grouped_Target_Against,Data_Grouped_Target_ProBrexit)


png("PDF_Edge_Grouped_Case2_Against_ProBrexit_Stance.png", width = 3200, height = 1800, res = 300)
ggplot(Data_Case2_AP,aes (EdgeHomogeneity,color=factor(TStance))) + 
  geom_density(lwd = 2) +
  theme_classic()+
  ggtitle(" Density of Edge Homogeneity of Case2 for AgainstBrexit and ProBrexit  Stances Together")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual("Stances",values = c("red","green"))
dev.off()

