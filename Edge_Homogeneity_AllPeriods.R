source('./library_loader.R')
source("./utils.R")



brexit<-  read.csv('./Data/Reddit_Brexit_All.csv')

brexit$polarization_class = factor(brexit$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

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


png("PDF_Edge_Root_FirstCommenter_Neutral.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_source_N,aes(EdgeHomogeneity))+
  geom_density(lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Neutral and Target Node other stances") +
  theme_classic()
dev.off()


png("PDF_Edge_Root_FirstCommenter_SA.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_source_A,aes(EdgeHomogeneity))+
  geom_density(lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Against and Target Node other stances") +
  theme_classic()
dev.off()



png("PDF_Edge_Root_FirstCommenter_P.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_source_P,aes(EdgeHomogeneity))+
  geom_density(lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as ProBrexit and Target Node other stances ") +
  theme_classic()
dev.off()


df_Target_N <- subset(df5, TStance == "Neutral")

df_Target_A <- subset(df5, TStance == "AgainstBrexit")

df_Target_P <- subset(df5, TStance ==  "ProBrexit")


png("PDF_Edge_Root_FirstCommenter_TNN.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_Target_N,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Neutral and Source Node other stances") +
  theme_classic()
dev.off()


png("PDF_Edge_Root_FirstCommenter_TAA.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_Target_A,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Against and Source Node other stances") +
  theme_classic()
dev.off()



png("PDF_Edge_Root_FirstCommenter_TPP.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_Target_P,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as ProBrexit and Source Node other stances ") +
  theme_classic()
dev.off()




write.csv(df_source_A, './Data/Root_FirstCommenters_Source_Target_Data_Against.csv')


write.csv(df_source_N, './Data/Root_FirstCommenters_Source_Target_Data_Neutral.csv')


write.csv(df_source_P, './Data/Root_FirstCommenters_Source_Target_Data_ProBrexit.csv')


write.csv(df_Target_N, './Data/Root_FirstCommenters_Target_Source_Data_Neutral.csv')


write.csv(df_Target_A, './Data/Root_FirstCommenters_Target_Source_Data_Against.csv')


write.csv(df_Target_P, './Data/Root_FirstCommenters_Target_Source__Data_ProBrexit.csv')











#Checking if our calculation is correct on small thread
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


dtt111 <- select(dtt111, Author, Score.x,Stance.x,ParentID.x, Reply, Score.y, Stance.y,EntryID.y )

colnames(dtt111) <- c("Target","TScore","TStance","TargetID","Source","SScore","SStance","SourceID")



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


png("PDF_Edge_SecondCommenter_Neutral.png", width = 3200, height = 1800, res = 300)
ggplot(data =dff_source_N,aes(EdgeHomogeneity))+
  geom_density(lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Neutral and Target Node other stances") +
  theme_classic()
dev.off()


png("PDF_Edge_SecondCommenter_Against.png", width = 3200, height = 1800, res = 300)
ggplot(data =dff_source_A,aes(EdgeHomogeneity))+
  geom_density(lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Against and Target Node other stances") +
  theme_classic()
dev.off()



png("PDF_Edge_SecondCommenter_ProBrexit.png", width = 3200, height = 1800, res = 300)
ggplot(data =dff_source_P,aes(EdgeHomogeneity))+
  geom_density(lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as ProBrexit and Target Node other stances ") +
  theme_classic()
dev.off()


dff_Target_N <- subset(df6, TStance == "Neutral")

dff_Target_A <- subset(df6, TStance == "AgainstBrexit")

dff_Target_P <- subset(df6, TStance ==  "ProBrexit")


png("PDF_Edge_SecondCommenter_TNN.png", width = 3200, height = 1800, res = 300)
ggplot(data =dff_Target_N,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Neutral and Source Node other stances") +
  theme_classic()
dev.off()


png("PDF_Edge_SecondCommenter_TAA.png", width = 3200, height = 1800, res = 300)
ggplot(data =dff_Target_A,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Against and Source Node other stances") +
  theme_classic()
dev.off()



png("PDF_Edge_SecondCommenter_TPP.png", width = 3200, height = 1800, res = 300)
ggplot(data =dff_Target_P,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as ProBrexit and Source Node other stances ") +
  theme_classic()
dev.off()








df5 <- subset(df4,ParentID.x == SubmissionID.y)
df6 <- subset(df4,SubmissionID.x == ParentID.y)

colnames(df4) <- c("Source","SScore","SStance","Target","TScore","TStance")



df4<-df4 %>%
  mutate(EdgeHomogeneity = (SScore * TScore))


df_source_N <- subset(df4, SStance == "Neutral")

df_source_A <- subset(df4, SStance == "AgainstBrexit")

df_source_P <- subset(df4, SStance ==  "ProBrexit")


png("PDF_EdgeS_SN.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_source_N,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Neutral and Target Node other stances") +
  theme_classic()
dev.off()


png("PDF_EdgeS_SA.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_source_A,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as Against and Target Node other stances") +
  theme_classic()
dev.off()



png("PDF_EdgeS_SP.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_source_P,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Source Node as ProBrexit and Target Node other stances ") +
  theme_classic()
dev.off()


df_Target_N <- subset(df4, TStance == "Neutral")

df_Target_A <- subset(df4, TStance == "AgainstBrexit")

df_Target_P <- subset(df4, TStance ==  "ProBrexit")


png("PDF_EdgeS_TNN.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_Target_N,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Neutral and Source Node other stances") +
  theme_classic()
dev.off()


png("PDF_EdgeS_TAA.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_Target_A,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as Against and Source Node other stances") +
  theme_classic()
dev.off()



png("PDF_EdgeS_TPP.png", width = 3200, height = 1800, res = 300)
ggplot(data =df_Target_P,aes(EdgeHomogeneity))+
  geom_density()+
  labs(x="Edge Homogenity",y="PDF", title = "PDF of Edge Homogeneity of Target Node as ProBrexit and Source Node other stances ") +
  theme_classic()
dev.off()


#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_With_Neutral_Users_On_Source_node_Stance.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_source_N,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Neutral Users on source node stances") +theme_classic()+ 
  scale_colour_manual("SourceStances",values = c("orange", "blue","red"))
dev.off()












#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_With_Neutral_Users_On_Target_node_Stance.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_source_N,aes(EdgeHomogeneity,color=factor(TStance))) + 
  geom_density(aes(linetype = factor(TStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Neutral Users on Target node stances") +theme_classic()+ 
  scale_colour_manual("TargetStances",values = c("orange", "blue","red"))
dev.off()



#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_target_Neutral_SStance_Neutral.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_Target_N,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Neutral Users on target node stances") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_target_Neutral_TStance_other.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_Target_N,aes(EdgeHomogeneity,color=factor(TStance))) + 
  geom_density(aes(linetype = factor(TStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Neutral Users on Source node stances ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()



#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_target_Against_SStance_Against.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_Target_A,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Against Users on target node stances") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_target_Against_TStance_other.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_Target_A,aes(EdgeHomogeneity,color=factor(TStance))) + 
  geom_density(aes(linetype = factor(TStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Against Users on Source node stances ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_target_Probexit_SStance_Against.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_Target_P,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With ProBrexit Users on target node stances") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_target_Probexit_TStance_other.png", width = 3200, height = 1800, res = 300)
ggplot(data=dff_Target_P,aes(EdgeHomogeneity,color=factor(TStance))) + 
  geom_density(aes(linetype = factor(TStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With ProBrexit Users on Source node stances ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()






dff_ST_N <- subset(dff_source_N, TStance == "Neutral")

dff_ST_A <- subset(dff_source_A, TStance == "AgainstBrexit")

dff_ST_P<- subset(dff_source_P, TStance == "ProBrexit")




data_plot_C11 <- rbind(dff_source_A,dff_source_P)

data_plot_C2 <- rbind(dff_ST_A,dff_ST_P,dff_ST_N) 

data_plot_C22 <- rbind(dff_ST_A,dff_ST_P)



#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_Co_Case1_WithNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=df3,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Neutral Users") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()



#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_Co_Case1_WithoutNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_C11,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity without Neutral Users ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()



#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_Co_Case2_WithNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_C2,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF pf Edge Homogeneity Case 2  With Neutral Users") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_Co_Case2_WithoutNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_C22 ,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF pf Edge Homogeneity Case 2 without Neutral Users ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()






df4 <- data_full
df4 <- select(df4, Author, SubmissionID, ParentID, CommentID, polarization_prob, polarization_class)

colnames(df4) <- c("Author", "SubmissionID", "ParentID", "CommentID", "Score", "Stance")






df_thread <- select(df4,SubmissionID)

df_thread <- count(df_thread,SubmissionID)

df_5 <- df4 %>%
  mutate(i = match(ParentID,SubmissionID),
          Reply = Author[i]) %>%
  filter(!is.na(i)) %>%
  select(-i) %>%
  select(Author, Score, Stance, Reply) %>%
  left_join(df4 %>% select(Author, Score, Stance), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


colnames(df_5)<-c("Source","SScore","SStance","Target","TScore","TStance")

df_5 <- df_5 %>%
  mutate(EdgeHomogeneity = (SScore * TScore))


df_source_N <- subset(df_5, SStance == "Neutral")

df_source_A <- subset(df_5, SStance == "AgainstBrexit")

df_sourece_P <- subset(df_5, SStance ==  "ProBrexit")


df_ST_N <- subset(df_source_N, TStance == "Neutral")

df_ST_A <- subset(df_source_A, TStance == "AgainstBrexit")

df_ST_P<- subset(df_sourece_P, TStance == "ProBrexit")


data_plot_case1 <- rbind(df_ST_A,df_ST_N,df_ST_P)

data_plot_case11<- rbind(df_ST_A,df_ST_P)


data_plot_case2 <- rbind(df_source_A ,df_sourece_P , df_source_N)


data_plot_case22 <- rbind(df_source_A ,df_sourece_P)

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_Case1s_WithNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_case2,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of With Neutral Users") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


#Plotting for PDF of the Edge_Homogeneity_Cas1
png("PDF_Edge_Homenitythread_Case1s_WithoutNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_case22,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity without Neutral Users ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()







#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_Case2s_WithNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_case1,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF pf Edge Homogeneity Case 2  With Neutral Users") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread_Case2s_WithoutNeutralUsers.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_case11,aes(EdgeHomogeneity,color=factor(SStance))) + 
  geom_density(aes(linetype = factor(SStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity Case 2 without Neutral Users ") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


saveRDS(data_plot_C11, file = './Data/data_plot_C11.rds')

saveRDS(data_plot_C2, file = './Data/data_plot_C2.rds')

saveRDS(data_plot_C22, file = './Data/data_plot_C22.rds')

saveRDS(data_plot_case1, file = './Data/data_plot_case1.rds')

saveRDS(data_plot_case11, file = './Data/data_plot_case11.rds')

saveRDS(data_plot_case2, file = './Data/data_plot_case2.rds')

saveRDS(data_plot_case22, file = './Data/data_plot_case22.rds')



data_all <- rbind(df_5,df3)





# Calculation of edge homogeneity between the user belong to highest thread with 326 user observation as thread1 in the whole data

data_thread1<- subset(data_full, SubmissionID == "amkjm0" )
data_thread1$polarization_class = factor(data_thread1$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))
 
data1<- select(data_thread1,Author,polarization_prob,polarization_class)

data<- select(data_thread1,Author,CommentID,ParentID,SubmissionID,polarization_prob,polarization_class)

colnames(data)<- c("Author","CommentID","ParentID","SubmissionID","Score","Stance")

data_first_commenter <- subset(data_thread1 ,ParentID == "amkjm0" )

data2<- data


df2 <- data2 %>%
  mutate(i = match(ParentID,CommentID),
         Reply = Author[i]) %>%
  filter(!is.na(i)) %>%
  select(-i) %>%
  select(Author, Score, Stance,ParentID, SubmissionID,CommentID, Reply) %>%
  left_join(data2 %>% select(Author, Score, Stance,ParentID, SubmissionID,CommentID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df2 <- subset(df2, ParentID.x == CommentID.y)






data_thread11<- subset(data_full, SubmissionID == "476nsv" )
data_thread11$polarization_class = factor(data_thread11$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data11<- select(data_thread11,Author,polarization_prob,polarization_class)

data22<- select(data_thread11,Author,CommentID,ParentID,SubmissionID,polarization_prob,polarization_class)

colnames(data22)<- c("Author","CommentID","ParentID","SubmissionID","Score","Stance")

data_first_commenter1 <- subset(data_thread11 ,ParentID == "476nsv" )

data2<- data22


df22 <- data2 %>%
  mutate(i = match(SubmissionID,ParentID),
         Reply = Author[i]) %>%
  filter(!is.na(i)) %>%
  select(-i) %>%
  select(Author, Score, Stance,ParentID, SubmissionID,CommentID, Reply) %>%
  left_join(data2 %>% select(Author, Score, Stance,ParentID, SubmissionID,CommentID), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df22 <- subset(df22, ParentID.x == CommentID.y)






dt <- subset(reddit, Submission.ID == "amkjm0" )

dt <- select(dt, Parent.ID, Entry.ID,Comment.ID, Author,Submission.ID,leave_probability, Stance)

dt<- dt %>%
  mutate(PScore = ((2*leave_probability)-1))

hist(dt$PScore)
hist(dt$Stance)

colnames(dt)<- c("ParentID","EntryID","CommentID","Author","SubmissionID","proScore","Stance","PolScore")




df6 <- dt %>% 
  mutate(i = match(ParentID,CommentID),
         Reply = Author[i]) %>%
  filter(!is.na(i)) %>%
  select(-i) %>%
  select(Author, PolScore, Stance, Reply,ParentID,EntryID,CommentID,SubmissionID) 


  
df7 <- df6 %>%
  left_join(dt %>% select(Author, PolScore, Stance), by = c("Reply" = "Author")) %>%
  select(-matches("id$"), everything(), matches("id$"))


df8<- df7 %>%
  mutate(match(ParentID,CommentID))


df8<- df7 %>% 
  filter(!is.na(match(ParentID,CommentID)))


write.csv(data_selected, './Data/df_thread1_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread1_rootnode.csv')
edge2 <- read.csv('./Data/df_thread1_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread1_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread1_Case1, './Data/DataPlotting/data_plot_thread1_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread1_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread1_Case2, './Data/DataPlotting/data_plot_thread1_Case2.csv')


data_plot_thread1_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread1_Case1.csv')
data_plot_thread1_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread1_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread1_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread1_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread1_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread1_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 295 user observation as thread2 in the whole data

data_thread2<- subset(data_full, SubmissionID == "asku8e" )
data_thread2$polarization_class = factor(data_thread2$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data2<- select(data_thread2,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread2,ParentID == "asku8e" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread2_firstCommenter.csv')



edge1 <- read.csv('./Data/df_thread2_rootnode.csv')
edge2 <- read.csv('./Data/df_thread2_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread2_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread2_Case1, './Data/DataPlotting/data_plot_thread2_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread2_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread2_Case2, './Data/DataPlotting/data_plot_thread2_Case2.csv')


data_plot_thread2_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread2_Case1.csv')
data_plot_thread2_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread2_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread2_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread2_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 295 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case2
png("PDF_Edge_Homenitythread2_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread2_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 295 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 283 user observation as thread1 in the whole data

data_thread3<- subset(data_full, SubmissionID == "4pksyi" )
data_thread3$polarization_class = factor(data_thread3$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data3<- select(data_thread3,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread3 ,ParentID == "4pksyi" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread3_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread3_rootnode.csv')
edge2 <- read.csv('./Data/df_thread3_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread3_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread3_Case1, './Data/DataPlotting/data_plot_thread3_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread3_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread3_Case2, './Data/DataPlotting/data_plot_thread3_Case2.csv')


data_plot_thread3_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread3_Case1.csv')
data_plot_thread3_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread3_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread3_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread3_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 283 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread3_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread3_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 283 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 272 user observation as thread1 in the whole data

data_thread4<- subset(data_full, SubmissionID == "b0bha8" )
data_thread4$polarization_class = factor(data_thread4$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread4,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread4 ,ParentID == "b0bha8")

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread4_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread4_rootnode.csv')
edge2 <- read.csv('./Data/df_thread4_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread4_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread4_Case1, './Data/DataPlotting/data_plot_thread4_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread4_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread4_Case2, './Data/DataPlotting/data_plot_thread4_Case2.csv')


data_plot_thread4_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread4_Case1.csv')
data_plot_thread4_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread4_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread4_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread4_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 277 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread4_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread4_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 277 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 272 user observation as thread1 in the whole data

data_thread5<- subset(data_full, SubmissionID == "b5pyse" )
data_thread5$polarization_class = factor(data_thread5$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))


data_first_commenter <- subset(data_thread5 ,ParentID == "b5pyse" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread5_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread5_rootnode.csv')
edge2 <- read.csv('./Data/df_thread5_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread5_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread5_Case1, './Data/DataPlotting/data_plot_thread5_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread5_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread5_Case2, './Data/DataPlotting/data_plot_thread5_Case2.csv')


data_plot_thread5_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread5_Case1.csv')
data_plot_thread5_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread5_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread5_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread5_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count  272 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread5_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread5_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count  272 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 263 user observation as thread1 in the whole data

data_thread6<- subset(data_full, SubmissionID == "agcda2" )
data_thread6$polarization_class = factor(data_thread6$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread6,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread6 ,ParentID == "agcda2" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread6_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread6_rootnode.csv')
edge2 <- read.csv('./Data/df_thread6_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread6_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread6_Case1, './Data/DataPlotting/data_plot_thread6_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread6_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread6_Case2, './Data/DataPlotting/data_plot_thread6_Case2.csv')


data_plot_thread6_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread6_Case1.csv')
data_plot_thread6_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread6_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread6_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread6_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 263 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread6_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread6_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 263 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 258 user observation as thread1 in the whole data

data_thread7<- subset(data_full, SubmissionID == "b3nuiy" )
data_thread7$polarization_class = factor(data_thread7$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread7,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread7 ,ParentID == "b3nuiy" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread7_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread7_rootnode.csv')
edge2 <- read.csv('./Data/df_thread7_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread7_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread7_Case1, './Data/DataPlotting/data_plot_thread7_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread7_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread7_Case2, './Data/DataPlotting/data_plot_thread7_Case2.csv')


data_plot_thread7_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread7_Case1.csv')
data_plot_thread7_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread7_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread7_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread7_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 258 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread7_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread7_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 258 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 243 user observation as thread1 in the whole data

data_thread8<- subset(data_full, SubmissionID == "b3ejdh" )
data_thread8$polarization_class = factor(data_thread8$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread8,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread8 ,ParentID == "b3ejdh" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread8_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread8_rootnode.csv')
edge2 <- read.csv('./Data/df_thread8_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread8_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread8_Case1, './Data/DataPlotting/data_plot_thread8_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread8_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread8_Case2, './Data/DataPlotting/data_plot_thread8_Case2.csv')


data_plot_thread8_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread8_Case1.csv')
data_plot_thread8_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread8_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread8_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread8_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 243  comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread8_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread8_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 243  comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread9 in the whole data

data_thread9<- subset(data_full, SubmissionID == "b0q0lb" )
data_thread9$polarization_class = factor(data_thread9$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread9,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread9 ,ParentID == "b0q0lb" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread9_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread9_rootnode.csv')
edge2 <- read.csv('./Data/df_thread9_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread9_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread9_Case1, './Data/DataPlotting/data_plot_thread9_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread9_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread9_Case2, './Data/DataPlotting/data_plot_thread9_Case2.csv')


data_plot_thread9_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread9_Case1.csv')
data_plot_thread9_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread9_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread9_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread9_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 230 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread9_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread9_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 230 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 222 user observation as thread10 in the whole data

data_thread10<- subset(data_full, SubmissionID == "adfm5g" )
data_thread10$polarization_class = factor(data_thread10$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread10,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread10 ,ParentID == "adfm5g" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread10_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread10_rootnode.csv')
edge2 <- read.csv('./Data/df_thread10_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread10_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread10_Case1, './Data/DataPlotting/data_plot_thread10_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread10_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread10_Case2, './Data/DataPlotting/data_plot_thread10_Case2.csv')


data_plot_thread10_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread10_Case1.csv')
data_plot_thread10_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread10_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread10_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread10_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 222 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread10_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread10_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 222 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 209 userobservation as thread11 in the whole data

data_thread11<- subset(data_full, SubmissionID == "ab8wek" )
data_thread11$polarization_class = factor(data_thread11$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread11,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread11 ,ParentID == "ab8wek" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread11_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread11_rootnode.csv')
edge2 <- read.csv('./Data/df_thread11_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread11_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread11_Case1, './Data/DataPlotting/data_plot_thread11_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread11_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread11_Case2, './Data/DataPlotting/data_plot_thread11_Case2.csv')


data_plot_thread11_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread11_Case1.csv')
data_plot_thread11_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread11_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread11_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread11_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 209 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread11_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread11_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 209 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 209 user observation as thread12 in the whole data

data_thread12<- subset(data_full, SubmissionID == "b4h6ob" )
data_thread12$polarization_class = factor(data_thread12$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread12,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread12 ,ParentID == "b4h6ob" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread12_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread12_rootnode.csv')
edge2 <- read.csv('./Data/df_thread12_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread12_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread12_Case1, './Data/DataPlotting/data_plot_thread12_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread12_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread12_Case2, './Data/DataPlotting/data_plot_thread12_Case2.csv')


data_plot_thread12_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread12_Case1.csv')
data_plot_thread12_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread12_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread12_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread12_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 207 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread12_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread12_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 207 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()
# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread13 in the whole data

data_thread13<- subset(data_full, SubmissionID == "aqu0i4" )
data_thread13$polarization_class = factor(data_thread13$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread13,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread13 ,ParentID == "aqu0i4" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread13_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread13_rootnode.csv')
edge2 <- read.csv('./Data/df_thread13_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread13_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread13_Case1, './Data/DataPlotting/data_plot_thread13_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread13_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread13_Case2, './Data/DataPlotting/data_plot_thread13_Case2.csv')


data_plot_thread13_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread13_Case1.csv')
data_plot_thread13_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread13_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread13_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread13_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread13_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread13_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()



# Calculation of edge homogeneity between the user belong to highest thread with 201 user observation as thread14 in the whole data

data_thread14<- subset(data_full, SubmissionID == "b3rgu1" )
data_thread14$polarization_class = factor(data_thread14$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread14,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread14 ,ParentID == "b3rgu1")

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread14_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread14_rootnode.csv')
edge2 <- read.csv('./Data/df_thread14_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread14_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread14_Case1, './Data/DataPlotting/data_plot_thread14_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread14_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread14_Case2, './Data/DataPlotting/data_plot_thread14_Case2.csv')


data_plot_thread14_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread14_Case1.csv')
data_plot_thread14_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread14_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread14_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread14_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 201 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread14_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread14_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 201 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 200 user observation as thread15 in the whole data

data_thread15<- subset(data_full, SubmissionID == "b3cufx" )
data_thread15$polarization_class = factor(data_thread15$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread15,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread15 ,ParentID == "b3cufx" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread15_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread15_rootnode.csv')
edge2 <- read.csv('./Data/df_thread15_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread15_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread15_Case1, './Data/DataPlotting/data_plot_thread15_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread15_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread15_Case2, './Data/DataPlotting/data_plot_thread15_Case2.csv')


data_plot_thread15_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread15_Case1.csv')
data_plot_thread15_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread15_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread15_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread15_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 200 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread15_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread15_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 200 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()



# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread16 in the whole data

data_thread16<- subset(data_full, SubmissionID == "apebfi" )
data_thread16$polarization_class = factor(data_thread16$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread16,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread16 ,ParentID == "apebfi" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread16_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread16_rootnode.csv')
edge2 <- read.csv('./Data/df_thread16_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread16_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread16_Case1, './Data/DataPlotting/data_plot_thread16_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread16_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread16_Case2, './Data/DataPlotting/data_plot_thread16_Case2.csv')


data_plot_thread16_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread16_Case1.csv')
data_plot_thread16_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread16_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread16_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread16_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread16_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread16_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread17 in the whole data

data_thread17<- subset(data_full, SubmissionID == "4q2lsb" )
data_thread17$polarization_class = factor(data_thread17$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread17,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread17 ,ParentID == "4q2lsb" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread17_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread17_rootnode.csv')
edge2 <- read.csv('./Data/df_thread17_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread17_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread17_Case1, './Data/DataPlotting/data_plot_thread17_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread17_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread17_Case2, './Data/DataPlotting/data_plot_thread17_Case2.csv')


data_plot_thread17_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread17_Case1.csv')
data_plot_thread17_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread17_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread17_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread17_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread17_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread17_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread18 in the whole data

data_thread18<- subset(data_full, SubmissionID == "6bz2fw" )
data_thread18$polarization_class = factor(data_thread18$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread18,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread18 ,ParentID == "6bz2fw" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread18_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread18_rootnode.csv')
edge2 <- read.csv('./Data/df_thread18_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread18_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread18_Case1, './Data/DataPlotting/data_plot_thread18_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread18_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread18_Case2, './Data/DataPlotting/data_plot_thread18_Case2.csv')


data_plot_thread18_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread18_Case1.csv')
data_plot_thread18_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread18_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread18_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread18_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread18_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread18_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread19 in the whole data

data_thread19<- subset(data_full, SubmissionID == "aciva9" )
data_thread19$polarization_class = factor(data_thread19$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread19,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread19 ,ParentID == "aciva9" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread19_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread19_rootnode.csv')
edge2 <- read.csv('./Data/df_thread19_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread19_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread19_Case1, './Data/DataPlotting/data_plot_thread19_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread19_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread19_Case2, './Data/DataPlotting/data_plot_thread19_Case2.csv')


data_plot_thread19_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread19_Case1.csv')
data_plot_thread19_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread19_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread19_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread19_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread19_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread19_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread20 in the whole data

data_thread20<- subset(data_full, SubmissionID == "at1gg2" )
data_thread20$polarization_class = factor(data_thread20$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread20,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread20 ,ParentID == "at1gg2" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread20_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread20_rootnode.csv')
edge2 <- read.csv('./Data/df_thread20_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread20_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread20_Case1, './Data/DataPlotting/data_plot_thread20_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread20_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread20_Case2, './Data/DataPlotting/data_plot_thread20_Case2.csv')


data_plot_thread20_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread20_Case1.csv')
data_plot_thread20_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread20_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread20_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread20_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread20_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread20_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread21 in the whole data

data_thread21<- subset(data_full, SubmissionID == "ao2hrv" )
data_thread21$polarization_class = factor(data_thread21$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread21,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread21 ,ParentID == "ao2hrv" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread21_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread21_rootnode.csv')
edge2 <- read.csv('./Data/df_thread21_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread21_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread21_Case1, './Data/DataPlotting/data_plot_thread21_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread21_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread21_Case2, './Data/DataPlotting/data_plot_thread21_Case2.csv')


data_plot_thread21_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread21_Case1.csv')
data_plot_thread21_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread21_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread21_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread21_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread21_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread21_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread22 in the whole data

data_thread22<- subset(data_full, SubmissionID == "auo0bz" )
data_thread22$polarization_class = factor(data_thread22$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread22,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread22 ,ParentID == "auo0bz" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread22_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread22_rootnode.csv')
edge2 <- read.csv('./Data/df_thread22_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread22_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread22_Case1, './Data/DataPlotting/data_plot_thread22_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread22_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread22_Case2, './Data/DataPlotting/data_plot_thread22_Case2.csv')


data_plot_thread22_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread22_Case1.csv')
data_plot_thread22_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread22_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread22_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread22_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread22_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread22_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread23 in the whole data

data_thread23<- subset(data_full, SubmissionID == "b3qepz" )
data_thread23$polarization_class = factor(data_thread23$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread23,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread23 ,ParentID == "b3qepz" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread23_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread23_rootnode.csv')
edge2 <- read.csv('./Data/df_thread23_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread23_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread23_Case1, './Data/DataPlotting/data_plot_thread23_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread23_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread23_Case2, './Data/DataPlotting/data_plot_thread23_Case2.csv')


data_plot_thread23_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread23_Case1.csv')
data_plot_thread23_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread23_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread23_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread23_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread23_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread23_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread24 in the whole data

data_thread24<- subset(data_full, SubmissionID == "b4jgy6" )
data_thread24$polarization_class = factor(data_thread24$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread24,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread24 ,ParentID == "b4jgy6" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread24_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread24_rootnode.csv')
edge2 <- read.csv('./Data/df_thread24_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread24_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread24_Case1, './Data/DataPlotting/data_plot_thread24_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread24_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread24_Case2, './Data/DataPlotting/data_plot_thread24_Case2.csv')


data_plot_thread24_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread24_Case1.csv')
data_plot_thread24_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread24_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread24_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread24_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread24_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread24_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread25 in the whole data

data_thread25<- subset(data_full, SubmissionID == "agxc3p" )
data_thread25$polarization_class = factor(data_thread25$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread25,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread25 ,ParentID == "agxc3p" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread25_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread25_rootnode.csv')
edge2 <- read.csv('./Data/df_thread25_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread25_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread25_Case1, './Data/DataPlotting/data_plot_thread25_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread25_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread25_Case2, './Data/DataPlotting/data_plot_thread25_Case2.csv')


data_plot_thread25_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread25_Case1.csv')
data_plot_thread25_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread25_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread25_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread25_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread25_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread25_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread26 in the whole data

data_thread26<- subset(data_full, SubmissionID == "au5zep" )
data_thread26$polarization_class = factor(data_thread26$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread26,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread26 ,ParentID == "au5zep" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread26_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread26_rootnode.csv')
edge2 <- read.csv('./Data/df_thread26_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread26_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread26_Case1, './Data/DataPlotting/data_plot_thread26_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread26_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread26_Case2, './Data/DataPlotting/data_plot_thread26_Case2.csv')


data_plot_thread26_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread26_Case1.csv')
data_plot_thread26_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread26_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread26_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread26_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread26_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread26_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread27 in the whole data

data_thread27<- subset(data_full, SubmissionID == "b27uqy" )
data_thread27$polarization_class = factor(data_thread27$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread27,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread27 ,ParentID == "b27uqy" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread27_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread27_rootnode.csv')
edge2 <- read.csv('./Data/df_thread27_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread27_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread27_Case1, './Data/DataPlotting/data_plot_thread27_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread27_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread27_Case2, './Data/DataPlotting/data_plot_thread27_Case2.csv')


data_plot_thread27_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread27_Case1.csv')
data_plot_thread27_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread27_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread27_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread27_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread27_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread27_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread28 in the whole data

data_thread28<- subset(data_full, SubmissionID == "7m9d6t" )
data_thread28$polarization_class = factor(data_thread28$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread28,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread28 ,ParentID == "7m9d6t" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread28_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread28_rootnode.csv')
edge2 <- read.csv('./Data/df_thread28_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread28_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread28_Case1, './Data/DataPlotting/data_plot_thread28_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread28_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread28_Case2, './Data/DataPlotting/data_plot_thread28_Case2.csv')


data_plot_thread28_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread28_Case1.csv')
data_plot_thread28_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread28_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread28_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread28_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread28_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread28_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread29 in the whole data

data_thread29<- subset(data_full, SubmissionID == "ay0zuu" )
data_thread29$polarization_class = factor(data_thread29$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread29,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread29 ,ParentID == "ay0zuu" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread29_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread29_rootnode.csv')
edge2 <- read.csv('./Data/df_thread29_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread29_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread29_Case1, './Data/DataPlotting/data_plot_thread29_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread29_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread29_Case2, './Data/DataPlotting/data_plot_thread29_Case2.csv')


data_plot_thread29_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread29_Case1.csv')
data_plot_thread29_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread29_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread29_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread29_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread29_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread29_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread30 in the whole data

data_thread30<- subset(data_full, SubmissionID == "amobk6" )
data_thread30$polarization_class = factor(data_thread30$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread30,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread30 ,ParentID == "amobk6" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread30_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread30_rootnode.csv')
edge2 <- read.csv('./Data/df_thread30_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread30_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread30_Case1, './Data/DataPlotting/data_plot_thread30_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread30_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread30_Case2, './Data/DataPlotting/data_plot_thread30_Case2.csv')


data_plot_thread30_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread30_Case1.csv')
data_plot_thread30_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread30_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread30_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread30_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread30_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread30_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread31 in the whole data

data_thread31<- subset(data_full, SubmissionID == "as2pv8" )
data_thread31$polarization_class = factor(data_thread31$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread31,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread31 ,ParentID == "as2pv8" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread31_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread31_rootnode.csv')
edge2 <- read.csv('./Data/df_thread31_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread31_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread31_Case1, './Data/DataPlotting/data_plot_thread31_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread31_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread31_Case2, './Data/DataPlotting/data_plot_thread31_Case2.csv')


data_plot_thread31_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread31_Case1.csv')
data_plot_thread31_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread31_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread31_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread31_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread31_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread31_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread32 in the whole data

data_thread32<- subset(data_full, SubmissionID == "ayx8tj" )
data_thread32$polarization_class = factor(data_thread32$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread32,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread32 ,ParentID == "ayx8tj" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread32_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread32_rootnode.csv')
edge2 <- read.csv('./Data/df_thread32_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread32_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread32_Case1, './Data/DataPlotting/data_plot_thread32_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread32_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread32_Case2, './Data/DataPlotting/data_plot_thread32_Case2.csv')


data_plot_thread32_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread32_Case1.csv')
data_plot_thread32_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread32_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread32_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread32_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread32_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread32_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread33 in the whole data

data_thread33<- subset(data_full, SubmissionID == "b5bkme" )
data_thread33$polarization_class = factor(data_thread33$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread33,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread33 ,ParentID == "b5bkme" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread33_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread33_rootnode.csv')
edge2 <- read.csv('./Data/df_thread33_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread33_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread33_Case1, './Data/DataPlotting/data_plot_thread33_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread33_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread33_Case2, './Data/DataPlotting/data_plot_thread33_Case2.csv')


data_plot_thread33_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread33_Case1.csv')
data_plot_thread33_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread33_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread33_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread33_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread33_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread33_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread34 in the whole data

data_thread34<- subset(data_full, SubmissionID == "a0xlit" )
data_thread34$polarization_class = factor(data_thread34$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread34,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread34 ,ParentID == "a0xlit")

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread34_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread34_rootnode.csv')
edge2 <- read.csv('./Data/df_thread34_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread34_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread34_Case1, './Data/DataPlotting/data_plot_thread34_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread34_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread34_Case2, './Data/DataPlotting/data_plot_thread34_Case2.csv')


data_plot_thread34_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread34_Case1.csv')
data_plot_thread34_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread34_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread34_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread34_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread34_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread34_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


# Calculation of edge homogeneity between the user belong to highest thread with 230 user observation as thread35 in the whole data

data_thread35<- subset(data_full, SubmissionID == "b0tp4a" )
data_thread35$polarization_class = factor(data_thread35$polarization_class, labels = c("AgainstBrexit","Neutral", "ProBrexit"))

data1<- select(data_thread35,Author,polarization_prob,polarization_class)

data_first_commenter <- subset(data_thread35 ,ParentID == "b0tp4a" )

data_selected <- select(data_first_commenter, Author, polarization_prob, polarization_class )

colnames(data_selected) = c("Author", "Score", "User_Stance")



write.csv(data_selected, './Data/df_thread35_firstCommenter.csv')


edge1 <- read.csv('./Data/df_thread35_rootnode.csv')
edge2 <- read.csv('./Data/df_thread35_firstCommenter.csv')

edge_hom1 <- select(edge1,Score)
edge_hom2 <- select(edge2,Score)

edge_cal1 <- data.frame(edge_hom1[,1]*edge_hom2[,1])

colnames(edge_cal1) <- "EdgeHomogeneity"

stanceCase1 <- select(edge1,User_Stance )
names(stanceCase1) <- "UserStance"


stanceCase2 <- select(edge2,User_Stance )
names(stanceCase2) <- "UserStance"


#Case1 taking user stance of the root node  where user attracted towards the root node user stance
data_plot_thread35_Case1 <- cbind(edge_cal1,stanceCase1)

write.csv(data_plot_thread35_Case1, './Data/DataPlotting/data_plot_thread35_Case1.csv')

#Case2 taking user stance of the other node  where node is attracts the Other node user stance
data_plot_thread35_Case2 <- cbind(edge_cal1,stanceCase2)

write.csv(data_plot_thread35_Case2, './Data/DataPlotting/data_plot_thread35_Case2.csv')


data_plot_thread35_Case1 <- read.csv('./Data/DataPlotting/data_plot_thread35_Case1.csv')
data_plot_thread35_Case2 <- read.csv('./Data/DataPlotting/data_plot_thread35_Case2.csv')

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread35_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread35_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread35_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_plot_thread35_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread count 376 comments") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


data_grouped_Case1 = rbind(data_plot_thread1_Case1,data_plot_thread2_Case1,data_plot_thread3_Case1,data_plot_thread4_Case1,data_plot_thread5_Case1,data_plot_thread6_Case1,data_plot_thread7_Case1,data_plot_thread8_Case1,data_plot_thread9_Case1,data_plot_thread10_Case1,data_plot_thread11_Case1,data_plot_thread12_Case1,data_plot_thread13_Case1,data_plot_thread14_Case1,data_plot_thread15_Case1,
                           data_plot_thread16_Case1,data_plot_thread17_Case1,data_plot_thread18_Case1,data_plot_thread19_Case1,data_plot_thread20_Case1,data_plot_thread21_Case1,data_plot_thread22_Case1,data_plot_thread23_Case1,data_plot_thread24_Case1,data_plot_thread25_Case1,data_plot_thread26_Case1,data_plot_thread27_Case1,data_plot_thread28_Case1,data_plot_thread29_Case1,data_plot_thread30_Case1,data_plot_thread31_Case1,data_plot_thread32_Case1,data_plot_thread33_Case1,data_plot_thread34_Case1,data_plot_thread35_Case1)

data_grouped_Case2 = rbind(data_plot_thread1_Case2,data_plot_thread2_Case2,data_plot_thread3_Case2,data_plot_thread4_Case2,data_plot_thread5_Case2,data_plot_thread6_Case2,data_plot_thread7_Case2,data_plot_thread8_Case2,data_plot_thread9_Case2,data_plot_thread10_Case2,data_plot_thread11_Case2,data_plot_thread12_Case2,data_plot_thread13_Case2,data_plot_thread14_Case2,data_plot_thread15_Case2,
                           data_plot_thread16_Case2,data_plot_thread17_Case2,data_plot_thread18_Case2,data_plot_thread19_Case2,data_plot_thread20_Case2,data_plot_thread21_Case2,data_plot_thread22_Case2,data_plot_thread23_Case2,data_plot_thread24_Case2,data_plot_thread25_Case2,data_plot_thread26_Case2,data_plot_thread27_Case2,data_plot_thread28_Case2,data_plot_thread29_Case2,data_plot_thread30_Case2,data_plot_thread31_Case2,data_plot_thread32_Case2,data_plot_thread33_Case2,data_plot_thread34_Case2,data_plot_thread35_Case2)



#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_withNeutralStance_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_grouped_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 Thread with Neutral Stance") +theme_classic()+
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_withNeutralStance_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_grouped_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 Thread with Neutral Stance") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()





data_group_Case1=  data_grouped_Case1[data_grouped_Case1$UserStance != "Neutral",]
data_group_Case2 = data_grouped_Case2[data_grouped_Case2$UserStance != "Neutral",]

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_withoutNeutralStance_Case1.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_group_Case1,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case1 without Neutral Stance") +theme_classic()+
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()

#Plotting for PDF of the Edge_Homogeneity_Case1
png("PDF_Edge_Homenitythread_withoutNeutralStance_Case2.png", width = 3200, height = 1800, res = 300)
ggplot(data=data_group_Case2,aes(EdgeHomogeneity,color=factor(UserStance))) + 
  geom_density(aes(linetype = factor(UserStance)),
               lwd=1.5)+
  labs(x="Edge Homogenity",y="PDF",color = "Against","Neutal","ProBrexit", title = "PDF of Edge Homogeneity of Case2 without Neutral Stance") +theme_classic()+ 
  scale_colour_manual("Stances",values = c("orange", "blue","red"))
dev.off()


 






