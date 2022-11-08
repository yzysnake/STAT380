library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(ClusterR)


# load in data 
data<-fread("./project/volume/data/interim/data.csv")
sub<-fread("./project/volume/data/interim/example_sub.csv")
data$id<-NULL

# do a pca
pca<-prcomp(data)

# look at the percent variance explained by each pca
screeplot(pca)

# look at the rotation of the variables on the PCs
pca

# see the values of the scree plot in a table 
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt<-data.table(unclass(pca)$x)


# run t-sne on the PCAs, note that if you already have PCAs you need to set pca=F or it will run a pca again. 
# pca is built into Rtsne, ive run it seperatly for you to see the internal steps

tsne<-Rtsne(pca_dt,pca = F,perplexity=50,check_duplicates = F)

# grab out the coordinates
tsne_dt<-data.table(tsne$Y)



# use a gaussian mixture model to find optimal k and then get probability of membership for each row to each group

# this fits a gmm to the data for all k=1 to k= 4, we then look for a major change in likelihood between k values
k_bic<-Optimal_Clusters_GMM(tsne_dt[,.(V1,V2)],max_clusters = 4,criterion = "BIC")

# now we will look at the change in model fit between successive k values
delta_k<-c(NA,k_bic[-1] - k_bic[-length(k_bic)])

# I'm going to make a plot so you can see the values, this part isnt necessary
del_k_tab<-data.table(delta_k=delta_k,k=1:length(delta_k))

# plot 
ggplot(del_k_tab,aes(x=k,y=-delta_k))+geom_point()+geom_line()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_text(aes(label=k),hjust=0, vjust=-1)


# set cluster = 4
opt_k<-4

# now we run the model with our chosen k value
gmm_data<-GMM(tsne_dt[,.(V1,V2)],opt_k)

# the model gives a log-likelihood for each datapoint's membership to each cluster, me need to convert this 
# log-likelihood into a probability

l_clust<-gmm_data$Log_likelihood^10

l_clust<-data.table(l_clust)

net_lh<-apply(l_clust,1,FUN=function(x){sum(1/x)})

cluster_prob<-1/l_clust/net_lh

# add back to check if each cluster is corresponding to each breed

sub$Cluster_1_prob<-cluster_prob$V1
sub$Cluster_2_prob<-cluster_prob$V2
sub$Cluster_3_prob<-cluster_prob$V3
sub$Cluster_4_prob<-cluster_prob$V4

View(sub)

# reload Sub data and notice V3 and V4 need to be exchanged
sub<-fread("./project/volume/data/interim/example_sub.csv")

sub$breed_1<-cluster_prob$V1
sub$breed_2<-cluster_prob$V2
sub$breed_3<-cluster_prob$V4
sub$breed_4<-cluster_prob$V3

fwrite(sub, "./project/volume/data/processed/submission5.csv")








