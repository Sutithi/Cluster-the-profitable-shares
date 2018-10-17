
#READ THE FILE#

    return_all_company<-read.csv("c:/document/return_all_company.csv")
    View(return_all_company)
    return_all_company_withoutlabel<-return_all_company[,-1]
    View(return_all_company_withoutlabel)
    summary(return_all_company_withoutlabel)
    plot(return_all_company_withoutlabel)

#PRINCIPAL COMPONENT ANALYSIS#

     principal_compo<-prcomp(return_all_company_withoutlabel)
     (principal_compo)
     summary(principal_compo)
     plot(principal_compo,xlab="Principal Components",ylab="proportion of variance",main="principal Component Analysis")
     str(principal_compo)
     principal_compo$x
     str(principal_compo$sdev)
     plot(c(1:12),principal_compo$sdev,type="l")
     total_sdev<-sum(c(principal_compo$sdev))
     plot(c(1:12),(principal_compo$sdev/total_sdev)*100,type="l")
     cum_total_sdev<-cumsum((principal_compo$sdev/total_sdev)*100)
     plot(c(1:12),cum_total_sdev,type="l",xlab="Principal Components",ylab="Cumulative variance",main="principal Component Analysis")
     return_all_company_2<-cbind(return_all_company,principal_compo$x[,1:3])
     head(return_all_company_2)
     library(ggplot2)
     ggplot(return_all_company_2,aes(PC1,PC2,PC3),col=c.1.490,fill=c.1.490)
           +stat_ellipse(geom="polygon",col="black",alpha=0.5)
           +geom_point(shape=21,col="black")
     cor(return_all_company[,-1],return_all_company_2[,14:16])
     return_all_company_3<-cbind(return_all_company_2[,c(1,14:16)])

##HIERARCHICAL CLUSTERING##

    distance_principalComponent<-dist(return_all_company_3[,2:4],method="euclidean")
    fitH<-hclust(distance_principalComponent)
    plot(fitH)
    rect.hclust(fitH,k=20,border="red")
    cluster<-cutree(fitH,k=20)
    cluster

    class_H<-list()
    for(i in 1:20){class_H[[i]]=label_H[label_H$cluster==i,c("c.1.490.")]}
    class_H

#k-means clustering#

    plot(return_all_company_3)
    dt_k<-return_all_company_3[,-1]
    fit_k_20<-kmeans(dt_k,20)
    fit_k_20
    str(fit_k_20)
    plot(dt_k,col=fit_k_20$cluster)
    k<-list()
    for(i in 1:100){k[[i]]<-kmeans(dt_k,i)}
    between_totss<-list()
    for(i in 1:100){between_totss[[i]]=k[[i]]$betweenss/k[[i]]$totss}
    between_totss
    plot(c(1:100),between_totss,type="l")
    efficiency_k<-data.frame(cbind(c(1:100),between_totss))
    appropriate_k<-efficiency_k[efficiency_k$between_totss==max(data.frame(efficiency_k$between_totss)),c("V1")]
    appropriate_k
    plot(1:100,between_totss,type="l",ylab="betweenSumOfSquare",xlab="Cluster(k)")
    for(i in 1:20){plot(dt_k,col=k[[i]]$cluster)}
    View(efficiency_k)
    fit_k_20$cluster
    class_k<-list()
    label_k<-data.frame(cbind(c(1:490),fit_k_20$cluster))
    for(i in 1:20){class_k[[i]]=label_k[label_k$X2==i,c("X1")]}
    class_k
    comparison_K_H<-data.frame(cbind(c(1:490),label_k$X2,label_H$cluster))
    View(comparison_K_H)

#SELF-ORGANISING-MAP#

    ##LOADING THE REQUIRED PACKAGES##
    library(RcolorBrewer)
    library(iFigure)
    library(SOMbrero)
    set.seed(255)
    all_data.som<-trainSOM(x.data=all.data[,2:13], verbose=TRUE, nb.save=5)
    ##ENERGY CONVERGENCE##
    plot(all_data.som, what="energy")
    ##INITIAL CLUSTERING##
    all_data.som$clustering
    ##INFLUENCE OF VARIABLES ON INITIAL CLUSTERING#
    table(all_data.som$clustering)
    ##INDICES OF SHARES IN DIFFERENT CLUSTERS##
    plot(all_data.som, what="obs", type="radar", key.loc=c(-0.5,5), mar=c(0,10,2,0))
    rownames(all.data)
    plot(all_data.som, what="obs", type="names", print.title=TRUE, scale=c(0.9,0.5))
    ##QUALITY VERIFICATION OF INITIAL CLUSTERING## 
    quality(all_data.som)
    ##INTRODUCTION OF SUPERCLUSTER##
    plot(superClass(all_data.som))
    ##OBTAINING OPTIMUM CLUSTER##
    my.sc <- superClass(all_data.som, k=8)
    summary(my.sc)
    ##DENDOGRAM OF SUPERCLUSTER##
    plot(my.sc, plot.var=FALSE)
    ##3-D DENDOGRAM OF SUPERCLUSTER##
    plot(my.sc, type="dendro3d")
    ##INFLUENCE OF VARIABLES ON SUPER CLUSTER#
    plot(my.sc, type="barplot", print.title=TRUE)
    plot(my.sc, type="radar", key.loc=c(-0.5,5), mar=c(0,10,2,0))
    ##INDICES OF SHARES IN DIFFERENT SUPER CLUSTERS##
    my.sc$cluster


