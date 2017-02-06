# The following program is used in the following publication:
#
# Z. Liu, E. Blasch, and V. John, "Statistical comparison of image fusion algorithms: Recommendations", 
# Information Fusion, 36(2017) 251-260 
# http://www.sciencedirect.com/science/article/pii/S1566253516302421 
#
# All these functions are provided "as is" without express or implied warranty.
#
# Please send your comments/questions to: 
#  
# Dr. Zheng liu
# University of British Columbia Okanagan
# Contact: zheng.liu@ieee.org
#-----------------------------------------------------------------------


# @article{Liu2017251,
#   title = "Statistical comparison of image fusion algorithms: Recommendations ",
#   journal = "Information Fusion ",
#   volume = "36",
#   number = "",
#   pages = "251 - 260",
#   year = "2017",
#   note = "",
#   issn = "1566-2535",
#   doi = "http://dx.doi.org/10.1016/j.inffus.2016.12.007",
#   url = "//www.sciencedirect.com/science/article/pii/S1566253516302421",
#   author = "Z. Liu and E. Blasch and V. John",
# }




library('scmamp')
#browseVignettes(package = 'scmamp')
library('XLConnect')
library('corrplot')

# Need to put the data spreadsheet and this program in the same folder. 
# Otherwise, need to change work director:
# > setwd("your folder")
#
# Check the first sheet in the Excel file for the information about the fusion algorithms.
#



#########################################################
#
# Experiments
#
######################################################### 

# These are the definitions for the colors in the plot. 
col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))  
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F"))   


#=========================================================
# algorithm-1
#=========================================================

data1<-readFusionData(2)

# test each combination
test1<-testFusionData(data1,'CS.SD')

# test for each metric
test1<-testMetric(data1,'CS.SD','MI')
test1<-testMetric(data1,'CS.SD','ENL')
test1<-testMetric(data1,'CS.SD','CC')


#=========================================================
# algorithm-2
#=========================================================

data2<-readFusionData(3)

# test each combination
test2<-testFusionData(data2,'GFF')

# test all with rank
test2<-testFusionData1(data2)


# =====================
# test image for fixed metric
# testMetric (1 X N)
# testMetric1 (N X N)
test2<-testMetric(data2,'GFF','Qy')
test2<-testMetric1(data2,'GFF','Qy')

test2<-testMetric(data2,'GFF','Qc')
test2<-testMetric1(data2,'GFF','Qc')

test2<-testMetric(data2,'GFF','Qg')
test2<-testMetric1(data2,'GFF','Qg')

test2<-testMetric(data2,'GFF','Qp')
test2<-testMetric1(data2,'GFF','Qp')

test2<-testMetric(data2,'GFF','Qmi')
test2<-testMetric1(data2,'GFF','Qmi')

#=========================================================
# More experiments about ``sample size"
# Use this algorithm's data
#=========================================================

#---------------------------------------------------------
# 1) remove SWT
data2$dataset<-data2$dataset[,-3]

# test each combination
test2<-testFusionData(data2,'GFF')

# test all with rank
test2<-testFusionData1(data2)

#--------------------------------------------------------
# 2) remove SWT+CVT 
data2$dataset<-data2$dataset[,-3]

# test each combination
test2<-testFusionData(data2,'GFF')

# test all with rank
test2<-testFusionData1(data2)

#--------------------------------------------------------
# 3) remove SWT+CVT+LAP
data2$dataset<-data2$dataset[,-3]

# test each combination
test2<-testFusionData(data2,'GFF')

# test all with rank
test2<-testFusionData1(data2)

#--------------------------------------------------------
# 4) remove SWT+CVT+LAP+NSCT
data2$dataset<-data2$dataset[,-3]

# test each combination
test2<-testFusionData(data2,'GFF')

# test all with rank
test2<-testFusionData1(data2)

#--------------------------------------------------------
# 5) remove SWT+CVT+LAP+NSCT+GRW
data2$dataset<-data2$dataset[,-3]

# test each combination
test2<-testFusionData(data2,'GFF')

# test all with rank
test2<-testFusionData1(data2)


#=========================================================
# algorithm-3
#=========================================================
data3<-readFusionData(4)

# test each combination
test3<-testFusionData(data3,'DSR')

# test all with rank
test3<-testFusionData1(data3)




###########################################################
#----------------------------------------------------------
# Function reads the image fusion data from spreadsheet
#
#

readFusionData<-function(sheetNum){
  # read data
  dataset<- readWorksheetFromFile("fusionData.xlsx", sheet=sheetNum,header = TRUE)
  
  # find the number of images and fusion metrics
  nImage<-nlevels(as.factor(dataset$Image))
  nMetric<-nlevels(as.factor(dataset$Index))
  
  cat('The number of images is', nImage, '\n')
  cat('The number of metrics is', nMetric, '\n')
  cat('The number of algorithm is', dim(dataset)[2]-2, '\n')
  
  res<-list("dataset"=dataset,"nImage"=nImage, "nMetric"=nMetric)
  return(res)
}


#-----------------------------------------------------------------
# Function test the fusion data (1 X N)
# 1) Find if there is significant difference
# 2) Test for each combination of image and metric with a control
#


testFusionData<-function(myData, myControl){
  
  dataset<-myData$dataset
  #  dataset.means<-summarizeData(dataset, group.by = c('Image','Index'), fun = mean , na.rm=TRUE)
  
  size<-dim(dataset)[2]-2
  
  # if there are only two algorithm, apply Wilcoxon test
  if(size==2){
    res<-wilcoxonSignedTest(dataset[,3], dataset[,4]) 
    cat('Two algorithms only !!')
    return(res)
  }
  
  
  if(size>5){
    test<-imanDavenportTest(dataset[,-(1:2)])
  }else{
    test<-quadeTest(dataset[,-(1:2)])
  }
  
  cat('Significance test result:', test$p.value, '\n \n')
  
  if(test$p.value<0.05){ # significant difference
    
    #res<-postHocTest(data = dataset.means, algorithms = 3:dim(dataset)[2], test = 'friedman', correct = 'finner', control = myControl, use.rank = TRUE)
    res<-postHocTest(data = dataset[,-(1:2)], test = 'friedman', correct = 'finner', control = myControl, use.rank = TRUE)
    
    best.res <- res$summary == min(res$summary)
    stat.diff <- res$corrected.pval < 0.05
    stat.diff[is.na(stat.diff)] <- FALSE
    writeTabular(table = res$summary, format = 'f', bold = best.res, mark = stat.diff, digits = 4)
  }else{ # no big difference
    cat("Get large P-value and No big difference")
    return(0)
  }
  
  
  return(list("significance"=test, "test"=res))
}


#---------------------------------------------------------
# all vs. all comparison with rank (N X N)
#

testFusionData1<-function(myData){
  data <- myData$dataset[,-(1:2)]
  
  size<-dim(data)[2]
  
  if(size>5){
    test<-imanDavenportTest(data)
  }else{
    test<-quadeTest(data)
  }
  
  if(test$p.value<0.05){ # significant difference
    
    if(size<9){
      res <- postHocTest(data, test = "friedman", use.rank=TRUE, correct="bergmann")
    }else{
      res <- postHocTest(data, test = "friedman", use.rank = TRUE, correct ="shaffer")
    }
    bold <- res$corrected.pval < 0.05
    bold[is.na(bold)] <- FALSE
    writeTabular(table = res$corrected.pval, format = 'f', bold = bold,hrule = 0, vrule = 0)
    
    drawAlgorithmGraph(res$corrected.pval, res$summary)
  }else{ # no big difference
    cat("Get large P-value")
  }
  
  par(ask = TRUE)
  
  corrplot.mixed(cor(data), p.mat = res$corrected.pval, sig.level=0.05, 
                 cl.lim=c(0,1), upper="pie", col=col2(200), pch = 4, pch.col = "red", 
                 pch.cex = 4, cl.cex = 1.2, tl.cex = 1, tl.col ="black", number.cex=1.2)
  
  
  return(list("significance"=test, "test"=res))
  
}


#-------------------------------------------------------------
# Function consider the different image while fixing the fusion metric.
#
# fusionMetric: metric name, for example, fusionMetric<-'PNSR'
# newMethod: name of proposed method, for example, newMethod<-'GFF'
# 1 vs. all

testMetric<-function(mydata, newMethod, fusionMetric){
  
  cond<-paste0("Index==","'",fusionMetric,"'")
  sub.dataset<-filterData(data=mydata$dataset,condition = cond, remove.cols = 'Index' )
  
  # test if significant or not
  
  size<-dim(sub.dataset)[2]-1
  
  # if there are only two algorithm, apply Wilcoxon test
  if(size==2){
    res<-wilcoxonSignedTest(sub.dataset[,2],sub.dataset[,3])
    cat('Two algorithms only !!')
    return(res)
  }  
  
  if(size>5){
    test<-imanDavenportTest(sub.dataset)
  }else{
    test<-quadeTest(sub.dataset)
  }
  cat("Significance test result:", test$p.value, '\n \n')  
  
  if(test$p.value<0.05){ # significant difference
    sub.res<-postHocTest(data = sub.dataset[,-1], test = 'friedman',correct = 'finner',sum.fun = mean, control = newMethod)
    
    best.res <- sub.res$summary == max(sub.res$summary)
    
    stat.diff <- sub.res$corrected.pval < 0.05
    stat.diff[is.na(stat.diff)] <- FALSE
    
    # corrected p-value is printed
    writeTabular(table = sub.res$corrected.pval, format = 'f', bold = best.res, mark = stat.diff, hrule = 0, vrule = 0)
    
  }else{ # no big difference
    cat("Get large P-value")
  }
  return(list("significance"=test, "test"=sub.res))
  
}



#-------------------------------------------------------------
# Function consider the different image while fixing the fusion metric.
#
# fusionMetric: metric name, for example, fusionMetric<-'PNSR'
# newMethod: name of proposed method, for example, newMethod<-'GFF'
# all vs. all

testMetric1<-function(mydata, newMethod, fusionMetric){
  
  cond<-paste0("Index==","'",fusionMetric,"'")
  sub.dataset<-filterData(data=mydata$dataset,condition = cond, remove.cols = 'Index' )
  
  # test if significant or not
  
  size<-dim(sub.dataset)[2]-1
  
  if(size>5){
    test<-imanDavenportTest(sub.dataset)
  }else{
    test<-quadeTest(sub.dataset)
  }
  cat("Significance test result:", test$p.value, '\n \n')  
  
  
  if(size<9){
    sub.res<-postHocTest(data = sub.dataset[,-1], test = 'friedman',correct = 'bergmann',sum.fun = mean)
  }else{
    sub.res<-postHocTest(data = sub.dataset[,-1], test = 'friedman',correct = 'shaffer',sum.fun = mean)
  }
  
  
  ###--- If there is no group, do not have to use Wilcoxon test
  ## sub.res <- postHocTest(data = sub.dataset, group.by = 'Index', test = 'wilcoxon',correct = 'finner', control = newMethod)
  
  ##--- If you want to use RANK in the draw, the following two commands can help.   
  # average.ranking <- colMeans(rankMatrix(sub.dataset[,-1]))
  # drawAlgorithmGraph(pvalue.matrix = sub.res$corrected.pval, mean.value = average.ranking)
  
  drawAlgorithmGraph(pvalue.matrix = sub.res$corrected.pval, mean.value = sub.res$summary, alpha = 0.05, digits=4)
  
  
  bold <- sub.res$corrected.pval < 0.05
  bold[is.na(bold)] <- FALSE
  writeTabular(table = sub.res$corrected.pval, format = 'f', bold = bold, hrule = 0, vrule = 0)
  
  
  # Plot the correlation matrix
  par(ask = TRUE)
  
  corrplot.mixed(cor(sub.dataset[,-1]), p.mat = sub.res$corrected.pval, sig.level=0.05, 
                 cl.lim=c(-1,1), upper="pie", col=col2(200), pch = 4, pch.col = "red", 
                 pch.cex = 4, cl.cex = 1.2, tl.cex = 1, tl.col ="black", number.cex=1.2)
  
  
  return(list("significance"=test, "test"=sub.res))
  
}

#--------------------------------------------------------


#-------------------------------------------------------------
# Function color management for [corrplot]
#
#
#corPlotColor<-function(){
#  col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
#                             "cyan", "#007FFF", "blue","#00007F"))
#  col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
#                             "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))  
#  col3 <- colorRampPalette(c("red", "white", "blue")) 
#  col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
#                             "cyan", "#007FFF", "blue","#00007F"))   
#  wb <- c("white","black")
#  
#}


