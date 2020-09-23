connect<-readMat("/Users/jiahe/Desktop/Power System/bus 39/conncet_info.mat")
connect<-connect$connect.info

num.node=39
num.line=46


onehop=NULL
for (j in 1:num.node)  {
  tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
  onehop[[j]]=unique(tmp)
}

##
##read final data

#################single line outage#################
##read single line outage cases###
#setwd("/Users/jiahe/Desktop/CNN/39 bus")
setwd("/Users/jiahe/Desktop/39 double")
file_list.single <- mixedsort(list.files())
length(file_list.single)
check=str_extract(file_list.single, "\\-*\\d+")
table(check)


out_idx=4
out_num=100
not_out_num=8
select_file_idx=NULL
for (i in 1:num.line) {
  if (i==out_idx) {
    tt=which(check==i)[1:out_num]
    select_file_idx=c(select_file_idx,tt)
    }
  else {
    tt=which(check==i)[1:not_out_num]
    select_file_idx=c(select_file_idx,tt)
    }
}
table(check[select_file_idx])
outcome=c(rep(1:(out-1), each=not_out_num),rep(out,out_num),rep((out+1):num.line, each=not_out_num))
#outcome=c(rep(1,sum(check==1)),rep(2:num.line,each=15))
#outcome=rep(1:num.line,each=15)
num.obs=length(outcome)

case_comb = lapply(file_list.single[select_file_idx], function(x) readMat(x, header=FALSE)$a)

#selected_file_list=file_list.single[c(1:15,401:length(file_list.single))]
# check if selects right amount
#check=str_extract(selected_file_list, "\\-*\\d+")

#singleoutage.case = lapply(selected_file_list, function(x)readMat(x, header=FALSE))



###corresponding line_outage index##



##########################
### variation of node info
ori.result=NULL
hah=NULL
num.node=39
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  
  ori.result[[i]]=y
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  tmp=ori.result[[i]]^2
  tmp=scale(tmp,center=TRUE,scale=TRUE)
  hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

case_comb=NULL



indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.node) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:100]))
    
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
indexa=NULL

##intersect within cases
single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)


single.oo=NULL
tmp1=NULL


pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=length(union.allcase1[[i]])
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
}

mag=NULL
for(i in 1:num.node) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}

for(i in 1:length(mag)){
  print(sum(is.na(mag[[i]])))
}





#### no fft + node new version
ori.result=NULL
hah=NULL
num.node=39
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  ori.result[[i]]=y
  
}


setwd('/Users/jiahe/Desktop/CNN/line 1/39 no fft node new dl')
file_name=str_c('outage',str_extract(file_list.single[kk], "\\-*\\d+\\.*\\d*"))

for (i in 1:length(ori.result)) {
  write.table(ori.result[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}


## node fft
ori.result=NULL
hah=NULL
for (i in 1:length(case_comb)) {

  ori.result[[i]]=case_comb[[i]]
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  tmp=ori.result[[i]]^2
  tmp=scale(tmp,center=TRUE,scale=TRUE)
  hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.node) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:500]))
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
indexa=NULL
##intersect within cases


single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)


single.oo=NULL
tmp1=NULL

# if there is na
pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=which(is.na(union.allcase1[[i]]))
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:(min(pointer)-1)]
}


pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=length(union.allcase1[[i]])
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
}



mag=NULL
for(i in 1:num.line) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}


#prepare for input data
setwd('/Users/jiahe/Desktop/CNN/equal number/39 node fft') 
file_name=str_c('outage',str_extract(file_list.single, "\\-*\\d+\\.*\\d*"))

Input=NULL
for (i in 1:num.obs) {
  y=NULL
  for (j in 1:num.node){
    tmp=mag[[j]][i,]
    y=rbind(y,tmp)
  }
  Input[[i]]=y
  write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}







#########118 
#### source localization
connect<-readMat("/Users/jiahe/Desktop/CNN/connect118.mat")
connect<-connect$con
num.line=dim(connect)[1]


Line_idx_com1=c(1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,
                14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
                27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
                40,  41,  42,  43,  54, 111, 112, 113, 178, 179, 180, 181, 182,
                184)
Line_idx_com2=c( 46,  47,  48,  49,  50,  52,  53,  55,  56,  57,  58,  59,  60,
                  61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,
                  74,  75,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,  86,
                  87,  88,  89,  90,  91,  92,  93,  94,  95,  97,  98,  99, 100,
                  101, 102, 103, 104, 105, 106, 107, 108, 114, 115, 116, 117, 118,
                  119, 120, 121, 122, 123, 124, 125, 126, 127, 152, 153, 183, 185,
                  186)
Line_idx_com3=c(129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141,
                142, 143, 144, 145, 146, 147, 149, 150, 154, 155, 156, 157, 160,
                161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
                174, 175, 176, 177)

BL_idx=c( 44,  45,  51,  96, 109, 110, 128, 148, 151, 158, 159)

num.node=118
onehop=NULL
for (j in 1:num.node)  {
  
  tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
  onehop[[j]]=unique(tmp)
}

setwd('/Users/jiahe/Desktop/CNN/118 train')
file_list<- mixedsort(list.files())
check=str_extract(file_list, "\\-*\\d+")
table(check)
bl_num=50
not_bl_num=15
select_file_idx=NULL
for (i in c(1:103,105:num.line)) {
  if (i %in% BL_idx) {
    tt=which(check==i)[1:bl_num]
    select_file_idx=c(select_file_idx,tt)
  }
  else {
    tt=which(check==i)[1:not_bl_num]
    select_file_idx=c(select_file_idx,tt)
  }
}
table(check[select_file_idx])


case_comb = lapply(file_list[select_file_idx], function(x) readMat(x, header=FALSE)$a)

outcome=NULL
for (i in 1:length(file_list[select_file_idx])) {
  if (str_extract(file_list[select_file_idx][i], "\\-*\\d+") %in% Line_idx_com1){
    com=1
    outcome=c(outcome,com)
  } else if (str_extract(file_list[select_file_idx][i], "\\-*\\d+") %in% Line_idx_com2){
    com=2
    outcome=c(outcome,com)
  } else if (str_extract(file_list[select_file_idx][i], "\\-*\\d+") %in% Line_idx_com3){
    com=3
    outcome=c(outcome,com)
  } else if (str_extract(file_list[select_file_idx][i], "\\-*\\d+") %in% BL_idx){
    com=4
    outcome=c(outcome,com)
  } 
}

#outcome=rep(1,sum(check==1))
num.obs=length(case_comb)




### variation of node info
ori.result=NULL
hah=NULL
num.node=118
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  
  ori.result[[i]]=y
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  tmp=ori.result[[i]]^2
  tmp=scale(tmp,center=TRUE,scale=TRUE)
  hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

case_comb=NULL

stt=Sys.time()
indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.node) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:300]))
    
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

end=Sys.time()

#tmp1 splits each indexa[[i]] into 46
tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
indexa=NULL

##intersect within cases
single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)


single.oo=NULL
tmp1=NULL


pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=length(union.allcase1[[i]])
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
}

mag=NULL
for(i in 1:num.node) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}


setwd('/Users/jiahe/Desktop/CNN/118 bus feature/118 fft node')
file_name=str_c('outage',str_extract(file_list[786:length(file_list)], "\\-*\\d+\\.*\\d*"))

Input=NULL
for (i in 1:num.obs) {
  y=NULL
  for (j in 1:num.node){
    tmp=mag[[j]][i,]
    y=rbind(y,tmp)
  }
  Input[[i]]=y
  write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}








#### within community
### line 1 and 8 out test in Com1
#### collect all cases in community 0 for further training in Python
## following is from python, code as follows
## com_src=0
## com_src_components=[i+1 for i in range(len(community_index)) if community_index[i]==com_src]
num.node=118
onehop=NULL
for (j in 1:num.node)  {
  
  tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
  onehop[[j]]=unique(tmp)
}


Line_idx_com1=c(1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,
  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,
  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,
  40,  41,  42,  43,  54, 111, 112, 113, 178, 179, 180, 181, 182,
  184)
Line_idx_com3=c(129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141,
                 142, 143, 144, 145, 146, 147, 149, 150, 154, 155, 156, 157, 160,
                 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
                 174, 175, 176, 177)


BL_idx=c( 44,  45,  51,  96, 109, 110, 128, 148, 151, 158, 159)

  
Line_idx_com=Line_idx_com1
setwd('/Users/jiahe/Desktop/CNN/118 train')
#setwd('/Users/jiahe/Desktop/CNN/118 dl')
file_list<- mixedsort(list.files())
Line_idx=as.numeric(str_extract(file_list, "\\-*\\d+"))
Intersect=which(Line_idx %in% Line_idx_com)

table(Line_idx[Intersect])
out_idx=8     #static load: line 1,8 for com1; line 129, 132 for com3 ; dynamic load: line 1,2 for com1
out_num=200
not_out_num=8
select_file_idx=NULL
for ( i in Line_idx_com) {
  if (i==out_idx) {
    tt=which(Line_idx[Intersect]==i)[1:out_num]
    select_file_idx=c(select_file_idx,tt)
  }
  else {
    tt=which(Line_idx[Intersect]==i)[1:not_out_num]
    select_file_idx=c(select_file_idx,tt)
  }
}

if(  out_idx==Line_idx_com[1]) {
  outcome= c(rep(out_idx,out_num),rep(Line_idx_com[-1], each=not_out_num))
}  else  {
  outcome= c(rep(Line_idx_com[1:(which(Line_idx_com==out_idx)-1)], each=not_out_num),rep(out_idx,out_num),rep(Line_idx_com[(which(Line_idx_com==out_idx)+1):length(Line_idx_com)], each=not_out_num))
}
table(outcome)

num.obs=length(outcome)

case_comb = lapply(file_list[Intersect][select_file_idx], function(x) readMat(x, header=FALSE)$a)




### variation of node info
ori.result=NULL
hah=NULL
num.node=118
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  
  ori.result[[i]]=y
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  tmp=ori.result[[i]]^2
  tmp=scale(tmp,center=TRUE,scale=TRUE)
  hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

case_comb=NULL

stt=Sys.time()
indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.node) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:100]))
    
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

end=Sys.time()

tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
indexa=NULL

##intersect within cases
single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)


single.oo=NULL
tmp1=NULL


pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=length(union.allcase1[[i]])
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
}

mag=NULL
for(i in 1:num.node) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}

for(i in 1:length(mag)){
  print(sum(is.na(mag[[i]])))
}

setwd('/Users/jiahe/Desktop/CNN/118 community/118 community_1/line 3 dynamic')
file_name=str_c('outage',str_extract(file_list[Intersect][select_file_idx], "\\-*\\d+\\.*\\d*"))


Input=NULL
for (i in 1:num.obs) {
  y=NULL
  for (j in 1:num.node){
    tmp=mag[[j]][i,]
    y=rbind(y,tmp)
  }
  Input[[i]]=y
  write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}







#### BL community



## com_src=0
## com_src_components=[i+1 for i in range(len(community_index)) if community_index[i]==com_src]
BL_idx=c( 44,  45,  51,  96, 109, 110, 128, 148, 151, 158, 159)

setwd('/Users/jiahe/Desktop/CNN/118 boundary community/118 more BL')
file_list<- mixedsort(list.files())
check=as.numeric(str_extract(file_list, "\\-*\\d+"))
table(check)

case_comb = lapply(file_list, function(x) readMat(x, header=FALSE)$a)



###corresponding line_outage index##
outcome=c(rep(BL_idx[1],sum(check==44)),rep(BL_idx[-1],each=35))
#outcome=rep(1,sum(check==1))
num.obs=length(outcome)



### variation of node info
ori.result=NULL
hah=NULL
num.node=118
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  
  ori.result[[i]]=y
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  tmp=ori.result[[i]]^2
  tmp=scale(tmp,center=TRUE,scale=TRUE)
  hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

case_comb=NULL

stt=Sys.time()
indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.node) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:100]))
    
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

end=Sys.time()

#tmp1 splits each indexa[[i]] into 46
tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
indexa=NULL

##intersect within cases
single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)


single.oo=NULL
tmp1=NULL


pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=length(union.allcase1[[i]])
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
}

mag=NULL
for(i in 1:num.node) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}

for(i in 1:length(mag)){
  print(sum(is.na(mag[[i]])))
}

setwd('/Users/jiahe/Desktop/CNN/118 community/118 BL/line 44 static')
file_name=str_c('outage',str_extract(file_list, "\\-*\\d+\\.*\\d*"))

Input=NULL
for (i in 1:num.obs) {
  y=NULL
  for (j in 1:num.node){
    tmp=mag[[j]][i,]
    y=rbind(y,tmp)
  }
  Input[[i]]=y
  write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}
















#########118  add more BL

connect<-readMat("/Users/jiahe/Desktop/CNN/connect118.mat")
connect<-connect$con
num.line=dim(connect)[1]

one.hop=NULL
onehop=NULL
for (i in 1:num.line)  {
  for (j in 1:2) {
    one.hop[[j]]=c(which(connect[,j]==connect[i,1]),which(connect[,j]==connect[i,2]))
  }
  onehop[[i]]=unique(unlist(one.hop))
}
##

setwd('/Users/jiahe/Desktop/CNN/input for GFT/118 binary line 1')
file_list<- mixedsort(list.files())
check=str_extract(file_list, "\\-*\\d+")
table(check)
case_comb = lapply(file_list[586:length(file_list)], function(x) readMat(x, header=FALSE)$a)
#BL_idx=c(44, 45, 51, 96, 109, 110, 128, 148, 157, 158, 159)
BL_idx=c( 44,  45,  51,  96, 109, 110, 128, 148, 151, 158, 159)
Line_idx=c(1:103,105:num.line)

###corresponding line_outage index##
BL_num=50
non_BL_num=15
freq=ifelse(Line_idx %in% BL_idx,BL_num,non_BL_num)
outcome=rep(Line_idx,freq)
#outcome=rep(1,sum(check==1))
num.obs=length(outcome)



### variation of node info
ori.result=NULL
hah=NULL
num.node=118
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  
  ori.result[[i]]=y
  ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
  ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
  ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
  
  ##gumbel distribution result=max(|ori.result|^2)
  tmp=ori.result[[i]]^2
  tmp=scale(tmp,center=TRUE,scale=TRUE)
  hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
}

case_comb=NULL

stt=Sys.time()
indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
for (j in 1:num.node) {
  mylist=NULL
  for(k in 1:num.obs) {
    mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:300]))
    
  }
  indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
}

end=Sys.time()

#tmp1 splits each indexa[[i]] into 46
tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
indexa=NULL

##intersect within cases
single.oo=NULL
for (i in 1:length(tmp1)) {
  single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
  single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
}
union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
union.allcase1=lapply(union.allcase1,na.omit)


single.oo=NULL
tmp1=NULL


pointer=NULL
for (i in 1:length(union.allcase1)) {
  tmp=length(union.allcase1[[i]])
  pointer=c(pointer,tmp)
}

for (i in 1:length(union.allcase1)) {
  union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
}

mag=NULL
for(i in 1:num.node) {
  v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
  mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
}


for(i in 1:length(mag)){
  print(sum(is.na(mag[[i]])))
}


setwd('/Users/jiahe/Desktop/CNN/118 bus feature/118 dl node')
file_name=str_c('outage',str_extract(file_list[586:length(file_list)], "\\-*\\d+\\.*\\d*"))
cc=str_extract(file_list[586:length(file_list)], "\\-*\\d+")

Input=NULL
for (i in 1:num.obs) {
  y=NULL
  for (j in 1:num.node){
    tmp=mag[[j]][i,]
    y=rbind(y,tmp)
  }
  Input[[i]]=y
  write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}










###############################

onehop=NULL
for (j in 1:num.node)  {
  
  tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
  onehop[[j]]=unique(tmp)
}


total=NULL
Result=matrix(NA,nrow=19,ncol=2)
for(out_idx in 1:19) {
  # setwd("/Users/jiahe/Desktop/CNN/39 bus dynamic load")
  setwd('/Users/jiahe/Desktop/CNN/39 bus')
  file_list.single <- mixedsort(list.files())
  length(file_list.single)
  check=str_extract(file_list.single, "\\-*\\d+")
  table(check)
  
  out_num=100
  not_out_num=8
  select_file_idx=NULL
  for (i in 1:num.line) {
    if (i==out_idx) {
      tt=which(check==i)[1:out_num]
      select_file_idx=c(select_file_idx,tt)
    }
    else {
      tt=which(check==i)[1:not_out_num]
      select_file_idx=c(select_file_idx,tt)
    }
  }
  table(check[select_file_idx])
  if(  out_idx==1) {
    outcome= c(rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }  else  {
    outcome= c(rep(1:(out_idx-1), each=not_out_num),rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }
  num.obs=length(outcome)
  
  case_comb = lapply(file_list.single[select_file_idx], function(x) readMat(x, header=FALSE)$a)
  
  
  ori.result=NULL
  hah=NULL
  num.node=39
  for (i in 1:length(case_comb)) {
    
    y=NULL
    
    for (j in 1:num.node) {
      tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
      neighbor=tmp[which(tmp!=j)]
      if  (length(neighbor)!=1) {
        sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
      }  else   {         
        sum= case_comb[[i]][,j]
      }
      y=cbind(y,sum)
    }
    
    ori.result[[i]]=y
    ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
    ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
    ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
    
    ##gumbel distribution result=max(|ori.result|^2)
    tmp=ori.result[[i]]^2
    tmp=scale(tmp,center=TRUE,scale=TRUE)
    hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
  }
  
  
  indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
  for (j in 1:num.node) {
    mylist=NULL
    for(k in 1:num.obs) {
      mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:100]))
      
    }
    indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
  }
  
  tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
  
  ##intersect within cases
  single.oo=NULL
  for (i in 1:length(tmp1)) {
    single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
    single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
  }
  union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
  union.allcase1=lapply(union.allcase1,na.omit)
  
  pointer=NULL
  for (i in 1:length(union.allcase1)) {
    tmp=length(union.allcase1[[i]])
    pointer=c(pointer,tmp)
  }
  
  for (i in 1:length(union.allcase1)) {
    union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
  }
  
  
  mag=NULL
  for(i in 1:num.node) {
    v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
    mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
  }
  
  ################
  ###############
  ####one hop
  i=out_idx
  nodes=unique(unlist(onehop[connect[out_idx,]]))
  total[[i]]=mag[nodes]
  total[[i]]=do.call(cbind,lapply(total[[i]],matrix,nrow=num.obs))
  line.outage=as.factor(ifelse(outcome==i,1,0))
  total[[i]]=data.frame(total[[i]],line.outage)
  colnames(total[[i]])<-c(paste0("X", seq(1:(dim(total[[i]])[2]-1))),"outcome")
  
  
  ######logistic
  Sample=sample(1:dim(total[[i]])[1], dim(total[[i]])[1]*0.8)
  
  glm.fit<-glm(outcome~.,data=total[[i]],subset=Sample,family=binomial)
  glm.probs=predict(glm.fit,total[[i]][-Sample,],type="response")
  glm.pred=rep("0",dim(total[[i]])[1]-length(Sample))
  glm.pred[glm.probs>.5]="1"
  table=table(pred=glm.pred,true=total[[i]][-Sample,]$outcome)
  print(i)
  print(table)
  R1_P1_T1=c(table[2,2]/sum(table[,2]),table[2,2]/sum(table[2,]))
  Result[out_idx,]=R1_P1_T1
  
  
  ###rf
  
#  r<-randomForest(outcome~.,total[[i]],subset=Sample,importance=TRUE,ntree=20)

#  predictions<-predict(r,total[[i]][-Sample,])  
#  table=table(pred=predictions,true=total[[i]][-Sample,]$outcome)
#  print(table)
#  R2_P2_T2=c(table[2,2]/sum(table[,2]),table[2,2]/sum(table[2,]))
# Result[out_idx,]=c(R1_P1_T1, R2_P2_T2)
  
  
}

















#########generate CNN feature automatically  

for(out_idx in 2:19) {
  setwd("/Users/jiahe/Desktop/CNN/39 bus")
  file_list.single <- mixedsort(list.files())
  length(file_list.single)
  check=str_extract(file_list.single, "\\-*\\d+")
  table(check)
  
  out_num=100
  not_out_num=8
  select_file_idx=NULL
  for (i in 1:num.line) {
    if (i==out_idx) {
      tt=which(check==i)[1:out_num]
      select_file_idx=c(select_file_idx,tt)
    }
    else {
      tt=which(check==i)[1:not_out_num]
      select_file_idx=c(select_file_idx,tt)
    }
  }
  table(check[select_file_idx])
  if(  out_idx==1) {
    outcome= c(rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }  else  {
    outcome= c(rep(1:(out_idx-1), each=not_out_num),rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }
  
  
  num.obs=length(outcome)
  
  case_comb = lapply(file_list.single[select_file_idx], function(x) readMat(x, header=FALSE)$a)
  
  ori.result=NULL
  hah=NULL
  num.node=39
  for (i in 1:length(case_comb)) {
    
    y=NULL
    
    for (j in 1:num.node) {
      tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
      neighbor=tmp[which(tmp!=j)]
      if  (length(neighbor)!=1) {
        sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
      }  else   {         
        sum= case_comb[[i]][,j]
      }
      y=cbind(y,sum)
    }
    
    ori.result[[i]]=y
    ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
    ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
    ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
    
    ##gumbel distribution result=max(|ori.result|^2)
    tmp=ori.result[[i]]^2
    tmp=scale(tmp,center=TRUE,scale=TRUE)
    hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
  }
  
  case_comb=NULL
  
  indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
  for (j in 1:num.node) {
    mylist=NULL
    for(k in 1:num.obs) {
      mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:100]))
      
    }
    indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
  }
  
  tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
  indexa=NULL
  
  ##intersect within cases
  single.oo=NULL
  for (i in 1:length(tmp1)) {
    single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
    single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
  }
  union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
  union.allcase1=lapply(union.allcase1,na.omit)
  
  
  single.oo=NULL
  tmp1=NULL
  
  
  pointer=NULL
  for (i in 1:length(union.allcase1)) {
    tmp=length(union.allcase1[[i]])
    pointer=c(pointer,tmp)
  }
  
  for (i in 1:length(union.allcase1)) {
    union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
  }
  
  mag=NULL
  for(i in 1:num.node) {
    v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
    mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
  }
  
  for(i in 1:length(mag)){
    print(sum(is.na(mag[[i]])))
  }
  
  cur_path=str_c('/Users/jiahe/Desktop/CNN/39 dl CNN feature/line ',out_idx)
  dir.create(cur_path)
  setwd(cur_path)
  file_name=str_c('outage',str_extract(file_list.single[select_file_idx], "\\-*\\d+\\.*\\d*"))
  
  Input=NULL
  for (i in 1:num.obs) {
    y=NULL
    for (j in 1:num.node){
      tmp=mag[[j]][i,]
      y=rbind(y,tmp)
    }
    Input[[i]]=y
    write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
  }
  
}










#########generate node new no fft for GFT in python 

###  39 bus
for(out_idx in 2:19) {
  setwd("/Users/jiahe/Desktop/CNN/39 bus dynamic load")
  file_list.single <- mixedsort(list.files())
  length(file_list.single)
  check=str_extract(file_list.single, "\\-*\\d+")
  table(check)
  
  out_num=100
  not_out_num=8
  select_file_idx=NULL
  for (i in 1:num.line) {
    if (i==out_idx) {
      tt=which(check==i)[1:out_num]
      select_file_idx=c(select_file_idx,tt)
    }
    else {
      tt=which(check==i)[1:not_out_num]
      select_file_idx=c(select_file_idx,tt)
    }
  }
  table(check[select_file_idx])
  if(  out_idx==1) {
    outcome= c(rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }  else  {
    outcome= c(rep(1:(out_idx-1), each=not_out_num),rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }
  
  
  num.obs=length(outcome)
  
  case_comb = lapply(file_list.single[select_file_idx], function(x) readMat(x, header=FALSE)$a)
  
  ori.result=NULL
  hah=NULL
  num.node=39
  for (i in 1:length(case_comb)) {
    
    y=NULL
    
    for (j in 1:num.node) {
      tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
      neighbor=tmp[which(tmp!=j)]
      if  (length(neighbor)!=1) {
        sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
      }  else   {         
        sum= case_comb[[i]][,j]
      }
      y=cbind(y,sum)
    }
    
    ori.result[[i]]=y
  }
  
  
  cur_path=str_c('/Users/jiahe/Desktop/CNN/input for GFT/39 bus node new no fft dl/line ',out_idx)
  dir.create(cur_path)
  setwd(cur_path)
  file_name=str_c('outage',str_extract(file_list.single[select_file_idx], "\\-*\\d+\\.*\\d*"))
  
  for (i in 1:length(ori.result)) {
    write.table(ori.result[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
  }
}  



### 118 bus





ori.result=NULL
hah=NULL
num.node=118
for (i in 1:length(case_comb)) {
  
  y=NULL
  
  for (j in 1:num.node) {
    tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
    neighbor=tmp[which(tmp!=j)]
    if  (length(neighbor)!=1) {
      sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
    }  else   {         
      sum= case_comb[[i]][,j]
    }
    y=cbind(y,sum)
  }
  
  ori.result[[i]]=y
}

cur_path=str_c('/Users/jiahe/Desktop/CNN/input for GFT/118 binary line 1')
dir.create(cur_path)
setwd(cur_path)
file_name=str_c('outage',str_extract(file_list[select_file_idx], "\\-*\\d+\\.*\\d*"))
#file_name=str_c('outage',str_extract(file_list[Intersect][select_file_idx], "\\-*\\d+\\.*\\d*"))

for (i in 1:length(ori.result)) {
  write.table(ori.result[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
}



## prepare input features for spatial GCN
test_line=c(1,BL_idx)
for(out_idx in test_line) {
  
  setwd("/Users/jiahe/Desktop/CNN/118 dl")
  file_list.single <- mixedsort(list.files())
  length(file_list.single)
  check=str_extract(file_list.single, "\\-*\\d+")
  table(check)
  
  out_num=100
  not_out_num=5
  select_file_idx=NULL
  for (i in c(1:103,105:num.line)) {
    if (i==out_idx) {
      tt=which(check==i)[1:out_num]
      select_file_idx=c(select_file_idx,tt)
    }
    else {
      tt=which(check==i)[1:not_out_num]
      select_file_idx=c(select_file_idx,tt)
    }
  }
  table(check[select_file_idx])
  if(  out_idx==1) {
    outcome= c(rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }  else  {
    outcome= c(rep(1:(out_idx-1), each=not_out_num),rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }
  
  outcome=outcome[-which(outcome==104)]
  num.obs=length(outcome)
  
  case_comb = lapply(file_list.single[select_file_idx], function(x) readMat(x, header=FALSE)$a)
  
  ori.result=NULL
  hah=NULL
  num.node=118
  for (i in 1:length(case_comb)) {
    
    y=NULL
    
    for (j in 1:num.node) {
      tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
      neighbor=tmp[which(tmp!=j)]
      if  (length(neighbor)!=1) {
        sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
      }  else   {         
        sum= case_comb[[i]][,j]
      }
      y=cbind(y,sum)
    }
    
    ori.result[[i]]=y
    ori.result[[i]]<-apply(ori.result[[i]],2,fft)/length(ori.result[[i]])
    ori.result[[i]]<-apply(ori.result[[i]],2,Mod)
    ori.result[[i]]<-head(ori.result[[i]],nrow(ori.result[[i]])/2)[-1,]
    
    ##gumbel distribution result=max(|ori.result|^2)
    tmp=ori.result[[i]]^2
    tmp=scale(tmp,center=TRUE,scale=TRUE)
    hah[[i]]=apply(tmp,2, function(m) sort(m,index.return=TRUE,decreasing=TRUE))
  }
  
  case_comb=NULL
  
  indexa=NULL  #indexa[[1]] is the freq of all the obs when line1 out
  for (j in 1:num.node) {
    mylist=NULL
    for(k in 1:num.obs) {
      mylist[[k]]=as.data.frame(t(hah[[k]][[j]]$ix[1:100]))
      
    }
    indexa[[j]]=as.matrix(do.call(rbind.fill,mylist))
  }
  
  tmp1 <- lapply(indexa, function(m) split.data.frame(m,outcome)) 
  indexa=NULL
  
  ##intersect within cases
  single.oo=NULL
  for (i in 1:length(tmp1)) {
    single.oo[[i]]=lapply(tmp1[[i]],function(m) alply(m,1))
    single.oo[[i]]=lapply(single.oo[[i]], function(m) Reduce(intersect,m))
  }
  union.allcase1=lapply(single.oo,function(m) Reduce(union,m))
  union.allcase1=lapply(union.allcase1,na.omit)
  
  
  single.oo=NULL
  tmp1=NULL
  
  
  pointer=NULL
  for (i in 1:length(union.allcase1)) {
    tmp=length(union.allcase1[[i]])
    pointer=c(pointer,tmp)
  }
  
  for (i in 1:length(union.allcase1)) {
    union.allcase1[[i]]=union.allcase1[[i]][1:min(pointer)]
  }
  
  mag=NULL
  for(i in 1:num.node) {
    v=lapply(ori.result,function(m) m[,i][c(union.allcase1[[i]])])
    mag[[i]]=do.call(rbind,lapply(v,matrix,ncol=length(union.allcase1[[i]]),byrow=TRUE))
  }
  
  for(i in 1:length(mag)){
    print(sum(is.na(mag[[i]])))
  }
  
  cur_path=str_c('/Users/jiahe/Desktop/CNN/118 binary spatial dl/line ',out_idx)
  dir.create(cur_path)
  setwd(cur_path)
  file_name=str_c('outage',str_extract(file_list.single[select_file_idx], "\\-*\\d+\\.*\\d*"))
  
  Input=NULL
  for (i in 1:num.obs) {
    y=NULL
    for (j in 1:num.node){
      tmp=mag[[j]][i,]
      y=rbind(y,tmp)
    }
    Input[[i]]=y
    write.table(Input[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
  }
}


num.node=118
num.line=186
#########generate node new no fft for GFT in python 
for(out_idx in test_line) {
  
  setwd("/Users/jiahe/Desktop/CNN/118 dl")
  file_list.single <- mixedsort(list.files())
  length(file_list.single)
  check=str_extract(file_list.single, "\\-*\\d+")
  table(check)
  
  out_num=100
  not_out_num=5
  select_file_idx=NULL
  for (i in c(1:103,105:num.line)) {
    if (i==out_idx) {
      tt=which(check==i)[1:out_num]
      select_file_idx=c(select_file_idx,tt)
    }
    else {
      tt=which(check==i)[1:not_out_num]
      select_file_idx=c(select_file_idx,tt)
    }
  }
  table(check[select_file_idx])
  if(  out_idx==1) {
    outcome= c(rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }  else  {
    outcome= c(rep(1:(out_idx-1), each=not_out_num),rep(out_idx,out_num),rep((out_idx+1):num.line, each=not_out_num))
  }
  
  outcome=outcome[-which(outcome==104)]
  num.obs=length(outcome)
  
  case_comb = lapply(file_list.single[select_file_idx], function(x) readMat(x, header=FALSE)$a)
  
  ori.result=NULL
  hah=NULL
  for (i in 1:length(case_comb)) {
    
    y=NULL
    
    for (j in 1:num.node) {
      tmp=c(connect[ c(which(connect[,1]==j),which(connect[,2]==j)),])
      neighbor=tmp[which(tmp!=j)]
      if  (length(neighbor)!=1) {
        sum=rowSums(case_comb[[i]][,neighbor]-case_comb[[i]][,j])
      }  else   {         
        sum= case_comb[[i]][,j]
      }
      y=cbind(y,sum)
    }
    
    ori.result[[i]]=y
  }
  
  cur_path=str_c('/Users/jiahe/Desktop/CNN/input for GFT/118 binary dl/line ',out_idx)
  dir.create(cur_path)
  setwd(cur_path)
  file_name=str_c('outage',str_extract(file_list.single[select_file_idx], "\\-*\\d+\\.*\\d*"))
  #file_name=str_c('outage',str_extract(file_list[Intersect][select_file_idx], "\\-*\\d+\\.*\\d*"))
  
  for (i in 1:length(ori.result)) {
    write.table(ori.result[[i]],str_c(file_name[i],'.txt'),col.names = F,row.names = F)
  }
}











