m<--2.3+2*2.2-0.5*3.6
exp(1)^m/(1+exp(1)^m)


library('data.table')

setwd('C:/Users/russe/Desktop/social networks/hw4')
producers_and_films<-read.csv('producers_and_films.csv')
producers_and_films<-as.data.table(producers_and_films)
producer<-producers_and_films[country=='us']
keywods<-read.csv('film_keywords.csv')
box_revenue<-read.csv('box_office_revenues.csv')
prod_subsidiary<-read.csv('production_subsidiaries.csv')
talent<-read.csv('film_cast_members.csv')

#Q1.A
#use measure 1 to identify film type
#get number of films each company shoots each year
producer[,unique_films:=uniqueN(pindex),by=.(year,pcindex)]
#calculate the top quantile bar of each year
producer[,top_quartile:=lapply(.SD,quantile,0.75),by='year',.SDcols='unique_films']
#classify company as generalist and specialist
producer[unique_films>=top_quartile,type_measure1:='generalist']
producer[unique_films<top_quartile,type_measure1:='specialist']
#no need to delete duplicate as i group by pindex in the later section
dt_measure1<-producer

#measure 2 to identify film type
df_producer<-as.data.frame(producer)
library('gtools')
#get years
years<-unique(producer$year)
#create empty df
edgelist<-as.data.frame(matrix(ncol = 3))
colnames(edgelist)<-c('V1','V2','year')
#get edge list 
for (i in years){
  for (j in unique(producer[year==i,pindex])){
    comb<-as.character(unique(producer[year==i&pindex==j,pcindex]))
    if(length(comb)>1){
    edge<-as.data.frame(t(combn(comb,2)))
    edge$year=i
    edgelist<-rbind(edgelist,edge)
    }
  }
  print(i)
}
edgelist<-na.omit(edgelist)
library(igraph)

#get coreness using igraph package
years<-unique(edgelist$year)
dt0<-data.table(pcindex=NA,year=NA,core=NA)
for (i in years){
  graph<-graph_from_edgelist(as.matrix(edgelist[edgelist$year==i,1:2]),directed = FALSE)
  core<-as.data.frame(coreness(graph))
  dt1<-data.table(pcindex=rownames(core),year=i,core=core$`coreness(graph)`)
  dt0<-rbind(dt0,dt1)
}
dt0<-na.omit(dt0)

#identify type using measure 2

#i=2
dt0[,type_measure2:='s']
dt0<-as.data.table(dt0)
for (i in 1:nrow(dt0)){
  q<-dt0[pcindex==dt0[i,pcindex]&year<dt0[i,year],]
  q<-q[order(q,-year),]$core
  q[(length(q)+1):11]<-0
  q<-head(q,10)
  quan<-quantile(q,0.75)
  if (dt0[i,core]>=quan){
    dt0[i,4]<-'generalist'
  }else{
    dt0[i,4]<-'specialist'
  }
  print(i)
}

#join measure 2 with original df
library(dplyr)
dt_measure2<-inner_join(producers_and_films,dt0,by=c('pcindex','year'))

#identify type of films using measure1
dt_measure1[,filmtype_measure1:={
  if (sum('specialist' == .SD[,type_measure1])==1) 'peripheral_solo'
  else if(sum('generalist' == .SD[,type_measure1])==1) 'central-solo'
  else if(sum('specialist' == .SD[,type_measure1])>1&sum('generalist' == .SD[,type_measure1])<=1) 'peripheral_co'
  else if(sum('generalist' == .SD[,type_measure1])>1&sum('specialist' == .SD[,type_measure1])<=1) 'central_co'
  else 'hybrid'
},
by=.(pindex)]
dt_measure1<-unique(dt_measure1,by=c('pindex','year'))

#identify type of films using measure2
dt_measure2<-as.data.table(dt_measure2)
dt_measure2[,filmtype_measure2:={
  if (sum('specialist' == .SD[,type_measure2])==1) 'peripheral_solo'
  else if(sum('generalist' == .SD[,type_measure2])==1) 'central-solo'
  else if(sum('specialist' == .SD[,type_measure2])>1&sum('generalist' == .SD[,type_measure2])<=1) 'peripheral_co'
  else if(sum('generalist' == .SD[,type_measure2])>1&sum('specialist' == .SD[,type_measure2])<=1) 'central_co'
  else 'hybrid'
},
by=.(pindex)]
dt_measure2<-unique(dt_measure2,by=c('pindex','year'))

#join keywords with year
keyw<-inner_join(keywods,producers_and_films[,c('year','pindex')],by='pindex')
keyw<-unique(keyw)


#check if the single word is new or not
keyw<-as.data.table(keyw)
keyw[,oldest:=min(year),by=.(keyword)]
keyw[years-oldest<=3,type:='new']
keyw[years-oldest>3,type:='old']

film_count_new<-keyw[type=='new',.N,by=.(pindex,year)]

#key words comb
keyw_comb<-inner_join(keywods,producers_and_films[,c('year','pindex')],by='pindex')
keyw_comb<-unique(keyw_comb)
keyw_comb<-as.data.table(keyw_comb)
head(keyw_comb)
#count number of key words of each film each year
test<-keyw_comb[,count:=uniqueN(keyword),by=c('pindex','year')]
#select out films whose key words number is larger than 1
test<-test[count>1,]
#find combinaiton of keywords of each film each year and paste them into one column, each row represent one combination
test1<-test[,apply(t(combn(unique(as.character(keyword)),2)),1,paste,collapse=" "),by=c('pindex','year')]
#calculate the year that the key words combination appeared
test2<-test1[,oldest:=min(year),by=.(V1)]
#identify the combination of that year is within 3 years or not
test3<-test2[year-oldest<=3,type:='new']
test3<-test2[year-oldest>3,type:='old']
#count number of combinations appearing within 3 years
film_count_comb<-test3[type=='new',.N,by=.(pindex,year)]



#join count of new words for each film and film type using measure1
count_keywords_measure1<-inner_join(film_count_new,dt_measure1,by='pindex')[,c(1,2,3,12)]
count_keywords_measure1<-unique(count_keywords_measure1)
count_keywords_measure1<-as.data.table(count_keywords_measure1)
#average the count of new words within each category
count_keywords_measure1<-count_keywords_measure1[,.(avg=mean(N)),by=.(filmtype_measure1,year.x)]
#graph using measure 1 using only keywords
library(ggplot2)
ggplot(count_keywords_measure1,aes(x=year.x,y=avg,colour=filmtype_measure1))+geom_line()

#join count of new words_comb for each film and film type using measure1
count_keywords_measure1_comb<-inner_join(film_count_comb,dt_measure1,by='pindex')[,c(1,2,3,12)]
count_keywords_measure1_comb<-unique(count_keywords_measure1_comb)
count_keywords_measure1_comb<-as.data.table(count_keywords_measure1_comb)
#average the count of new words within each category
count_keywords_measure1_comb<-count_keywords_measure1_comb[,.(avg=mean(N)),by=.(filmtype_measure1,year.x)]
#graph using measure 1 using only keywords
library(ggplot2)
ggplot(count_keywords_measure1_comb,aes(x=year.x,y=avg,colour=filmtype_measure1))+geom_line()


#join count of new words for each film and film type using measure2
count_keywords_measure2<-inner_join(film_count_new,dt_measure2,by='pindex')[,c(1,2,3,11)]
count_keywords_measure2<-unique(count_keywords_measure2)
count_keywords_measure2<-as.data.table(count_keywords_measure2)
#average the count of new words within each category
count_keywords_measure2<-count_keywords_measure2[,.(avg=mean(N)),by=.(filmtype_measure2,year.x)]
#graph using measure 2 using only keywords
ggplot(count_keywords_measure2,aes(x=year.x,y=avg,colour=filmtype_measure2))+geom_line()

#join count of new words_comb for each film and film type using measure2
count_keywords_measure2_comb<-inner_join(film_count_comb,dt_measure2,by='pindex')[,c(1,2,3,11)]
count_keywords_measure2_comb<-unique(count_keywords_measure2_comb)
count_keywords_measure2_comb<-as.data.table(count_keywords_measure2_comb)
#average the count of new words within each category
count_keywords_measure2_comb<-count_keywords_measure2_comb[,.(avg=mean(N)),by=.(filmtype_measure2,year.x)]
#graph using measure 2 using only keywords_comb
ggplot(count_keywords_measure2_comb,aes(x=year.x,y=avg,colour=filmtype_measure2))+geom_line()



#Q1B

#single new key word of each producer
keyw_q2<-inner_join(keywods,producers_and_films[,c('year','pindex','pcindex')],by='pindex')
keyw_q2<-as.data.table(unique(keyw_q2))
#find the year the words appearing for the first time
keyw_q2[,oldest:= min(year),by=.(keyword)]
#check if it's new word or not
keyw_q2<-keyw_q2[year-oldest<=3,type:='new']
keyw_q2<-keyw_q2[year-oldest>3,type:='old']
#aggregate new words number by companies
prod_count_keyword<-keyw_q2[type=='new',.N,by=.(pcindex,year)]

#key words combination of each producer
#key words comb
keyw_comb<-inner_join(keywods,producers_and_films[,c('year','pindex','pcindex')],by='pindex')
keyw_comb<-unique(keyw_comb)
keyw_comb<-as.data.table(keyw_comb)
head(keyw_comb)
#count number of key words of each filmr each year
test_q2<-keyw_comb[,count:=uniqueN(keyword),by=c('pindex','year')]
#select out producer whose key words number is larger than 1
test_q2<-test_q2[count>1,]
#find combinaiton of keywords of each producer each year and paste them into one column, each row represent one combination
test1_q2<-test_q2[,apply(t(combn(unique(as.character(keyword)),2)),1,paste,collapse=" "),by=c('pindex','year')]
#calculate the year that the key words combination appeared
test2_q2<-test1_q2[,oldest:=min(year),by=.(V1)]
#identify the combination of that year is within 3 years or not
test3_q2<-test2_q2[year-oldest<=3,type:='new']
test3_q2<-test2_q2[year-oldest>3,type:='old']
#count number of combinations appearing within 3 years of each film
film_count_comb_q2<-test3_q2[type=='new',.N,by=.(pindex,year)]
#join with pcindex
join_q2<-inner_join(film_count_comb_q2,producers_and_films[,c(1,4)],by='pindex')
#count new key words comb of each company
join_q2<-as.data.table(join_q2)
prod_count_comb<-join_q2[,count:=sum(N),by=.(pcindex)]
#unique by year and pcindex
prod_count_comb<-unique(prod_count_comb[,c(2,4,5)],by=c('pcindex','year'))

#--------------------------------------------------
#number of films made of each type for each producer using measure 1
film_eachtype_m1<-dt_measure1
film_eachtype_m1[,central_co:=1]
#check if it's centrao_co-production
film_eachtype_m1[film_eachtype_m1$filmtype_measure1=='central_co',]$central_co<-1
film_eachtype_m1[film_eachtype_m1$filmtype_measure1!='central_co',]$central_co<-0
#aggregate number of films made of centro_co type for each producer each year
film_eachtype_m1[,central_co_prod:=sum(central_co),by=.(year,pcindex)]

film_eachtype_m1[,peripheral_co:=1]
film_eachtype_m1[film_eachtype_m1$filmtype_measure1=='peripheral_co',]$peripheral_co<-1
film_eachtype_m1[film_eachtype_m1$filmtype_measure1!='peripheral_co',]$peripheral_co<-0
#aggregate number of films made of peripheral_co type for each producer each year
film_eachtype_m1[,peripheral_co_prod:=sum(peripheral_co),by=.(year,pcindex)]

film_eachtype_m1[,hybrid:=1]
film_eachtype_m1[film_eachtype_m1$filmtype_measure1=='hybrid',]$hybrid<-1
film_eachtype_m1[film_eachtype_m1$filmtype_measure1!='hybrid',]$hybrid<-0
#aggregate number of films made of hybrid type for each producer each year
film_eachtype_m1[,hybrid_co:=sum(hybrid),by=.(year,pcindex)]

#join the revenue of each film using measure1
film_type_m1_revenue<-inner_join(film_eachtype_m1,box_revenue[,c(1,4)],by='pindex')
#aggrete by producer
film_type_m1_revenue<-as.data.table(film_type_m1_revenue)
film_type_m1_revenue[,rev_prod:=sum(total_box),by=.(pcindex)]


#number of films made of each type for each producer using measure 2
film_eachtype_m2<-dt_measure2
film_eachtype_m2[,central_co:=1]
film_eachtype_m2[film_eachtype_m2$filmtype_measure2=='central_co',]$central_co<-1
film_eachtype_m2[film_eachtype_m2$filmtype_measure2!='central_co',]$central_co<-0
film_eachtype_m2[,central_co_prod:=sum(central_co),by=.(year,pcindex)]

film_eachtype_m2[,peripheral_co:=1]
film_eachtype_m2[film_eachtype_m2$filmtype_measure2=='peripheral_co',]$peripheral_co<-1
film_eachtype_m2[film_eachtype_m2$filmtype_measure2!='peripheral_co',]$peripheral_co<-0
film_eachtype_m2[,peripheral_co_prod:=sum(peripheral_co),by=.(year,pcindex)]

film_eachtype_m2[,hybrid:=1]
film_eachtype_m2[film_eachtype_m2$filmtype_measure2=='hybrid',]$hybrid<-1
film_eachtype_m2[film_eachtype_m2$filmtype_measure2!='hybrid',]$hybrid<-0
film_eachtype_m2[,hybrid_co:=sum(hybrid),by=.(year,pcindex)]

#join the revenue of each film using measure2
film_type_m2_revenue<-inner_join(film_eachtype_m2,box_revenue[,c(1,4)],by='pindex')
#aggrete by producer
film_type_m2_revenue<-as.data.table(film_type_m2_revenue)
film_type_m2_revenue[,rev_prod:=sum(total_box),by=.(pcindex)]

#----------------------------------------------------------------
#how many years in operation + single keyword
prod_count_keyword_numberofyear<-prod_count_keyword
prod_count_keyword_numberofyear[,oldest:=min(year),by=.(pcindex)][,numberofyear:=year-oldest,by=.(pcindex)]
#how many years in operation + combination
prod_count_comb_numberofyear<-prod_count_comb
prod_count_comb_numberofyear[,oldest:=min(year),by=.(pcindex)][,numberofyear:=year-oldest,by=.(pcindex)]

#-----------------------------------
#if it's subsidiary 
prod_count_keyword_numberofyear_issub<-left_join(prod_count_keyword_numberofyear,prod_subsidiary,by='pcindex')
#check if subsidiary using single key word
prod_count_keyword_numberofyear_issub<-as.data.table(prod_count_keyword_numberofyear_issub)
prod_count_keyword_numberofyear_issub[,issub:=ifelse(year>=first_year&year<=last_year,1,0)]
prod_count_keyword_numberofyear_issub[is.na(prod_count_keyword_numberofyear_issub$issub),issub:=0]

#check if subsidiary using comb key word
prod_count_comb_numberofyear_issub<-left_join(prod_count_comb_numberofyear,prod_subsidiary,by='pcindex')
prod_count_comb_numberofyear_issub<-as.data.table(prod_count_comb_numberofyear_issub)
prod_count_comb_numberofyear_issub[,issub:=ifelse(year>=first_year&year<=last_year,1,0)]
prod_count_comb_numberofyear_issub[is.na(prod_count_comb_numberofyear_issub$issub),issub:=0]

#-----------------------------------------
#jaccard distance
library('proxy')
library('Matrix')
keyword_year<-as.data.table(inner_join(keywods,producers_and_films[,c(1,2,4)],by='pindex'))

#create empty df
emp<-as.data.frame(matrix(ncol = 5))
#set name
colnames(emp)<-c('V1','V2','pcindex','year','avg')
yearlist<-unique(keyword_year$year)

#define function to calculate jaccard distance (online resource)
jaccard_distance <- function(m) {
  A <- tcrossprod(m)
  im <- which(A > 0, arr.ind=TRUE, useNames = F)
  b <- rowSums(m)
  Aim <- A[im]
  sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = 1-Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
}

#calculate jaccard distance matrix
for (y in yearlist){
  dt1<-keyword_year[year==y|year==y-1|year==y-2,c(2,3,4)]
  dt1[,year:=NULL]
  dt1[,value:=1]
  dt1<-as.data.frame(dt1)
  #aff_matrix<-as.data.frame(dcast(dt1,pcindex~keyword,fill=0))
  list1 <- data.frame(pc=unique(dt1$pcindex))
  list2<-data.frame(ke=unique(dt1$keyword))
  dt1$pcnumber<-match(dt1$pcindex,list1$pc)
  dt1$kenumber<-match(dt1$keyword,list2$ke)
  aff_matrix<-sparseMatrix(i=dt1$pcnumber,j=dt1$kenumber,x=dt1$value,dims = c(nrow(list1), nrow(list2)), 
                           dimnames = list(list1$pc, list2$ke))
  aff_matrix[aff_matrix>0]<-1
  jacc_matrix<-jaccard_distance(aff_matrix)
  #multidimensional scaling
  scale<-cmdscale(jacc_matrix)
  jacc_matrix[jacc_matrix==1]<-NA
  #calculate average jaccard distance of each producer each 
  colmean<-colMeans(jacc_matrix,na.rm = TRUE)
  scale<-as.data.frame(scale)
  scale$pcindex<-list1$pc
  scale$year<-y
  scale$avg<-colmean
  emp<-rbind(emp,scale)
  print(y)
}

#delete first NA row
jaccard<-emp[-1,]

#-------------------------------------
#total film made that year
totalfilm<-as.data.table(producers_and_films)
totalfilm[,totalfilm:=uniqueN(pindex),by=.(year)]

#--------------------------------------
#regression
library('MASS')
#single keyword + measure1
#join calculated columns from each dataframe
reg2.1<-inner_join(prod_count_keyword_numberofyear_issub,film_type_m1_revenue,by=c('year','pcindex'))
reg2.1<-inner_join(reg2.1,jaccard,by=c('pcindex','year'))
reg2.1<-inner_join(reg2.1,totalfilm,by=c('pcindex','year'))
reg2.1<-reg2.1[,c('N','central_co_prod','peripheral_co_prod','hybrid_co','V1','V2','rev_prod','numberofyear','issub','year','totalfilm','pcindex')]
reg2.1<-unique(reg2.1)
s1b1<-summary(glm.nb(N~central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg2.1,offset(totalfilm)))
s1b1
#single keyword+measure2
#join calculated columns from each dataframe
reg2.2<-inner_join(prod_count_keyword_numberofyear_issub,film_type_m2_revenue,by=c('year','pcindex'))
reg2.2<-inner_join(reg2.2,jaccard,by=c('pcindex','year'))
reg2.2<-inner_join(reg2.2,totalfilm,by=c('pcindex','year'))
reg2.2<-reg2.2[,c('N','central_co_prod','peripheral_co_prod','hybrid_co','V1','V2','rev_prod','numberofyear','issub','year','totalfilm','pcindex')]
reg2.2<-unique(reg2.2)
s1b2<-summary(glm.nb(N~central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg2.2,offset(totalfilm),na.action = 'na.omit'))
s1b2

#keywords combination + measure 1
#join calculated columns from each dataframe
reg2.3<-inner_join(prod_count_comb_numberofyear_issub,film_type_m1_revenue,by=c('year','pcindex'))
reg2.3<-inner_join(reg2.3,jaccard,by=c('pcindex','year'))
reg2.3<-inner_join(reg2.3,totalfilm,by=c('pcindex','year'))
reg2.3<-reg2.3[,c('count','central_co_prod','peripheral_co_prod','hybrid_co','V1','V2','rev_prod','numberofyear','issub','year','totalfilm','pcindex')]
reg2.3<-unique(reg2.3)
s1b3<-summary(glm.nb(count~central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg2.3,offset(totalfilm)))
s1b3

#keywords combination + measure 2
#join calculated columns from each dataframe
reg2.4<-inner_join(prod_count_comb_numberofyear_issub,film_type_m2_revenue,by=c('year','pcindex'))
reg2.4<-inner_join(reg2.4,jaccard,by=c('pcindex','year'))
reg2.4<-inner_join(reg2.4,totalfilm,by=c('pcindex','year'))
reg2.4<-reg2.4[,c('count','central_co_prod','peripheral_co_prod','hybrid_co','V1','V2','rev_prod','numberofyear','issub','year','totalfilm','pcindex')]
reg2.4<-unique(reg2.4)
s1b4<-summary(glm.nb(count~central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg2.4,offset(totalfilm)))
s1b4


#Q2
#use single keyword
loess1<-inner_join(prod_count_keyword[,c('N','year','pcindex')],jaccard[,c('pcindex','year','avg')],by=c('year','pcindex'))
ggplot(loess1,aes(avg,N))+geom_smooth(method = 'loess',se=T)+labs(x='average jaccard distance',y='new keywords')

#use keyword combination
loess1<-inner_join(prod_count_comb[,c('count','year','pcindex')],jaccard[,c('pcindex','year','avg')],by=c('year','pcindex'))
ggplot(loess1,aes(avg,count))+geom_smooth(method = 'loess',se=T)+labs(x='average jaccard distance',y='new keywords')

#Q3
#yearly return
#join calculated columns from each dataframe
return_norm<-inner_join(box_revenue,producer[,c('pindex','year','pcindex')],by='pindex')
return_norm<-as.data.table(return_norm)
#calculate total box office of each firm 
return_norm[,total_box_prod:=sum(total_box),by=.(pcindex,year)]
#calculate total release coverage of each firm
return_norm[,release_prod:=sum(release_coverage),by=.(pcindex,year)]
return_norm<-unique(return_norm[,c(4:8)])
#divide box office by release coverage
return_norm[,return:=total_box_prod/release_prod]
return_norm<-return_norm[!is.infinite(return_norm$return),]
#calculate mean and standard deviation
return_norm[,mean:=mean(return),by=.(year)][,std:=sd(return),by=.(year)]
#normalize
total_return<-return_norm[,norm:=(return-mean)/std][,c(2,3,9)]
#delete duplicate
total_return<-unique(total_return,by=c('year','pcindex'))


#regression predicting normalized returns
reg3<-inner_join(reg2.2,total_return,by=c('year','pcindex'))
s3<-summary(lm(total_norm~central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg3.2))
s3


#Q4A
#predicting the count of new keywords in solo film
#join needed columns
solo_prod_film_m1<-as.data.table(inner_join(film_eachtype_m1[,c('pindex','filmtype_measure1','pcindex')],prod_count_keyword,by=c('pcindex')))
#aggregate number of new keywords of solo films for each producer
solo_prod_film_m1[filmtype_measure1=='central-solo'|filmtype_measure1=='peripheral_solo',count_new_solo:=sum(N),by=.(pcindex,year)]
#reorder
solo_prod_film_m1<-solo_prod_film_m1[order(solo_prod_film_m1$year),]
solo_prod_film_m1<-unique(solo_prod_film_m1,by=c('pindex','pcindex','year'))
#check if it's hybrid 
solo_prod_film_m1[,hybrid_cum:=ifelse(filmtype_measure1=='hybrid',N,0)]
#aggregate cumulative number of new keywords from hybrid films of each producer
solo_prod_film_m1[,cum_hrbyid:=cumsum(hybrid_cum),by=.(pcindex)][,hybrid_cum:=NULL]
solo_prod_film_m1[is.na(count_new_solo),count_new_solo:=0]
solo_prod_film_m1<-solo_prod_film_m1[,c('pcindex','year','count_new_solo','cum_hrbyid')]
solo_prod_film_m1<-unique(solo_prod_film_m1)
solo_prod_film_m1<-solo_prod_film_m1[count_new_solo!=0,]
#join the control variables
reg4.1<-inner_join(solo_prod_film_m1,reg2.1[,c(5:12)],by=c('pcindex','year'))
#colnames(reg4.1)<-c('pcindex','year','count_new_solo','numberofyear','V1','V2','rev_prod','cum_hybrid','issub','totalfilm')
s4.1<-summary(glm.nb(count_new_solo~cum_hybrid+V1+V2+rev_prod+numberofyear+issub+factor(year),reg4.1,offset(totalfilm)))
s4.1

#Q4B
#does introduce new keyword result into higher box office?
#using measure1
#join needed columns
reg4.21<-inner_join(reg2.1,total_return,by=c('year','pcindex'))
reg4.21<-unique(reg4.21)
#colnames(reg4.21)<-c('rev_prod','central_co_prod','peripheral_co_prod','hybrid_co','V1','V2','N','numberofyear','issub','year','totalfilm','pcindex','norm','total_norm')
s4.21<-summary(lm(total_norm~N+central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg4.21))
s4.21
#using measure2
#join needed columns
reg4.22<-inner_join(reg2.2,total_return,by=c('year','pcindex'))
#colnames(reg4.22)<-c('rev_prod','central_co_prod','peripheral_co_prod','hybrid_co','V1','V2','N','numberofyear','issub','year','totalfilm','pcindex','norm','total_norm')
s4.22<-summary(lm(total_norm~N+central_co_prod+peripheral_co_prod+hybrid_co+V1+V2+rev_prod+numberofyear+issub+factor(year),reg4.22))
s4.22

 

#Extra
#Does engaging in more hybrid collaborations seem to help with hiring more innovative creative talent?
#join talent with number of new key words dt
talent_inno<-inner_join(film_count_new,talent[,2:4],by=c('pindex'))
talent_inno<-inner_join(producer[,c('pindex','pcindex')],talent_inno,by='pindex')
talent_inno<-as.data.table(talent_inno)
talent_inno<-talent_inno[order(talent_inno$year.x),]
#accumulate number of new keywords by talent
talent_inno[,cum_talent:=cumsum(N)-N,by='nconst']
#accumulate number of new keywords by producer
talent_inno<-unique(talent_inno[,cum_prod:=sum(cum_talent),by=.(year.y,pcindex)][,c(1,2,3,8)])

#join with #of each film type producer made
pred_inno<-unique(inner_join(film_eachtype_m1[,c(2,4,12,14,16)],talent_inno[,c(2:4)],by=c('year'='year.x','pcindex')))
pred_inno<-unique(inner_join(pred_inno,totalfilm[,c(2,7)],by='year'))

s_extra<-summary(glm.nb(cum_prod~central_co_prod+peripheral_co_prod+hybrid_co,pred_inno,offset(totalfilm)))
s_extra


