#Homework 1 for data mining
#Authored by tengshuang 
#Student number: 2012011270
#Finished time: 2015-03-29

#This function written for getting vectors from all xmls
#input： null
#calls:read.doc in 01.R
#output：1、 filename 
#        2、 file's vector
question1.all <- function() {
  source("01.R")
  path = getwd()
  setwd("nyt_corpus/nyt_news_corpus")
  file_name = dir()
  temp <- list()
  for (i in 1:length(file_name)){
	#print ("--------------------------------------------------------------------------------")
	#print(file_name[i])
	temp[[i]] <- read.doc(file_name[i])
	#print (file_vector) 
 }
  setwd(path)
  return(temp)
}

#This function written for getting a vector extracted from a xml
#input:filename
#calls:read.doc in 01.R
#output:document vector

question1 <- function(filename) {
  source("01.R")
  path = getwd() #get current directory
  setwd("nyt_corpus/nyt_news_corpus") #change the directory to XML file
  file_vector = read.doc(filename) #extract vector
  setwd(path) #revert the directory
  return(file_vector) #return the vector
}

#This fuction written for extracting the ("number")th word of story "number".xml
#input:filename and number
#calls:question1
#output:word
question2.word <- function(filename,number){
	file_vector = question1(filename) #get the document(named "filename")'s vector
	#print(file_vector)
	return(file_vector[number]) #return the word ranked "number"
}

#This fuction written for counting the number of times the word(ranked "number") appears in a story named "filename".
#input:filename and number
#calls:question2.word question1
#output:word times
question2.times <-function(filename,number){
	a = question2.word(filename,number) 
	b = question1(filename)
	c = table(b) #get the Contingency function
	return(c[a])
}
#This function written for finding weather "word" exists in the "filename"
find_word <- function(filename,word){
	a = question1(filename)
	for ( i in 1:length(a)){
		if(a[i] == word){
			return (1)
		}
	}
	return (0)
}

#This function written for counting the inverse-document-frequency of the "word"
#The "word" is the "number"th word of the "filename" 
#input: filename and number
#calls:question2.word find_word
#output:idf of the word
question2.idf <- function(filename,number){
	word = question2.word(filename,number)
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	count = 0
	for(k in 1:length(file_name))
		if(find_word(file_name[k],word) == 1)
		{
			count = count + 1
		}
	w <- log(length(file_name)/count)
	return (w)
}

#This function written for counting the inverse-document-frequency of the "word" directly
#input: word
#calls: find_word
#output:idf of the word
question2.idf2 <- function(word){
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path) #files to deal with
	count = 0 #DW
	for(k in 1:length(file_name))
		if(find_word(file_name[k],word) == 1) #to judge weather the word existing
		{
			count = count + 1
		}
	w <- log(length(file_name)/count)
	return (w) #idf
}

#This function written for constructing a bag-of-words data-frame from the document vectors for the stories
#input:NULL
#calls: question1 and make.BoW.frame in "01.R"
#output:bag-of-words data-frame
question3 <- function(){
	#getwd()
	source("01.R")
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path) #files to deal with
	temp <- list() 
	for(k in 1:length(file_name)){
	#for(k in 1:3){
		temp[[k]] <- table(question1(file_name[k]))
	}
	return (make.BoW.frame(temp))# in "01.R"
}

#This function written for visualizing the distribution of word length in one file
#input:filename
#calls:question1()
#output:the distribution of one file
question4.hist <- function(data){
	m = question1(data)
	a = nchar(m)
	b = hist(a,labels = TRUE) #draw the pictrue
	return (a)
}

question4.hist_all <- function(){

  	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path) #files to deal with
	print (file_name)
	temp <- vector()
    for (i in 1:length(file_name)){
	temp <- c(temp,question1(file_name[i]))
	}
	print (temp)
	a = nchar(temp)
	b = hist(a,labels = TRUE) #draw the pictrue
	return (a)
}
#This function written for calculating the categories of a file
#It means that one file may belong to different categories 
#Mergence applied to deal with different categories of the same root directory
#input:filename
#calls:functions in XML package
#output:the categories of a file(vector)
question5.get_cat<- function(filename){
	require(XML)
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	filename = paste(path,filename,sep = "/")
	doc <- xmlRoot(xmlTreeParse(filename))
	node.set <- getNodeSet(doc,path="//classifier[@type='taxonomic_classifier']")
	a <- sapply(node.set,xmlValue)
	#print(mode(a))
	#print(a)
	return (merge_category_names(a))#merge the categories of the same root directory
}
#This function written for visualizing the number of documents in each category 
#input:NULL
#calls:question5.get_cat
#output:draw a picture of the number of documents in each category 

question5.category<- function(){
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	b <- vector()
	for (i in 1:length(file_name))
	#for (i in 1:2)
	{
		#print(file_name[i])
		a <- question5.get_cat(file_name[i])
		#print(a)
		#number <- rep(file_name[i],length(a))
		#names(a) <- number
		b <- c(b,a)
	}
	x <- table(b)
	#print(b)
	par(mar = c(5,11,2,2))
	barplot(x,horiz = "TRUE",las = 2,cex.names = 0.3,space = 1,col=cm.colors(length(x)))
	#print(names(b))
	#c = hist(b)
	return (x)
}

#This function written for getting the month of a document
#input:filename
#calls:functions in XML package
#output:month of a document
question6.get_month<- function(filename){
	require(XML)
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	filename = paste(path,filename,sep = "/")
	doc <- xmlRoot(xmlTreeParse(filename))
	node.set <- getNodeSet(doc,path="//meta[@name='publication_month']")
	a <- sapply(node.set, xmlGetAttr, "content")
	print(mode(a))
	return (a)
}
#This function written for visualizing the number of documents in each month
#input:NULL
#calls:question6.get_month
#output:draw a picture of the number of documents in each month

question6.monHist <- function(){
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	b <- vector()
	for (i in 1:length(file_name))
	{
		a <- as.numeric(question6.get_month(file_name[i]))
		b <- c(a,b)
		#names(b[i]) = file_name[i]
	}
	print(b)
	x <- table(b)
	c = barplot(x,col=topo.colors(length(x)))
	return (b)
}

#This function written for getting the location of a document
#Be aware that only a few documents have the location
#input:filename
#calls:functions in XML package
#output:month of a document

question7.dateline<- function(filename){
	require(XML)
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	filename = paste(path,filename,sep = "/")
	doc <- xmlRoot(xmlTreeParse(filename))
	node.set <- getNodeSet(doc,path="//dateline")
	fulltext <- sapply(node.set,xmlValue) 
	return(fulltext)
	#location <- strsplit(as.character(fulltext),split=",")[[1]][1]
	mode(location)
	#return (location)
}
#This function written for visualizing the number of documents in each location
#Pay attention that operations are applied to split the location getted from question7.dateline in a correct format
#input:NULL
#calls:question7.dateline
#output:draw a picture of the number of documents in each location

question7.location<- function(){
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	b <- vector()
	for (i in 1:length(file_name))
	{
		a <- as.character(question7.dateline(file_name[i]))
		#print (mode(a))
		b <- c(b,a)
		#names(b[i]) = file_name[i]
	}
	c <- vector()
	for (i in 1:length(b)){
		c[i] <- strsplit(b[i],",")[[1]][1]  #split to get clear location
	}
	c[length(c) + 1:length(file_name)] <- "NA"
	x <- table(c)
	barplot(x,horiz = "TRUE",las = 2,col=cm.colors(length(x)))
	return (c)#得到类别
}

#This function written for calculating the scalar product of two vector 
#input:x,y as vector
#calls:NULL
#output:the scalar product of two vector 

scalar.product <- function(x,y = x){
	x <- as.matrix(x)
	x <- as.matrix(y)
	xy = x %*% t(y)
	return (xy)
}

#This function written for creating distance matrices from a data frame for the cosine “distance” 
#input:bag-of-words data-frame calculated in question3()
#calls:scalar.product
#output:distance matrices

similarity.cosine <- function(x){
	a = scalar.product(x)
	#print(a)
	n <- nrow(x)
    d <- array(NA,c(n,n),list(rownames(x),rownames(x))) #normalization
    for(i in 1:n) {
      for(j in 1:n) {
		#print(i)
		#print(j)
        d[i,j] <- a[i,j] / (sqrt(a[i,i])*sqrt(a[j,j])) #calculated cosine value
		#print(d[i,j])
      }
    }
    # we're done
    return(d)
}

#This function written for getting a Mapping matrix between categories and filenames
#the rownames of the matrix are filenames
#the colnames of the matrix are categories
#the matrix[i][j] == 1 means the filenames[i] belong to categories[j]
#input:NULL
#calls:question5.get_cat
#output:Mapping matrix
classifier.category <- function(){
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	b <- vector()
	for (i in 1:length(file_name))
	#for (i in 1:2)
	{
		a <- question5.get_cat(file_name[i])
		#number <- rep(i,length(a))
		number <- rep(file_name[i],length(a))
		names(a) <- number
		#print(a)
		b <- c(b,a) 
	}
	x <- table(b)
	#print (names(x))
	dd <- array(0,c(length(file_name),length(x)))
	colnames(dd) = names(x)
	rownames(dd) = file_name
	#print (dd)
	for(i in 1:length(b)){
		#print(names(b[i]))
		dd[names(b[i]),b[i]] = 1
		#print(dd[as.numeric(names(b[i])),b[i]])
		
	}
	return (dd)
}

#This function written for merging different categories in the same root dictionary
#input:categories as vector
#calls:NULL
#output:categories after merging as vector

merge_category_names<- function(a){
	#print(a)
	if(length(a) == 1) return(a)
	x <- rep(1:length(a))
	biao <- rep(0,length(a))
	#print (biao)
	count <- 1
	for (i in 1:(length(a)-1)){
		for (j in (i+1):length(a)){
			if((biao[j] != -1) == TRUE) {
			if(a[i] == substr(a[j],0,nchar(a[i]))) #
			{
				a[i] <- a[j]
				x[i] <- x[j]
				biao[j] <- -1
			}
			if(a[j] == substr(a[i],0,nchar(a[j])))
			{
				biao[j] <- -1
			}
			}
		}
	}
	#print (biao)
	count <- 1
	ans <- vector()
	for(i in 1:length(biao)){
		if(biao[i] != -1){
			ans[count] <- a[x[i]]
			count = count + 1
		}
	}
	return (ans)
}

comp <- function(dd,i,j){
	#print("hello")
	#print(i)
	#print(j)
	for(pos in 1:ncol(dd)){
		#print(pos)
		#print(dd[i,pos])
		#print(dd[j,pos])
		if(dd[i,pos] != dd[j,pos])
		{
			#print (FALSE)
			return(FALSE)
		}	
	}
	#print(TRUE)
	return (TRUE)
}

#This function written for getting the matrice of categories distribution of documents
#input:matrice getted from classifier.category
#calls:NULL
#output:categories distribution

refresh_vector <- function(dd){
	x <- rep(0,nrow(dd))
	m <- list()
	count <- 1
	for (i in 1:(nrow(dd))){
		same <- vector()
		if(x[i] != -1){
			same[1] <- i
			#print(same[1])
			if(i != nrow(dd)){
			
			for (j in (i + 1):nrow(dd)){
				if(x[j] != -1){
					#print(j)
					if(comp(dd,i,j)) 
					{
						print("TRUE1")
						x[j] <- -1
						same[length(same) + 1] = j
						print(length(same))
					}
				}
			}
			}
			m[[count]] <- same
			count = count + 1
			print(same)
			print(x)
		}
	}
	print(m)
	print(length(m))
	new_dd <- matrix(0,nrow = nrow(dd),ncol = length(m))
	print(new_dd)
	for (i in 1:length(m)){
		for (j in 1:length(m[[i]])){
			#print (j)
			print(m[[i]][j])
			kk <- m[[i]][j]
			new_dd[kk,i] = 1
		}
	}
	print(new_dd)
	return(new_dd)
}

#This function written for getting the sum of distances between stories in a category
#input:number of files in a category
#calls:NULL
#output:sum 
sc.same.category <- function(xx,sc){
	sum1 <- 0
	a = length(xx)
	for (i in 1:(length(xx) - 1)){
		for (j in (i + 1):length(xx)){
			sum1 = sum1+ sc[xx[i],xx[j]]
			print(sum1)
		}
	}
	return(sum1)
}
#This function written for getting the average distance between stories in the same category
#input:matrice getted from refresh_vector
#calls:similarity.cosine and sc.same.category
#output:the average distance between stories in the same category
same.average.distance<- function(dd){
	sc = similarity.cosine(question3())
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	count = 1;
	same <- vector()
	a <- 0
	number <- 0
	for (j in 1:ncol(dd)){
		for(i in 1:nrow(dd)){
			if(dd[i,j] == 1)
			{
				same[count] = i
				count = count + 1
			}
		}
		print(same)
		a = a + sc.same.category(same,sc)
		number <- number + length(same)*(length(same)-1)/2
	}
	#print(dd[,2])
	return(a/number)
}
#This function written for getting the sum of cosine distances between category x and category y
#input:sc is the cosine product matrices
#calls:NULL
#output:sum of distances

sc.differ.category <- function(x,y,sc){
	sum1 <- 0
	for (i in 1:length(x)){
		for(j in 1:length(y)){
			sum1 = sum1 + sc[x[i],y[j]]
		}
	}
	return (sum1)
}

#This function written for getting the documents belong to a category
#input:matrice getted from refresh_vector
#calls:NULL
#output:vector for catergory[col]
frame.col <- function(dd,col){
	count <- 1;
	a <- vector();
	for(i in 1:nrow(dd)){
			if(dd[i,col] == 1){
				a[count] = i
				count = count + 1
			}
		}
	print("frame.col")
	#print(a)
	return (a)
}

#This function written for getting the average distance  between stories in different categories
#input:matrice getted from refresh_vector
#calls:similarity.cosine and sc.differ.category
#output:the average distance between stories in the different category
differ.average.distance <- function(dd){
	sc = similarity.cosine(question3())
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	sum1 <- 0
	count <- 0
	for (i in 1:(ncol(dd) - 1)){
		x <- frame.col(dd,i)
		print(x)
		for (j in (i + 1):ncol(dd)){
			y <- frame.col(dd,j)
			print(y)
			sum1 = sum1 + sc.differ.category(x,y,sc)
			count = count + length(x) * length(y)
		}
	}
	return(sum1/count)
}

#This function written for getting the inverse document-frequency weighting of a document vector
#input:data-frame of documents
#calls:NULL
#output:matrices of TFIDF

TFIDF.weight <- function(aa){
	aa.rowsum <-  rowSums(aa)
	TF <- array(0,c(nrow(aa),ncol(aa)))
	for(i in 1:nrow(TF) ){
		for(j in 1:ncol(TF)){
			TF[i,j] <- aa[i,j] / aa.rowsum[i]
		}
	}
	
	aa.colsum <- colSums(aa)
	#print(TF)
	IDF <- log(nrow(aa)/(aa.colsum+1))
	#print(IDF)
	TFIDF <- array(0,c(nrow(aa),ncol(aa)))
	for (j in 1: ncol(TF))
	{
		TFIDF[,j] <- TF[,j] * IDF[j] 
	}
	#print(TFIDF)
	return(TFIDF)
}
#This function written for deleting the useless words in vector of the query
#input:names of data-frame getted from question3() and word vector of the query
#calls:NULL
#output:new vector of the query without useless words 
delete <-function(x,name.dd){
	#y <- intersect(x,name.dd)
	#print (y)
	k <- 0
	exis <- vector()
	for(i in 1:length(x)){
		#print(i)
		if(x[i] %in% name.dd){
			#print ("---------------")
			#print (name.x[i])
			k = k + 1
			exis[k] <- i
		}
	}
	xx <- vector()
	for (i in 1: length(exis)){
		xx[i] <- x[exis[i]]
		
	}
	#print(xx)
	return (xx)
}
#This function written for finding the document which best matches a given query string
#input:names of data-frame getted from question3() and a query
#calls:delete,make.BoW.frame in "01.R" and TFIDF.weight
#output:list of best matches documents and nearest distance
question2_3 <- function(dd,query){
	source("01.R")
	x <- strip.text(query)
	name.dd <- colnames(dd)
	x <- delete(x,name.dd)
	x <- table(x)
	temp <- list()
	for(i in 1:nrow(dd)){ 
		temp[[i]] <- dd[i,]
	}
	temp[[nrow(dd) + 1]] <- x
	aa <- make.BoW.frame(temp)
	cc <- TFIDF.weight(aa) * aa
	#query.new <- rbind(aa[nrow(aa),])
	query.new <- rbind(cc[nrow(aa),])
	bb <- aa[1:nrow(aa) - 1,]
	bb <- TFIDF.weight(bb) * bb
	#query.new <- TFIDF.weight(query.new) * query.new

	ans <- nearest.points(query.new,bb)
	return(ans)	
}

#This function written for converting a row number to filename
#input:a row number
#calls:NULL
#output:filename
convert_to_filename <- function(a)
{
	path = paste(getwd(),"nyt_corpus/nyt_news_corpus",sep = "/")
	file_name = dir(path)
	return(file_name[a[[1]]])
}



