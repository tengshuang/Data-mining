#command:

#1）首先将工作目录设置到2015_datamining_project1所在文件夹

setwd("F:/2015_datamining_project1/2012011270_滕爽_计22")

#2）将函数引入到R

source("main.R")

#3）执行以下指令：

#question1_1:Create document vector

#获取一个文件的向量：

ans1 = question1("0023931.xml")

#save(ans1, file = "./data/Q1.Rdata")

#获取所有文件的向量列表：

ans1_all = question1.all()

save(ans1_all,ans1, file = "./data/Q1.Rdata")
#question1_2:

#extract the 37th word of story number 1595645.xml

ans2_1 = question2.word("1595645.xml",37)

#count the number of times the word “experiencing” appears in that story.

ans2_2 = question2.times("1595645.xml",37)

#count the inverse-document-frequency of the word “experiencing”

ans2_3 = question2.idf("1595645.xml",37)

ans2_3_2 = question2.idf2("experiencing")

save(ans2_1,ans2_2,ans2_3,ans2_3_2,file = "./data/Q2.Rdata")
#question1_3:

#construct a bag-of-words data-frame from the document vectors for the stories

ans3 = question3()
save(ans3,file = "./data/Q3.Rdata")
#question1_4:

#Visualize the distribution of word length in a document

ans4 = question4.hist("0023931.xml")

#Visualize the tatal distribution of word length

ans4_2 = question4.hist_all()
save(ans4,ans4_2,file = "./data/Q4.Rdata")
#question1_5

#Visualize the number of documents in each category

ans5 = question5.category()
save(ans5,file = "./data/Q5.Rdata")
#question1_6:

#Visualize the number of documents in each month

ans6 = question6.monHist()
save(ans6,file = "./data/Q6.Rdata")
#question1_7:

#Visualize the number of documents in each location

ans7 = question7.location()
save(ans7,file = "./data/Q7.Rdata")
#question2_1:

#Create distance matrices from this data frame for the cosine “distance”

Tans1 = similarity.cosine(ans3)
save(Tans1,file = "./data/2_Q1.Rdata")
#question2_2:

#the average distance between stories in the same category

a = classifier.category() 
b = refresh_vector(a)
Tans2_1 = same.average.distance(b)
Tans2_2 = differ.average.distance(b)
save(Tans2_1,Tans2_2,file = "./data/2_Q2.Rdata")
#question2_3:

#find the document which best matches a given query string.

query2 <- question1("0023931.xml") 
query3 <- question1("0068412.xml")
query1 <- "An old woman had a cat. The cat was very old; she could not run quickly, and she could not bite, because she was so old. One day the old cat saw a mouse; she jumped and caught the mouse. But she could not bite it; so the mouse got out of her mouth and ran away, because the cat could not bite it."
Tans2_3_1 <- question2_3(ans3,query1) #neariest xml number and distance
Tans2_3_2 <- question2_3(ans3,query2)
Tans2_3_3 <- question2_3(ans3,query3)

aa = convert_to_filename(Tans2_3_1) #convert xml number to filename
bb = convert_to_filename(Tans2_3_2)
cc = convert_to_filename(Tans2_3_3)

save(Tans2_3_1,Tans2_3_2,Tans2_3_3,aa,bb,cc,file = "./data/2_Q3.Rdata")

