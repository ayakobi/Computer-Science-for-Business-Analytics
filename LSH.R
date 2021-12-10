#509792ay - Computer Science for Marketing Analytics
install.packages("dplyr")
install.packages("proxy")
install.packages("stringr")
install.packages("data.table")
install.packages("rjson")
install.packages("matlab")
install.packages("LambertW")


library(dplyr)
library(proxy)
library(stringr)
library(data.table)
library(rjson)
library(matlab)
library(LambertW)

#Pair accuracy measure 
accuracy = function(tpairs,pairs){
  c= 0
  if(nrow(pairs)!=0){
    for(i in 1:nrow(pairs)){
      for(j in 1:nrow(tpairs)){
        if(pairs[i,1] %in% tpairs[j,1] & pairs[i,2] %in% tpairs[j,2]){
          c = c + 1
        }
      }
    }
  }
  return(c)
}

#Jaccard Similarity function
jaccard = function(x,y){
  v = c(x,y)
  jaccard = length( which(v == c(1,1) ) )/length(which(v == c(1,0) | v == c(1,0)| v == c(1,1)))
  return(jaccard)
}


#Import the JSON-type file en set the seed
set.seed(1)
JSONfile = fromJSON(file = "C:\\Users\\509792ay\\OneDrive - Erasmus University Rotterdam\\Documents\\TVs-all-merged.json")
rawdata = t(as.data.frame(JSONfile))

#Structure the data so that every columns represents an individual product
indeces = which(
  rawdata == "bestbuy.com" |
    rawdata == "newegg.com"  |
    rawdata == "amazon.com"   |
    rawdata == "thenerds.net"
)

structureddata = matrix(NA, max(diff(indeces)), length(indeces))

for (i in 1:(length(indeces) - 1)) {
  for (j in 1:(indeces[i + 1] - indeces[i])) {
    structureddata[j, i] = rawdata[indeces[i]:indeces[i + 1]][j]
  }
}

structureddata[,length(indeces)] = c(rawdata[tail(indeces,1):length(rawdata)], rep(NA,max(diff(indeces))-(length(rawdata)-tail(indeces,1)+1) ) )  
inputdata = structureddata

#Extract websites and ids
website = structureddata[1,]
modelids = structureddata[3,]
modelids = gsub("[[:punct:]]", "", modelids)


#Determine the true pairs using the modelIDs and consequently delete de modelIDs for the algorithm
truepairs = matrix(NA,2,1)
for(j in 1:ncol(structureddata)){
  for(k in 1:ncol(structureddata)){
    if(modelids[j] == modelids[k] & j<k )
      truepairs = cbind(truepairs, c(j,k))
    
  }
}
truepairs = na.omit(t(truepairs))
Dn = nrow(truepairs)

#Decompose the title name and add it to the structured matrix as model words
titlewords = matrix(NA,34,ncol(structureddata))

for(j in 1:ncol(structureddata)){
  modelnames = tail(na.omit(structureddata[,j]),n=1)
  unpunc =  gsub("[[:punct:]]", "", modelnames)
  strvec = unlist(strsplit(unpunc, " "))
  vec = rep(NA,34-length(strvec))
  titlewords[,j] = c(strvec ,vec)
}

modelwords = tolower(titlewords)

#MIDE
lettersonly = function(x) !grepl("[^A-Za-z]", x)
extractedids = matrix(NA, 1, ncol(titlewords))

#For the title words, remove frequent occurring words
tables = as.data.frame(table(titlewords))
names = tables[which(tables[,2]>4),1]

for(i in 1:nrow(titlewords)){
  for(j in 1:ncol(titlewords)){
    if(titlewords[i,j] %in%  names ){
      titlewords[i,j] = NA}
  }
}

#For the infrequent words, remove those only that are empty, containing letters only or do contain a lower case letter.
for(i in 1:ncol(titlewords)){
  index = which.max(nchar(titlewords[,i]))
  if(all(is.na(titlewords[,i])) || lettersonly(titlewords[index,i]) || str_detect(titlewords[index,i],"[[:lower:]]") ){
    extractedids[i] = NA 
  }
  else{
    extractedids[i] = titlewords[index,i]  
  }
}

#Form the initial pairs from the extracted model IDs 
extractedids = t(extractedids)

midepairs = matrix(NA,2,1)
for(j in 1:ncol(titlewords)){
  for(k in 1:ncol(titlewords)){
    if(extractedids[j] == extractedids[k] & j<k & !is.na(extractedids[j] == extractedids[k]))
      midepairs = cbind(midepairs, c(j,k))
    
  }
}
midepairs = na.omit(t(midepairs))

#Data Cleaning
#Substitution
structureddata = tolower(structureddata)
structureddata = gsub("\"", "inch", structureddata)
structureddata = gsub("inches", "inch", structureddata)
structureddata = gsub("hertz", "hz", structureddata)
structureddata = gsub("[[:punct:]]", "", structureddata)
structureddata = gsub(" ", "", structureddata)

#For Struc alter the data a bit and delete empty rows

tables = sort(table(structureddata)) #12146 unique charachters
table = t(as.data.frame(which(tables < 21 | tables> 399)))
names = colnames(table)
for(i in 1:nrow(structureddata)){
  for(j in 1:ncol(structureddata)){
    if(structureddata[i,j] %in%  names){
      structureddata[i,j] = NA
    }
  }
}


#Final Transformations
structureddata = structureddata[c(-1,-3),]
structureddata = rbind(structureddata,modelwords)
structureddata = structureddata[rowSums(is.na(structureddata)) != ncol(structureddata),]


#Calculate the Simularity matrix
uniquecharacters = unique(as.character(unique(unlist(structureddata))))
lengthuniques = length(uniquecharacters)

sim = matrix(0,length(uniquecharacters),ncol(structureddata))

for(i in 1:length(uniquecharacters)){
  for(j in 1:ncol(structureddata)){
    if( uniquecharacters[i] %in%  structureddata[,j]){
      sim[i,j] = 1
    } 
  }
}


#Perform MinHashing

#Generate random permutation using a hash functions 
primenumber = lengthuniques
while(isprime(primenumber) == 0){
  primenumber = primenumber + 1
}

signaturenumber = 810
a = sample(length(uniquecharacters), signaturenumber )
b = sample(length(uniquecharacters), signaturenumber )

permutations = matrix(NA,nrow(sim), signaturenumber)

for(i in 1:nrow(sim)){
  for(j in 1:signaturenumber){
    permutations[i,j] = (a[j]*i + b[j] ) %% primenumber
  }
}

#Calculate the Signature matrix
signature = matrix(0,signaturenumber, ncol(sim))
for(i in 1:signaturenumber){
  for(j in 1:ncol(sim)){
    c = which(sim[,j]>0 )
    signature[i,j] =  min((permutations[,i]*sim[,j])[c] )
  }
}

#Perform LSH and find the optimal pairs
bands = 120
rows = nrow(signature)/bands
lshpairs = matrix(NA,4,1)
for(i in 1:bands){
  for(j in 1:ncol(signature)){
    for(k in 1:ncol(signature)){
      if(all(signature[((i-1)*rows+1):(i*rows),j] == signature[((i-1)*rows+1):(i*rows),k]) & j<k & website[j]!=website[k]){
        # & !(j  %in% midepairs[,1] & k %in% midepairs[,2]): normally, this is added to the if statement for less comparisons, but LSH has to be evaluated
        lshpairs = cbind(lshpairs, c(i,j,k, mean(signature[((i-1)*rows+1):(i*rows),j])) ) 
      }
    }
  }
}


lshpairs = na.omit(t(lshpairs))

#Assign each candidate pair to bucket
interest = lshpairs[,c(1,4)]
buckets = numeric(nrow(lshpairs))
uniqueinterest = unique(interest)

for(i in 1:nrow(lshpairs)){
  for(j in 1:nrow(uniqueinterest)){
    if(all(uniqueinterest[j,] == interest[i,]))
      buckets[i] = j
  }
  
}

lshpairs = cbind(lshpairs,buckets)



#Calculate the Jaccard Simulation of the candidate pairs in each bucket and cut the pairs that are below the threshold t
jaccardsimilarity = numeric(nrow(lshpairs))
for(i in 1:nrow(lshpairs)){
  jaccardsimilarity[i] = jaccard(sim[,lshpairs[i,2]],lshpairs[i,3])
}
lshpairs = cbind(lshpairs,jaccardsimilarity)

threshold = 0.025
lshpairs = lshpairs[which(lshpairs[,6] > threshold),2:3]

#LSH Performance Evaluation
N = nrow(lshpairs)
Df = accuracy(truepairs,lshpairs)

PQ = Df / N
PC = Df / Dn
F = 2*((PC*PQ)/(PC + PQ))

#MIDE Performance Evaluation
N = nrow(lshpairs)
Df = accuracy(truepairs,midepairs)

PQ = Df / N
PC = Df / Dn
F = 2*((PC*PQ)/(PC + PQ))


#LSH+MIDE Performance Evaluation
finalpairs= rbind(midepairs,lshpairs[,2:3])
N = nrow(finalpairs)
Df = accuracy(truepairs,finalpairs)

PQ = Df / N
PC = Df / Dn
F = 2*((PC*PQ)/(PC + PQ))

#Bootstrapping and parameter optimization
LSH = function(structureddata,signaturenumber,bandnumber){
  
  #Extract websites and ids
  website = structureddata[1,]
  modelids = structureddata[3,]
  modelids = gsub("[[:punct:]]", "", modelids)
  
  
  #Determine the true pairs using the modelIDs and consequently delete de modelIDs for the algorithm
  truepairs = matrix(NA,2,1)
  for(j in 1:ncol(structureddata)){
    for(k in 1:ncol(structureddata)){
      if(modelids[j] == modelids[k] & j<k )
        truepairs = cbind(truepairs, c(j,k))
      
    }
  }
  truepairs = na.omit(t(truepairs))
  Dn = nrow(truepairs)
  
  #Decompose the title name and add it to the structured matrix as model words
  titlewords = matrix(NA,34,ncol(structureddata))
  
  for(j in 1:ncol(structureddata)){
    modelnames = tail(na.omit(structureddata[,j]),n=1)
    unpunc =  gsub("[[:punct:]]", "", modelnames)
    strvec = unlist(strsplit(unpunc, " "))
    vec = rep(NA,34-length(strvec))
    titlewords[,j] = c(strvec ,vec)
  }
  
  modelwords = tolower(titlewords)
  
  #MIDE
  lettersonly = function(x) !grepl("[^A-Za-z]", x)
  extractedids = matrix(NA, 1, ncol(titlewords))
  
  #For the title words, remove frequent occurring words
  tables = as.data.frame(table(titlewords))
  names = tables[which(tables[,2]>4),1]
  
  for(i in 1:nrow(titlewords)){
    for(j in 1:ncol(titlewords)){
      if(titlewords[i,j] %in%  names ){
        titlewords[i,j] = NA}
    }
  }
  
  #For the infrequent words, remove those only that are empty, containing letters only or do contain a lower case letter.
  for(i in 1:ncol(titlewords)){
    index = which.max(nchar(titlewords[,i]))
    if(all(is.na(titlewords[,i])) || lettersonly(titlewords[index,i]) || str_detect(titlewords[index,i],"[[:lower:]]") ){
      extractedids[i] = NA 
    }
    else{
      extractedids[i] = titlewords[index,i]  
    }
  }
  
  #Form the initial pairs from the extracted model IDs 
  extractedids = t(extractedids)
  
  midepairs = matrix(NA,2,1)
  for(j in 1:ncol(titlewords)){
    for(k in 1:ncol(titlewords)){
      if(extractedids[j] == extractedids[k] & j<k & !is.na(extractedids[j] == extractedids[k]))
        midepairs = cbind(midepairs, c(j,k))
      
    }
  }
  midepairs = na.omit(t(midepairs))
  
  #Data Cleaning
  #Substitution
  structureddata = tolower(structureddata)
  structureddata = gsub("\"", "inch", structureddata)
  structureddata = gsub("inches", "inch", structureddata)
  structureddata = gsub("hertz", "hz", structureddata)
  structureddata = gsub("[[:punct:]]", "", structureddata)
  structureddata = gsub(" ", "", structureddata)
  
  #For Struc alter the data a bit and delete empty rows
  
  tables = sort(table(structureddata)) #12146 unique charachters
  table = t(as.data.frame(which(tables < 21 | tables> 399)))
  names = colnames(table)
  for(i in 1:nrow(structureddata)){
    for(j in 1:ncol(structureddata)){
      if(structureddata[i,j] %in%  names){
        structureddata[i,j] = NA
      }
    }
  }
  
  
  #Final Transformations
  structureddata = structureddata[c(-1,-3),]
  structureddata = rbind(structureddata,modelwords)
  structureddata = structureddata[rowSums(is.na(structureddata)) != ncol(structureddata),]
  
  
  #Calculate the Simularity matrix
  uniquecharacters = unique(as.character(unique(unlist(structureddata))))
  lengthuniques = length(uniquecharacters)
  
  sim = matrix(0,length(uniquecharacters),ncol(structureddata))
  
  for(i in 1:length(uniquecharacters)){
    for(j in 1:ncol(structureddata)){
      if( uniquecharacters[i] %in%  structureddata[,j]){
        sim[i,j] = 1
      } 
    }
  }
  
  
  #Perform MinHashing
  
  #Generate random permutation using a hash functions 
  primenumber = lengthuniques
  while(isprime(primenumber) == 0){
    primenumber = primenumber + 1
  }
  
  signaturenumber = signaturenumber
  a = sample(length(uniquecharacters), signaturenumber )
  b = sample(length(uniquecharacters), signaturenumber )
  
  permutations = matrix(NA,nrow(sim), signaturenumber)
  
  for(i in 1:nrow(sim)){
    for(j in 1:signaturenumber){
      permutations[i,j] = (a[j]*i + b[j] ) %% primenumber
    }
  }
  
  #Calculate the Signature matrix
  signature = matrix(0,signaturenumber, ncol(sim))
  for(i in 1:signaturenumber){
    for(j in 1:ncol(sim)){
      c = which(sim[,j]>0 )
      signature[i,j] =  min((permutations[,i]*sim[,j])[c] )
    }
  }
  
  #Perform LSH and find the optimal pairs
  bands = bandnumber
  rows = nrow(signature)/bands
  lshpairs = matrix(NA,4,1)
  for(i in 1:bands){
    for(j in 1:ncol(signature)){
      for(k in 1:ncol(signature)){
        if(all(signature[((i-1)*rows+1):(i*rows),j] == signature[((i-1)*rows+1):(i*rows),k]) & j<k & website[j]!=website[k]){
          lshpairs = cbind(lshpairs, c(i,j,k, mean(signature[((i-1)*rows+1):(i*rows),j])) ) 
        }
      }
    }
  }
  
  
  lshpairs = na.omit(t(lshpairs))
  
  
  N = nrow(lshpairs)
  Df = accuracy(truepairs,lshpairs[,2:3])
  
  
  PQ = Df / N
  PC = Df / Dn
  F = 2*((PC*PQ)/(PC + PQ))  
  
  return(c(F,PQ,PC))
}

Clustering = function(structureddata,signaturenumber,optimalbandnumber, thresholdlevel){
  #Extract websites and ids
  website = structureddata[1,]
  modelids = structureddata[3,]
  modelids = gsub("[[:punct:]]", "", modelids)
  
  
  #Determine the true pairs using the modelIDs and consequently delete de modelIDs for the algorithm
  truepairs = matrix(NA,2,1)
  for(j in 1:ncol(structureddata)){
    for(k in 1:ncol(structureddata)){
      if(modelids[j] == modelids[k] & j<k )
        truepairs = cbind(truepairs, c(j,k))
      
    }
  }
  truepairs = na.omit(t(truepairs))
  Dn = nrow(truepairs)
  
  #Decompose the title name and add it to the structured matrix as model words
  titlewords = matrix(NA,34,ncol(structureddata))
  
  for(j in 1:ncol(structureddata)){
    modelnames = tail(na.omit(structureddata[,j]),n=1)
    unpunc =  gsub("[[:punct:]]", "", modelnames)
    strvec = unlist(strsplit(unpunc, " "))
    vec = rep(NA,34-length(strvec))
    titlewords[,j] = c(strvec ,vec)
  }
  
  modelwords = tolower(titlewords)
  
  #MIDE
  lettersonly = function(x) !grepl("[^A-Za-z]", x)
  extractedids = matrix(NA, 1, ncol(titlewords))
  
  #For the title words, remove frequent occurring words
  tables = as.data.frame(table(titlewords))
  names = tables[which(tables[,2]>4),1]
  
  for(i in 1:nrow(titlewords)){
    for(j in 1:ncol(titlewords)){
      if(titlewords[i,j] %in%  names ){
        titlewords[i,j] = NA}
    }
  }
  
  #For the infrequent words, remove those only that are empty, containing letters only or do contain a lower case letter.
  for(i in 1:ncol(titlewords)){
    index = which.max(nchar(titlewords[,i]))
    if(all(is.na(titlewords[,i])) || lettersonly(titlewords[index,i]) || str_detect(titlewords[index,i],"[[:lower:]]") ){
      extractedids[i] = NA 
    }
    else{
      extractedids[i] = titlewords[index,i]  
    }
  }
  
  #Form the initial pairs from the extracted model IDs 
  extractedids = t(extractedids)
  
  midepairs = matrix(NA,2,1)
  for(j in 1:ncol(titlewords)){
    for(k in 1:ncol(titlewords)){
      if(extractedids[j] == extractedids[k] & j<k & !is.na(extractedids[j] == extractedids[k]))
        midepairs = cbind(midepairs, c(j,k))
      
    }
  }
  midepairs = na.omit(t(midepairs))
  
  #Data Cleaning
  #Substitution
  structureddata = tolower(structureddata)
  structureddata = gsub("\"", "inch", structureddata)
  structureddata = gsub("inches", "inch", structureddata)
  structureddata = gsub("hertz", "hz", structureddata)
  structureddata = gsub("[[:punct:]]", "", structureddata)
  structureddata = gsub(" ", "", structureddata)
  
  #For Struc alter the data a bit and delete empty rows
  
  tables = sort(table(structureddata)) #12146 unique charachters
  table = t(as.data.frame(which(tables < 21 | tables> 399)))
  names = colnames(table)
  for(i in 1:nrow(structureddata)){
    for(j in 1:ncol(structureddata)){
      if(structureddata[i,j] %in%  names){
        structureddata[i,j] = NA
      }
    }
  }
  
  
  #Final Transformations
  structureddata = structureddata[c(-1,-3),]
  structureddata = rbind(structureddata,modelwords)
  structureddata = structureddata[rowSums(is.na(structureddata)) != ncol(structureddata),]
  
  
  #Calculate the Simularity matrix
  uniquecharacters = unique(as.character(unique(unlist(structureddata))))
  lengthuniques = length(uniquecharacters)
  
  sim = matrix(0,length(uniquecharacters),ncol(structureddata))
  
  for(i in 1:length(uniquecharacters)){
    for(j in 1:ncol(structureddata)){
      if( uniquecharacters[i] %in%  structureddata[,j]){
        sim[i,j] = 1
      } 
    }
  }
  
  
  #Perform MinHashing
  
  #Generate random permutation using a hash functions 
  primenumber = lengthuniques
  while(isprime(primenumber) == 0){
    primenumber = primenumber + 1
  }
  
  signaturenumber = signaturenumber
  a = sample(length(uniquecharacters), signaturenumber )
  b = sample(length(uniquecharacters), signaturenumber )
  
  permutations = matrix(NA,nrow(sim), signaturenumber)
  
  for(i in 1:nrow(sim)){
    for(j in 1:signaturenumber){
      permutations[i,j] = (a[j]*i + b[j] ) %% primenumber
    }
  }
  
  #Calculate the Signature matrix
  signature = matrix(0,signaturenumber, ncol(sim))
  for(i in 1:signaturenumber){
    for(j in 1:ncol(sim)){
      c = which(sim[,j]>0 )
      signature[i,j] =  min((permutations[,i]*sim[,j])[c] )
    }
  }
  
  #Perform LSH and find the optimal pairs
  bands = optimalbandnumber
  rows = nrow(signature)/bands
  lshpairs = matrix(NA,4,1)
  for(i in 1:bands){
    for(j in 1:ncol(signature)){
      for(k in 1:ncol(signature)){
        if(all(signature[((i-1)*rows+1):(i*rows),j] == signature[((i-1)*rows+1):(i*rows),k]) & j<k & website[j]!=website[k]){
          lshpairs = cbind(lshpairs, c(i,j,k, mean(signature[((i-1)*rows+1):(i*rows),j])) ) 
        }
      }
    }
  }
  
  
  lshpairs = na.omit(t(lshpairs))
  
  #Assign each candidate pair to bucket
  interest = lshpairs[,c(1,4)]
  buckets = numeric(nrow(lshpairs))
  uniqueinterest = unique(interest)
  
  for(i in 1:nrow(lshpairs)){
    for(j in 1:nrow(uniqueinterest)){
      if(all(uniqueinterest[j,] == interest[i,]))
        buckets[i] = j
    }
    
  }
  
  lshpairs = cbind(lshpairs,buckets)
  
  
  
  #Calculate the Jaccard Simulation of the candidate pairs in each bucket and cut the pairs that are below the threshold t
  jaccardsimilarity = numeric(nrow(lshpairs))
  for(i in 1:nrow(lshpairs)){
    jaccardsimilarity[i] = jaccard(sim[,lshpairs[i,2]],lshpairs[i,3])
  }
  lshpairs = cbind(lshpairs,jaccardsimilarity)
  
  threshold = thresholdlevel
  lshpairs = lshpairs[which(lshpairs[,6] > threshold),2:3]
  
  #LSH Performance Evaluation
  N = nrow(lshpairs)
  Df = accuracy(truepairs,lshpairs)
  
  PQ = Df / N
  PC = Df / Dn
  F = 2*((PC*PQ)/(PC + PQ))  
  
  return(c(F,PQ,PC))
}

#Optimizing the number of t-measure for LSH
bands = c(1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 16, 20, 24, 30, 32, 40, 48, 60, 80, 96, 120, 160, 240, 480)
Fmeasure = numeric(length(bands))

for(i in 1:10){
  sample = unique(sample(ncol(inputdata),ncol(inputdata), replace = TRUE))
  train = inputdata[,sample]
  test = inputdata[,-sample]
  
  for(j in 1:length(bands)){
    Fmeasure[j] = LSH(train, 480, bands[j])[1] 
  }
  
  optimalbandtrain = bands[which.max(Fthres)]
  optimalbandtest = bands[which.min(abs(bands - exp(W(-240*log(optimalbandtrain)))))]
  
  print(optimalbandtrain)
  print(optimalbandtest)
  print(LSH(test,240,optimalbandtrain))
}

Fmeasures = matrix(NA,10,length(bands))
PQmeasures = matrix(NA,10,length(bands))
PCmeasures = matrix(NA,10,length(bands))

for(i in 1:10){
  sample = unique(sample(ncol(inputdata),ncol(inputdata), replace = TRUE))
  train = structureddata[,sample]
  
  for(j in 1:length(bands)){
    Fmeasures[i,j] = LSH(train, 480, bands[j])[1]
    PQmeasures[i,j] = LSH(train, 480, bands[j])[2] 
    PCmeasures[i,j] = LSH(train, 480, bands[j])[3] 
  }
  
}

#Plot of the average F,PQ and PC results for all the bootstraps
t = bands^(-(bands/480))
plot(t,colMeans(Fmeasures))
plot(t,colMeans(PQmeasures))
plot(t,colMeans(PCmeasures))

#Optimizing the threshold t for the Clustering Algorithm
thresholds = c(0.05,0.01,0.015,0.02,0.025,0.03,0.035,0.04)
Fmeasure = numeric(length(thresholds))


for(i in 1:10){
  sample = unique(sample(ncol(inputdata),ncol(inputdata), replace = TRUE))
  train = inputdata[,sample]
  test = inputdata[,-sample]
  
  for(j in 1:length(thresholds)){
    Fmeasure[j] = Clustering(train, 480, 80, thresholds[j])[1] 
  }
  
  optimalt = thresholds[which.max(Fmeasure)]
  print(optimalt)
  print(Clustering(test, 240, 40, optimalt))
}

