

# Set work directory to current R code document path 
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())

# Read War&Peace book 
data <- read.csv("war&peac.txt", header = F, sep = " ", stringsAsFactors = F) # loading "war & peace"
data <- unlist(data)
data <- data[grepl("^[[:alpha:]]+$", data)] 
data <- data[which(sapply(data, nchar) == 5)] # keeping only 5 letters word 
data <- unique(data)
data <- tolower(data)
data <- data[-which(sapply(data, function(x) substring(x, 1, 1)) == "x")] 

# All combinations of 2 letters. example: aa,ab,ac .... ba,bb,.....zz
x <- expand.grid(rep(list(letters), 2))
let_comb <- do.call(paste0, x)

# Prepare empty matrix which will be filled with probabilities of letters combs
comb_list <- matrix(,676,4)
pm        <- matrix(,676,4)
rownames(comb_list) <- let_comb

# Combinations probability matrix  - Calculating  probability of each comb per location in  word
for (n in 1:4) {
  
  for (i in  1:length(let_comb)) {
    
    comb <- let_comb[i]
    a    <- which ( sapply(data, function(x) substring(x, n, n+1) ) == comb ) #Locations which the data is similar to specific combination  
    z    <- data[a]
    comb_list[i,n] <-length(z)
  }
  
  pm[,n] <- comb_list[,n]/sum(comb_list[,n]) # calculating probabilities
  rownames(pm) <- let_comb
}

# results matrix 
head(pm)

# test - probabilities in each column (location in the word) add up to one 
colSums(pm)
  
# save
m.text <- pm
save(m.text,file = "TEXT_Model.Rdata" )

# load check
rm(m.text)
load("TEXT_Model.Rdata")
which( (m.text == pm) == FALSE)  # integer(0) --> there is no false values 

