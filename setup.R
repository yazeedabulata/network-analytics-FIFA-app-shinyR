#Set up of the dataset
setwd("C:/Users/abula/OneDrive/Desktop/Archive")

data <- read.csv("C:/Users/abula/OneDrive/Desktop/Archive/data.csv", header=TRUE, sep=";")
str(data)

df.complete <- data

# Factorize the necessary variables
df.complete$POS <- as.factor(df.complete$POS)
df.complete$VER <- as.factor(df.complete$VER)
df.complete$SKI <- as.factor(df.complete$SKI)
df.complete$WF <- as.factor(df.complete$WF)
df.complete$WR <- as.factor(df.complete$WR)
df.complete$Height_in_cm <- as.numeric(df.complete$Height_in_cm)
df.complete$Weight_in_kg <- as.numeric(df.complete$Weight_in_kg)
df.complete$Price <- as.numeric(df.complete$Price)
df.complete$nationality <- as.factor(df.complete$nationality)
df.complete$club_name <- as.factor(df.complete$club_name)
df.complete$league_name <- as.factor(df.complete$league_name)
df.complete$league_rank <- as.factor(df.complete$league_rank)
str(df.complete)

# NA Analysis
sum(is.na(df.complete))
table(is.na(df.complete))
colSums(is.na(df.complete))

# Add Labeling per Factor
df.complete$PAC_R <- NA

for(i in 1:length(df.complete$PAC)){
  if(df.complete$PAC[i] > 89) {  
    df.complete$PAC_R[i]<-"very good pace"
    
  } else if(df.complete$PAC[i] > 79 & df.complete$PAC[i] < 90) {
    df.complete$PAC_R[i]<-"good pace" 
    
  }else if((df.complete$PAC[i] > 69) & (df.complete$PAC[i] < 80)) {
    df.complete$PAC_R[i] <- "average pace" # add [i] here
  } else{
    df.complete$PAC_R[i]<- "" 
  } 
}

df.complete$SHO_R <- NA

for(i in 1:length(df.complete$SHO)){
  if(df.complete$SHO[i] > 89) {  
    df.complete$SHO_R[i]<-"very good shooting" 
    
  } else if(df.complete$SHO[i] > 79 & df.complete$SHO[i] < 90) {
    df.complete$SHO_R[i]<-"good shooting" 
    
  }else if((df.complete$SHO[i] > 69) & (df.complete$SHO[i] < 80)) {
    df.complete$SHO_R[i] <- "average shooting" # add [i] here
  } else{
    df.complete$SHO_R[i]<- "" 
  } 
}


df.complete$PAS_R <- NA

for(i in 1:length(df.complete$PAS)){
  if(df.complete$PAS[i] > 89) {  
    df.complete$PAS_R[i]<-"very good passing" 
    
  } else if(df.complete$PAS[i] > 79 & df.complete$PAS[i] < 90) {
    df.complete$PAS_R[i]<-"good passing" 
    
  }else if((df.complete$PAS[i] > 69) & (df.complete$PAS[i] < 80)) {
    df.complete$PAS_R[i] <- "average passing" # add [i] here
  } else{
    df.complete$PAS_R[i]<- "" 
  } 
}


df.complete$DRI_R <- NA

for(i in 1:length(df.complete$DRI)){
  if(df.complete$DRI[i] > 89) {  
    df.complete$DRI_R[i]<-"very good dribbling" 
    
  } else if(df.complete$DRI[i] > 79 & df.complete$DRI[i] < 90) {
    df.complete$DRI_R[i]<-"good dribbling" 
    
  }else if((df.complete$DRI[i] > 69) & (df.complete$DRI[i] < 80)) {
    df.complete$DRI_R[i] <- "average dribbling" # add [i] here
  } else{
    df.complete$DRI_R[i]<- "" 
  } 
}


df.complete$DEF_R <- NA

for(i in 1:length(df.complete$DEF)){
  if(df.complete$DEF[i] > 89) {  
    df.complete$DEF_R[i]<-"very good defending" 
    
  } else if(df.complete$DEF[i] > 79 & df.complete$DEF[i] < 90) {
    df.complete$DEF_R[i]<-"good defending" 
    
  }else if((df.complete$DEF[i] > 69) & (df.complete$DEF[i] < 80)) {
    df.complete$DEF_R[i] <- "average defending" # add [i] here
  } else{
    df.complete$DEF_R[i]<- "" 
  } 
}


df.complete$PHY_R <- NA

for(i in 1:length(df.complete$PHY)){
  if(df.complete$PHY[i] > 89) {  
    df.complete$PHY_R[i]<-"very good physical" 
    
  } else if(df.complete$PHY[i] > 79 & df.complete$PHY[i] < 90) {
    df.complete$PHY_R[i]<-"good physical" 
    
  }else if((df.complete$PHY[i] > 69) & (df.complete$PHY[i] < 80)) {
    df.complete$PHY_R[i] <- "average physical" 
  } else{
    df.complete$PHY_R[i]<- "" 
  } 
}

write.csv(df.complete,"C:/Users/abula/OneDrive/Desktop/Archive/finished.data.csv", row.names = FALSE)

