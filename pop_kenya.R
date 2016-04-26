#Popular Kenyan Musics
setwd("C:\\Users\\user\\Google Drive\\Data_Science\\kenyan_music")
source("C:\\Users\\user\\Google Drive\\Data_Science\\utilities.R")

library(data.table)

data = fread("sdata_2.csv")
excl = c(10,11,12)

data2 = to_numerics(excl, data)

data2 = data2[,dates:=as.Date(unixtodate(timestamp))]
data2 = data2[,datesong:=paste(dates, s_id, sep="_")]
data2 = data2[,datesong:=paste(s_id,ytub_url,  sep="_")]

morevars = quote( .(mean_views = mean(views),mean_score = mean(score),min_views = min(views), max_views = max(views),mean_likes = mean(likes),min_likes = min(likes), max_likes = max(likes), vid_name = unique(vid_name), ytub_url = unique(ytub_url), art_name = unique(art_name), d_posted = unique(d_posted))) 
data3 = data2[, eval(morevars), by=datesong] 

data4 = data2[, eval(morevars), by=ytub_url]

data4 = data4[order(mean_score, decreasing = TRUE)]

# get artists

artists = fread("kenyan_artists.csv")


anames = paste(artists[,names], collapse=" ")

splited = strsplit(anames, " ")[[1]]
splited = splited[!splited %in% ""]


#functions to get things done

get_me_names = function(art, splited){
nome = tolower(strsplit(art, " ")[[1]])
nome = nome[!nome %in% ""]
    ff = list()
      for (i in 1:length(nome)){
        ff[i] = list(which(!is.na(match(splited,nome[i]))))
  
         }
  return(ff)
  
}



get_me_codes = function(list_ff){
  ddx  = list()
  list_ff = list_ff[sapply(list_ff, length)>0]
  for( i in 1:length(list_ff)){
    
    ddx2  = list()
    ll = len(list_ff[[i]])
    for(j in 1:ll){
      
      if(length(list_ff) > i){
        nxt = list_ff[[i]][j] + 1
        ddx2[j] = list(!is.na(match(nxt,list_ff[[i+1]])) && length(list_ff)>i)  
      } else if (length(list_ff) == i){
        
        nxt =  list_ff[[i]]
        ddx2[j] = list(!is.na(match(nxt,list_ff[[i]])) && length(list_ff)==i)
      }
      
    }
    
    ddx[i] = list(unlist(ddx2))
    
  }
  
  namess = list()
  for(i in 1:length(list_ff)){
    if(i == 1){
      namess[i] = ifelse(length((list_ff[[i]])[(ddx[[i]])]) > 0, (list_ff[[i]])[(ddx[[i]])],list_ff[[i]])  
    } else if(i > 1){
      namess[i] = ifelse(length((list_ff[[i-1]])[(ddx[[i-1]])]) > 0 , (list_ff[[i-1]])[(ddx[[i-1]])]+1,(list_ff[[i]])[(ddx[[i]])])
      if(is.na(namess[i])==TRUE){
        namess[i] = list_ff[[i]] 
      }
        
    }
      
  }
  
  return(namess)
}

tell_me_artists = function(name_codesx){
  name_codesx = name_codesx[!is.na(name_codesx)]
  name_codes = unlist(name_codesx)
  name_codes = as.numeric(na.omit(name_codes))
  gg = list()
  sxd =  diff(name_codes)
  ssdf = !is.na(match(sxd,1))
  nimo = grep("TRUE", ssdf)
  complete = list()
  complete = c(complete,name_codesx)
  if(length(nimo) > 0){
  
  for(i in 1:length(nimo)){
    gg[i] = list(c(nimo[i], nimo[i] + 1))
  }
  
  conn = list()
  complete = list()
  
  name1 = unique(name_codes[unlist(gg)])
  loners = ifelse(length(setdiff(name_codes, name1)) > 0, list(c(setdiff(name_codes, name1))),c(0))
  complete = c(complete, unlist(loners),list(name1))
  
  for (i in 1:(length(gg)-1)){
    if(length(gg) > 1) {
      conn[i] =  ifelse(length(intersect(gg[[i]],gg[[i+1]])) > 0, 1,0)
    }
    
  }
  cons = unlist(conn)
  
  
  if(length(cons) > 0 ){
    
    for(l in 1:length(cons)){
        if(cons[l] != 0){
        ppx = match(cons[l],1)
    
            for(i in 1:length(ppx)){
            #complet[i] = list(c((ppx[i]+i-1),(ppx[i]+i)))
            complete = c(complete,list(name_codes[unique(c(gg[[(ppx[i]+i-1)]], gg[[(ppx[i]+i)]]))]))
    
           }
        } else if(cons[l] == 0){
      
      #complete = c(complete,list(c(gg[[(i+i-1)]])))
      complete = c(complete,list(unlist(name_codes[gg[[(l)]]])))
      #complete = c(complete,list(unlist(name_codes[gg[[(i+i)]]])))
      
      if(l == length(cons)){complete = c(complete,list(unlist(name_codes[gg[[(l+1)]]]))) }
            }
  
        }
    
  
    names(complete)[2] = "comb"
  }
 
  }  
  
  return(complete)

  }

len = function(lista){
  xxl = length(lista)
  return(xxl)
}

ret_name = function(cd, splited){
  ss = splited[cd]
  return(ss)
}

name_them = function(codes, splited){
  if(length(grep("comb",names(codes)))>0){
    
    codes = codes[-grep("comb",names(codes))]
  }
   ppp =  lapply(codes, ret_name, splited)
   es = list()
   for(i in 1:length(ppp)){
     es[i] = paste(ppp[[i]], collapse="")
     
   }
   ddh = unlist(es)
   ddh = ddh[ddh !=""]
 return(ddh) 
}

#### TESTING THE CODE ####

ultimate_test = "Super Concaves Mr T"

list_ff = get_me_names(ultimate_test, splited)
name_codesx = get_me_codes(list_ff)
codes = tell_me_artists(name_codesx)
name_them(codes, splited)


final_data = data.table()

for(i in 86:dim(data3)[1]){

  tryCatch({
    
    drow = data3[i] 
    artn = drow[,art_name]
    
    list_ff = get_me_names(artn, splited)
    name_codesx = (get_me_codes(list_ff))
    codes = tell_me_artists(name_codesx)
    namesart = name_them(codes, splited)
    colab = ifelse(length(namesart)>1, "yes","no")
    
    for(j in 1:length(namesart)){
      cartist = namesart[j]
      
      datav = cbind(drow,cartist, colab)
      final_data = rbind(final_data, datav) 
    } 
    
    
    
    
    print(i)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  
}



toget = c("khaligraphjones", "kaka", "juliani")
analysisdata = final_data[cartist %in% toget]
analysisdata$cartist = as.factor(analysisdata$cartist)
analysisdata$colab= as.factor(analysisdata$colab)
analysisdata$l_score= log(analysisdata$mean_score)
analysisdata$age= round(difftime(Sys.Date(),unixtodate(analysisdata$d_posted), units = "days"),0)
analysisdata = analysisdata[analysisdata$l_score != '-Inf',]
analysisdata$colabdummy= ifelse(analysisdata$colab=="yes",1,0)
analysisdata$age = as.numeric(analysisdata$age)

out <- lm(l_score ~ age + cartist + colabdummy + 0, data=analysisdata)
summary(out)
coef <- coefficients(out)
coef

png(file="king.png",width=500,height=350,res=72)

plot(analysisdata$age, analysisdata$l_score, type = "n", xlab="Age of video (days)",ylab="Log of Score", main="#WhoIsKing")

for (cart in levels(analysisdata$cartist)) {
  k <- cart
  points( analysisdata$age[k], analysisdata$l_score[k],col=grep(cart, levels(analysisdata$cartist)) )
  predname <- paste("cartist", cart, sep="")
  abline(coef[predname], coef["age"], col=grep(cart, levels(analysisdata$cartist)))
  #legend("top", levels(analysisdata$cartist), pch = 1, title = "top")
  #legend("top", levels(analysisdata$cartist), pch = 1, col = grep(cart, levels(analysisdata$cartist)))
}

legend("topright", levels(analysisdata$cartist), lty=c(1,1,1), lwd=c(0.5,0.5,0.5),text.col = 1:length(levels(analysisdata$cartist)), text.font = 1:3 )

dev.off()

png(file="age_of_vids.png",width=500,height=350,res=72)
boxplot(age ~ cartist,data=analysisdata, main="#WhoIsKing",xlab="Artist", ylab="Age of videos (days)", col=rainbow(3))

dev.off()





