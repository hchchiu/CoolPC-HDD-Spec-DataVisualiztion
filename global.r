library(RMySQL)

#con = dbConnect(dbDriver("MySQL"), user='chiu', password='123', dbname='coolpc', host='140.138.152.145',port=3306)
con = dbConnect(dbDriver("MySQL"), user='chiu', password='123', dbname='coolpc', host='localhost')
dbGetQuery(con, "show processlist")

###get hdd_coolpc###
sql = paste("SELECT * FROM `hdd_coolpc`",sep="")
dbSendQuery(con,"SET NAMES utf8")
res = dbSendQuery(con, sql)
hddcoolpc = fetch(res, n = -1)

###get toshiba pchome
sql = paste("SELECT * FROM `toshiba_hdd_pchome`",sep="")
res = dbSendQuery(con, sql)
toshiba = fetch(res, n = -1)

###get WD pchome
sql = paste("SELECT * FROM `wd_hdd_pchome`",sep="")
res = dbSendQuery(con, sql)
WD = fetch(res, n = -1)

###get seagate pchome
sql = paste("SELECT * FROM `seagate_hdd_pchome2`",sep="")
res = dbSendQuery(con, sql)
seagate = fetch(res, n = -1)

###get coolpclocation
sql = paste("SELECT * FROM `coolpclocation`",sep="")
res = dbSendQuery(con, sql)
coolpclocation = fetch(res, n = -1)

###get coolpc month 11
sql = paste("SELECT * FROM `hdd_11_18r`",sep="")
res = dbSendQuery(con, sql)
coolpc_11 = fetch(res, n = -1)
coolpc_11=coolpc_11[,c(6,8)]
coolpc_11=coolpc_11[!duplicated(coolpc_11[,2]),]#delet dup model
colnames(coolpc_11)=c('Price11','Model')

###get coolpc month 12
sql = paste("SELECT * FROM `hdd_12_17r`",sep="")
res = dbSendQuery(con, sql)
coolpc_12 = fetch(res, n = -1)
coolpc_12=coolpc_12[,c(6,8)]
coolpc_12=coolpc_12[!duplicated(coolpc_12[,2]),]#delet dup model
colnames(coolpc_12)=c('Price12','Model')

###get coolpc month 1
sql = paste("SELECT * FROM `hdd_01_30r`",sep="")
res = dbSendQuery(con, sql)
coolpc_1 = fetch(res, n = -1)
coolpc_1=coolpc_1[,c(6,8)]
coolpc_1=coolpc_1[!duplicated(coolpc_1[,2]),]#delet dup model
colnames(coolpc_1)=c('Price1','Model')

###get coolpc month 2
sql = paste("SELECT * FROM `hdd_02_17r`",sep="")
res = dbSendQuery(con, sql)
coolpc_2 = fetch(res, n = -1)
coolpc_2=coolpc_2[,c(6,8)]
coolpc_2=coolpc_2[!duplicated(coolpc_2[,2]),]#delet dup model
colnames(coolpc_2)=c('Price2','Model')

###get coolpc month 3
sql = paste("SELECT * FROM `hdd_03_17r`",sep="")
res = dbSendQuery(con, sql)
coolpc_3 = fetch(res, n = -1)
coolpc_3=coolpc_3[,c(6,8)]
coolpc_3=coolpc_3[!duplicated(coolpc_3[,2]),]#delet dup model
colnames(coolpc_3)=c('Price3','Model')

###disconnect mysql
dbClearResult(dbListResults(con)[[1]])
dbDisconnect(con)


######original read file#####
# hddcoolpc=read.csv("hdd_coolpc.csv",header = T)
# toshiba=read.csv("toshiba_hdd_pchome.csv",header = T)
# WD=read.csv("WD_hdd_pchome.csv",header = T)
# seagate=read.csv("seagate_hdd_pchome2.csv",header = T)
# coolpclocation=read.csv("coolpclocation.csv",header=T)
############################

# ts=unique(seagate$Model)
# ts
# #write.csv(seagate,file="seagate_hdd_pchome2.csv",row.names=FALSE)
# length(unique(seagate$Model))
# check=duplicated(WD$Model)
# check[1:69]
# WD[duplicated(WD$Model)==TRUE,] 


tomerge=toshiba
tomerge=rbind(tomerge,WD)
tomerge=rbind(tomerge,seagate)
#merge different data
allstorage=merge(x=hddcoolpc,y=tomerge, by="Model",all.x=TRUE)
allstorage=merge(x=allstorage,y=coolpc_11, by="Model",all.x=TRUE)
allstorage=merge(x=allstorage,y=coolpc_12, by="Model",all.x=TRUE)
allstorage=merge(x=allstorage,y=coolpc_1, by="Model",all.x=TRUE)
allstorage=merge(x=allstorage,y=coolpc_2, by="Model",all.x=TRUE)
allstorage=merge(x=allstorage,y=coolpc_3, by="Model",all.x=TRUE)
#####################

#transfer to numeric
mode(allstorage[1,4])
allstorage[,4]=as.numeric(allstorage[,4]) #Size
allstorage[,5]=as.numeric(allstorage[,5]) #RPM
allstorage[,7]=as.numeric(allstorage[,7]) #Price.x
allstorage[,8]=as.numeric(allstorage[,8]) #Warranty
allstorage[,9]=as.numeric(allstorage[,9]) #Price.y


coolpclocation[,1]=as.numeric(coolpclocation[,1])
coolpclocation[,2]=as.numeric(coolpclocation[,2])
####################

#tranfer TB to GB
for(i in 1:nrow(allstorage)){
  if(allstorage[i,4]<1){
    allstorage[i,4]=allstorage[i,4]*1000
  }
}

#replace na with 0
allstorage[is.na(allstorage$Price.y),9]=0
allstorage[is.na(allstorage$Price11),10]=0
allstorage[is.na(allstorage$Price12),11]=0
allstorage[is.na(allstorage$Price1),12]=0
allstorage[is.na(allstorage$Price2),13]=0
allstorage[is.na(allstorage$Price3),14]=0

######### debug area start ############
#allstorage=as.data.frame(allstorage)
# inputbrand=c("WD","Toshiba","Seagate")
# chooseprice=allstorage[allstorage$Brand==inputbrand,7]
# chooseprice=sort(chooseprice)
# chooseprice
# length(unique(test$Model))
# dup=duplicated(test$Model)
# dup[1:108]

# ck=duplicated(allstorage$Model)
# allstorage[duplicated(allstorage$Model),]


######### debug area end ##############