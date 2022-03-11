nba<-read.csv("getting_there2.csv")
college<-read.csv("college.csv")
college %>%
  mutate(X3PAr=(X3PA/FGA))->college
college  %>%
  filter(Year<2016,Pk<31)->college
college %>%
  select(Name,TRB,AST,STL,BLK,TOV,SOS,X3PAr)->college1
colnames(college1)<-c("Name","NCAA_TRB","NCAA_AST","NCAA_STL","NCAA_BLK","NCAA_TOV","NCAA_SOS","NCAA_3PAr")

getting_there<-merge(nba,college1,by.x = "player",by.y = "Name",all.x = T)

a<-filter(gt,player==("Luol Deng"))
b<-filter(gt,player==("Mario Hezonja"))
c<-filter(gt,player==("Tony Snell"))
d<-filter(gt,player==("Miles Plumlee"))
lost_players<-rbind(a,b,c,d)
getting_there2<-rbind.fill(getting_there,lost_players)
getting_there2<-getting_there2[,-c(46,47,48)]

write_csv(getting_there2,"nbaproject2.csv")
value<-c()
mxvalue<-c()
sdval<-c()
for (i in 1:30){
  value<-c(value,mean(nba$ws[nba$Pick==i])*1.681379)
  mxvalue<-c(mxvalue,max(nba$ws[nba$Pick==i])*.4230118)
  sdval<-c(sdval,sd(nba$ws[nba$Pick==i]))
}

mean(nba$ws[nba$Pick==1])
