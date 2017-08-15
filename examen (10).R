rm(list=(ls()))
wd<-"~/Documents/WD"
setwd(wd)
#########DESCARGAR CENSO (2010) Y ENCUESTA INTERCENSAL (2015)
url<-c("http://www.beta.inegi.org.mx/contenidos/proyectos/ccpv/2010/microdatos/ageb_y_manzana/resageburb_09_2010_dbf.zip", 
       "http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/intercensal/2015/microdatos/eic2015_09_csv.zip")
temp<-c(tempfile(), tempfile())
lapply(1:length(url), function(x) download.file(url[x], temp[[x]], mode="wb"))
lapply(temp, unzip)
library(foreign)
ccpv<-read.dbf("RESAGEBURB_09DBF10.dbf")
ein15<-read.csv("TR_PERSONA09.CSV")

##########PROCESAR CENSO 2010############
ids<-c("ENTIDAD", "MUN", "LOC", "AGEB")
edades<-c("P_0A2", "P_3A5", "P_6A11", "P_12A14", "P_15A17", "P_18A24")
#Datos ageb
ccpv<-ccpv[ccpv$AGEB!="0000",]
ccpv<-ccpv[ccpv$NOM_LOC=="Total AGEB urbana",]
#Delegacion Alvaro Obregon
ccpv$NOM_MUN<-iconv(as.character(ccpv$NOM_MUN), 'utf-8', 'ascii', sub="")
ccpv<-ccpv[ccpv$NOM_MUN=="lvaro Obregn",]

#Cambiar variables a class adecuada
ccpv<-cbind(ccpv[, ids], ccpv[edades], ccpv[, "POBTOT"])
ccpv[, ids]<-apply(ccpv[, ids], 2, as.character)
ccpv[,edades]<-apply(ccpv[,edades], 2, function(x) as.numeric(as.character(x)))
names(ccpv)[names(ccpv)=="ccpv[, \"POBTOT\"]"]<-"POBTOT"
ccpv$POBTOT<-as.numeric(as.character(ccpv$POBTOT))

#Suma acumulada de grupos de edad
foo<-apply(t(ccpv[, edades]), 2, cumsum)
#Prueba de interpolacion
plot(c(2, 5, 11, 14, 17, 24),  foo[,1], col="green")
lines(spline(c(2, 5, 11, 14, 17, 24), foo[,1], method="hyman"), col="red")
# interpolar suma acumulada de 0 a 0.5 (0.5 anios=6 meses)
#ref: https://www.researchgate.net/publication/26438106_Spline_Interpolation_for_Demographic_Variables_The_Monotonicity_Problem
ccpv$P_0A6m<-round(apply(foo, 2, function(x) spline(c(2, 5, 11, 14, 17, 24), x, method="hyman", xout=0.5)$y))

##########PROCESAR INTERCENSAL 2015##########
#Delegacion Alvaro Obregon
ein15$NOM_MUN<-iconv(as.character(ein15$NOM_MUN), 'utf-8', 'ascii', sub="")
ein15<-ein15[ein15$NOM_MUN=="lvaro Obregn",]

#recodificar edad
library(car)
ein15$EDAD<-recode(ein15$EDAD, "0:2=1; 3:5=2; 6:11=3; 12:14=4; 15:17=5; 18:24=6; 25:999=7")

#colapsar factor de expansion por edad, para obtener los mismos cortes que en la interpolacion 2010 
#Solo podemos sacar resultado agregado porque la intercensal no es representativa por ageb
gr<-aggregate(FACTOR~EDAD, data=ein15, sum)
gr$FACTOR<-cumsum(gr$FACTOR)
sum(ein15$FACTOR)

##########ESTIMACION############
plot(c(2, 5, 11, 14, 17, 24), gr$FACTOR[1:6])
lines(spline(c(2, 5, 11, 14, 17, 24), gr$FACTOR[1:6], method = "hyman"))

#obtener interpolacion 2015
bbs15<-round(spline(c(2, 5, 11, 14, 17, 24), gr$FACTOR[1:6], method = "hyman", xout=0.5)$y)

#dato censal agregado 2010
bbs10<-sum(ccpv$P_0A6m, na.rm = T)

#comparar metodos log y geometrico para estimar el factor correspondiente a 2017
#REF
factor17.log<-(bbs15/bbs10)^((2017-2015)/(2015-2010))
r<-(bbs15/bbs10)^(1/(2015-2010))-1
factor17.geom<-(1+r)^(2017-2015)
#media (aunque son iguales)
factor.17<-mean(factor17.log, factor17.geom)

#Estimacion final 2017. SUPUESTO: EL FACTOR DE 2017 AGREGADO APLICA PARA EL DATO POR AGEB.Las agebs no son muy
#diferentes de la grand mean. 
out<-cbind(ccpv[ids],  ccpv[, "P_0A6m"], round(ccpv[, "P_0A6m"]*factor.17))
names(out)<-c(ids, "pob.0a6meses.2010", "pob.esp.0a6meses.2017")
summary(out)
out$pob.0a6meses.2010[out$pob.0a6meses.2010<0] <- NA
out$pob.esp.0a6meses.2017[out$pob.esp.0a6meses.2017<0] <- NA

plot(out$pob.0a6meses.2010, out$pob.esp.0a6meses.2017,
main="Comparacion estimacion 2017 v. Censo 2010",
xlab="Estimacion Censo 2010", ylab="Estimacion 2017", sub="linea=x=y")
lines(0:250, 0:250, col="red")
View(out)

###################pregunta 2a###################
rm(list=(ls()))
wd<-"~/Documents/WD"
#tmp<-list(tempfile(), tempfile(), tempfile())
setwd(wd)
#lapply(c(1:length(tmp))+3, function(x) download.file(paste0("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2017-0", x, ".csv"), tmp[[x-3]], mode="wb"))
#data<-lapply(tmp, read.csv)

#saveRDS(do.call(rbind, data), "ecobici")
#data<-do.call(rbind, data)
data<-readRDS("ecobici")
head(data)
data[, grep("Fecha", names(data))]<-apply(data[, grep("Fecha", names(data))], 2, function(x) as.POSIXlt(as.character(x), format="%d/%m/%Y") )

data[, grep("Hora", names(data))]<-apply(data[, grep("Hora", names(data))], 2, function(x) as.POSIXlt(as.character(x), format="%H:%M:%S") )
data<-data[format(data$Fecha_Arribo, '%m') %in% c("04", "05", "06"),]
data<-data[format(data$Fecha_Retiro, '%m') %in% c("04", "05", "06"),]
##########1. en que horarios y estaciones hay mas afluencia##########
horas<-aggregate(rep(1, nrow(data)), by=list(format(data$Fecha_Retiro, '%m'), format(data$Hora_Retiro, '%H')), sum)
names(horas)<-c("mes", "hora", "freq")


library(ggplot2)
ggplot(data=horas, aes(x=hora, y=freq))+
  geom_line(data=horas, aes(x=hora, y=freq, group=mes, color=mes))+
  ggtitle("Numero de viajes en Ecobici por hora y mes")+
  xlab("Hora de Retiro de Ecobici")+
  ylab("Numero de viajes")
dev.copy(png, "horas.png")
dev.off()
estaciones<-aggregate(rep(1, nrow(data)), by=list(data$Ciclo_Estacion_Retiro), sum)
names(estaciones)<-c("Estacion", "freq")
estaciones<-estaciones[order(estaciones$freq, decreasing = T),]
estaciones

######analisis temporal##################

dias<-aggregate(rep(1, nrow(data)), by=list(format(data$Fecha_Retiro, "%m-%d"), data$Ciclo_Estacion_Retiro), sum)
names(dias)<-c("dia", "estacion", "viajes")
dias$dia_num<-as.numeric(factor(dias$dia, unique(dias$dia)))

fines<-seq(12, 91, 7)
fines<-c(fines, fines+1, 5, 6)
dias$fin<-1
dias$fin[dias$dia_num %in% fines]<-2
dias$fin<-factor(dias$fin, levels=c("no fin", "fin"))

#prueba<-lm(viajes~dia_num, data=dias)$coefficients["dia_num"]
cor<-unlist(lapply(split(dias, dias$estacion), function(x) tryCatch(lm(viajes~dia_num+fin, data=x)$coefficients["dia_num"], error=function(e) lm(viajes~dia_num, data=x)$coefficients["dia_num"])))
a.la.alza<-cor[cor>quantile(cor, 0.75, na.rm = T)]
foo<-as.numeric(gsub(".dia_num", "", names(a.la.alza)))
dias$alza<-"75% menos a la alza"
dias$alza[foo]<-"25% mas a la alza"

ggplot(data=dias, aes(x=dia, y=viajes))+
  geom_line(data=dias[dias$alza=="75% menos a la alza",], aes(x=dia, y=viajes, group=estacion, color=alza), alpha=1/10)+
  geom_line(data=dias[dias$alza=="25% mas a la alza",], aes(x=dia, y=viajes, group=alza, color=alza), alpha=1)+
  ggtitle("Numero de viajes en Ecobici por estacion y dia")+
  xlab("Dia de Retiro de Ecobici")+
  ylab("Numero de viajes")+
  theme(axis.text.x = element_text(colour="grey20",size=5,angle=90,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=5,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=5,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=5,angle=90,hjust=.5,vjust=.5,face="plain"))
dev.copy(png, "a.la.alza.png")
dev.off()

########relaciones entre estaciones
library(reshape2)
est<-aggregate(rep(1, nrow(data)), by=list(data$Ciclo_Estacion_Arribo, data$Ciclo_Estacion_Retiro), sum)
names(est)<-c("arribo", "retiro", "freq")
summary(est)
est<-est[est$arribo<999,]
est<-est[est$retiro<999,]
est$freq<-est$freq
ggplot(data = est, aes(x=arribo, y=retiro, fill=freq)) + 
  geom_tile()+ggtitle("Numero de viajes en Ecobici por par de estaciones. Desviaciones estandar")
dev.copy(png, "heat.png")
dev.off()

#######clusters#######
for.clust<-aggregate(rep(1, nrow(data)), by=list(data$Ciclo_Estacion_Retiro), sum)
data$dia_num<-as.numeric(factor(format(data$Fecha_Retiro, "%m-%d"), unique(format(data$Fecha_Retiro, "%m-%d"))))
fines<-seq(12, 91, 7)
fines<-c(fines, fines+1, 5, 6)
data$fin<-0
data$fin[data$dia_num %in% fines]<-1
summary(data$fin)
for.clust.fines<-aggregate(data$fin, by=list(data$Ciclo_Estacion_Retiro), mean)
data$dura<-as.numeric(data$Hora_Arribo-data$Hora_Retiro)
for.clust.dura<-aggregate(data$dura, by=list(data$Ciclo_Estacion_Retiro), mean)
data$hora_ret_num<-as.numeric((format(data$Hora_Retiro, '%H')))
data$hora_ret_num[data$hora_ret_num==0]<-24
for.clust.hora<-aggregate(data$hora_ret_num, by=list(data$Ciclo_Estacion_Retiro), mean)
for.clust.edad<-aggregate(data$Edad_Usuario, by=list(data$Ciclo_Estacion_Retiro), mean)

for.clust<-merge(for.clust, for.clust.dura, by="Group.1")
for.clust<-merge(for.clust, for.clust.edad, by="Group.1")
for.clust<-merge(for.clust, for.clust.fines, by="Group.1")
names(for.clust)<-c("stations.id", "viajes", "duracion.prom", "edad.prom", "fines.sh")
#estandarizar
for.clust[,-1]<-apply(for.clust[,-1], 2, scale)

dist<-dist(for.clust)
clust<-hclust(dist, method="complete" )
plot(clust)
clust <- cutree(clust, 6)
clusters<-cbind(clust, for.clust)
table(clusters$clust)
sum<-aggregate(cbind(clusters$viajes, clusters$duracion.prom, clusters$edad.prom, clusters$fines.sh), by=list(clusters$clust), mean)
names(sum)<-c("cluster","viajes", "duracion.prom", "edad.prom", "fines.sh")

#bonus 
library(jsonlite)

data.estaciones<-as.data.frame(fromJSON("estaciones.json"))
loc<-cbind(data.estaciones$stations.location, data.estaciones$stations.id)
names(loc)<-c("lat", "lon", "stations.id")

clusters<-merge(data.estaciones, clusters, by="stations.id")
clusters<-cbind(clusters$stations.location, clusters)

ggplot(data=clusters, aes(x=lon, y=lat))+
  geom_point(data=clusters, aes(x=lon, y=lat, size=viajes))
dev.copy(png, "mapa_clust.png")
dev.off()

ggplot(data=clusters, aes(x=lon, y=lat))+
  geom_point(data=clusters, aes(x=lon, y=lat, size=duracion.prom))
dev.copy(png, "mapa_dura.png")


ggplot(data=clusters, aes(x=lon, y=lat))+
  geom_point(data=clusters, aes(x=lon, y=lat, size=viajes))
dev.copy(png, "mapa_viajes.png")