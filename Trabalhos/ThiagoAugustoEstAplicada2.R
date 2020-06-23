#Thiago Augusto - Aula 2 - Estatística Aplicada
dados=revisao_th_vg
#4
#H0: sigma1²=sigma²  H1: sigma1²!=sigma2² 
var.test(dados$CR~dados$mora.no.municipio,alt="two.sided",conf.level=0.95)
t.test(dados$CR~dados$mora.no.municipio,alt="greater",var.equal=T,conf.level=0.95)
t.test(dados$CR[dados$mora.no.municipio=="S"],dados$CR[dados$mora.no.municipio=="N"],alt="greater",var.equal = T,conf.level=0.95)
#6
#H0: Independentes  H1: Dependentes
chisq.test(dados$sexo,dados$turno.do.curso,correct=T)

#7
#Supondo igualdade de variancias
bartlett.test(dados$CR~dados$turno.do.curso)
oneway.test(dados$CR~dados$turno.do.curso,var.equal=F)
TukeyHSD(aov(dados$CR~dados$turno.do.curso))
PostHocTest(aov(dados$CR~dados$turno.do.curso),method="bonferroni")
plot(TukeyHSD(aov(dados$CR~dados$turno.do.curso)))


#Gráficos
turno.ord=factor(dados$turno.do.curso,levels=c("M","T","N"))
barplot(prop.table(table(turno.ord)), main= "Distribuição de Frequencia Turno", xlab="Turno", ylab="Frequência Relativa",col=c("Blue","red","yellow"))

boxplot(dados$CR~dados$mora.no.municipio,main="Cr por local de residência",xlab="Mora no monicípio",ylab="CR",col="yellow")
abline(h=mean(dados$CR[dados$mora.no.municipio=="S"]),col="red",lty=3,lwd=2)
abline(h=mean(dados$CR[dados$mora.no.municipio=="N"]),col="blue",lty=3,lwd=2)
#Tempo de lazer depois - CR
bartlett.test(dados$tempo.de.lazer.depois~dados$turno.do.curso)
oneway.test(dados$tempo.de.lazer.depois~dados$turno.do.curso,var.equal = T)
TukeyHSD(aov(dados$tempo.de.lazer.depois~dados$turno.do.curso))
plot(TukeyHSD(aov(dados$tempo.de.lazer.depois~dados$turno.do.curso)))
#Tempo de lazer antes - CR
bartlett.test(dados$tempo.de.lazer.antes~dados$turno.do.curso)
oneway.test(dados$tempo.de.lazer.antes~dados$turno.do.curso,var.equal = T)
TukeyHSD(aov(dados$tempo.de.lazer.antes~dados$turno.do.curso))
plot(TukeyHSD(aov(dados$tempo.de.lazer.antes~dados$turno.do.curso)))
