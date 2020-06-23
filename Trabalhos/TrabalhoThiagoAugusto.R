#Thiago Augusto - TRABALHO DATASUS

install.packages("read.dbc")
require(read.dbc)
dados<-read.dbc("DNPE2017.dbc")
require(dplyr)
dados$IDADEMAE=as.numeric(as.character(dados$IDADEMAE))
dados1<-filter(dados,IDADEMAE>=18, IDADEMAE<=40, LOCNASC==1, IDANOMAL==2)
summary(dados1)
dados1<-select(dados1,NUMERODN, IDADEMAE,ESTCIVMAE,ESCMAE2010,RACACORMAE,TPROBSON,PARIDADE,GRAVIDEZ,SEMAGESTAC,KOTELCHUCK,TPAPRESENT,PARTO,SEXO,PESO,RACACOR )
dados1$TPROBSON=as.numeric(as.character(dados1$TPROBSON))
dados1$PESO=as.numeric(as.character(dados1$PESO))
dados1$SEMAGESTAC=as.numeric(as.character(dados1$SEMAGESTAC))
dados1<-filter(dados1,ESTCIVMAE!=9,ESCMAE2010!=9,KOTELCHUCK!=9,TPAPRESENT!=9)
dados1<-na.omit(dados1)

dados1$ESTCIVMAE=factor(dados1$ESTCIVMAE,labels=c("Solteira","Casada","Viúva","Separada judicialmente","União Consensual"))
dados1$ESCMAE2010=factor(dados1$ESCMAE2010,labels=c("Sem Escolaridade","Fundamental 1","Fundamental 2","Médio","Superior Incompleto","Superior Completo"))
dados1$RACACORMAE=factor(dados1$RACACORMAE,labels=c("Branca","Preta","Amarela","Parda","Indígena"))
dados1$PARIDADE=factor(dados1$PARIDADE,labels=c("1ªGestação","Não é a 1ªGestação"))
dados1$GRAVIDEZ=factor(dados1$GRAVIDEZ,labels=c("Única","Dupla","Tripla ou mais"))
dados1$TPAPRESENT=factor(dados1$TPAPRESENT,labels=c("Cefálica","Pélvica ou Podálica","Transversa"))
dados1$PARTO=factor(dados1$PARTO,labels=c("Vaginal","Cesário"))
dados1$RACACOR=factor(dados1$RACACOR,labels=c("Branca","Preta","Amarela","Parda","Indígena"))
dados1$SEXO=factor(dados1$SEXO,labels=c("Homem","Mulher"))
dados1$KOTELCHUCK=factor(dados1$KOTELCHUCK,labels=c("Não fez Pré-Natal","Inadequado","Intermediário","Adequado","Mais que Adequado"))

attach(dados1)
#EXERCÍCIOS
#1
#Gráficos COR/RAÇA
table(ESTCIVMAE,RACACORMAE)
t=table(ESCMAE2010,ESTCIVMAE)
barplot(prop.table(table(ESCMAE2010)),main="Proporção de Mães por Raça(ou Cor)",col="yellow")

write.csv(t,"TESTE.csv")
#2
require(DescTools)
#a)

t.test(SEMAGESTAC~PARTO,alt="two.sided",conf.level=0.95)
#b)
chisq.test(GRAVIDEZ,PARTO,correct = T)
#c)
dados1$IDADECAT=cut(IDADEMAE,breaks=c(17,20,34,41))
tabela =table(PARTO=="Cesário",IDADECAT)
tabela=tabela[2,]
tabela
chisq.test(tabela)
#d)

ks.test(PESO[RACACOR=="Branca"],mean(PESO[RACACOR=="Branca"]),sd(PESO[RACACOR=="Branca"]))
ks.test()
dados1$RACACOR2=RACACOR
RACACOR2=for(i in 1:length(RACACOR)){if(RACACOR=="Amarela"||RACACOR=="Parda"||RACACOR=="Indígena"){RACACOR[i]="Outros"}}
  
LeveneTest(PESO~RACACOR)
oneway.test(PESO~RACACOR,dados1,var.equal=F)
x=aov(PESO~RACACOR,dados1)
summary(dados1)
PostHocTest(x,method = "bonferroni")
t=PostHocTest(x,method = "bonferroni")
plot(t)
