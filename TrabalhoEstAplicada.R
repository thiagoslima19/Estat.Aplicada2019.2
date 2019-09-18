#Thiago Augusto - TRABALHO DATASUS

install.packages("read.dbc")
require(read.dbc)
dados<-read.dbc("DNPE2017.dbc")
require(dplyr)
dados$IDADEMAE=as.numeric(as.character(dados$IDADEMAE))
dados1<-filter(dados,IDADEMAE>=18, IDADEMAE<=40, LOCNASC==1, IDANOMAL==2)
summary(dados)
dados1<-select(dados1,NUMERODN,CONSPRENAT, IDADEMAE,ESTCIVMAE,ESCMAE2010,RACACORMAE,TPROBSON,PARIDADE,GRAVIDEZ,SEMAGESTAC,KOTELCHUCK,TPAPRESENT,PARTO,SEXO,PESO,RACACOR )
dados1$TPROBSON=as.numeric(as.character(dados1$TPROBSON))
dados1$CONSPRENAT=as.numeric(as.character(dados1$CONSPRENAT))
dados1$PESO=as.numeric(as.character(dados1$PESO))
dados1$SEMAGESTAC=as.numeric(as.character(dados1$SEMAGESTAC))
dados1<-filter(dados1,CONSPRENAT!=99,ESTCIVMAE!=9,ESCMAE2010!=9,KOTELCHUCK!=9,TPAPRESENT!=9)
dados1<-na.omit(dados1)

dados1$ESTCIVMAE=factor(dados1$ESTCIVMAE,labels=c("Solteira","Casada","Viúva","Separada judicialmente","União Consensual"))
dados1$ESCMAE2010=factor(dados1$ESCMAE2010,labels=c("Sem Escolaridade","Fundamental 1","Fundamental 2","Médio","Superior Incompleto","Superior Completo"))
dados1$RACACORMAE=factor(dados1$RACACORMAE,labels=c("Branca","Preta","Amarela","Parda","Indígena"))
dados1$PARIDADE=factor(dados1$PARIDADE,labels=c("1ªGestação","Não é a 1ªGestação"))
dados1$GRAVIDEZ=factor(dados1$GRAVIDEZ,labels=c("Única","Dupla","Tripla ou mais"))
dados1$TPAPRESENT=factor(dados1$TPAPRESENT,labels=c("Cefálica","Pélvica ou Podálica","Transversa"))
dados1$PARTO=factor(dados1$PARTO,labels=c("Vaginal","Cesário"))
dados1$RACACOR=factor(dados1$RACACOR,labels=c("Branca","Negra","Amarela","Parda","Indígena"))
dados1$SEXO=factor(dados1$SEXO,labels=c("Homem","Mulher"))
dados1$KOTELCHUCK=factor(dados1$KOTELCHUCK,labels=c("Não fez Pré-Natal","Inadequado","Intermediário","Adequado","Mais que Adequado"))

attach(dados1)
#EXERCÍCIOS
#1

x=table(ESTCIVMAE)
table(ESCMAE2010,ESTCIVMAE)
RACACORMAE=ordered(RACACORMAE,levels=c("Amarela","Indígena","Preta","Branca","Parda"))
barplot(prop.table(table(RACACORMAE)),main="Proporção de Mães por Raça(ou Cor)",col=50)
#bebe
boxplot(PESO~RACACOR2,main="Peso dos Bebês em Relação a Cor(Ou Raça)",ylab="Peso em gramas",col=4)
summary(IDADEMAE)
hist(IDADEMAE,breaks=c(17,20,23,26,29,32,35,38,41),col=3,main="Histograma Idade das Mães",ylab="Frequência",xlab="Idade das Mães")
table(GRAVIDEZ)
summary(SEMAGESTAC)
summary(CONSPRENAT)
hist(PESO)
table(PARTO,TPAPRESENT)
table(PARTO)
pie(prop.table(table(PARTO)),main="Tipos de Partos")
write.csv(t,"TESTE.csv")
#2
require(DescTools)
#a)

t.test(SEMAGESTAC~PARTO,alt="two.sided",conf.level=0.95)
#b)
dados2=filter()
chisq.test(GRAVIDEZ,PARTO,correct = T)
#c)
dados1$IDADECAT=cut(IDADEMAE,breaks=c(17,20,34,41))
tabela =table(PARTO=="Cesário",IDADECAT)
tabela=tabela[2,]
tabela
chisq.test(tabela)
#d)


RACACOR3=as.character(dados1$RACACOR)
RACACOR4=NULL

for(i in 1:length(RACACOR3)){if(RACACOR3[i]!="Branca" && RACACOR3[i]!="Negra"){RACACOR4[i]="Outros"}else{RACACOR4[i]=RACACOR3[i]}}
dados1$RACACOR2=RACACOR4
dados1$RACACOR2=factor(RACACOR4,levels=c("Branca","Negra","Outros"))

LeveneTest(PESO~RACACOR2)
oneway.test(PESO~RACACOR2,dados1,var.equal=F)
x=aov(PESO~RACACOR2,dados1)
t=PostHocTest(x,method = "bonferroni")
plot(t)
x
summary(dados1)
sd(SEMAGESTAC)
sd(CONSPRENAT)
