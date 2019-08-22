#Aula 1 - Estatística Aplicada
#1-
dados=revisao_th_vg
x=ifelse(dados$`trabalha/estagio`== "N",1,0)
prop.test(sum(x),length(x),p=0.70,alt="two.sided",conf.level=0.95)
#Como p-valor é 0.1427 é maior que alfa, não rejeitamos H0.
#2
y=ifelse(dados$`mora no municipio`=="S",1,0)
prop.test(sum(y),length(y),p=0.75,alt="greater",conf.level=0.95)
#Como p-valor é 0.9396 é maior que alfa, não rejeitamos H0, ou seja não há evidências que é maior que 0.75
#3
qqnorm(dados$idade)
shapiro.test(dados$idade)
#Hipóteses:
# H0: u=21,5   H1: u!=21,5
t.test(dados$idade, mu=21.5 ,alt="two.sided" ,conf.level = 0.95)
#Como o p-valor é menor que alfa, rejeitamos H0, ou seja, há evidências que a idade média
#Extra1
# H0: u=7   H1: u>7
t.test(dados$CR,mu=7,alt="greater",conf.level = 0.95)
#Como p-valor > alfa, não rejeito H0.
#Extra2
#H0: u=8  H1: u<8
t.test(dados$`tempo de lazer depois`,mu=8,alt="less",conf.level=0.95)
#Como p-valor< alfa, rejeito H0.
#5
#H0: d=0  H1:d>0
d=dados$`tempo de lazer antes`-dados$`tempo de lazer depois`
t.test(d,mu=0,alt="greater",conf.level=0.95)
#Como p-valor < alfa, rejeitamos H0, ou seja, há evidências de que houve uma redução no tempo médio.

#Análise Exploratória de Dados
#Gráfico de Colunas
par(mfrow=c(2,2))
prop.table(table(dados$sexo))
barplot(prop.table(table(genero.ord)),main="Distrib. Freq. dos Sexos",xlab="Sexo", ylab="Frequência Relativa",ylim=c(0,0.7),col=c("blue","red"))
barplot(prop.table(table(dados$`trabalha/estagio`)), main= "Distribuição de Frequencia Trabalha/Estagia", xlab="Trabalha/Estagia", ylab="Frequência Relativa",col=c("Blue","red"))
turno.ord=factor(dados$`turno do curso`,levels=c("M","T","N"))
barplot(prop.table(table(turno.ord)), main= "Distribuição de Frequencia Turno", xlab="Turno", ylab="Frequência Relativa",col=c("Blue","red","yellow"))

#Gráfico de Setores
genero.ord=factor(dados$sexo,levels=c("M","F"))
pie(prop.table(table(genero.ord)),main="Distribuição de Frequências do Sexos", clockwise = T,col=c("yellow","green"),labels = paste(round(prop.table(table(dados$sexo)),digits=2)))
legend("topleft",legend=c("M","F"),fill=c("yellow","green"))

summary(dados)
