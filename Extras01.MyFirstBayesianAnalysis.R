# Modelos Hierárquicos e Bayesianos aplicados à Ecologia
# Cristian S. Dambros - csdambros@gmail.com


# Meu primeiro modelo bayesiano
# 

# O intuito deste script é mostrar como um modelo bayesiano pode ser ridiculamente simples (estatistica bayesiana não é o mesmo que analise complexa!)
# 
# 
# Vamos criar uma sequência de valores quaisquer:
# 
meuVec<-c(10,15,12,14.2,16,11,13.5)

meuVec


# Este vetor poderia representar qualquer medida obtida na natureza, como tamanho corpóreo de indivíduos, número de espécies em fragmentos, etc.


# Vamos simplesmente calcular a média dos valores neste vetor:
mean(meuVec)


# Poderíamos utilizar uma análise estatística para testar se esta média é significativamente diferente de zero (ou de qualquer outro valor):

# o valor médio de meuVec é significativamente diferente de zero?

# Usando "regressão"
M1<-lm(meuVec~1)
summary(M1)

# Usando "anova"
M2<-aov(meuVec~0)
M3<-aov(meuVec~1)

anova(M2,M3)

# Usando teste t
t.test(meuVec)

# Resposta: Sim!Temos evidência que estes dados seriam bem improváveis de serem observados se a média fosse zero.


# Bom, enquanto podemos ver que a média é significativamente diferente de zero, isso não é muito útil neste caso, pois é óbvio que é diferente de zero (ex. não tem como o peso dos animais ter média zero). Provavelmente seria mais interessante perguntarmos, qual a probabilidade da média da população ser de fato 13.1 ou qualquer outro valor. Ou então se é possível que a média da população  seja de fato 10 ou algum outro valor de interesse.

# Neste caso, não estaríamos interessados em saber a probabilidade de observar estes dados se a média da população fosse 0, mas sim a probabilidade da média ser zero (ou algum outro valor) considerando estes dados. Em estatística Bayesiana é exatamente isto que calculamos:

# Vamos carregar o pacote rjags
library(rjags)

# Agora vamos utilizar um modelo que calcula a média de um grupo de valores usando estatística bayesiana:

# Cria uma pasta para armazenar o modelo.
dir.create("JAGS/",showWarnings = FALSE)

# Baixa o modelo
download.file("https://raw.githubusercontent.com/csdambros/bayesian-examples/master/JAGS/MeuPrimeiroModelo1.jags","JAGS/MeuPrimeiroModelo1.jags")

##** Veja como criar o modelo abaixo

# Executa o modelo
MJ1<-jags.model("JAGS/MeuPrimeiroModelo1.jags",data = list(meuVec=meuVec))

MJ1.1<-coda.samples(MJ1,variable.names = c("media","sd"),n.iter = 10000)


MJ1.1mat<-as.matrix(MJ1.1)

plot(density(MJ1.1mat[,1]))

# Qual a probabilidade da média ser exatamente zero?
mean(MJ1.1mat[,1]==0)

# Qual a probabilidade da média ser menor que zero?
mean(MJ1.1mat[,1]<0)

# Qual a probabilidade da média ser menor que 10?
mean(MJ1.1mat[,1]<10)

# Qual a probabilidade da média ser maior que 10?
mean(MJ1.1mat[,1]>10)

# Qual a probabilidade da média ser exatamente 13.1?
mean(MJ1.1mat[,1]==13.1)

# Qual a probabilidade da média ser algo entre 10 e 20?
mean(MJ1.1mat[,1]>10&MJ1.1mat[,1]<20)

# Qual a probabilidade da média ser algo entre 15 e 17?
mean(MJ1.1mat[,1]>15&MJ1.1mat[,1]<17)


# Tanto na estatística convencional quanto Bayesiana iríamos concluir que a média de tamanho não é zero. Na estatística frequentista (anova, regressão e teste t acima), concluiríamos que seria muito difícil observar estes dados se a média fosse zero. Já no modelo bayesiano concluiríamos que a probabilidade da média ser zero considerando estes dados é ridiculamente baixa. Ou seja, concluiríamos que a média destes valores difere de zero.

# Entretanto, vamos pensar em algumas situações onde queremos informações mais interessantes do que afirmar que a média é zero ou não:

# Situação 1:
# Imagine a seguinte situação. Você possui um barco pesqueiro e é extremamente custoso ir pescar em alto mar. Entretanto, se o peso médio dos peixes da área for maior do que 15, então valerá a pena ir pescar em alto mar. Qual informação seria mais relevante para tomar esta decisão, a da estatística convencional ou do modelo bayesiano.

# Situação 2:
# Você precisa conservar uma espécie de mamífero de grande porte. Você está avaliando se um tipo florestal mantém abundância de mamífero acima de 15 indivíduos, o mínimo aceitável para a conservação desta espécie.

# Situação 3:
# Em um estudo prévio, pesquisadores afirmaram que a riqueza de espécies de um taxon em uma tipo de ambiente é gira torno de 13 espécies. Com base nos dados, é plausível afirmar que o número médio de espécies neste ambiente é em torno de 13 espécies? Qual método permitiria afirmar isso?


