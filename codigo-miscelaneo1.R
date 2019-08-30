notas <- list(aluno_1 = c(7.1, 3.2, NA),
              aluno_2 = c(2.7, 8.8, 10.0),
              aluno_3 = c(0.0, NA, NA),
              aluno_4 = c(7.7, 8.4, 6.3),
              aluno_5 = c(3.6, 6.6, 8.1),
              aluno_6 = c(NA, NA, NA),
              aluno_7 = c(7.4, 7.1, 7.3),
              aluno_8 = c(10.0, NA, 7.0),
              aluno_9 = c(1.6, 3.2, 5.3),
              aluno_10 = c(8.8, 9.2, 8.0))

#a)
status <- c("F","A","REP","A","F","REP","A","F","REP","A")

#b)
alunos <- paste("Aluno",1:10,sep = "_")

#c)
historico <- data.frame(Nomes=alunos,Notas=I(notas),Status=status)

#d)
aprovados <- historico[historico$Status== "A",]
reprovados <- historico[historico$Status=="REP",]
final <- historico[historico$Status=="F",]

#e)
bons_alunos <- rbind(aprovados,final)

#f)
rownames(historico) <- paste("Id",1:nrow(historico),sep="_")

#g)
lapply(historico$Notas,FUN = is.na)
lapply(lapply(historico$Notas,FUN=is.na),FUN=sum)
situacao <- unlist(lapply(lapply(historico$Notas,FUN=is.na),FUN=sum))
historico_na <- historico[situacao !=0,]

#h)
situacao <- unlist(lapply(lapply(historico$Notas,FUN=is.na),FUN=sum))
media <- historico[situacao==0,]
media
medasnotas <- unlist(lapply(media$Notas,FUN =  mean))
medasnotas

media$Medias <- medasnotas
media
view(media)


triang <- function(num = 5){
  for(i in num:1){
    for(j in 1:i){
      if((j%%2) == 1)
        cat("*")
      else
        cat("-")
    }
    cat("\n")
  }
}


aprox_pi <- function(n = 100){
  soma_1 = 0
  soma_2 = 0
  for(i in 1:n){
    if((i%%2) == 1)
      soma_1 = soma_1 + 4/(2*i - 1)
    else
      soma_2 = soma_2 - 4/(2*i - 1)
  }
  soma = soma_1 + soma_2
  soma
}
aprox_pi(1e6)

x= 0

aprox_e <- function(x = 1, n = 100){
  
  exponencial = 1
  for(i in 1:n){
    exponencial = exponencial + (x^i)/factorial(i)
  }
  exponencial
}

aprox_e(,9)


n <- 7

for(i in 1:n){
  for(j in 1:n){
    if((i+j)<=n)
      cat(" ")
    else
      cat("*")
    }
  cat("\n")
}


sorteio <- function(n = 1, N = 1e3){
  retiradas <- numeric(N)
  for(i in 1:N){
    repeat{
      bola <- sample(x = 1:6, size = 3)
      retiradas[i] <- retiradas[i] + 1
      if(bola == n)
        break
    }
  }
  mean(retiradas)
}

sorteio(7)


vetor <- numeric(1e3)
set.seed(0)
for(i in 1:1e3){
  vetor[i] <- sum(sample(1:100, size = 2, replace = T))
}

hist(vetor)


