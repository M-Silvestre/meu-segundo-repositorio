tabuada <- function(num = 1, inicio = 1, fim = 10){
  i = 1
  x = NULL
  y = inicio
  
  while(y <= fim){
    x[i] =  num*y
    cat(" ",num,"*",y,"=",x[i],"\n",sep = " ")
    y = inicio + i
    i = i + 1
  }
  x
}

length(tabuada(5,1,10))

vetor <- NULL
vetor[1] <- 5*6


caso <- "Arredondar"

vetor <- runif(10,0,5);

vetor

switch (caso,
        "Tamanho" = length(vetor),
        "Média" = mean(vetor),
        "Min" = min(vetor),
        "Max" = max(vetor),
        "Arredondar" = round(vetor, 1)
)



caso <- "MAX"

vetor <- runif(10,0,5)

vetor

switch (tolower(caso),
  "tamanho" = length(vetor),
  "média" = mean(vetor),
  "min" = min(vetor),
  "max" = max(vetor)
)



operacao <- 3

x <- 0:10

switch (EXPR = operacao,
        mean(x),
        range(x),
        length(x)
)




alunos <- paste("Aluno", 1:5, sep = "_")

avaliacoes <- list(c(6.5, 7.2, 8.1), c(7.7, 5.6, 6.9), c(4.3, 5.5, 8.3, 6.0),
                   c(9.5, 7.2, 6.0), c(8.0, 9.5))

df <- data.frame(aluno = alunos, notas = NA)

df$notas <- avaliacoes

df$media <- I(lapply(df$notas,FUN = mean))

df <- df[,-3]



pi <- 1
for(i in 1:1e3){
  pi <- pi + (-1^i)/(2*i + 1)
}
pi
