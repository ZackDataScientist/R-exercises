dados <- data.frame(read.csv("C:\\Dados\\Daytrade\\Macro_Watchlist_MACROEXCEL.csv", stringsAsFactors = FALSE))

dados

# Função para limpar e converter variação para numérico
limpar_variacao <- function(x) {
  x <- gsub("%", "", x)  # Remove o símbolo "%"
  x <- gsub(",", ".", x) # Substitui a vírgula por ponto (caso necessário)
  x <- trimws(x)         # Remove espaços extras
  x <- as.numeric(x)     # Converte para numérico
  return(x)
}

# Dados de 'risk' e 'safety'
# Para o exemplo, usarei o conteúdo dos dados fornecidos
# O DataFrame 'dados' deve já estar carregado no ambiente com as colunas "Nome", "Códigos", e "Var."

# Dividindo os dados em dois DataFrames (risk e safety)
risk <- dados[1:18, ]
safety <- dados[19:27, ]

# Limpando e convertendo as variações nos dois DataFrames
risk$Var. <- limpar_variacao(risk$Var.)
safety$Var. <- limpar_variacao(safety$Var.)

# Inicializando as variáveis de contagem
alta <- 0
queda <- 0
neutro <- 0

# Analisando os ativos de risco (1 a 18) - a lógica é inversa para 'risk'
for (i in 1:nrow(risk)) {
  if (risk$Var.[i] > 0.30) {
    alta <- alta + 1
  } else if (risk$Var.[i] < -0.30) {
    queda <- queda + 1
  } else {
    neutro <- neutro + 1
  }
}

# Analisando os ativos de segurança (19 a 27)
for (i in 1:nrow(safety)) {
  if (safety$Var.[i] > 0.30) {
    alta <- alta + 1
  } else if (safety$Var.[i] < -0.30) {
    queda <- queda + 1
  } else {
    neutro <- neutro + 1
  }
}

# Resultado final
cat("Risco (Queda):", queda, "\n")
cat("Estabilidade (Alta):", alta, "\n")
cat("Neutro:", neutro, "\n")
