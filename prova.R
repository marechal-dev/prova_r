# Primeiro, definimos o espaço de trabalho
workingDirectory = getwd()
setwd(workingDirectory)

# Após, realizamos a leitura do banco de dados
zCityData = read.table("TRABALHO2.txt", header = TRUE)

# Questão 1:
# Separamos os dados da variável desejada (Preço Comercializado)
commercializedPriceData = zCityData$prc

# Realizamos ordenação dos dados
sort(commercializedPriceData)

# Armazenamos o resumo dos dados
dataSummary = summary(commercializedPriceData)

# Retiramos do resumo:
# O primeiro quartil;
# O terceiro quartil;
# A mediana;
# A média aritmética.
firstQuartile = as.numeric(dataSummary[2])
firstQuartile

thirdQuartile = as.numeric(dataSummary[5])
thirdQuartile

dataMedian = as.numeric(dataSummary[3])
dataMedian

dataMean = as.numeric(dataSummary[4])
dataMean

# Calculamos o Desvio Padrão
standardDeviation = sd(commercializedPriceData)
standardDeviation

# Calculamos o coeficiente de assimetria de Person
pearsonAsymmetryCoefficient = 3 * (dataMean - dataMedian) / standardDeviation
pearsonAsymmetryCoefficient

# Calculamos o décimo e o nonagésimo percentil
dataPercentiles = quantile(commercializedPriceData, seq(.1, .9, by = .1))
tenthPercentile = as.numeric(dataPercentiles[1])
ninetiethPercentile = as.numeric(dataPercentiles[9])

# Calculamos a curtose
kurtosis = (thirdQuartile - firstQuartile) / (2 * (ninetiethPercentile - tenthPercentile))
kurtosis

# Geramos o boxplot!
boxplotStats = boxplot.stats(commercializedPriceData)
outliers = sort(boxplotStats$out)
boxplot(commercializedPriceData, ylab = "Preço (R$)", xlab = paste("Outliers: ", paste(outliers, collapse = ", ")), main = "Preço comercializado de imóveis na Cidade Z")


# Questão 2:
# Separamos as variáveis de idade e taxas pagas
propertiesAges = zCityData$id
propertiesTaxes = zCityData$tax

# Calculamos o coeficiente de correlação linear de Pearson
correlationCoefficient = cor(propertiesAges, propertiesTaxes, method = "pearson")

# Geramos um gráfico de dispersão!
plot(propertiesAges, propertiesTaxes, ylab = "Taxas cobradas", xlab = "Idade do imóvel", main = "Relação Taxas cobradas e Idade do imóvel")
