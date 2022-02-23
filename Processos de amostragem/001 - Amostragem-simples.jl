#Inventário Florestal🌳
#Amostragem aleatória simples
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função ASS: amostragem aleatória simples
function AAS(Unidades, Volume) #Determina a função
    Volume = (Conversor.*Dados.Volume)
    Unidades = Dados.Unidades
    ASS= DataFrame(Unidades = Unidades, Volume= Volume)
    mean(Volume) #Média
    (length(Unidades)) #Número de unidades
    var(Volume) #Variância 
    sqrt(var(Volume)) #Desvio padrão
    (1-(length(Unidades)/N)) #Fração da amostragem
    (0.1*mean(Volume)) #Limite de erro da amostragem requerido
    t=quantile(TDist(length(Unidades)-1),1-alpha/2) #Valor de t
    if (1-(length(Unidades)/N)) ≥ 0.98 #f maior ou igual a 0,98 população infinita
    População = "é considerada infinita"   
        println(População)
    elseif (1-(length(Unidades)/N)) < 0.98 #f menor que 0,98 população finita
    População = "é considerada finita"    
        println(População)
        end     
    Intensidade =   if (1-(length(Unidades)/N)) ≥ 0.98 #f maior ou igual a 0,98 população infinita
        #População infinita. A intensidade de amostragem é calculada pela seguinte equação:
        int_infinita=(((t)^2)*var(Volume))/(((0.1*mean(Volume)))^2) 
        round(int_infinita)
    elseif (1-(length(Unidades)/N)) < 0.98 #f menor que 0,98 população finita
        #População finita. A intensidade de amostragem é calculada pela seguinte equação:
        int_finita=(N*((t)^2)*var(Volume))/((N*(((0.1*mean(Volume)))^2))+(((t)^2)*var(Volume)))
        round(int_finita)
    end 
    (var(Volume)/length(Unidades))*(1-(length(Unidades)/N)) #Variância média
    (sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))) #Erro padrão
    ((sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N)))/mean(Volume))*100 #Erro padrão relativo
    ((sqrt(var(Volume))/mean(Volume))*100) #Coeficiente de variação
    (((((sqrt(var(Volume))/mean(Volume))*100))^2)/(length(Unidades)))*(1-(length(Unidades)/N)) #Variância média relativa
    #Erro de amostragem
    (t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N)))) #Absoluto
    ((t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))/mean(Volume))*100 #Relativo
    #Limite do intervalo de confiança para média 
    (mean(Volume)-(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))) #Inferior
    (mean(Volume)+(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))) #Superior
    ((N*mean(Volume))/Conversor) #Total da população
    #Limite do intervalo de confiança para o total   
    ((N*mean(Volume))-N*(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N)))))/Conversor #Inferior
    ((N*mean(Volume))+N*(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))) #Inferior
    mean(Volume)-(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N)))) #Estimativa mínima de confiança
    #Tabela com os resultados
    if ((t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))/mean(Volume))*100 > EAR
        Observação = "Diante do exposto, conclui-se que os resultados obtidos na amostragem não satisfazem as exigências de
        precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média  para confiabilidade designada. 
        O erro estimado foi maior que o limite fixado, sendo recomendado incluir mais unidades amostrais no inventário."
        println(Observação)
        elseif ((t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))/mean(Volume))*100 ≤ EAR
        Observação  = "Diante do exposto, conclui-se que os resultados obtidos na amostragem satisfazem as exigências de
        precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média para confiabilidade designada. 
        O erro estimado foi menor que o limite fixado, assim as unidades amostrais são suficientes para o inventário."
        println(Observação)
    end       
    Resultados = DataFrames.DataFrame(Variáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Erro padrão relativo (%)", "Área da população (ha)", "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", 
    "Variância (m³/ha)²", "Variância da média (m³/ha)²", "Variância da média relativa (%)", "Coeficiente de variação (%)", "Limite de erro da amostragem requerido", "Estimativa mínima de confiança (m³/ha)",
    "Fração de amostragem", "Intensidade de amostragem", "População", "Número total de unidades amostrais da população", "Nível de significância (α)",  
    "Observação"], Valores=[mean(Volume), (mean(Volume)-(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))), 
    (mean(Volume)+(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))), 
    ((N*mean(Volume))/Conversor), 
    ((N*mean(Volume))-N*(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N)))))/Conversor, 
    ((N*mean(Volume))+N*(t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))),
    ((t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))))/mean(Volume))*100, Área,
    (t*(sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N)))),
    (sqrt(var(Volume))/sqrt(length(Unidades)))*sqrt((1-(length(Unidades)/N))), sqrt(var(Volume)),   
    var(Volume), (var(Volume)/length(Unidades))*(1-(length(Unidades)/N)), 
    (((((sqrt(var(Volume))/mean(Volume))*100))^2)/(length(Unidades)))*(1-(length(Unidades)/N)), 
    ((sqrt(var(Volume))/mean(Volume))*100), (0.1*mean(Volume)), EAR, (1-(length(Unidades)/N)),
    Intensidade, População, N, alpha, Observação]) #Tabela de resultados
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/1.xlsx"), Dados=(collect(DataFrames.eachcol(ASS)), 
        DataFrames.names(ASS)), Resultados=(collect(DataFrames.eachcol(Resultados)), 
        DataFrames.names(Resultados))) #Exportar para o Excel
end 
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/amostragem_simples.csv", DataFrame) 
#Informações necessárias
#Área da população
const Área = 45
#Número total de unidades de amostragem na população
const N = Área/0.1 
#Nível de significância (α)
const alpha = 0.05
const EAR = 10 #Erro da amostragem requerido
#Unidade de medida da variável
Unidade = "m³/0.1 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=0.1
Conversor=1/Área_da_parcela
#AAS(Unidades, Volume)
AAS(Dados.Unidades, Dados.Volume) #Saída dos dados