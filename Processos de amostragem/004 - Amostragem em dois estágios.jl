#Inventário Florestal🌳
#Amostragem em dois estágios 
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função Dois_estagios: amostragem em dois estágios
function Dois_estagios(Dados) #Determina a função
    Conjunto_de_dados = (Conversor.*Dados)
    #Tabela com estatítica descritiva por unidade secundária/bloco
    Tabela=transform(Conjunto_de_dados, AsTable(:) .=> ByRow.([I -> count(!ismissing, I), sum, mean, var]).=>[:n, :Soma, :Média, :Variância])
    (length(Tabela.n)) #Número de unidades/blocos primárias
    first(unique(Tabela.n)) #Número de unidades secundárias
    (sum(Tabela.Média)/length(Tabela.n)) #Média
    sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))
    sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))/Conversor #Variância dentro das unidades
    sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/(length(Tabela.n)-1)
    (sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/Conversor #Variância entre unidades
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/Conversor)+
    (sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))/Conversor) #Estimativa da variância 
        1-(length(Tabela.n)/N) #Fator de correção
    (0.1*(sum(Tabela.Média)/length(Tabela.n))) #Limite de erro da amostragem requerido
    quantile(TDist((length(Tabela.n))-1),1-alpha/2) #Valor de t 

    if 1-(length(Tabela.n)/N) ≥ 0.98 #f maior ou igual a 0,98 população infinita
        População = "é considerada infinita"   
            println(População)
        elseif 1-(length(Tabela.n)/N) < 0.98 #f menor que 0,98 população finita
        População = "é considerada finita"    
            println(População)
            end
    
    Tamanho_da_amostra = if 1-(length(Tabela.n)/N) ≥ 0.98
         #População infinita. O tamanho da amostra é calculado pela seguinte equação:
         Infinita=((quantile(TDist(length(Tabela.n)-1),1-alpha/2))^2)*((sum(first(unique(Tabela.n))*
        (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
        (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
        (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))+(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/
        (length(Tabela.n)*(first(unique(Tabela.n))-1))/first(unique(Tabela.n))))/
        (((0.1*(sum(Tabela.Média)/length(Tabela.n))))^2)
        round(Infinita)
    elseif 1-(length(Tabela.n)/N) < 0.98
         #População finita. O tamanho da amaostra é calculado pela seguinte equação:
         Finita=((quantile(TDist(length(Tabela.n)-1),1-alpha/2))^2)*((sum(first(unique(Tabela.n))*
        (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
        (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
        (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))+(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/
        (length(Tabela.n)*(first(unique(Tabela.n))-1))./first(unique(Tabela.n))))/
        ((((0.1*(sum(Tabela.Média)/length(Tabela.n))))^2)+(1/N)*((quantile(TDist(length(Tabela.n)-1),1-alpha/2))^2)*
        ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
        ((length(Tabela.n))-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
        (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))+(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/
        (length(Tabela.n)*(first(unique(Tabela.n))-1))/M)))
        round(Finita)
    end
    (((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+((M-first(unique(Tabela.n)))/M)*
    (sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n)))))/Conversor #Variância da média
    sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n)))))) #Erro padrão
    #Erro da amostragem
    quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*
    (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/
    (length(Tabela.n)*(first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n)))))) #Absoluto
    (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*
    (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n))))))/
    (sum(Tabela.Média)/length(Tabela.n)))*100 #Relativo
    #Limite do intervalo de confiança para média
    ((sum(Tabela.Média)/length(Tabela.n))-quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))) #Inferior
    ((sum(Tabela.Média)/length(Tabela.n))+quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))) #Superior
    #Total estimado
    ((N*M*sum(Tabela.Média)/length(Tabela.n))/Conversor)
    #Limite do intervalo de confiança para o total 
    (((N*M*sum(Tabela.Média)/length(Tabela.n))-(N*M*quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor) #Inferior 
    (((N*M*sum(Tabela.Média)/length(Tabela.n))+(N*M*quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor) #Superior
    if (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*
        (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
        (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
        (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
        ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
        (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n))))))/
        (sum(Tabela.Média)/length(Tabela.n)))*100 > EAR
        Observação = "Diante do exposto, conclui-se que os resultados obtidos na amostragem não satisfazem as exigências de
        precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média  para confiabilidade designada. 
        O erro estimado foi maior que o limite fixado, sendo recomendado incluir mais unidades amostrais no inventário."
        println(Observação)
        elseif (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*
            (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
            (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
            (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
            ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
            (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n))))))/
            (sum(Tabela.Média)/length(Tabela.n)))*100 ≤ EAR
                Observação  = "Diante do exposto, conclui-se que os resultados obtidos na amostragem satisfazem as exigências de
                precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média para confiabilidade designada. 
                O erro estimado foi menor que o limite fixado, assim as unidades amostrais são suficientes para o inventário."
                println(Observação)
    end       
    Resultados = DataFrames.DataFrame(Variânciaiáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro da amostragem relativo (%)", 
    "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Variância dentro das unidades (m³/ha)²", "Variância entre unidades (m³/ha)²", 
    "Estimativa da Variância (m³/ha)²", "Variância da média da população (m³/ha)²", "Limite do erro de amostragem requerido", 
    "Fator de correção", "População", "Tamanho da amostra", "Número total de unidades secundárias por unidade primária", "Número potencial de unidades primárias", 
    "Número de unidades primárias", "Número de unidades secundárias", "Nível de significância (α)", "Observação"], Valores=[(sum(Tabela.Média)/length(Tabela.n)), 
    ((sum(Tabela.Média)/length(Tabela.n))-quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))), ((sum(Tabela.Média)/length(Tabela.n))+quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))), ((N*M*sum(Tabela.Média)/length(Tabela.n))/Conversor), 
    (((N*M*sum(Tabela.Média)/length(Tabela.n))-(N*M*quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor), 
    (((N*M*sum(Tabela.Média)/length(Tabela.n))+(N*M*quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*
    ((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor), area, (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*
    (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n))))))/
    (sum(Tabela.Média)/length(Tabela.n)))*100, quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*
    (Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/
    (length(Tabela.n)*(first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n)))))), sqrt((((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+
    ((M-first(unique(Tabela.n)))/M)*(sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1))/(length(Tabela.n)*first(unique(Tabela.n)))))), 
    sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))/Conversor, 
    (sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/Conversor, (((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/Conversor)+
    (sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))/Conversor)), 
    (((N-length(Tabela.n))/N)*((sum(first(unique(Tabela.n))*(Tabela.Média.-(sum(Tabela.Média)/length(Tabela.n))).^2)/
    (length(Tabela.n)-1)-sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*
    (first(unique(Tabela.n))-1)))/first(unique(Tabela.n))/length(Tabela.n))+((M-first(unique(Tabela.n)))/M)*
    (sum(Tabela.Variância*(first(unique(Tabela.n))-1))/(length(Tabela.n)*(first(unique(Tabela.n))-1))/
    (length(Tabela.n)*first(unique(Tabela.n)))))/Conversor, (0.1*(sum(Tabela.Média)/length(Tabela.n))), 1-(length(Tabela.n)/N), População, Tamanho_da_amostra,  
    M, N, length(Tabela.n), first(unique(Tabela.n)), alpha, Observação]) #Tabela de resultados  
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/04.xlsx"), Dados=(collect(DataFrames.eachcol(Dados)), 
    DataFrames.names(Dados)), Analise_descritiva=(collect(DataFrames.eachcol(Tabela)), DataFrames.names(Tabela)), 
    Resultados=(collect(DataFrames.eachcol(Resultados)), DataFrames.names(Resultados))) #Exportar para o Excel     
end
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/dois_estagios.csv", DataFrame) 
#Informações necessárias
#Área da população
const area = 1000000
#Número potencial de unidades secundárias por primárias
const M = 40000
#Número potencial de unidades primárias
const N = area/10000
#Nível de significância (α)
const alpha = 0.05
const EAR = 10 #Erro da amostragem requerido
#Unidade de medida da variável
Unidade = "m³/0.25 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela = 0.25
Conversor = 1/Área_da_parcela
#function Dois_estagios(Dados)
Dois_estagios(Dados) #Saída dos dados
