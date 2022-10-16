#Inventário Florestal🌳
#Amostragem estratificada
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função Estratificada: amostragem estratificada
function Estratificada(Estrato, Unidade, Volume) #Determina a função

    Volume = (Conversor.*Dados.Volume)
    Estrato = Dados.Estrato
    Unidade = Dados.Unidade
    Conjunto_de_dados = DataFrame(Estrato = Estrato, Unidade = Unidade, Volume = Volume)
        length(unique(Estrato)) #Número de estratos
    length(Unidade) #Número de Unidades 
    #Número potencial de Unidades de estratos
    (round((area/(length(Unidade)))*nh1)*10) #Estrato I
    (round((area/(length(Unidade)))*nh2)*10) #Estrato II
    (round((area/(length(Unidade)))*nh3)*10) #Estrato III
    (round((area/(length(Unidade)))*nh1)*10)/N
    (round((area/(length(Unidade)))*nh2)*10)/N
    (round((area/(length(Unidade)))*nh3)*10)/N
    Informações_do_inventário = DataFrames.DataFrame(Variáveis=["Área da população (ha)", 
    "Número total potencial de Unidades da população", "Nível de significância (α)", "Número de Unidades amostradas no estrato I", 
    "Número de Unidades amostradas no estrato II", "Número de Unidades amostradas no estrato III", 
    "Número de estratos", "Número de Unidades totais", "Número potencial de Unidades do estrato I", 
    "Número potencial de Unidades do estrato II", "Número potencial de Unidades do estrato III"], 
    Valores=[area, N, alpha, nh1, nh2, nh3, length(unique(Estrato)), length(Unidade), 
    (round((area/(length(Unidade)))*nh1)*10), (round((area/(length(Unidade)))*nh2)*10), 
    (round((area/(length(Unidade)))*nh3)*10)/N]) #Tabela de resultados
    #Tabela com estatítica descritiva pro estrato
    Tabela= combine(groupby(Conjunto_de_dados, :Estrato)) do df
    (Unidade=length(unique(df.Unidade)), Total= sum(df.Volume), Média= mean(df.Volume), Variância= var(df.Volume), 
    Erro_padrão= sqrt(var(df.Volume)))
    end
    mean(Volume) #Média estratificada
    mean(Tabela.Variância) #Variância estratificada
    sqrt(mean(Tabela.Variância)) #Desvio padrão estratificado
    #Análise de Variância da estratificação 
    length(unique(Estrato))-1 #Grau de liberdade entre os estratos
    length(Unidade)-length(unique(Estrato)) #Grau de liberdade dentro dos estratos
    length(Unidade)-1 #Grau de liberdade total
    sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2)
    sum((Volume.-mean(Volume)).^2)
    sum((Volume.-mean(Volume)).^2)-sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2)
    sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2)/length(unique(Estrato))-1 
    (sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/(length(unique(Estrato))-1)
    (sum((Volume.-mean(Volume)).^2)-sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/
    (length(Unidade)-length(unique(Estrato)))
    (sum((Volume.-mean(Volume)).^2))/(length(Unidade)-1)
    ((sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/(length(unique(Estrato))-1))/
    ((sum((Volume.-mean(Volume)).^2)-sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/
    (length(Unidade)-length(unique(Estrato))))
    Anova_da_estratificação = DataFrame(Fontes_de_variação=["Entre estratos", "Dentro dos estratos", "Total"], 
    gl=[length(unique(Estrato))-1 , length(Unidade)-length(unique(Estrato)), length(Unidade)-1], 
    SQ=[sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2), 
    sum((Volume.-mean(Volume)).^2)-sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2), 
    sum((Volume.-mean(Volume)).^2)], 
    QM=[(sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/(length(unique(Estrato))-1), 
    (sum((Volume.-mean(Volume)).^2)-sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/
    (length(Unidade)-length(unique(Estrato))), (sum((Volume.-mean(Volume)).^2))/
    (length(Unidade)-1)], F=[((sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/
    (length(unique(Estrato))-1))/
    ((sum((Volume.-mean(Volume)).^2)-sum(Tabela.Unidade.*(Tabela.Média.-mean(Volume)).^2))/
    (length(Unidade)-length(unique(Estrato)))), missing, missing])
    ((round((area/(length(Unidade)))*nh1)*10)/N; (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância) 
    sum(((round((area/(length(Unidade)))*nh1)*10)/N; (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))
    (sum(((round((area/(length(Unidade)))*nh1)*10)/N; (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)
    #Tamanho da amostra
    (0.1*mean(Volume)) #Limite de erro da amostragem requerido
    t=quantile(TDist(length(Unidade)-1),1-alpha/2) #Valor de t 
    (1-(length(Unidade)/N)) #Fator de correção
    if (1-(length(Unidade)/N)) ≥ 0.98 #f maior ou igual a 0,98 população infinita
        População = "é considerada infinita"   
            println(População)
        elseif (1-(length(Unidade)/N)) < 0.98 #f menor que 0,98 população finita
        População = "é considerada finita"    
            println(População)
            end
    Tamanho_da_amostra = if (1-(length(Unidade)/N)) ≥ 0.98
    #População infinita. O tamanho da amostra é calculado pela seguinte equação:
    Infinita=(((t)^2)*sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância)))/(((0.1*mean(Volume)))^2)
    round(Infinita)
    elseif (1-(length(Unidade)/N)) < 0.98
    #População finita. O tamanho da amostra é calculado pela seguinte equação:
    Finita=(((t)^2)*sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância)))/
    (((0.1*mean(Volume)))^2)+((t)^2)*(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)
    round(Finita)
    end
    #Dados necessários para calcular o tamanho da amostra em amostragem estratificada
    (round((area/(length(Unidade)))*nh1)*10)/N*(round(Finita))
    (round((area/(length(Unidade)))*nh2)*10)/N*(round(Finita))
    (round((area/(length(Unidade)))*nh3)*10)/N*(round(Finita))
    #Variância em cada estrato
    ((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))
    #Variância estratificada
    ((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)
    #Erro padrão
    sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N))
    #Grau de liberdade
    ((round((area/(length(Unidade)))*nh1)*10)*((round((area/(length(Unidade)))*nh1)*10)-nh1))/nh1
    ((round((area/(length(Unidade)))*nh2)*10)*((round((area/(length(Unidade)))*nh2)*10)-nh2))/nh2
    ((round((area/(length(Unidade)))*nh3)*10)*((round((area/(length(Unidade)))*nh3)*10)-nh3))/nh3
    #Erro da amostragem
    (t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N))) #Absoluto
    (((t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)))/
    mean(Volume))*100) #Relativo
    #Limite do intervalo de confiança para média
    (mean(Volume)-(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N)))) #Inferior 
    (mean(Volume)+(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N)))) #Superior
    #Total por estrato
    (round((area/(length(Unidade)))*nh1)*10); (round((area/(length(Unidade)))*nh2)*10); 
    (round((area/(length(Unidade)))*nh3)*10).*Tabela.Média
    #Total da população
    ((N*mean(Volume))/Conversor)
    #Limite do intervalo de confiança para o total 
    (((N*mean(Volume))-N*(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*
    sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N))))/Conversor) #Inferior
    (((N*mean(Volume))+N*(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*
    sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N))))/Conversor) #Superior

    if (((t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
        ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
        ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
        length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; (round((area/(length(Unidade)))*nh2)*10)/N; 
        (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)))/mean(Volume))*100) > EAR
        Observação = "Diante do exposto, conclui-se que os resultados obtidos na amostragem não satisfazem as exigências de
        precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da Média  para confiabilidade designada. 
        O erro estimado foi maior que o limite fixado, sendo recomendado incluir mais Unidades amostrais no inventário."
        println(Observação)
        elseif (((t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
            ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
            ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
            length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; (round((area/(length(Unidade)))*nh2)*10)/N; 
            (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)))/mean(Volume))*100) ≤ EAR
            Observação  = "Diante do exposto, conclui-se que os resultados obtidos na amostragem satisfazem as exigências de
            precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da Média para confiabilidade designada. 
            O erro estimado foi menor que o limite fixado, assim as Unidades amostrais são suficientes para o inventário."
                println(Observação)
    end       
    Resultados = DataFrames.DataFrame(Variáveis=["Média estratificada (m³/ha)", "Limite inferior do intervalo de confiança para Média (m³/ha)", 
    "Limite superior do intervalo de confiança para Média (m³/ha)", "Total da população (m³)", 
    "Limite inferior do intervalo de confiança para o total (m³)", "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)",
    "Erro da amostragem relativo (%)", "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)", "Desvio padrão (m³/ha)", 
    "Variância estrato I (m³/ha)²", "Variância estrato II (m³/ha)²", "Variância estrato III (m³/ha)²", "Variância estratificada (m³/ha)²", 
    "Variância da média relativa (%)", "Fator de correção", "Limite de erro da amostragem requerido", 
    "Tamanho da amostra estrato I", "Tamanho da amostra estrato II", "Tamanho da amostra estrato III", 
    "Tamanho da amostra", "População", "Observação"], Valores=[mean(Volume), (mean(Volume)-(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N)))), (mean(Volume)+(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N)))), ((N*mean(Volume))/Conversor), (((N*mean(Volume))-N*(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*
    sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N))))/Conversor), (((N*mean(Volume))+N*(t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*
    sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/
    N*Tabela.Variância))/N))))/Conversor), area, (((t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)))/
    mean(Volume))*100), (t*sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N))), sqrt(((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N)), sqrt(mean(Tabela.Variância)), 
    ((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)), 
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)), 
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)), mean(Tabela.Variância), 
    ((((((round((area/(length(Unidade)))*nh1)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh2)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade))+
    ((((round((area/(length(Unidade)))*nh3)*10)/N)^2)*sum(Tabela.Variância/Tabela.Unidade)))/
    length(unique(Estrato)))-(sum(((round((area/(length(Unidade)))*nh1)*10)/N; 
    (round((area/(length(Unidade)))*nh2)*10)/N; 
    (round((area/(length(Unidade)))*nh3)*10)/N*Tabela.Variância))/N), (1-(length(Unidade)/N)), (0.1*mean(Volume)), 
    (round((area/(length(Unidade)))*nh1)*10)/N*(round(Finita)), 
    (round((area/(length(Unidade)))*nh2)*10)/N*(round(Finita)), 
    (round((area/(length(Unidade)))*nh3)*10)/N*(round(Finita)), Tamanho_da_amostra, População, Observação]) #Tabela de resultados    
   
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/02.xlsx"), Dados=(collect(DataFrames.eachcol(Dados)), 
       DataFrames.names(Dados)), Informações_do_inventário=(collect(DataFrames.eachcol(Informações_do_inventário)), 
       DataFrames.names(Informações_do_inventário)), Por_estrato=(collect(DataFrames.eachcol(Tabela)), DataFrames.names(Tabela)),  
       Anova_da_estratificação=(collect(DataFrames.eachcol(Anova_da_estratificação)), 
       DataFrames.names(Anova_da_estratificação)), Resultados=( collect(DataFrames.eachcol(Resultados)), 
       DataFrames.names(Resultados))) #Export to Excel
end
________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/estratificada.csv", DataFrame) 
#Informações necessárias
#Área da população
const area=45
#Número potencial de Unidades populacionais 
const N = area/0.1 
#Nível de significância (α)
const alpha = 0.05
const EAR = 10 #Erro da amostragem requerido
#Unidade de medida da variável
Unidade = "m³/0.1 ha" #Alterar em função do inventário
#Conversor para a Unidade de área por hectare
Área_da_parcela=0.1
Conversor=1/Área_da_parcela
#Número potencial de Unidades por estrato
const nh1=7 #estrato I
const nh2=8 #estrato II
const nh3=7 #estrato III
#Estratificada(Estrato, Unidade, Volume)
Estratificada(Dados.Estrato, Dados.Unidade, Dados.Volume) #Saída dos dados
