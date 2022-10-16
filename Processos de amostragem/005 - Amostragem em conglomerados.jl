#Inventário Florestal🌳
#Amostragem em conglomerados 
_________________________________________________________________________________________________________________________________________

using DataFrames, Statistics, Distributions, CSV, XLSX #Habilitar pacotes
_________________________________________________________________________________________________________________________________________

#Função Conglomerados: amostragem em conglomerados
function Conglomerados(Dados) #Determina a função
    Conjunto_de_dados = (Conversor.*Dados)
    #Tabela com estatítica descritiva por unidades/blocos secundários
    Tabela=transform(Conjunto_de_dados, AsTable(:) .=> ByRow.([I -> count(!ismissing, I), sum, mean, var]).=>[:n, :Soma, :Média, :Variância])
    length(Tabela.n) #Número de unidades primárias
    first(unique(Tabela.n)) #Número de unidades secundárias
    sum(Tabela.Média)/(length(Tabela.n)) #Média
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))+
    (sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n)))/Conversor #Variância da população por subunidades 
    sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))/Conversor #Mqdentro dos conglomerados
    sum(Tabela.Variância/length(Tabela.n))/Conversor #Variância dentro dos conglomerados
    sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1)/Conversor #MQentre conglomerados
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))))/Conversor #Variância entre as subunidades
    ((((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))))/Conversor)+(sum(Tabela.Variância/length(Tabela.n))/Conversor) #Variância total
    (sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/((sum(first(unique(Tabela.n)).*
    (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))+sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1))) #Coeficiente de correlação intraconglomerados 
    (0.1*sum(Tabela.Média)/(length(Tabela.n))) #Limite de erro da amostragem requerido
    quantile(TDist(length(Tabela.n)-1),1-alpha/2) #Valor de t 
    round(((((quantile(TDist(length(Tabela.n)-1),1-alpha/2))^2)*(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))+(sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/
    (length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n)))))./((((0.1*sum(Tabela.Média)/
    (length(Tabela.n)))).^2).*(first(unique(Tabela.n))))).*(1+(sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/
    ((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))+sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))'*
    (first(unique(Tabela.n)).-1)) #Tamanho da amostra
    (((N-length(Tabela.n))/N))*((((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))))/Conversor)/(length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))/Conversor)./
    (length(Tabela.n)*first(unique(Tabela.n))) #Variância da média
    sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n)))))) #Erro padrão
    #Erro da amostragem
    quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
    (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./
    (length(Tabela.n)*first(unique(Tabela.n)))))) #Absoluto
    (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
    (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./
    (length(Tabela.n)*first(unique(Tabela.n))))))/(sum(Tabela.Média)/(length(Tabela.n))))*100 #Relativo
    #Limite do intervalo de confiança para média 
    ((sum(Tabela.Média)/(length(Tabela.n)))-(quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/
    length(Tabela.n))+(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))/
    (sum(Tabela.Média)/(length(Tabela.n))))*100) #Inferior 
    (sum(Tabela.Média)/(length(Tabela.n)))+(quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./(length(Tabela.n)*
    first(unique(Tabela.n))))))) #Superior
    #Total estimado
    ((N*(first(unique(Tabela.n)))*(sum(Tabela.Média)/(length(Tabela.n))))/Conversor)
    #Limite do intervalo de confiança para o total 
    (((N*(first(unique(Tabela.n)))*(sum(Tabela.Média)/(length(Tabela.n))))-((N*first(unique(Tabela.n)))*
    quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor) #Inferior
    (((N*(first(unique(Tabela.n)))*(sum(Tabela.Média)/(length(Tabela.n))))+((N*first(unique(Tabela.n)))*
    quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
    (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor) #Superior
    
    if (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
        (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
        (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
        (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./
        (length(Tabela.n)*first(unique(Tabela.n))))))/(sum(Tabela.Média)/(length(Tabela.n))))*100 > EAR
        Observação = "Diante do exposto, conclui-se que os resultados obtidos na amostragem não satisfazem as exigências de
        precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média  para confiabilidade designada. 
        O erro estimado foi maior que o limite fixado, sendo recomendado incluir mais unidades amostrais no inventário."
        println(Observação)
        elseif (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
            (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
            (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
            (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./
            (length(Tabela.n)*first(unique(Tabela.n))))))/(sum(Tabela.Média)/(length(Tabela.n))))*100 ≤ EAR
            Observação  = "Diante do exposto, conclui-se que os resultados obtidos na amostragem satisfazem as exigências de
                precisão estabelecidas para o inventário, ou seja, um erro de amostragem máximo de ±10% da média para confiabilidade designada. 
                O erro estimado foi menor que o limite fixado, assim as unidades amostrais são suficientes para o inventário."
                println(Observação)
    end   
    Resultados = DataFrames.DataFrame(Variáveis=["Média (m³/ha)", "Limite inferior do intervalo de confiança para média (m³/ha)", 
    "Limite superior do intervalo de confiança para média (m³/ha)", "Total da população (m³)", "Limite inferior do intervalo de confiança para o total (m³)", 
    "Limite superior do intervalo de confiança para o total (m³)", "Área da população (ha)", "Erro padrão relativo (%)", "Erro da amostragem absoluto (m³/ha)", "Erro padrão (m³/ha)",
    "Variância dentro dos conglomerados (m³/ha)²", "Variância entre conglomerados (m³/ha)²", "Variância da população por subunidade (m³/ha)²", 
    "Variância da população total (m³/ha)²", "Variância da média (m³/ha)²", "Coeficiente de correlação intraconglomerados", "Tamanho da amostra",
    "Limite do erro de amostragem requerido", "Número de unidades primárias", "Número de unidades secundarias", "Nível de significância (α)", "Observação"], 
    Valores=[sum(Tabela.Média)/(length(Tabela.n)), ((sum(Tabela.Média)/(length(Tabela.n)))-(quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))/(sum(Tabela.Média)/(length(Tabela.n))))*100), (sum(Tabela.Média)/(length(Tabela.n)))+(quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))), ((N*(first(unique(Tabela.n)))*
    (sum(Tabela.Média)/(length(Tabela.n))))/Conversor), (((N*(first(unique(Tabela.n)))*(sum(Tabela.Média)/(length(Tabela.n))))-
    ((N*first(unique(Tabela.n)))*quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor), (((N*(first(unique(Tabela.n)))*
    (sum(Tabela.Média)/(length(Tabela.n))))+((N*first(unique(Tabela.n)))*quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*
    (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n))))))))/Conversor), area, 
    (quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
    (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./
    (length(Tabela.n)*first(unique(Tabela.n))))))/(sum(Tabela.Média)/(length(Tabela.n))))*100, quantile(TDist(length(Tabela.n)-1),1-alpha/2)*sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*
    (Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n)))))), 
    sqrt((((N-length(Tabela.n))/N))*(((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/length(Tabela.n))+
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))./(length(Tabela.n)*first(unique(Tabela.n)))))), 
    sum(Tabela.Variância/length(Tabela.n))/Conversor, (((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))))/Conversor, 
    (sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))+(sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/
    (length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n)))/Conversor, ((((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))))/Conversor)+(sum(Tabela.Variância/length(Tabela.n))/Conversor), (((N-length(Tabela.n))/N))*((((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))))/Conversor)/
    (length(Tabela.n))+(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1))/Conversor)./
    (length(Tabela.n)*first(unique(Tabela.n))), (sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/
    (length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/
    ((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))+sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1))), round(((((quantile(TDist(length(Tabela.n)-1),1-alpha/2))^2)*(sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*
    (first(unique(Tabela.n)).-1))+(sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n)))))./((((0.1*sum(Tabela.Média)/(length(Tabela.n)))).^2).*(first(unique(Tabela.n))))).*(1+(sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/
    (length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))/
    ((sum(first(unique(Tabela.n)).*(Tabela.Média.-sum(Tabela.Média)/(length(Tabela.n))).^2)/(length(Tabela.n)-1).-sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/
    (length(Tabela.n)*(first(unique(Tabela.n)).-1)))./first(unique(Tabela.n))+sum(Tabela.Variância.*(first(unique(Tabela.n)).-1))/(length(Tabela.n)*(first(unique(Tabela.n)).-1)))'*
    (first(unique(Tabela.n)).-1)), (0.1*sum(Tabela.Média)/(length(Tabela.n))), length(Tabela.n), first(unique(Tabela.n)), alpha, Observação]) #Tabela de resultados  
    XLSX.writetable(("F:/Version_09_07_21/iflorestal.jl/05.xlsx"), Dados=(collect(DataFrames.eachcol(Dados)), DataFrames.names(Dados)),  
    Analise_descritiva=(collect(DataFrames.eachcol(Tabela)), DataFrames.names(Tabela)),     
    Resultados=(collect(DataFrames.eachcol(Resultados)), DataFrames.names(Resultados))) #Exportar para o Excel
end
_________________________________________________________________________________________________________________________________________

#Processamento do inventário
#Importar dados
Dados = CSV.read("F:/Version_09_07_21/conglomerados.csv", DataFrame) 
#Informações necessárias
#Área da população
const area = 4000
#Número potencial de unidades populacionais 
const N = area
#Nível de significância (α)
const alpha = 0.05
const EAR = 10 #Erro da amostragem requerido
#Unidade de medida da variável
Unidade = "m³/0.25 ha" #Alterar em função do inventário
#Conversor para a unidade de área por hectare
Área_da_parcela=0.25
Conversor=1/Área_da_parcela
#function Conglomerados(Dados)
Conglomerados(Dados) #Saída dos dados
