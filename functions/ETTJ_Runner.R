################
# Projeto ETTJ #
################


Dates = c('2023-01-31', '2023-02-28', '2023-03-31', '2023-04-30', '2023-05-31', '2023-06-30', '2023-08-31', '2023-09-30')


getETTJ <- function(Dates){
  
  library(DBI); library(bizdays); library(odbc) 

  source("C:/Cursos/Juros/fxRates/LibJuros.R")
  
 
    
  check_dates = !bizdays::adjust.previous(ymd(Dates), cal = "Brazil/ANBIMA") %in% ymd((DBI::dbGetQuery(conn = sql.con.SandBox, statement = "Select Date from DataAnalyticsSandBox..ETTJ_Parameters") %>% unique())$Date)
  
  if(sum(check_dates) != 0){
  
  # Create Objetcs ----
  final.ETTJ = data.frame()
  final.Parametros = data.frame()
  
  DateToScrap = Dates[check_dates]
  
  for(i in 1:length(DateToScrap)){
    
    writeLines(paste("Getting the ETTJ Data from:", DateToScrap[i], sep = ""))
    
    Ano = lubridate::year(lubridate::ymd(DateToScrap[i])); AnoMes = paste(Ano, stringr::str_pad(lubridate::month(lubridate::ymd(DateToScrap[i])), width = 2, pad = "0"), sep = "")
    url_anbima = paste("https://www.gov.br/susep/pt-br/arquivos/arquivos-ettj/", Ano, "/curvazero_", AnoMes, ".txt/@@download/file", sep = "")
    url_susep = paste("https://www.gov.br/susep/pt-br/arquivos/arquivos-ettj/", Ano ,"/resultados_divulgacao_", gsub(pattern = "-", "", bizdays::adjust.previous(lubridate::ymd(DateToScrap[i]), cal = "Brazil/ANBIMA")), ".txt/@@download/file", sep = "")
    
    if(class(try(rvest::read_html(url_anbima), silent = T)) == "try-error"){next}

    download.file(url_anbima, destfile = paste("./ETTJ_Anbima_", AnoMes, ".txt", sep = ""), cacheOK = T);download.file(url_susep, destfile = paste("./ETTJ_Susep_", AnoMes, ".txt", sep = ""), cacheOK = T)
    
    aux_last_wd = bizdays::adjust.previous(lubridate::ymd(DateToScrap[i]), cal = "Brazil/ANBIMA")
    
    anbima = 
      read.delim(file = paste("C:/Cursos/Juros/fxRates/ETTJ_Anbima_", AnoMes, ".txt", sep = ""), header = T, sep = "@", skip = 2,  nrows = 2)[,-1] %>% 
      dplyr::mutate(Group = c("PRE", "IPCA")) %>% 
      dplyr::select(Group, "Beta.0" = Beta.1, "Beta.1" = Beta.2, "Beta.2" = Beta.3, "Beta.3" = Beta.4, "Lambda.1", "Lambda.2") %>% 
      dplyr::mutate("Date" = aux_last_wd, "Source" = "Anbima")
    
    susep = read.delim(file = paste("C:/Cursos/Juros/fxRates/ETTJ_Susep_", AnoMes, ".txt", sep = ""), header = T, sep = ";")[c(2,3),] %>% 
      dplyr::select("Group" = tipo.curva, -data, "Beta.0" = beta.0, "Beta.1" = beta.1, "Beta.2" = beta.2, "Beta.3" = beta.3, "Lambda.1" = lambda.1, "Lambda.2" = lambda.2) %>% 
      dplyr::mutate(Group = c("IGPM", "TR")) %>% replace(is.na(.), 0) %>%  dplyr::mutate("Date" = aux_last_wd, "Source" = "Susep")

    parametros = rbind(anbima, susep) %>% dplyr::select(Date, Source, Group, Beta.0, Beta.1, Beta.2, Beta.3, Lambda.1, Lambda.2)
    
    start_date = DateToScrap[i] %>% lubridate::ymd()
    
    Date = seq(as.Date(floor_date(start_date, unit = "month")), length=1234, by="1 month") %>% lubridate::ceiling_date(unit = "month") -1 
    
    lag.DU = dplyr::lag(Date) %>% du_252(DataInicial = start_date, DataFinal = .); lag.DU.final = lag.DU[!is.na(lag.DU)]; lag.DU.final[1] = 0
    
    ETTJ = 
      data.frame(
        PRE = nelson_siegel_svensson(b0 = parametros$Beta.0[1], b1 = parametros$Beta.1[1], b2 = parametros$Beta.2[1], b3 = parametros$Beta.3[1], 
                                     lam1 = parametros$Lambda.1[1], lam2 = parametros$Lambda.2[1], t = lag.DU.final, TR = F),
        IPCA = nelson_siegel_svensson(b0 = parametros$Beta.0[2], b1 = parametros$Beta.1[2], b2 = parametros$Beta.2[2],b3 = parametros$Beta.3[2], 
                                      lam1 = parametros$Lambda.1[2], lam2 = parametros$Lambda.2[2], t = lag.DU.final, TR = F),
        IGPM = nelson_siegel_svensson(b0 = parametros$Beta.0[3], b1 = parametros$Beta.1[3], b2 = parametros$Beta.2[3], b3 = parametros$Beta.3[3], 
                                      lam1 = parametros$Lambda.1[3], lam2 = parametros$Lambda.2[3], t = lag.DU.final, TR = F),
        TR = nelson_siegel_svensson(b0 = parametros$Beta.0[4], b1 = parametros$Beta.1[4], b2 = parametros$Beta.2[4], b3 = parametros$Beta.3[4], 
                                    lam1 = parametros$Lambda.1[4], lam2 = parametros$Lambda.2[4], t = lag.DU.final, TR = T)
      )
    
    ETTJ = ETTJ %>% replace(is.na(.), values = 0) %>% 
      dplyr::mutate(Du.Year = lag.DU.final, "Date" = aux_last_wd, Du = Du.Year * 252) %>% 
      dplyr::select(Date, Du, Du.Year, PRE, IPCA, IGPM, TR)
    
    final.ETTJ = rbind(final.ETTJ, ETTJ);  final.Parametros = rbind(final.Parametros, parametros)
    
    
  }
 

  
  # Output
  return(
    list(ETTJ = ETTJ, parametros = ETTJ_Parameters)
  
  )
  
  }  else {
  
    writeLines(paste("No ETTJ Data to retrieve: Up to Date", sep = ""))
    
  }  

  
}


output = getETTJ(Dates = Dates)














