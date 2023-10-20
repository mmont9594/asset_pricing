
# LibJuros Functions ----

suppressPackageStartupMessages(library(bizdays))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(lubridate))


du_252 <- function(DataInicial, DataFinal, cal = "Brazil/ANBIMA"){
  return(bizdays(from = DataInicial, to = DataFinal, cal = cal)/252)
}

du_adjust_next <- function(DataJuros, cal = "Brazil/ANBIMA"){
  return(bizdays::adjust.next(dates = DataJuros,  cal = cal))
}

# CashFlow LTN
cashflows_ltn <- function(dt, vencimento){
  fluxo_Pagamento <- vector()
  fluxo_Vencimento <- vector()
  if(dt <= vencimento){
    pgto_final = 1000.0
    dt_pgto = du_adjust_next(DataJuros = lubridate::ymd(vencimento))
    fluxo_Pagamento = pgto_final
    fluxo_Vencimento = dt_pgto
  }
  
  out = data.frame(
    Valor_Hoje = fluxo_Pagamento,
    Valor_Vencimento = fluxo_Pagamento,
    Data_Vencimento = fluxo_Vencimento)
  
  return(out)
}    

# CashFlow NTN-F
cashflows_ntnf <- function(dt, vencimento){
  fluxo_Pagamento <- vector()
  fluxo_Vencimento <- vector()
  if(dt <= vencimento){
    # pagamento final
    pgto_final <- round(1000 * (1.1)**0.5, 5)
    fluxo_Pagamento <- fluxo_Pagamento %>% append(pgto_final)
    fluxo_Vencimento <- fluxo_Vencimento %>% append(du_adjust_next(vencimento))
    # pagamentos de cupom
    dt_juros <- lubridate::ymd(vencimento)
    valor_juros <- round(1000 * (1.1**0.5 - 1), 5)
    while(TRUE){
      
      dt_juros <- add_with_rollback(ymd(dt_juros), -months(6))
      
      if(dt_juros < dt){
        break
      } else {
        fluxo_Pagamento <- fluxo_Pagamento %>% append(valor_juros)
        fluxo_Vencimento <- fluxo_Vencimento %>% append(du_adjust_next(dt_juros))
      }
    }
    
  }
  out = data.frame(
    "Pagamento" = rev(fluxo_Pagamento),
    "Vencimento"  = rev(fluxo_Vencimento)
  )
  
  
  return(out)
  
}

# Nelson Siegel
nelson_siegel <- function(par, t){ #c(b0, b1, b2, lam)
  r = (par[1] + 
         par[2] * (1-exp(-par[4]*t))/(par[4]*t) +
         par[3] * ((1-exp(-par[4]*t))/(par[4]*t) - exp(-par[4]*t)))
  return(r) 
  
}

# Nelson Siegel Svensson
nelson_siegel_svensson <- function(b0, b1, b2, b3, lam1, lam2, t, TR){
  
  if(isTRUE(TR)){
    
    r = (
      b0 + 
        b1 * (1-exp(-lam1*t))/(lam1*t) + 
        b2 * (((1-exp(-lam1*t))/(lam1*t)) - exp(-lam1*t)) +
        b3 * (ifelse(is.na((1-exp(-lam2*t))/(lam2*t)),0,(1-exp(-lam2*t))/(lam2*t)) - exp(-lam2*t))
      
    )
    
  } else {
    r = (
      b0 + 
        b1 * (1-exp(-lam1*t))/(lam1*t) + 
        b2 * (((1-exp(-lam1*t))/(lam1*t)) - exp(-lam1*t)) +
        b3 * (((1-exp(-lam2*t))/(lam2*t)) - exp(-lam2*t))
    )
  }
  
  
  return(r) 
  
}

# Web Scrapping ----

# Taxas Referncias (YTM / PU)
read_taxas_referencia_rf <- function(dt, debug=T){
  if(debug){
    print(paste("Lendo Taxas de Referência ANBIMA TPF para", dt, sep = " "))
  }
  
  str_dt = lubridate::ymd(dt) %>% format('%y%m%d')
  
  url = paste("https://www.anbima.com.br/informacoes/merc-sec/arqs/ms", str_dt, ".txt", sep = "")
  
  return(read.delim(file = url, header = T, sep = "@", skip = 2, dec = ",")[, c(1:9)])
}

# VNA
read_vna_anbima <- function(dt, debug=False){
  if(debug){
    writeLines(paste("Lendo VNA ANBIMA para", dt, sep = " "))
    writeLines("")
  }
  
  url = "https://www.anbima.com.br/informacoes/vna/vna-down.asp"
  
  r = httr::POST(url, 
                 body = list(
                   Data = lubridate::ymd(dt) %>% format("%d%m%Y"),
                   Idioma = "PT",
                   saida = "csv"),
                 encode = "form")
  
  return(
    rawToChar(httr::content(r, as = "raw")) %>% 
      textConnection() %>% 
      read.csv2(header = T, sep = ";", dec = ",", skip = 7) %>% 
      dplyr::mutate(
        VNA = stringr::str_replace(VNA, pattern = "[.]", replacement = ""),
        VNA = stringr::str_replace(VNA, pattern = "[,]", replacement = "."),
        VNA = as.numeric(VNA)
      )
  )
  
  
}

# ETTJ
read_ettj_anbima <- function(dt, debug=F){
  if(debug){
    writeLines(paste("Lendo ETTJ ANBIMA para", dt, sep = " "))
  }
  url = "https://www.anbima.com.br/informacoes/est-termo/CZ-down.asp"
  payload = list("idioma" = "US", "saida" =  "csv", "Dt_ref" = lubridate::ymd(dt) %>% format("%d%m%Y"))
  r = httr::POST(url, 
                 body = payload,
                 encode = "form")
  
  return(rawToChar(httr::content(r, as = "raw")) %>% 
           textConnection() %>% 
           read.csv2(header = T, sep = ";", dec = ".", nrows = 2) %>% 
           dplyr::rename("Titulo" = 1) 
  )
  
}

# YTM Function
price <- function(r, dt, cashflows){
  p = 0
  for(i in 1:length(cashflows$Vencimento)){
    t = du_252(lubridate::ymd(dt), lubridate::ymd(cashflows$Vencimento[i])) 
    fd = (1+r)**(-t)
    p = p + cashflows$Pagamento[i] * fd
  }
  return(p)
}

ytm <- function(p, dt, cashflows){
  f_optim <- function(x){
    return(price(x, dt, cashflows) - p)
  }
  r = uniroot(f_optim, c(0.0, 1.0), tol = 1e-12)$root
  return(r)
}

cashflows_pre <- function(dt, principal, period, taxa, vencimento){
  out <- vector()
  out_dt <- vector()
  
  dt_fim <- vencimento
  dt_ini <- lubridate::add_with_rollback(lubridate::ymd(dt_fim), -months(period))
  
  fator_juros <- (1 + taxa/100)**du_252(DataInicial = dt_ini, DataFinal = dt_fim)
  out <- append(out, fator_juros * principal)
  out_dt <- append(out_dt, du_adjust_next(DataJuros = dt_fim))
  
  while(TRUE){
    dt_fim = dt_ini
    if(dt_fim < dt){
      break
    } else {
      dt_ini <- lubridate::add_with_rollback(lubridate::ymd(dt_fim), -months(period))
      fator_juros <- (1 + taxa/100)**du_252(DataInicial = dt_ini, DataFinal = dt_fim)
      out <- append(out, (fator_juros-1) * principal)
      out_dt <- append(out_dt, du_adjust_next(DataJuros = dt_fim))
    }
  }
  
  out <- rev(out); out_dt <- rev(out_dt)
  
  return(list(
    cf = out, data = out_dt
  ))
  
  
}


# CashFLow CDI+

# Funções Auxiliares

taxa_zero <- function(dt, vencimento, curva){
  tau <- du_252(DataInicial = dt, DataFinal = vencimento)
  
}

LinearInterpolation_taxa_zero <- function(dt, vencimento, curva){
  return(dt + (curva- dt)/(vencimento - dt)(vencimento - dt))
}

taxa_forward <- function(dt, t1, t2, curva){
  r1 <- taxa_zero(dt, t1, curva)
  fte1 <- (1+r1)**du_252(dt, t1)
  
  r2 <- taxa_zero(dt, t2, curva)
  fte2 <- (1+r2)**du_252(dt, t2)
  
  fte_fwd <- fte2/fte1
  r_fwd <- fte_fwd**(1/du_252(t1, t2)) -1
  
  return(r_fwd)
  
}

cashflow = function(Data, VNA, Vencimento, QTD, Ativo, BPS, YTM){
  
  cashflow_valor = vector()
  cashflow_data  = vector()
  juros_shock_vec = vector()
  
  # Pagamento Final ----
  
  if(Ativo == "NTN-B"){
    vl_juros = round(VNA * round((1.06)**0.5, 8), 6)
    juros = round(VNA * round((1.06)**0.5 - 1, 8), 6)
    juros_shock = round((1.06)**0.5, 8)
  } else if(Ativo == "LTN") {
    vl_juros = 1000
    juros = 0
    juros_shock = 0
  } else if(Ativo == "NTN-F") {
    vl_juros = round(VNA * round((1.10)**0.5, 8), 6)
    juros = round(VNA * round((1.10)**0.5 - 1, 8), 6)
    juros_shock = round((1.10)**0.5, 8)
  } else if(Ativo == "NTN-C") {
    vl_juros = round(VNA * round((1.12)**0.5, 8), 6)
    juros = round(VNA * round((1.12)**0.5 - 1, 8), 6)
    juros_shock = round((1.12)**0.5, 8)
  }   
  
  dt_juros = Vencimento
  
  cashflow_valor = append(cashflow_valor, vl_juros) 
  cashflow_data  = append(cashflow_data, du_adjust_next(dt_juros))
  
  while(TRUE){
    
    dt_juros <- add_with_rollback(ymd(dt_juros), -months(6))
    
    if(dt_juros < Data){
      vl_juros_shock = juros_shock
      juros_shock_vec = append(juros_shock_vec, vl_juros_shock)
      break
    } else {
      
      if(Ativo == "NTN-B"){
        vl_juros = round(VNA * round((1.06)**0.5 - 1, 8), 6)
        vl_juros_shock = round((1.06)**0.5 - 1, 8)
      } else if(Ativo == "LTN") {
        vl_juros = 0
        vl_juros_shock = 0
      } else if(Ativo == "NTN-F") {
        vl_juros = round(VNA * round((1.10)**0.5 - 1, 8), 6)
        vl_juros_shock = round((1.10)**0.5 - 1, 8)
      } else if(Ativo == "NTN-C") {
        vl_juros = round(VNA * round((1.12)**0.5 - 1, 8), 6)
        vl_juros_shock = round((1.12)**0.5 - 1, 8)
      }
      
      dt_pgto = dt_juros
      cashflow_valor = append(cashflow_valor, vl_juros)
      cashflow_data = append(cashflow_data, du_adjust_next(dt_pgto))
      juros_shock_vec = append(juros_shock_vec, vl_juros_shock)
    }
    
  }
  
  cashflow_valor = rev(cashflow_valor)
  cashflow_data  = rev(cashflow_data)
  DU_Year = du_252(DataInicial = Data, DataFinal = cashflow_data)
  DU = DU_Year * 252
  
  
  if(Ativo == "LTN"){
    juros_sensibilidade = juros_shock_vec[length(juros_shock_vec)] = (1+YTM/100)^(-DU_Year[length(DU_Year)]) 
  } else {
    juros_sensibilidade = juros_shock_vec * ((1+(YTM +BPS/100)/100)**-DU_Year)
  }
  
  NewPU = VNA * sum(juros_sensibilidade)
  
  return(
    list( 
      
      FluxoAtivos = 
        data.frame(
          Data = cashflow_data, Valor = cashflow_valor, # Data_Vencimento = Vencimento
          Juros = juros, QTD = QTD, DU = DU, DU_Year = DU_Year,# Duration = duration, 
          Cupom_Juros = juros_shock_vec, YTM = YTM, VNA = VNA, BPS = BPS #, Juros_shock = juros_shock
        ),
      
      FluxoAtivoSensibilidade = data.frame(
        Data = cashflow_data, QTD = QTD, DU = DU, DU_Year = DU_Year,# Duration = duration, 
        YTM = YTM, VNA = VNA, BPS = BPS, Fluxo_Juros = juros_shock_vec, 
        Juros_Sensibilidade = juros_sensibilidade, PU_Mercado = NewPU  
      )
    )
  )
  
  
}


# Price Function
envName = getwd() %>% stringr::str_split(pattern = "/") %>% unlist() %>% tail(1)

writeLines(
  paste("The following functions was loaded in your ", envName, ".R env:\n", 
        "Web Scrapping: read_ettj_anbima, read_vna_anbima, read_taxas_referencia_rf\n",
        "Asset Cashflow Projection: LTN, NTN-F\n",
        "Date: du_252, du_adjust_next\n",
        "Interest Rates Projection: nelson_siegel, nelson_siegel_svensson\n",
        "YTM/Price: ytm, price\n",
        "Sensibilidade MAG: YTM CF Projection",
        sep = "")
)
