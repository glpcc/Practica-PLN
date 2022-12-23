## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# save the built-in output hook
hook_output <- knitr::knit_hooks$get("output")

# set a new output hook to truncate text output
knitr::knit_hooks$set(output = function(x, options) {
  if (!is.null(n <- 20)) {
    x <- xfun::split_lines(x)
    if (length(x) > n) {
      # truncate the output
      x <- c(head(x, 5), "\n...\n",tail(x,6))
    }
    x <- paste(x, collapse = "\n")
    x <- xfun::split_lines(x)
    if (any(nchar(x) > 100)) x = strwrap(x, width = 100)
    x <- paste(x, collapse = "\n")
  }
  hook_output(x, options)
})


## ----instalar_rjson--------------------------------------------------------------------------------------------------------------------------------------------------------------
if (! require('rjson')){
  !install.packages('rjson');
}
library('rjson');


## ----cargar_json-----------------------------------------------------------------------------------------------------------------------------------------------------------------
f = fromJSON(file = 'datos/reduced_article_list3.json');
ids = unlist(lapply(f$articles, function (x) x$id ));
texto = unlist(lapply(f$articles, function (x) x$abstractText ));
df = data.frame(ids,texto)


## ---- carga, warning=FALSE,message=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
library(udpipe)
# udpipe_download_model(language = "spanish-ancora") #"spanish-ancora" or "spanish-gsd"
# Descarga "spanish-ancora-ud-2.5-191206.udpipe"
udmodel_es<-udpipe_load_model(file = 'spanish-ancora-ud-2.5-191206.udpipe');


## ----txt, warning=FALSE,message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
# Modo texto a texto
library(stringr)
enfermedades = c();
for (texto in df$texto){
  texto_analizado = as.data.frame(udpipe_annotate(udmodel_es,texto));
  posibles_enfermedades = str_detect(texto_analizado$token,
                                     regex('.*?itis$|.*?oma$|.*?algia$|^hipo.*|^hiper.*'))
  for (j in 1:nrow(texto_analizado)){
    if (texto_analizado$upos[[j]] == 'NOUN' & posibles_enfermedades[[j]]){
      if (texto_analizado$dep_rel[[j+1]] == 'amod'){
        enfermedades = c(enfermedades,paste(texto_analizado$token[[j]], 
                                            texto_analizado$token[[j+1]],sep = " ") )
      }else{
        enfermedades = c(enfermedades,texto_analizado$token[[j]])
      }
    }
  }
}
enfermedades = unique(enfermedades)
enfermedades = sort(enfermedades)
enfermedades


## ----separar, warning=FALSE,message=FALSE----------------------------------------------------------------------------------------------------------------------------------------
enfermedades2 = c()
for (i in 1:length(enfermedades)){
  enfermedades2[i] = str_extract(enfermedades[i], "^\\w+")
  if (str_detect(enfermedades2[i], "(?<!i)s$") == TRUE){
  enfermedades2[i] = gsub("s$", "", enfermedades2[i])
}
  }
enfermedades2 = unique(tolower(enfermedades2))
enfermedades2


## ----buscador, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------
library(stringi)
urls = c()
valid_urls = c()
enfermedades3 = c()
for (i in 1:length(enfermedades2)){
  url = "https://www.cun.es/diccionario-medico/terminos/"
  urlOK = paste(url, enfermedades2[i])
  urlOK = stri_replace_all_regex(urlOK, c(" ", "á", "é", "í", "ó", "ú"),
                                 c("", "a", "e", "i", "o", "u"), vectorize_all = FALSE)
  urls[i] = urlOK
  tryCatch(
  {
    lines = readLines(con=urlOK, warn = FALSE)
    enfermedades3 = c(enfermedades3, enfermedades2[i])
    valid_urls = c(valid_urls, urlOK)
  },
  error=function(cond){
    message(paste("URL no existe:", urlOK))
    return(NA)
  }
)
}
enfermedades3 = unique(enfermedades3)
enfermedades3


## ----lema------------------------------------------------------------------------------------------------------------------------------------------------------------------------
forma_invariable <- stri_replace_all_regex(enfermedades3, 
                                           "hipo|hiper|itis|algia|algia|oma","")


## ---- warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
limpiaBloquesConTrim <- function(vec_cadenas){
a <- gsub("<[^<>]*>", "", vec_cadenas)
trimws(a)}
reemplazaElemsHTML <- function(vec_cadenas){
traduc <- list(c("&aacute;", "á"),
               c("&Aacute;", "Á"),
               c("&eacute;", "e"),
               c("&Eacute;", "É"),
               c("&iacute;", "í"),
               c("&Iacute;", "Í"),
               c("&oacute;", "ó"),
               c("&Oacute;", "Ó"),
               c("&uacute;", "ú"),
               c("&Uacute;", "Ú"),
               c("&ntilde;", "ñ"),
               c("&Ntilde;", "Ñ"),
               c("&iquest;", "¿"),
               c("&iexcl;", "¡"),
               c("&laquo;", "«"),
               c("&raquo;", "»"))
stri_replace_all_regex(vec_cadenas,
                       pattern=unlist(lapply(traduc, function(x){x[1]})),
                       replacement=unlist(lapply(traduc, function(x){x[2]})),
                       vectorize=FALSE)
}
url_base = "https://www.cun.es/diccionario-medico/terminos/"
definiciones = c()
for (i in 1:length(valid_urls)){
  lines = readLines(valid_urls[i],
                    encoding = "UTF-8")
  definicion = lines[891]
  definicion <- limpiaBloquesConTrim(lines[891])
  definicionOK <- reemplazaElemsHTML(definicion)
  definiciones = c(definiciones, definicionOK)
}
names(definiciones) = enfermedades3


## ----txt2------------------------------------------------------------------------------------------------------------------------------------------------------------------------
con1 <- file(
  description = "Definiciones.txt",
  open = "wt",
  encoding = "UTF-8")
dict_enf = c("algia", "hipo", "hiper", "itis", "oma")
names(dict_enf) = c("Dolor", "Alteración", "Alteración", "Infección", "Cáncer")
for (i in 1:length(definiciones)){
  definicionFinal = paste(names(definiciones[i]), ": ", definiciones[i], sep="")
  writeLines(definicionFinal, con1)
  for (j in 1:5){
    tipo = unlist(str_extract_all(names(definiciones[i]), dict_enf[j]))
    if (!(is.null(names(which(dict_enf==tipo))))){
      tipo = names(which(dict_enf==tipo))
      writeLines(paste("Tipo:", tipo), con1)
      break
    }
  }
  lema = paste("Forma invariable: ", forma_invariable[i], sep="")
  writeLines(lema, con1)
  variaciones = unlist(str_extract_all(enfermedades, paste(names(definiciones[i]), ".*")))
  if (length(variaciones) > 1){
    variaciones = unique(variaciones)
    variaciones = paste("Variaciones:", paste(as.character(variaciones), collapse = ", "))
    writeLines(variaciones, con1)
  }
  writeLines("\n", con1)
}
close(con1)


## ----ejemplo_resultado-----------------------------------------------------------------------------------------------------------------------------------------------------------
lines = readLines("Definiciones.txt")
lines[lines != ""]

