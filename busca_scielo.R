# install.packages(RSelenium)
library(RSelenium)
checkForServer()

# Inicia o servidor Selenium e abre um navegador firefox
startServer()
remDrv <- remoteDriver(browserName = 'firefox')
remDrv$open()

# Expressao a ser buscada
minhaBusca <- "Infância"

# Realiza a busca
url <- 'http://www.scielo.br/cgi-bin/wxis.exe/iah/?IsisScript=iah/iah.xis&base=article^dlibrary&fmt=iso.pft&lang=p'
remDrv$navigate(url)
remDrv$findElement(using = "xpath", "//input[@name = 'exprSearch']")$sendKeysToElement(list(minhaBusca))
remDrv$findElement(using = "xpath", "//input[@src = '/iah/P/image/pesq.gif']")$clickElement()

# Captura a primeira pagina da busca
pagina <- remDrv$getPageSource()[[1]]
pagina <- htmlParse(pagina)
pagina <- xmlRoot(pagina)

# Captura o numero de paginas da busca
numeroPaginas <- getNodeSet(pagina, "//font/sup/b/i")
numeroPaginas <- xmlSApply(numeroPaginas, xmlValue)[1]
posicao <- regexpr("1 de ", numeroPaginas)[1] + 5
numeroPaginas <- substr(numeroPaginas, posicao, nchar(numeroPaginas))
numeroPaginas <- as.numeric(numeroPaginas)
print(paste("Total de paginas = ", numeroPaginas))

# Captura links dos artigos da primeira pagina
links <- c()
linksArtigos <- getNodeSet(pagina, "//a[@class = 'isoref']")
linksArtigos <- xmlSApply(linksArtigos, xmlGetAttr, "href")
linksArtigos <- grep("arttext", linksArtigos, value = T)
links <- c(links, linksArtigos)

# Captura links dos artigos das demais paginas
for (pag in 2:numeroPaginas){
  xpathProximaPagina <- paste("//input[@name = 'Page", pag, "']", sep = "")
  remDrv$findElement(using = "xpath", xpathProximaPagina)$clickElement()
  pagina <- remDrv$getPageSource()[[1]]
  pagina <- htmlParse(pagina)
  pagina <- xmlRoot(pagina)
  linksArtigos <- getNodeSet(pagina, "//a[@class = 'isoref']")
  linksArtigos <- xmlSApply(linksArtigos, xmlGetAttr, "href")
  linksArtigos <- grep("arttext", linksArtigos, value = T)
  links <- c(links, linksArtigos)
}

print(paste("Total de artigos = ", length(links)))

# Encerra Selenium
remDrv$closeWindow()
remDrv$quit()
remDrv$closeServer()

# Para cada link de artigo, capturar seu conteudo no XML
links.problema <- c()
dados <- data.frame()
i = 1
for (link in links[1410:2313]){
  print(i)
  pagina <- readLines(link)
  pagina <- htmlParse(pagina)
  pagina <- xmlRoot(pagina)
  link.xml <- getNodeSet(pagina,"//a[@target='xml']")
  link.xml <- xmlSApply(link.xml, xmlGetAttr, name = "href")
  link.xml <- "http://www.scielo.br/scieloOrg/php/articleXML.php?pid=S1516-14982014000200006&lang=pt"
  pagina.xml <- readLines(link.xml)
  erro <- try(xmlParse(pagina.xml), silent=TRUE)
  # Ignora links de xml com problemas e armazena a informacao do link no vetor links.problema
  if ('try-error' %in% class(erro)){
    links.problema = c(links.problema, j)
  }
  else {
    # Ainda nao descobri o enconding, mas basta usar a linha abaixo para resolver o problema
    #pagina.xml <- iconv(pagina.xml, from = "UTF-8", to = "UTF-8")
    pagina.xml <- xmlParse(pagina.xml)
    pagina.xml <- xmlRoot(pagina.xml)
    autores.sobrenomes <- getNodeSet(pagina.xml,"//article-meta/contrib-group/contrib/name/surname")
    autores.sobrenomes <- xmlSApply(autores.sobrenomes, xmlValue)
    autores.nomes <- getNodeSet(pagina.xml,"//article-meta/contrib-group/contrib/name/given-names")
    autores.nomes <- xmlSApply(autores.nomes, xmlValue)
    autores <- data.frame(autores.nomes, autores.sobrenomes)
    titulo <- getNodeSet(pagina.xml,"//article-meta//article-title")
    titulo <- xmlSApply(titulo, xmlValue)
    resumo <-  getNodeSet(pagina.xml,"//abstract[@xml:lang= 'pt']/p") 
    resumo <- xmlSApply(resumo, xmlValue)
    dados.artigo <- data.frame(autores, titulo, resumo)
    dados <- rbind(dados, dados.artigo)
  }
  i = i + 1
}

write.table(dados, file = "Infancia.csv", row.names = F, sep = ";")
