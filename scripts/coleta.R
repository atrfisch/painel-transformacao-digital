#!/usr/bin/env Rscript
# =============================================================================
# Painel da Transformação Digital — coleta e preparação dos dados
#
# 1. Baixa do SIOP, via orcamentoBR::despesaDetalhada(), a despesa de cada
#    exercício no nível Órgão > UO > Programa > Ação > Plano Orçamentário (PO).
# 2. Classifica cada PO nos programas de config/programas.csv segundo as
#    regras de config/regras.csv e os ajustes de config/vinculos_manuais.csv.
# 3. Monta a série mensal:
#    - meses fechados: arquivo mensal "Execução da despesa" do Portal da
#      Transparência (mesma base SIAFI, com código de PO), agregado por PO;
#    - mês corrente: retrato atual do SIOP menos o acumulado dos meses fechados;
#    - dotação atual ao longo do ano: retratos do SIOP guardados a cada execução.
# 4. Grava site/dados/painel.json (lido pela página) e site/dados/linhas.csv.
#
# Variáveis de ambiente (opcionais):
#   PAINEL_RAIZ     pasta raiz do repositório (padrão: diretório atual)
#   PAINEL_HOJE     data de referência AAAA-MM-DD (testes)
#   PAINEL_N_ANOS   quantos exercícios coletar (padrão 3)
#   PAINEL_PORTAL   "0" desliga o download do Portal da Transparência
#   PAINEL_PORTAL_URL modelo de URL do arquivo mensal (testes), com %s = AAAAMM
#   PAINEL_SIMULADO "1" marca o JSON como dados simulados (testes)
# =============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(data.table)
})

# textos em UTF-8 independentemente do locale da máquina
if (!l10n_info()$`UTF-8`) for (loc in c("C.UTF-8", "en_US.UTF-8", "pt_BR.UTF-8"))
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

RAIZ <- normalizePath(Sys.getenv("PAINEL_RAIZ", "."), mustWork = TRUE)
p <- function(...) file.path(RAIZ, ...)
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), sprintf(...), "\n")

HOJE         <- as.Date(Sys.getenv("PAINEL_HOJE", as.character(Sys.Date())))
ANO_CORRENTE <- as.integer(format(HOJE, "%Y"))
MES_CORRENTE <- as.integer(format(HOJE, "%m"))
N_ANOS       <- as.integer(Sys.getenv("PAINEL_N_ANOS", "3"))
ANOS         <- seq(ANO_CORRENTE - N_ANOS + 1, ANO_CORRENTE)
USAR_PORTAL  <- Sys.getenv("PAINEL_PORTAL", "1") == "1"
SIMULADO     <- Sys.getenv("PAINEL_SIMULADO", "0") == "1"
METRICAS     <- c("ploa", "loa", "atual", "emp", "liq", "pago")
UA <- "Mozilla/5.0 (X11; Linux x86_64) painel-transformacao-digital (GitHub Actions)"

for (d in c("cache/siop", "cache/portal", "dados/mensal", "dados/historico", "site/dados"))
  dir.create(p(d), recursive = TRUE, showWarnings = FALSE)

avisos <- character(0)
aviso  <- function(...) { txt <- sprintf(...); msg("AVISO: %s", txt); avisos <<- c(avisos, txt) }

# ---------------------------------------------------------------- utilidades --
ACENTOS <- "\u00e1\u00e0\u00e2\u00e3\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00e7\u00f1"
SEM_ACENTO <- "aaaaaeeeeiiiiooooouuuucn"
norm_txt <- function(x) {
  x <- ifelse(is.na(x), "", enc2utf8(as.character(x)))
  x <- tolower(x)
  x <- chartr(ACENTOS, SEM_ACENTO, x)
  gsub("\\s+", " ", x)
}
num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
num_br <- function(x) {
  x <- trimws(as.character(x))
  tem_virgula <- grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub(",", ".", gsub(".", "", x[tem_virgula], fixed = TRUE), fixed = TRUE)
  num(x)
}
pad_po <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x) | x == "" | x == "-1" | grepl("^SEM ", x)] <- "0000"
  curto <- nchar(x) < 4
  x[curto] <- paste0(strrep("0", 4 - nchar(x[curto])), x[curto])
  x
}
ler_csv <- function(f, ...) {
  if (!file.exists(f)) return(NULL)
  if (grepl("\\.gz$", f)) {
    con <- gzfile(f, "rt", encoding = "UTF-8"); on.exit(close(con))
    return(as.data.table(read.csv(con, colClasses = "character", check.names = FALSE, ...)))
  }
  fread(f, colClasses = "character", encoding = "UTF-8", ...)
}
gravar_csv_gz <- function(dt, f) {
  con <- gzfile(f, "wt", encoding = "UTF-8"); on.exit(close(con))
  write.csv(dt, con, row.names = FALSE, na = "")
}
ler_config <- function(nome) {
  f <- p("config", nome)
  if (!file.exists(f)) return(data.table())
  d <- fread(f, colClasses = "character", encoding = "UTF-8", na.strings = NULL, fill = TRUE)
  for (c in names(d)) d[[c]] <- trimws(d[[c]])
  d
}

# --------------------------------------------------------------- configuração --
programas <- ler_config("programas.csv")
regras    <- ler_config("regras.csv")
manuais   <- ler_config("vinculos_manuais.csv")
termos_cand <- if (file.exists(p("config", "termos_candidatos.txt"))) {
  t <- trimws(readLines(p("config", "termos_candidatos.txt"), encoding = "UTF-8", warn = FALSE))
  t[t != "" & !startsWith(t, "#")]
} else character(0)

stopifnot(nrow(programas) > 0, all(c("id", "nome") %in% names(programas)))
programas[, residual := residual %in% c("1", "sim", "TRUE", "true")]
if (nrow(regras)) {
  desconhecidos <- setdiff(regras$programa_id, programas$id)
  if (length(desconhecidos)) aviso("regras.csv cita programas inexistentes: %s", paste(desconhecidos, collapse = ", "))
  if (!"exige_texto" %in% names(regras)) regras[, exige_texto := ""]
  regras[is.na(exige_texto), exige_texto := ""]
  for (i in seq_len(nrow(regras))) if (nzchar(regras$exige_texto[i])) {
    ok <- tryCatch({ grepl(regras$exige_texto[i], "teste", perl = TRUE); TRUE }, error = function(e) FALSE)
    if (!ok) { aviso("exige_texto inválido em regras.csv (linha %d); filtro ignorado.", i + 1); regras$exige_texto[i] <- "" }
  }
  for (i in seq_len(nrow(regras))) if (regras$tipo[i] %in% c("texto", "fonte")) {
    ok <- tryCatch({ grepl(regras$valor[i], "teste", perl = TRUE); TRUE }, error = function(e) FALSE)
    if (!ok) { aviso("regex inválida em regras.csv (linha %d): %s", i + 1, regras$valor[i]); regras$tipo[i] <- "invalida" }
  }
}

# -------------------------------------------------------------- coleta SIOP --
siop <- function(ano, ...) {
  for (t in 1:3) {
    r <- tryCatch(orcamentoBR::despesaDetalhada(exercicio = ano, ...), error = function(e) e)
    if (!inherits(r, "error")) {
      r <- as.data.table(r)
      return(r)
    }
    msg("   tentativa %d falhou: %s", t, conditionMessage(r))
    Sys.sleep(5 * t)
  }
  NULL
}

padronizar_siop <- function(d) {
  if (is.null(d) || !nrow(d)) return(NULL)
  esperadas <- c("Orgao_cod", "UO_cod", "Programa_cod", "Acao_cod", "PlanoOrcamentario_cod")
  faltam <- setdiff(esperadas, names(d))
  if (length(faltam)) stop("colunas ausentes no retorno do orcamentoBR: ", paste(faltam, collapse = ", "),
                           " | recebidas: ", paste(names(d), collapse = ", "))
  desc <- function(col) if (col %in% names(d)) as.character(d[[col]]) else ""
  out <- data.table(
    og = as.character(d$Orgao_cod), ogd = desc("Orgao_desc"),
    uo = as.character(d$UO_cod), uod = desc("UO_desc"),
    pr = as.character(d$Programa_cod), prd = desc("Programa_desc"),
    ac = toupper(as.character(d$Acao_cod)), acd = desc("Acao_desc"),
    po = pad_po(d$PlanoOrcamentario_cod), pod = desc("PlanoOrcamentario_desc"),
    ploa = num(d$ploa), loa = num(d$loa), atual = num(d$loa_mais_credito),
    emp = num(d$empenhado), liq = num(d$liquidado), pago = num(d$pago)
  )
  out[, k := paste(uo, ac, po, sep = ".")]
  # agrega eventuais duplicidades (mesma chave em programas distintos é rara, mas possível)
  out[, c(lapply(.SD, sum), list(og = og[1], ogd = ogd[1], uod = uod[1], pr = pr[1], prd = prd[1],
                                 acd = acd[1], pod = pod[1])),
      by = .(k, uo, ac, po), .SDcols = METRICAS]
}

coletar_ano <- function(ano) {
  f_cache <- p("cache", "siop", sprintf("siop_%d.csv", ano))
  cache <- ler_csv(f_cache)
  if (!is.null(cache)) for (m in METRICAS) cache[[m]] <- num(cache[[m]])
  msg("SIOP %d: lista de órgãos", ano)
  orgs <- siop(ano, Orgao = TRUE, UO = TRUE, valorPLOA = FALSE, valorLOA = FALSE,
               valorEmpenhado = FALSE, valorLiquidado = FALSE, valorPago = FALSE)
  if (is.null(orgs) || !nrow(orgs)) {
    if (!is.null(cache)) { aviso("SIOP indisponível para %d; usando a última coleta guardada.", ano); return(cache) }
    aviso("SIOP indisponível para %d e não há coleta anterior.", ano); return(NULL)
  }
  cods <- sort(unique(as.character(orgs$Orgao_cod)))
  partes <- vector("list", length(cods)); names(partes) <- cods
  for (og in cods) {
    cod_og <- og
    d <- tryCatch(padronizar_siop(siop(ano, Orgao = og, UO = TRUE, Programa = TRUE, Acao = TRUE,
                                       PlanoOrcamentario = TRUE)),
                  error = function(e) { aviso("%d/%s: %s", ano, og, conditionMessage(e)); NULL })
    if (is.null(d)) {
      if (!is.null(cache) && nrow(cache[og == cod_og])) {
        aviso("Órgão %s (%d) não respondeu; mantida a coleta anterior.", og, ano)
        d <- cache[og == cod_og]
      } else next
    }
    partes[[og]] <- d
  }
  dados <- rbindlist(partes, fill = TRUE, use.names = TRUE)
  if (!nrow(dados)) return(cache)
  fwrite(dados, f_cache)
  msg("SIOP %d: %d linhas (UO x Ação x PO) em %d órgãos", ano, nrow(dados), uniqueN(dados$og))
  dados
}

coletar_fonte <- function(ano, orgao) {
  f_cache <- p("cache", "siop", sprintf("fonte_%d_%s.csv", ano, orgao))
  d <- siop(ano, Orgao = orgao, UO = TRUE, Acao = TRUE, PlanoOrcamentario = TRUE, Fonte = TRUE)
  if (is.null(d) || !nrow(d)) {
    cache <- ler_csv(f_cache)
    if (!is.null(cache)) for (m in METRICAS) cache[[m]] <- num(cache[[m]])
    if (!is.null(cache)) aviso("Detalhe por fonte do órgão %s (%d) indisponível; usando coleta anterior.", orgao, ano)
    return(cache)
  }
  out <- data.table(
    k = paste(as.character(d$UO_cod), toupper(as.character(d$Acao_cod)), pad_po(d$PlanoOrcamentario_cod), sep = "."),
    fonte = as.character(d$Fonte_cod),
    fonted = if ("Fonte_desc" %in% names(d)) as.character(d$Fonte_desc) else "",
    ploa = num(d$ploa), loa = num(d$loa), atual = num(d$loa_mais_credito),
    emp = num(d$empenhado), liq = num(d$liquidado), pago = num(d$pago))
  fwrite(out, f_cache)
  out
}

siop_anos <- list()
for (a in ANOS) {
  d <- coletar_ano(a)
  if (!is.null(d) && nrow(d)) { d[, ano := a]; siop_anos[[as.character(a)]] <- d }
}
if (!length(siop_anos)) stop("Nenhum exercício pôde ser coletado do SIOP. Nada a publicar.")
base <- rbindlist(siop_anos, fill = TRUE, use.names = TRUE)
base[, txt := norm_txt(paste(acd, "|", pod))]
ANOS_OK <- sort(unique(base$ano))
msg("Base consolidada: %d linhas em %s", nrow(base), paste(ANOS_OK, collapse = ", "))

# ---------------------------------------------------------------- classificação --
vinc <- list()
casa <- function(r, d) {
  vals <- strsplit(r$valor, "[;|]")[[1]]; vals <- trimws(vals)
  esc <- if (!is.null(r$escopo) && !is.na(r$escopo)) r$escopo else ""
  sel <- switch(r$tipo,
    uo       = d$uo %in% vals,
    acao     = d$ac %in% toupper(vals),
    programa = d$pr %in% vals,
    po       = d$k %in% toupper(vals),
    texto    = grepl(r$valor, d$txt, perl = TRUE),
    rep(FALSE, nrow(d)))
  if (nzchar(esc) && r$tipo != "fonte") sel <- sel & (d$og == esc | d$uo == esc)
  sel
}

regras_nr  <- regras[programa_id %in% programas[residual == FALSE]$id & tipo != "fonte"]
regras_res <- regras[programa_id %in% programas[residual == TRUE]$id & tipo != "fonte"]
regras_fon <- regras[tipo == "fonte"]

# linhas que casaram com uma regra mas foram barradas pelo filtro exige_texto
# (ex.: POs do FNDCT sem relação com transformação digital) vão para a aba de revisão
rejeitados <- data.table()
aplicar <- function(rr, d) {
  out <- list()
  for (i in seq_len(nrow(rr))) {
    s <- casa(rr[i], d)
    filtro <- if (!is.null(rr$exige_texto)) rr$exige_texto[i] else ""
    if (!is.na(filtro) && nzchar(filtro)) {
      passa <- grepl(filtro, d$txt, perl = TRUE)
      barr <- s & !passa
      if (any(barr)) rejeitados <<- rbind(rejeitados, data.table(p = rr$programa_id[i], ano = d$ano[barr], k = d$k[barr]))
      s <- s & passa
    }
    if (any(s)) out[[length(out) + 1]] <- data.table(p = rr$programa_id[i], ano = d$ano[s], k = d$k[s],
                                                     origem = sprintf("regra %s: %s", rr$tipo[i], rr$valor[i]))
  }
  rbindlist(out)
}
v_auto <- aplicar(regras_nr, base)
v_auto <- unique(v_auto, by = c("p", "ano", "k"))

# vínculos manuais (incluir / excluir), valem para todos os exercícios
v_man_inc <- data.table(); v_man_exc <- data.table()
if (nrow(manuais)) {
  manuais[, chave := toupper(chave)]
  manuais[, fator := ifelse(is.na(suppressWarnings(as.numeric(fator))) | fator == "", 1, as.numeric(fator))]
  inv <- manuais[!programa_id %in% programas$id]
  if (nrow(inv)) aviso("vinculos_manuais.csv: programa inexistente em %d linha(s).", nrow(inv))
  v_man_exc <- manuais[tolower(operacao) == "excluir", .(p = programa_id, k = chave)]
  inc <- manuais[tolower(operacao) != "excluir" & programa_id %in% programas$id]
  if (nrow(inc)) {
    v_man_inc <- merge(inc[, .(p = programa_id, k = chave, fator, obs = observacao)],
                       unique(base[, .(k, ano)]), by = "k", allow.cartesian = TRUE)
    nao_achou <- setdiff(inc$chave, base$k)
    if (length(nao_achou)) aviso("Chaves de vinculos_manuais.csv sem correspondência no SIOP: %s", paste(head(nao_achou, 10), collapse = ", "))
  }
}

v_auto[, fator := 1]
if (nrow(v_man_exc)) v_auto <- v_auto[!v_man_exc, on = .(p, k)]
if (nrow(v_man_inc)) {
  v_auto <- v_auto[!v_man_inc, on = .(p, k, ano)]
  v_auto <- rbind(v_auto, v_man_inc[, .(p, ano, k, origem = paste0("manual", ifelse(obs != "", paste0(": ", obs), "")), fator)], fill = TRUE)
}

# residual: só linhas sem nenhum outro programa
ja <- unique(v_auto[, .(ano, k)])
base_livre <- base[!ja, on = .(ano, k)]
v_res <- aplicar(regras_res, base_livre)
if (nrow(v_res)) {
  v_res <- unique(v_res, by = c("p", "ano", "k"))[, fator := 1]
  if (nrow(v_man_exc)) v_res <- v_res[!v_man_exc, on = .(p, k)]
}
vinculos <- rbind(v_auto, v_res, fill = TRUE)

# valores dos vínculos = valores do PO x fator
vinculos <- merge(vinculos, base[, c("ano", "k", METRICAS), with = FALSE], by = c("ano", "k"))
for (m in METRICAS) vinculos[[m]] <- vinculos[[m]] * vinculos$fator

# vínculos por fonte (ex.: FISTEL): valores só da parcela da fonte
for (i in seq_len(nrow(regras_fon))) {
  r <- regras_fon[i]
  orgs <- trimws(strsplit(r$escopo, "[;|]")[[1]]); orgs <- orgs[orgs != ""]
  if (!length(orgs)) { aviso("Regra de fonte de %s sem órgão no campo escopo.", r$programa_id); next }
  for (a in ANOS_OK) for (og in orgs) {
    fd <- coletar_fonte(a, og)
    if (is.null(fd) || !nrow(fd)) { aviso("Sem detalhe por fonte para %s/%d.", og, a); next }
    sel <- grepl(r$valor, norm_txt(paste(fd$fonte, fd$fonted)), perl = TRUE)
    if (!any(sel)) { aviso("Regra de fonte de %s não encontrou fontes no órgão %s em %d.", r$programa_id, og, a); next }
    ag <- fd[sel, lapply(.SD, function(x) sum(num(x))), by = k, .SDcols = METRICAS]
    ag[, `:=`(p = r$programa_id, ano = a, origem = sprintf("fonte: %s", r$valor), fator = NA_real_)]
    if (nrow(v_man_exc)) ag <- ag[!v_man_exc, on = .(p, k)]
    vinculos <- rbind(vinculos, ag[k %in% base[ano == a]$k], fill = TRUE)
  }
}
vinculos <- vinculos[rowSums(abs(as.matrix(vinculos[, ..METRICAS]))) > 0]
msg("Vínculos: %d (programa x exercício x PO)", nrow(vinculos))

# candidatos para a aba "Linhas a revisar":
#  (a) linhas retiradas automaticamente pelo filtro exige_texto (ex.: FNDCT sem termo de TD);
#  (b) linhas não classificadas que mencionam termos de tecnologia.
chaves_vinc <- unique(vinculos$k)
candidatos <- data.table()
if (nrow(rejeitados)) {
  rej <- unique(rejeitados)[ano >= ANO_CORRENTE - 1]
  rej <- rej[!vinculos[, .(p, k)], on = .(p, k)]          # já trazidas de volta manualmente
  rej <- merge(rej, base, by = c("ano", "k"))
  if (nrow(rej)) {
    rej <- rej[order(-ano, -atual)][, .SD[1], by = .(k, p)]
    nomes <- setNames(programas$nome_curto, programas$id)
    rej[, `:=`(termo = "", sug = p, motivo = sprintf("Retirada de %s: sem termo de transformação digital", nomes[p]))]
    candidatos <- rej[, !"p"]
  }
}
if (length(termos_cand)) {
  rx <- paste(termos_cand, collapse = "|")
  cand <- base[!k %in% chaves_vinc & !k %in% candidatos$k & grepl(rx, txt, perl = TRUE) & ano >= ANO_CORRENTE - 1]
  if (nrow(cand)) {
    cand <- cand[order(-ano, -atual)][, .SD[1], by = k]
    cand[, termo := vapply(txt, function(t) { h <- termos_cand[vapply(termos_cand, function(z) grepl(z, t, perl = TRUE), TRUE)]; paste(head(h, 3), collapse = ", ") }, "")]
    cand[, `:=`(sug = "", motivo = "Menciona termo de tecnologia")]
    candidatos <- rbind(candidatos, head(cand[order(-atual)], 600), fill = TRUE)
  }
}
if (nrow(rejeitados)) msg("Retiradas pelo filtro exige_texto: %d linhas (exercício corrente e anterior) -> aba de revisão", sum(candidatos$sug != "", na.rm = TRUE))

# ------------------------------------------------------- retratos do SIOP --
# um arquivo por mês (AAAA-MM.csv), regravado a cada execução daquele mês;
# meses anteriores ficam congelados. Guarda só linhas vinculadas ou candidatas.
chaves_hist <- unique(c(chaves_vinc, candidatos$k))
ret_mes <- base[ano == ANO_CORRENTE & k %in% chaves_hist, c("k", METRICAS), with = FALSE]
if (nrow(ret_mes)) {
  ret_mes[, data := as.character(HOJE)]
  fwrite(ret_mes[, c("data", "k", METRICAS), with = FALSE],
         p("dados", "historico", sprintf("%d-%02d.csv", ANO_CORRENTE, MES_CORRENTE)))
}
retratos <- rbindlist(lapply(list.files(p("dados", "historico"), "^\\d{4}-\\d{2}\\.csv$", full.names = TRUE), function(f) {
  d <- fread(f, colClasses = "character")
  d[, `:=`(ano = as.integer(substr(basename(f), 1, 4)), mes = as.integer(substr(basename(f), 6, 7)))]
  d
}), fill = TRUE)
if (nrow(retratos)) {
  for (m in METRICAS) retratos[[m]] <- num(retratos[[m]])
} else {
  retratos <- data.table(k = character(), ano = integer(), mes = integer(),
                         ploa = numeric(), loa = numeric(), atual = numeric(), emp = numeric(), liq = numeric(), pago = numeric())
}

# --------------------------------------------- Portal da Transparência (mensal) --
portal_modo <- "movimento"
meses_portal <- data.table(ano = integer(), mes = integer())
portal_baixar <- function(aaaamm) {
  destino <- p("dados", "mensal", sprintf("portal_%s.csv.gz", aaaamm))
  if (file.exists(destino)) return(TRUE)
  if (!USAR_PORTAL) return(FALSE)
  url <- sprintf(Sys.getenv("PAINEL_PORTAL_URL", "https://portaldatransparencia.gov.br/download-de-dados/despesas-execucao/%s"), aaaamm)
  zip <- p("cache", "portal", sprintf("%s.zip", aaaamm))
  ok <- tryCatch({
    download.file(url, zip, mode = "wb", quiet = TRUE, method = "libcurl", headers = c("User-Agent" = UA))
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !file.exists(zip) || file.size(zip) < 1000) { msg("Portal %s: indisponível", aaaamm); return(FALSE) }
  sig <- readBin(zip, "raw", 2)
  if (!identical(sig, charToRaw("PK"))) { msg("Portal %s: resposta não é zip", aaaamm); unlink(zip); return(FALSE) }
  pasta <- p("cache", "portal", aaaamm); dir.create(pasta, showWarnings = FALSE)
  arqs <- unzip(zip, exdir = pasta)
  csv <- arqs[grepl("\\.csv$", arqs, ignore.case = TRUE)][1]
  if (is.na(csv)) { unlink(c(zip, pasta), recursive = TRUE); return(FALSE) }
  linha1 <- readLines(csv, n = 1, warn = FALSE)
  enc <- if (validUTF8(linha1)) "UTF-8" else "Latin-1"
  cab <- names(fread(csv, nrows = 0, sep = ";", encoding = enc))
  ncab <- norm_txt(if (enc == "Latin-1") iconv(cab, "latin1", "UTF-8") else cab)
  ncab <- gsub("[^a-z0-9 ()$]", "", ncab)
  achar <- function(padrao, excluir = NULL) {
    i <- which(grepl(padrao, ncab) & (if (is.null(excluir)) TRUE else !grepl(excluir, ncab)))
    if (!length(i)) stop("coluna não encontrada: ", padrao, " | cabeçalho: ", substr(paste(ncab, collapse = "; "), 1, 300))
    cab[i[1]]
  }
  cols <- c(uo = achar("codigo unidade orcamentaria"), ac = achar("^codigo acao"),
            po = achar("codigo plano orcamentario"),
            emp = achar("valor empenhado", "restos"), liq = achar("valor liquidado", "restos"),
            pago = achar("valor pago", "restos"))
  d <- fread(csv, sep = ";", encoding = enc, select = unname(cols), colClasses = "character", quote = "\"")
  setnames(d, unname(cols), names(cols))
  ag <- d[, .(emp = sum(num_br(emp)), liq = sum(num_br(liq)), pago = sum(num_br(pago))),
          by = .(k = paste(trimws(uo), toupper(trimws(ac)), pad_po(po), sep = "."))]
  gravar_csv_gz(ag, destino)
  unlink(c(zip, pasta), recursive = TRUE)
  msg("Portal %s: %d POs agregados", aaaamm, nrow(ag))
  TRUE
}

for (a in ANOS_OK) {
  ult <- if (a == ANO_CORRENTE) MES_CORRENTE - 1 else 12
  for (m in seq_len(max(ult, 0))) {
    aaaamm <- sprintf("%d%02d", a, m)
    ok <- tryCatch(portal_baixar(aaaamm), error = function(e) { aviso("Portal %s: %s", aaaamm, conditionMessage(e)); FALSE })
    if (ok) meses_portal <- rbind(meses_portal, data.table(ano = a, mes = m))
  }
}
portal <- rbindlist(lapply(seq_len(nrow(meses_portal)), function(i) {
  a <- meses_portal$ano[i]; m <- meses_portal$mes[i]
  d <- ler_csv(p("dados", "mensal", sprintf("portal_%d%02d.csv.gz", a, m)))
  d[, c("emp", "liq", "pago") := lapply(.SD, num), .SDcols = c("emp", "liq", "pago")]
  d[, `:=`(ano = a, mes = m)]
}), fill = TRUE)
if (!nrow(portal)) portal <- data.table(k = character(), emp = numeric(), liq = numeric(), pago = numeric(),
                                        ano = integer(), mes = integer())

conciliacao <- list()
if (nrow(portal)) {
  # o arquivo traz o movimento do mês ou o acumulado no ano? decide pelo
  # exercício fechado mais recente com os 12 meses, comparando com o SIOP.
  fechados <- meses_portal[, .N, by = ano][N == 12 & ano < ANO_CORRENTE]$ano
  if (length(fechados)) {
    a <- max(fechados)
    s_siop <- sum(base[ano == a]$emp)
    s_soma <- sum(portal[ano == a]$emp); s_dez <- sum(portal[ano == a & mes == 12]$emp)
    if (s_siop > 0 && abs(s_dez - s_siop) < abs(s_soma - s_siop)) portal_modo <- "acumulado"
    msg("Portal: modo '%s' (SIOP %.0f | soma meses %.0f | dezembro %.0f)", portal_modo, s_siop, s_soma, s_dez)
  }
  if (portal_modo == "acumulado") {
    setorder(portal, k, ano, mes)
    portal[, `:=`(emp = emp - shift(emp, fill = 0), liq = liq - shift(liq, fill = 0), pago = pago - shift(pago, fill = 0)), by = .(k, ano)]
  }
  for (a in ANOS_OK) {
    mm <- meses_portal[ano == a]$mes
    if (!length(mm)) next
    s_portal <- sum(portal[ano == a]$emp)
    s_siop   <- sum(base[ano == a]$emp)
    # chaves do Portal que não existem no SIOP (diferença de codificação de PO)
    sem_par <- portal[ano == a & !k %in% base[ano == a]$k, sum(abs(emp))]
    conciliacao[[as.character(a)]] <- list(ano = a, meses = length(mm), ultimo_mes = max(mm),
      empenhado_portal = round(s_portal), empenhado_siop = round(s_siop),
      parcela_sem_correspondencia = if (s_portal != 0) round(sem_par / abs(s_portal), 4) else 0)
    if (s_portal != 0 && sem_par / abs(s_portal) > 0.05)
      aviso("Portal %d: %.1f%% do empenhado não casa com chaves do SIOP (codificação de PO).", a, 100 * sem_par / abs(s_portal))
  }
} else if (USAR_PORTAL) {
  aviso("Nenhum arquivo mensal do Portal da Transparência disponível; a série mensal usa só os retratos do SIOP.")
}

# ------------------------------------------------------------- série mensal --
chaves_m <- unique(vinculos$k)
mensal <- list()
mk <- function() rep(NA_real_, 12)
for (a in ANOS_OK) {
  pa <- split(portal[ano == a & k %in% chaves_m], by = "k")
  ra <- split(retratos[ano == a & k %in% chaves_m], by = "k")
  ba <- split(base[ano == a & k %in% chaves_m], by = "k")
  meses_p <- meses_portal[ano == a]$mes
  vazio_p <- portal[0]; vazio_r <- retratos[0]
  for (kk in names(ba)) {
    e <- mk(); l <- mk(); g <- mk(); dt <- mk()
    if (length(meses_p)) {
      e[meses_p] <- 0; l[meses_p] <- 0; g[meses_p] <- 0
      x <- if (is.null(pa[[kk]])) vazio_p else pa[[kk]]
      if (nrow(x)) { e[x$mes] <- x$emp; l[x$mes] <- x$liq; g[x$mes] <- x$pago }
    }
    r <- if (is.null(ra[[kk]])) vazio_r else ra[[kk]]
    if (nrow(r)) dt[r$mes] <- r$atual
    # meses sem Portal: diferença entre retratos acumulados do SIOP
    if (nrow(r)) for (m in setdiff(r$mes, meses_p)) {
      ant_e <- sum(e[seq_len(m - 1)], na.rm = TRUE); ant_l <- sum(l[seq_len(m - 1)], na.rm = TRUE); ant_g <- sum(g[seq_len(m - 1)], na.rm = TRUE)
      # se houver meses sem dado antes de m, a diferença inclui também esses meses
      e[m] <- r[mes == m]$emp - ant_e; l[m] <- r[mes == m]$liq - ant_l; g[m] <- r[mes == m]$pago - ant_g
    }
    # exercício corrente: o último ponto é sempre o SIOP de hoje. O valor do mês
    # corrente = SIOP - soma dos meses conhecidos; se faltar algum mês do Portal
    # (o arquivo mensal sai com atraso), a diferença cobre também esses meses.
    if (a == ANO_CORRENTE && MES_CORRENTE >= 1) {
      b <- ba[[kk]]
      prev <- seq_len(MES_CORRENTE - 1)
      e[MES_CORRENTE] <- b$emp - sum(e[prev], na.rm = TRUE); l[MES_CORRENTE] <- b$liq - sum(l[prev], na.rm = TRUE); g[MES_CORRENTE] <- b$pago - sum(g[prev], na.rm = TRUE)
      dt[MES_CORRENTE] <- b$atual
    }
    if (all(is.na(e)) && all(is.na(dt))) next
    mensal[[length(mensal) + 1]] <- list(a = a, k = kk, e = round(e), l = round(l), p = round(g), d = round(dt))
  }
}
fonte_mes <- lapply(ANOS_OK, function(a) {
  f <- rep(NA_character_, 12)
  f[meses_portal[ano == a]$mes] <- "portal"
  rm <- setdiff(unique(retratos[ano == a]$mes), meses_portal[ano == a]$mes)
  f[rm] <- "siop"
  if (a == ANO_CORRENTE) {
    f[MES_CORRENTE] <- "parcial"
    f[seq_len(MES_CORRENTE - 1)][is.na(f[seq_len(MES_CORRENTE - 1)])] <- "lacuna"
  }
  list(ano = a, fonte = I(f))
})

# ------------------------------------------------------------- saída JSON --
linhas_ids <- unique(c(vinculos$k, candidatos$k))
linhas <- base[k %in% linhas_ids]
setorder(linhas, ano, og, uo, ac, po)
cand_out <- if (nrow(candidatos)) candidatos[, .(k, ano, termo, sug, motivo)] else data.table()

lista_linhas <- lapply(seq_len(nrow(linhas)), function(i) {
  x <- linhas[i]
  list(a = x$ano, k = x$k, og = x$og, ogd = x$ogd, uo = x$uo, uod = x$uod, pr = x$pr, prd = x$prd,
       ac = x$ac, acd = x$acd, po = x$po, pod = x$pod,
       v = round(c(x$ploa, x$loa, x$atual, x$emp, x$liq, x$pago)))
})
lista_vinc <- lapply(seq_len(nrow(vinculos)), function(i) {
  x <- vinculos[i]
  list(p = x$p, a = x$ano, k = x$k, o = x$origem,
       v = round(c(x$ploa, x$loa, x$atual, x$emp, x$liq, x$pago)))
})

painel <- list(
  meta = list(
    gerado_em = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    data_referencia = as.character(HOJE),
    ano_corrente = ANO_CORRENTE, mes_corrente = MES_CORRENTE,
    anos = I(ANOS_OK),
    metricas = c("PLOA", "LOA", "Dotação atual", "Empenhado", "Liquidado", "Pago"),
    portal_modo = portal_modo,
    fonte_mes = fonte_mes,
    conciliacao = unname(conciliacao),
    avisos = I(unique(avisos)),
    simulado = SIMULADO,
    versao_orcamentoBR = tryCatch(as.character(packageVersion("orcamentoBR")), error = function(e) NA)
  ),
  programas = lapply(seq_len(nrow(programas)), function(i) {
    x <- programas[i]
    list(id = x$id, nome = x$nome, curto = if (!is.null(x$nome_curto) && nzchar(x$nome_curto)) x$nome_curto else x$nome,
         orgao = x$orgao_lider, descricao = x$descricao, nota = x$nota, residual = isTRUE(x$residual),
         regras = lapply(which(regras$programa_id == x$id), function(j)
           list(tipo = regras$tipo[j], valor = regras$valor[j], escopo = regras$escopo[j])))
  }),
  linhas = lista_linhas,
  vinculos = lista_vinc,
  mensal = mensal,
  candidatos = if (nrow(cand_out)) lapply(seq_len(nrow(cand_out)), function(i) as.list(cand_out[i])) else list()
)

json <- toJSON(painel, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
writeLines(json, p("site", "dados", "painel.json"), useBytes = TRUE)

# CSV para download: um registro por programa x exercício x PO
csv <- merge(vinculos[, c("p", "ano", "k", "origem", METRICAS), with = FALSE],
             base[, .(ano, k, og, ogd, uo, uod, pr, prd, ac, acd, po, pod)], by = c("ano", "k"))
csv <- merge(csv, programas[, .(p = id, programa_td = nome)], by = "p")
setnames(csv, c("p", "ano", "k", "og", "ogd", "uo", "uod", "pr", "prd", "ac", "acd", "po", "pod", METRICAS),
         c("programa_td_id", "exercicio", "chave", "orgao", "orgao_desc", "uo", "uo_desc", "programa_ppa",
           "programa_ppa_desc", "acao", "acao_desc", "po", "po_desc", "ploa", "loa", "dotacao_atual",
           "empenhado", "liquidado", "pago"))
fwrite(csv[order(programa_td_id, exercicio, chave)], p("site", "dados", "linhas.csv"), bom = TRUE)

msg("OK: %d linhas, %d vínculos, %d séries mensais, %d candidatos, %d avisos -> site/dados/painel.json (%.1f MB)",
    length(lista_linhas), length(lista_vinc), length(mensal), nrow(cand_out), length(unique(avisos)),
    file.size(p("site", "dados", "painel.json")) / 1e6)
