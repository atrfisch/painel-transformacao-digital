# Painel da Transformação Digital – execução orçamentária

Painel estático (GitHub Pages) que acompanha, **no nível de plano orçamentário (PO)**, a dotação e a execução das ações de transformação digital do Governo Federal:

PBIA · Supercomputadores · FNDCT · FUST · FISTEL · CIN · NIB Missão 4 · Nuvem de Governo · PNID · Gov.br · Outras ações de TD

Para cada programa, órgão ou PO o painel mostra:

- PLOA, LOA, dotação atual, empenhado, liquidado e pago;
- comparação com o exercício anterior (dotação anual e execução até a mesma data);
- saldo a empenhar, saldo a pagar e projeção de fim de ano pelo calendário do ano anterior.
- série mês a mês (empenhado, liquidado e pago) e acumulado;

## Estrutura

```
config/programas.csv          lista de programas (nome, órgão, nota)
config/regras.csv             regras automáticas de classificação
config/vinculos_manuais.csv   inclusões e exclusões manuais de POs
config/termos_candidatos.txt  termos que mandam uma linha para "Linhas a revisar"
scripts/coleta.R              coleta (orcamentoBR/SIOP + Portal da Transparência) e geração do JSON
site/                         página publicada (index.html, assets/)
dados/historico/              retratos mensais do SIOP (gravados pelo robô)
dados/mensal/                 execução mensal por PO vinda do Portal (gravada pelo robô)
.github/workflows/atualizar.yml  atualização automática e publicação
```

## Como publicar (pela interface web do GitHub)

1. Crie um repositório novo (por exemplo `painel-transformacao-digital`).
2. Em **Add file → Upload files**, arraste as pastas `config`, `scripts`, `site` e `dados` e os arquivos `README.md` e `.gitignore`. Faça o commit.
3. A pasta `.github` costuma ficar de fora do upload. Crie o arquivo à mão: **Add file → Create new file**, digite o nome `.github/workflows/atualizar.yml` (com as barras) e cole o conteúdo do arquivo.
4. **Settings → Pages → Build and deployment → Source: GitHub Actions.**
5. **Settings → Actions → General → Workflow permissions → Read and write permissions.**
6. Na aba **Actions**, abra “Atualizar painel” e clique em **Run workflow**. A primeira execução demora mais (baixa os arquivos mensais do Portal de três exercícios).
7. O endereço fica `https://SEU-USUARIO.github.io/NOME-DO-REPOSITORIO/`.

Depois disso, o painel se atualiza sozinho nos dias úteis e também sempre que você editar algo em `config/`, `scripts/` ou `site/`.

## Como ajustar a classificação

**Regras automáticas** (`config/regras.csv`), uma por linha:

| tipo | o que compara | exemplo de `valor` |
|---|---|---|
| `uo` | código da UO | `24901` |
| `acao` | código da ação (use `escopo` para limitar a uma UO) | `0A29` |
| `programa` | código do programa do PPA | `2304` |
| `po` | chave completa `UO.Ação.PO` | `24101.15XB.0000` |
| `texto` | expressão regular sobre a descrição da ação e do PO, em minúsculas e sem acento | `supercomput\|alto desempenho` |
| `fonte` | expressão regular sobre a fonte de recursos; `escopo` = órgão a consultar | `fistel`, escopo `41000` |

**Ajustes manuais** (`config/vinculos_manuais.csv`):

```
programa_id,chave,operacao,fator,observacao
PBIA,24901.XXXX.YYYY,incluir,1,PO do FNDCT ligado ao PBIA
PBIA,26101.2000.0006,excluir,,não é IA
NIB_M4,24901.0A29.0000,incluir,0.3,30% da subvenção atribuída à Missão 4
```

A chave aparece em cada linha do painel. Na aba **Linhas a revisar** há um botão que copia a linha pronta para incluir um PO; no detalhe de cada PO há um botão que copia a linha de exclusão.

## Fontes e método

- **SIOP**, via `orcamentoBR::despesaDetalhada()`, por Órgão × UO × Programa × Ação × PO (e por fonte, para o FISTEL). Orçamentos Fiscal e da Seguridade.
- **Série mensal**: o orcamentoBR não tem dimensão de mês. O painel usa o arquivo mensal *Execução da despesa* do Portal da Transparência (mesma base SIAFI, com código de PO) para os meses fechados, e o SIOP do dia para o mês corrente. A aba Metodologia mostra a conciliação Portal × SIOP.
- Valores correntes; restos a pagar não incluídos; Orçamento de Investimento das estatais e crédito do BNDES ficam de fora.

## Teste local (opcional)

```
Rscript -e 'install.packages(c("orcamentoBR","jsonlite","data.table"))'
Rscript scripts/coleta.R
cd site && python3 -m http.server 8000
```
