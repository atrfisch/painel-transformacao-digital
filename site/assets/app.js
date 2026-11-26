/* Painel da Transformação Digital — lógica da página (sem dependências além do Chart.js) */
(() => {
  'use strict';

  // ------------------------------------------------------------ constantes
  const V = { PLOA: 0, LOA: 1, ATUAL: 2, EMP: 3, LIQ: 4, PAGO: 5 };
  const MESES = ['jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago', 'set', 'out', 'nov', 'dez'];
  const COR = { emp: '#1351B4', liq: '#0F9D8A', pago: '#E8590C', dot: '#C5D4EB', dotLinha: '#5C6E8A', ant: '#8C8C8C', grade: '#EDEDED', texto: '#555555' };
  const POR_PAGINA = 50;
  
  const S = { ano: null, prog: 'TODOS', orgao: '', busca: '', aba: 'geral', progSel: null,
              ordPos: { col: 'atual', dir: -1 }, pagPos: 0, revFiltro: null, abertos: new Set() };
  const I = {}; let D = null; const graficos = {};

  // ------------------------------------------------------------- formatação
  const nf0 = new Intl.NumberFormat('pt-BR', { maximumFractionDigits: 0 });
  const nf1 = new Intl.NumberFormat('pt-BR', { minimumFractionDigits: 1, maximumFractionDigits: 1 });
  const nf2 = new Intl.NumberFormat('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
  function rs(v, curto = false) {
    if (v == null || isNaN(v)) return '–';
    const a = Math.abs(v), s = v < 0 ? '−' : '';
    const p = curto ? '' : 'R$ ';
    if (a >= 1e9) return `${s}${p}${nf2.format(a / 1e9)} bi`;
    if (a >= 1e6) return `${s}${p}${nf1.format(a / 1e6)} mi`;
    if (a >= 1e3) return `${s}${p}${nf0.format(a / 1e3)} mil`;
    return `${s}${p}${nf0.format(a)}`;
  }
  const pct = (x) => (x == null || !isFinite(x)) ? '–' : `${nf1.format(100 * x)}%`;
  const razao = (a, b) => (b ? a / b : null);
  function delta(atual, ant) {
    if (ant == null || !ant || atual == null) return '<span>–</span>';
    const d = atual / ant - 1;
    const cls = d >= 0 ? 'delta-pos' : 'delta-neg';
    return `<span class="${cls}">${d >= 0 ? '+' : '−'}${nf1.format(Math.abs(100 * d))}%</span>`;
  }
  const esc = (s) => String(s ?? '').replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const $ = (id) => document.getElementById(id);
  function toast(t) { const el = $('toast'); el.textContent = t; el.classList.add('ver'); clearTimeout(toast.t); toast.t = setTimeout(() => el.classList.remove('ver'), 2200); }
  const semAcento = (s) => String(s || '').normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();

  // ----------------------------------------------------------------- dados
  function indexar() {
    const comPO = new Set(D.vinculos.map((v) => v.p));
    D.programas = D.programas.filter((p) => comPO.has(p.id));
    I.linha = new Map(); D.linhas.forEach((l) => I.linha.set(`${l.a}|${l.k}`, l));
    I.mensal = new Map(); D.mensal.forEach((m) => I.mensal.set(`${m.a}|${m.k}`, m));
    I.prog = new Map(D.programas.map((p) => [p.id, p]));
    I.vincAno = new Map(); I.vincChave = new Map();
    D.vinculos.forEach((v) => {
      if (!I.vincAno.has(v.a)) I.vincAno.set(v.a, []);
      I.vincAno.get(v.a).push(v);
      const c = `${v.a}|${v.k}`;
      if (!I.vincChave.has(c)) I.vincChave.set(c, []);
      I.vincChave.get(c).push(v);
    });
    I.fonteMes = new Map((D.meta.fonte_mes || []).map((f) => [f.ano, f.fonte]));
    D.linhas.forEach((l) => { l._busca = semAcento(`${l.k} ${l.uo} ${l.uod} ${l.ac} ${l.acd} ${l.po} ${l.pod} ${l.ogd} ${l.prd}`); });
    const ref = new Date(D.meta.data_referencia + 'T12:00:00');
    I.dia = ref.getDate(); I.diasMes = new Date(ref.getFullYear(), ref.getMonth() + 1, 0).getDate();
  }

  function passaFiltro(l) {
    if (!l) return false;
    if (S.orgao && l.og !== S.orgao) return false;
    if (S.busca && !l._busca.includes(semAcento(S.busca))) return false;
    return true;
  }

  // itens de um exercício: {k, v, vinc, linha, progs}
  function selecao(ano, prog = S.prog) {
    const vs = I.vincAno.get(ano) || [];
    const porChave = new Map();
    for (const v of vs) {
      if (prog !== 'TODOS' && v.p !== prog) continue;
      const l = I.linha.get(`${ano}|${v.k}`);
      if (!passaFiltro(l)) continue;
      const atual = porChave.get(v.k);
      if (!atual || v.v[V.ATUAL] > atual.vinc.v[V.ATUAL]) porChave.set(v.k, { k: v.k, vinc: v, linha: l });
    }
    const out = [];
    porChave.forEach((it) => {
      it.v = it.vinc.v;
      it.progs = [...new Set((I.vincChave.get(`${ano}|${it.k}`) || []).map((x) => x.p))];
      out.push(it);
    });
    return out;
  }
  function soma(itens) {
    const s = [0, 0, 0, 0, 0, 0];
    itens.forEach((it) => { for (let i = 0; i < 6; i++) s[i] += it.v[i] || 0; });
    return s;
  }
  // série mensal de um item (escalada pela parcela do vínculo)
  function serieItem(ano, it) {
    const m = I.mensal.get(`${ano}|${it.k}`); if (!m) return null;
    const l = it.linha, v = it.v;
    const f = (i) => (l.v[i] ? v[i] / l.v[i] : (v[i] ? 1 : 1));
    const escalar = (arr, i) => arr.map((x) => (x == null ? null : x * f(i)));
    return { e: escalar(m.e, V.EMP), l: escalar(m.l, V.LIQ), p: escalar(m.p, V.PAGO), d: escalar(m.d, V.ATUAL) };
  }
  function serieAgregada(ano, itens) {
    const fonte = I.fonteMes.get(ano) || Array(12).fill(null);
    const out = { e: Array(12).fill(null), l: Array(12).fill(null), p: Array(12).fill(null), d: Array(12).fill(null) };
    for (let m = 0; m < 12; m++) if (TEM_DADO.has(fonte[m])) { out.e[m] = 0; out.l[m] = 0; out.p[m] = 0; }
    const temD = Array(12).fill(false);
    itens.forEach((it) => {
      const s = serieItem(ano, it); if (!s) return;
      for (let m = 0; m < 12; m++) {
        ['e', 'l', 'p'].forEach((c) => { if (s[c][m] != null && out[c][m] != null) out[c][m] += s[c][m]; });
        if (s.d[m] != null) { out.d[m] = (out.d[m] || 0) + s.d[m]; temD[m] = true; }
      }
    });
    return out;
  }
  const TEM_DADO = new Set(['portal', 'siop', 'parcial']);
  // meses sem arquivo do Portal antes do mês corrente: o valor do SIOP de hoje cobre esse intervalo
  function lacunas(ano) { const f = I.fonteMes.get(ano) || []; return f.map((x, i) => (x === 'lacuna' ? i : -1)).filter((i) => i >= 0); }
  function rotulosMeses(ano) {
    const f = I.fonteMes.get(ano) || [], lac = lacunas(ano);
    return MESES.map((m, i) => {
      if (f[i] !== 'parcial') return m;
      return lac.length ? `${MESES[lac[0]]}–${m}*` : `${m}*`;
    });
  }
  const tracejaLacuna = { borderDash: (ctx) => ((ctx.p0.skip || ctx.p1.skip) ? [5, 5] : undefined) };
  const acumular = (arr) => { let s = 0; return arr.map((x) => (x == null ? null : (s += x))); };
  function mesRef(ano) { return ano === D.meta.ano_corrente ? D.meta.mes_corrente : 12; }
  function fracMes(ano) { return ano === D.meta.ano_corrente ? I.dia / I.diasMes : 1; }
  // acumulado do ano anterior até o mesmo ponto do calendário
  function acumMesmoPeriodo(serie, ano) {
    const M = mesRef(ano), fr = fracMes(ano);
    let s = 0, ok = false;
    for (let m = 0; m < M; m++) {
      const x = serie[m]; if (x == null) continue; ok = true;
      s += (m === M - 1) ? x * fr : x;
    }
    return ok ? s : null;
  }

  // --------------------------------------------------------------- gráficos
  Chart.defaults.font.family = 'Rawline, Raleway, system-ui, sans-serif';
  Chart.defaults.font.size = 12;
  Chart.defaults.color = COR.texto;
  Chart.defaults.plugins.legend.labels.boxWidth = 14;
  Chart.defaults.plugins.legend.labels.boxHeight = 8;
  Chart.defaults.plugins.tooltip.backgroundColor = '#071D41';
  Chart.defaults.plugins.tooltip.padding = 10;
  Chart.defaults.maintainAspectRatio = false;
  const eixoValor = { grid: { color: COR.grade }, border: { display: false }, ticks: { callback: (v) => rs(v, true) } };
  const eixoCat = { grid: { display: false }, border: { color: '#CCCCCC' } };
  const dicaValor = { callbacks: { label: (c) => `${c.dataset.label}: ${rs(c.parsed.y ?? c.parsed.x)}` } };
  function desenhar(id, cfg) {
    if (graficos[id]) graficos[id].destroy();
    const el = $(id); if (!el) return;
    graficos[id] = new Chart(el, cfg);
  }

  function graficoAcumulado(id, ano, itens, itensAnt) {
    const s = serieAgregada(ano, itens);
    const sa = itensAnt ? serieAgregada(ano - 1, itensAnt) : null;
    const tot = soma(itens);
    const M = mesRef(ano);
    const corta = (arr) => arr.map((x, i) => (i < M ? x : null));
    const ds = [
      { label: `Empenhado ${ano}`, data: corta(acumular(s.e)), borderColor: COR.emp, backgroundColor: COR.emp, borderWidth: 2.5, pointRadius: 3, pointHoverRadius: 6, tension: .15, spanGaps: true, segment: tracejaLacuna },
      { label: `Pago ${ano}`, data: corta(acumular(s.p)), borderColor: COR.pago, backgroundColor: COR.pago, borderWidth: 2.5, pointRadius: 3, pointHoverRadius: 6, tension: .15, spanGaps: true, segment: tracejaLacuna },
    ];
    if (sa && sa.e.some((x) => x != null)) ds.push({ label: `Empenhado ${ano - 1}`, data: acumular(sa.e), borderColor: COR.ant, backgroundColor: COR.ant, borderDash: [6, 4], borderWidth: 2, pointRadius: 0, pointHoverRadius: 5, tension: .15 });
    ds.push({ label: 'Dotação atual', data: Array(12).fill(tot[V.ATUAL]), borderColor: COR.dotLinha, backgroundColor: COR.dotLinha, borderDash: [2, 3], borderWidth: 1.5, pointRadius: 0, pointHoverRadius: 0 });
    desenhar(id, { type: 'line', data: { labels: MESES, datasets: ds },
      options: { interaction: { mode: 'index', intersect: false }, scales: { y: { ...eixoValor, beginAtZero: true }, x: eixoCat },
        plugins: { tooltip: dicaValor, legend: { position: 'bottom' } } } });
  }
  function graficoMensal(id, ano, itens) {
    const s = serieAgregada(ano, itens);
    const fonte = I.fonteMes.get(ano) || [];
    const lab = rotulosMeses(ano);
    const ds = [
      { label: 'Empenhado no mês', data: s.e, backgroundColor: COR.emp, borderRadius: 3, borderSkipped: 'start', barPercentage: .9, categoryPercentage: .78 },
      { label: 'Liquidado no mês', data: s.l, backgroundColor: COR.liq, borderRadius: 3, borderSkipped: 'start', barPercentage: .9, categoryPercentage: .78 },
      { label: 'Pago no mês', data: s.p, backgroundColor: COR.pago, borderRadius: 3, borderSkipped: 'start', barPercentage: .9, categoryPercentage: .78 },
    ];
    desenhar(id, { type: 'bar', data: { labels: lab, datasets: ds },
      options: { interaction: { mode: 'index', intersect: false }, scales: { y: eixoValor, x: eixoCat },
        plugins: { tooltip: dicaValor, legend: { position: 'bottom' } } } });
    return fonte;
  }
  function notaFonte(ano) {
    const f = I.fonteMes.get(ano) || [];
    const p = f.filter((x) => x === 'portal').length, s = f.filter((x) => x === 'siop').length;
    const partes = [];
    if (p) partes.push(`${p} mês(es) pelo arquivo mensal do Portal da Transparência`);
    if (s) partes.push(`${s} mês(es) pela diferença entre retratos do SIOP`);
    const lac = lacunas(ano);
    if (f.includes('parcial')) {
      const ini = lac.length ? MESES[lac[0]] : MESES[f.indexOf('parcial')];
      partes.push(`* ${lac.length ? `${ini} a ${MESES[f.indexOf('parcial')]}` : 'mês corrente'}: SIOP de ${dataBR(D.meta.data_referencia)} menos o acumulado dos meses anteriores${lac.length ? ' (o arquivo mensal do Portal ainda não saiu para esses meses; no gráfico acumulado, o trecho tracejado liga o último mês do Portal ao SIOP de hoje)' : ''}`);
    }
    if (!partes.length) return 'Ainda não há série mensal para este exercício.';
    return `Série mensal: ${partes.join('; ')}.`;
  }
  const dataBR = (s) => s ? s.split('-').reverse().join('/') : '';

  // ------------------------------------------------------------------- KPIs
  function kpis(alvo, ano, itens, itensAnt) {
    const t = soma(itens), ta = itensAnt ? soma(itensAnt) : null;
    const s = serieAgregada(ano, itens), sa = itensAnt ? serieAgregada(ano - 1, itensAnt) : null;
    const corrente = ano === D.meta.ano_corrente;
    const M = mesRef(ano);
    const periodo = corrente ? `até ${I.dia}/${MESES[M - 1]}` : 'no ano';
    function comp(i, serieAnt) {
      if (!ta) return '';
      if (i === V.LOA || i === V.ATUAL || !corrente) return `${ano - 1}: ${rs(ta[i])} · ${delta(t[i], ta[i])}`;
      const va = sa ? acumMesmoPeriodo(serieAnt, ano) : null;
      if (va == null) return `${ano - 1} (ano): ${rs(ta[i])}`;
      return `${ano - 1} ${periodo}: ${rs(va)} · ${delta(t[i], va)}`;
    }
    const saldo = t[V.ATUAL] - t[V.EMP];
    const cards = [
      { c: 'k-dot', r: 'LOA', v: t[V.LOA], sub: `PLOA ${rs(t[V.PLOA])}`, comp: comp(V.LOA) },
      { c: 'k-dot', r: 'Dotação atual', v: t[V.ATUAL], sub: `${t[V.ATUAL] >= t[V.LOA] ? '+' : '−'}${rs(Math.abs(t[V.ATUAL] - t[V.LOA]))} vs LOA`, comp: comp(V.ATUAL) },
      { c: 'k-emp', r: 'Empenhado', v: t[V.EMP], sub: `${pct(razao(t[V.EMP], t[V.ATUAL]))} da dotação`, comp: comp(V.EMP, sa && sa.e) },
      { c: 'k-liq', r: 'Liquidado', v: t[V.LIQ], sub: `${pct(razao(t[V.LIQ], t[V.EMP]))} do empenhado`, comp: comp(V.LIQ, sa && sa.l) },
      { c: 'k-pago', r: 'Pago', v: t[V.PAGO], sub: `${pct(razao(t[V.PAGO], t[V.ATUAL]))} da dotação`, comp: comp(V.PAGO, sa && sa.p) },
      { c: 'k-saldo', r: corrente ? 'Saldo a empenhar' : 'Não empenhado', v: saldo, sub: `${pct(razao(saldo, t[V.ATUAL]))} da dotação`, comp: `A pagar (empenhado − pago): ${rs(t[V.EMP] - t[V.PAGO])}` },
    ];
    alvo.innerHTML = cards.map((k) => `<div class="kpi ${k.c}"><div class="kpi-rotulo">${k.r}</div><div class="kpi-valor">${rs(k.v)}</div><div class="kpi-sub">${k.sub}</div>${k.comp ? `<div class="kpi-comp">${k.comp}</div>` : ''}</div>`).join('');
    return { t, ta, s, sa };
  }
  function projecao(alvo, ano, r) {
    if (ano !== D.meta.ano_corrente || !r.ta || !r.sa) { alvo.innerHTML = ''; return; }
    const ateAgoraAnt = acumMesmoPeriodo(r.sa.e, ano);
    const totAnt = r.ta[V.EMP];
    const parc = razao(ateAgoraAnt, totAnt);
    if (!parc || parc < 0.08) { alvo.innerHTML = ''; return; }
    const proj = r.t[V.EMP] / parc;
    const dif = r.t[V.ATUAL] - proj;
    const txt = dif >= 0
      ? `sobrariam <strong>${rs(dif)}</strong> (${pct(razao(dif, r.t[V.ATUAL]))} da dotação atual) sem empenho`
      : `faltariam <strong>${rs(-dif)}</strong> de dotação para manter o ritmo`;
    alvo.innerHTML = `<strong>Projeção para ${ano}:</strong> até ${I.dia}/${MESES[mesRef(ano) - 1]} de ${ano - 1} tinham sido empenhados ${pct(parc)} do total daquele ano. Se ${ano} seguir o mesmo calendário, o empenho chegará a <strong>${rs(proj)}</strong> e ${txt}. É uma referência de ritmo, não uma previsão.`;
  }

  // ---------------------------------------------------- tabela de programas
  function linhaPrograma(p, ano) {
    const it = selecao(ano, p.id), t = soma(it);
    const ita = I.vincAno.has(ano - 1) ? selecao(ano - 1, p.id) : null, ta = ita ? soma(ita) : null;
    return { p, it, t, ta, n: it.length };
  }
  function barraPct(x, cls = '') {
    const w = Math.max(0, Math.min(1, x || 0)) * 100;
    return `<span class="barra-pct"><span>${pct(x)}</span><span class="trilho"><span class="preench ${cls}" style="width:${w}%"></span></span></span>`;
  }
  function tabelaProgramas(ano) {
    const linhas = D.programas.map((p) => linhaPrograma(p, ano));
    const geral = selecao(ano, 'TODOS'), tg = soma(geral);
    const ga = I.vincAno.has(ano - 1) ? soma(selecao(ano - 1, 'TODOS')) : null;
    const cab = `<thead><tr><th>Programa</th><th class="n">POs</th><th class="n">LOA</th><th class="n">Dotação atual</th><th class="n">Δ dotação vs ${ano - 1}</th><th class="n">Empenhado</th><th class="n">% emp.</th><th class="n">Pago</th><th class="n">% pago</th><th class="n">Saldo a empenhar</th></tr></thead>`;
    const corpo = linhas.map((r) => `<tr class="clicavel" data-prog="${r.p.id}" tabindex="0">
      <td><strong>${esc(r.p.curto)}</strong><div class="cod">${esc(r.p.orgao || '')}</div></td>
      <td class="n">${r.n || '<span class="chip cinza">nenhum</span>'}</td>
      <td class="n">${rs(r.t[V.LOA])}</td><td class="n">${rs(r.t[V.ATUAL])}</td>
      <td class="n">${r.ta ? delta(r.t[V.ATUAL], r.ta[V.ATUAL]) : '–'}</td>
      <td class="n">${rs(r.t[V.EMP])}</td><td class="n">${barraPct(razao(r.t[V.EMP], r.t[V.ATUAL]))}</td>
      <td class="n">${rs(r.t[V.PAGO])}</td><td class="n">${barraPct(razao(r.t[V.PAGO], r.t[V.ATUAL]), 'pago')}</td>
      <td class="n">${rs(r.t[V.ATUAL] - r.t[V.EMP])}</td></tr>`).join('');
    const total = `<tr class="total"><td>Total sem dupla contagem</td><td class="n">${geral.length}</td><td class="n">${rs(tg[V.LOA])}</td><td class="n">${rs(tg[V.ATUAL])}</td><td class="n">${ga ? delta(tg[V.ATUAL], ga[V.ATUAL]) : '–'}</td><td class="n">${rs(tg[V.EMP])}</td><td class="n">${barraPct(razao(tg[V.EMP], tg[V.ATUAL]))}</td><td class="n">${rs(tg[V.PAGO])}</td><td class="n">${barraPct(razao(tg[V.PAGO], tg[V.ATUAL]), 'pago')}</td><td class="n">${rs(tg[V.ATUAL] - tg[V.EMP])}</td></tr>`;
    $('t-programas').innerHTML = cab + `<tbody>${corpo}${total}</tbody>`;
    $('t-programas').querySelectorAll('tr[data-prog]').forEach((tr) => {
      const ir = () => { S.progSel = tr.dataset.prog; trocarAba('programas'); };
      tr.addEventListener('click', ir); tr.addEventListener('keydown', (e) => { if (e.key === 'Enter') ir(); });
    });
    // gráfico horizontal: percentual executado (compara programas de tamanhos muito diferentes)
    const ord = linhas.filter((r) => r.t[V.ATUAL] > 0).sort((a, b) => razao(b.t[V.EMP], b.t[V.ATUAL]) - razao(a.t[V.EMP], a.t[V.ATUAL]));
    const pctDe = (r, i) => 100 * (razao(r.t[i], r.t[V.ATUAL]) || 0);
    desenhar('g-programas', { type: 'bar', data: { labels: ord.map((r) => r.p.curto), datasets: [
      { label: '% empenhado', data: ord.map((r) => pctDe(r, V.EMP)), backgroundColor: COR.emp, borderRadius: 3, borderSkipped: 'start', barPercentage: .9, categoryPercentage: .75 },
      { label: '% pago', data: ord.map((r) => pctDe(r, V.PAGO)), backgroundColor: COR.pago, borderRadius: 3, borderSkipped: 'start', barPercentage: .9, categoryPercentage: .75 },
    ] }, options: { indexAxis: 'y', interaction: { mode: 'index', intersect: false },
      scales: { x: { grid: { color: COR.grade }, border: { display: false }, min: 0, suggestedMax: 100, ticks: { callback: (v) => `${v}%` } }, y: eixoCat },
      plugins: { tooltip: { callbacks: {
        label: (c) => `${c.dataset.label}: ${nf1.format(c.parsed.x)}%`,
        afterBody: (cs) => { const r = ord[cs[0].dataIndex]; return [`Dotação atual: ${rs(r.t[V.ATUAL])}`, `Empenhado: ${rs(r.t[V.EMP])}`, `Pago: ${rs(r.t[V.PAGO])}`]; } } },
        legend: { position: 'bottom' } } } });
    return linhas;
  }

  // --------------------------------------------------------------- abas
  function renderGeral() {
    const ano = S.ano;
    const itens = selecao(ano), itensAnt = I.vincAno.has(ano - 1) ? selecao(ano - 1) : null;
    $('geral-escopo').textContent = `· ${escopoTxt()}`;
    const r = kpis($('kpis'), ano, itens, itensAnt);
    projecao($('projecao'), ano, r);
    graficoAcumulado('g-acum', ano, itens, itensAnt);
    graficoMensal('g-mensal', ano, itens);
    $('nota-mensal').textContent = notaFonte(ano);
    tabelaProgramas(ano);
  }
  function escopoTxt() {
    const p = S.prog === 'TODOS' ? 'todos os programas' : (I.prog.get(S.prog) || {}).nome;
    const o = S.orgao ? ` · ${($('f-orgao').selectedOptions[0] || {}).textContent}` : '';
    return `${S.ano} · ${p}${o}${S.busca ? ` · “${S.busca}”` : ''}`;
  }

  function renderProgramas() {
    const ano = S.ano;
    const linhas = D.programas.map((p) => linhaPrograma(p, ano));
    if (!S.progSel || !I.prog.has(S.progSel)) S.progSel = S.prog !== 'TODOS' ? S.prog : (linhas.find((r) => r.n) || linhas[0]).p.id;
    $('lista-programas').innerHTML = linhas.map((r) => `<button class="item-programa" data-prog="${r.p.id}" aria-current="${r.p.id === S.progSel}">
      <div class="nome">${esc(r.p.curto)}</div>
      <div class="meta"><span>${rs(r.t[V.ATUAL])}</span><span>${r.n ? `${pct(razao(r.t[V.EMP], r.t[V.ATUAL]))} empenhado` : 'sem PO'}</span></div>
      <div class="trilho"><div class="preench" style="width:${Math.min(100, 100 * (razao(r.t[V.EMP], r.t[V.ATUAL]) || 0))}%"></div></div></button>`).join('');
    $('lista-programas').querySelectorAll('button').forEach((b) => b.addEventListener('click', () => { S.progSel = b.dataset.prog; renderProgramas(); }));
    const p = I.prog.get(S.progSel);
    const itens = selecao(ano, p.id), itensAnt = I.vincAno.has(ano - 1) ? selecao(ano - 1, p.id) : null;
    const el = $('detalhe-programa');
    el.innerHTML = `<div class="cartao"><div class="cab-programa"><span class="orgao-lider">${esc(p.orgao || '')}</span><h2>${esc(p.nome)}</h2><p class="texto-apoio">${esc(p.descricao || '')}</p></div>
      ${p.nota ? `<div class="nota-programa">${esc(p.nota)}</div>` : ''}
      <div id="p-kpis" class="kpis kpis-4"></div><div id="p-proj" class="projecao"></div>
      <div class="grade-2"><div><h3 class="cartao-cab">Execução acumulada</h3><div class="grafico"><canvas id="gp-acum"></canvas></div></div>
      <div><h3 class="cartao-cab">Execução mês a mês</h3><div class="grafico"><canvas id="gp-mensal"></canvas></div><p class="nota">${esc(notaFonte(ano))}</p></div></div></div>
      <div class="cartao"><header class="cartao-cab cab-flex"><div><h3>Planos orçamentários do programa</h3><p>${itens.length} PO(s) em ${ano}. Clique numa linha para ver a série mensal.</p></div><button class="btn-secundario" id="p-csv">Exportar (CSV)</button></header>
      <div class="tabela-rolagem"><table id="tp-pos" class="tabela tabela-pos"></table></div></div>
      <div class="cartao"><header class="cartao-cab"><h3>Como as linhas são identificadas</h3></header>
      <ul>${(p.regras || []).map((r) => `<li><strong>${esc(r.tipo)}</strong>: <code>${esc(r.valor)}</code>${r.escopo ? ` (escopo ${esc(r.escopo)})` : ''}</li>`).join('') || '<li>Somente vínculos manuais.</li>'}</ul>
      <p class="nota">Regras em <code>config/regras.csv</code>; inclusões e exclusões em <code>config/vinculos_manuais.csv</code>.</p></div>`;
    const r = kpis($('p-kpis'), ano, itens, itensAnt);
    projecao($('p-proj'), ano, r);
    graficoAcumulado('gp-acum', ano, itens, itensAnt);
    graficoMensal('gp-mensal', ano, itens);
    tabelaPOs($('tp-pos'), ano, itens, null, true);
    $('p-csv').addEventListener('click', () => exportar(ano, itens, `programa_${p.id}_${ano}.csv`));
  }

  // ------------------------------------------------------- tabela de POs
  const COLS = [
    { id: 'desc', rot: 'Ação › Plano orçamentário', ord: (it) => `${it.linha.acd} ${it.linha.pod}` },
    { id: 'uo', rot: 'UO', ord: (it) => it.linha.uod },
    { id: 'loa', rot: 'LOA', n: 1, ord: (it) => it.v[V.LOA] },
    { id: 'atual', rot: 'Dotação atual', n: 1, ord: (it) => it.v[V.ATUAL] },
    { id: 'emp', rot: 'Empenhado', n: 1, ord: (it) => it.v[V.EMP] },
    { id: 'pemp', rot: '% emp.', n: 1, ord: (it) => razao(it.v[V.EMP], it.v[V.ATUAL]) ?? -1 },
    { id: 'pago', rot: 'Pago', n: 1, ord: (it) => it.v[V.PAGO] },
    { id: 'ant', rot: 'Dotação ano ant.', n: 1, ord: (it) => it._ant ?? -1 },
    { id: 'dant', rot: 'Δ dotação', n: 1, ord: (it) => (it._ant ? it.v[V.ATUAL] / it._ant - 1 : -9) },
    { id: 'saldo', rot: 'Saldo a empenhar', n: 1, ord: (it) => it.v[V.ATUAL] - it.v[V.EMP] },
  ];
  function anteriorDe(ano, it) {
    const vs = I.vincChave.get(`${ano - 1}|${it.k}`); if (!vs) return null;
    const prog = S.prog !== 'TODOS' ? S.prog : null;
    const sel = prog ? vs.filter((x) => x.p === prog) : vs;
    if (!sel.length) return null;
    return Math.max(...sel.map((x) => x.v[V.ATUAL]));
  }
  function tabelaPOs(tabela, ano, itens, paginador, compacto = false) {
    itens.forEach((it) => { it._ant = anteriorDe(ano, it); });
    const ordCfg = compacto ? { col: 'atual', dir: -1 } : S.ordPos;
    const col = COLS.find((c) => c.id === ordCfg.col) || COLS[3];
    itens.sort((a, b) => { const x = col.ord(a), y = col.ord(b); return (x > y ? 1 : x < y ? -1 : 0) * ordCfg.dir; });
    let mostrar = itens;
    if (paginador) {
      const np = Math.max(1, Math.ceil(itens.length / POR_PAGINA));
      S.pagPos = Math.min(S.pagPos, np - 1);
      mostrar = itens.slice(S.pagPos * POR_PAGINA, (S.pagPos + 1) * POR_PAGINA);
      paginador.innerHTML = np > 1 ? Array.from({ length: np }, (_, i) => `<button data-p="${i}" aria-current="${i === S.pagPos}">${i + 1}</button>`).join('') : '';
      paginador.querySelectorAll('button').forEach((b) => b.addEventListener('click', () => { S.pagPos = +b.dataset.p; renderPOs(); }));
    }
    const cab = `<thead><tr>${COLS.map((c) => `<th class="${c.n ? 'n ' : ''}${compacto ? '' : 'ord'}" data-col="${c.id}" ${!compacto && c.id === ordCfg.col ? `data-dir="${ordCfg.dir}"` : ''}>${c.rot}</th>`).join('')}</tr></thead>`;
    const corpo = mostrar.map((it) => {
      const l = it.linha, v = it.v, id = `${ano}|${it.k}`;
      const aberto = S.abertos.has(id);
      const tr = `<tr class="clicavel" data-id="${esc(id)}" tabindex="0">
        <td class="desc"><strong>${esc(l.acd)}</strong> › ${esc(l.pod)}<div class="cod">${esc(l.uo)}.${esc(l.pr)}.${esc(l.ac)}.${esc(l.po)} ${it.progs.map((p) => `<span class="chip">${esc((I.prog.get(p) || {}).curto || p)}</span>`).join('')}</div></td>
        <td>${esc(l.uod)}<div class="cod">${esc(l.ogd)}</div></td>
        <td class="n">${rs(v[V.LOA])}</td><td class="n">${rs(v[V.ATUAL])}</td><td class="n">${rs(v[V.EMP])}</td>
        <td class="n">${barraPct(razao(v[V.EMP], v[V.ATUAL]))}</td><td class="n">${rs(v[V.PAGO])}</td>
        <td class="n">${it._ant != null ? rs(it._ant) : '<span class="chip cinza">novo</span>'}</td>
        <td class="n">${it._ant ? delta(v[V.ATUAL], it._ant) : '–'}</td><td class="n">${rs(v[V.ATUAL] - v[V.EMP])}</td></tr>`;
      return tr + (aberto ? `<tr class="detalhe" data-det="${esc(id)}"><td colspan="${COLS.length}">${htmlDetalhe(ano, it)}</td></tr>` : '');
    }).join('');
    tabela.innerHTML = cab + `<tbody>${corpo || `<tr><td colspan="${COLS.length}" class="vazio">Nenhum plano orçamentário no recorte selecionado.</td></tr>`}</tbody>`;
    if (!compacto) tabela.querySelectorAll('th.ord').forEach((th) => th.addEventListener('click', () => {
      S.ordPos = { col: th.dataset.col, dir: S.ordPos.col === th.dataset.col ? -S.ordPos.dir : -1 }; renderPOs();
    }));
    tabela.querySelectorAll('tr[data-id]').forEach((tr) => {
      const alterna = () => { const id = tr.dataset.id; S.abertos.has(id) ? S.abertos.delete(id) : S.abertos.add(id); compacto ? renderProgramas() : renderPOs(); };
      tr.addEventListener('click', (e) => { if (!e.target.closest('button')) alterna(); });
      tr.addEventListener('keydown', (e) => { if (e.key === 'Enter') alterna(); });
    });
    mostrar.forEach((it) => { const id = `${ano}|${it.k}`; if (S.abertos.has(id)) graficoPO(`gpo-${cssId(id)}`, ano, it); });
    tabela.querySelectorAll('[data-copiar]').forEach((b) => b.addEventListener('click', () => copiar(b.dataset.copiar)));
  }
  const cssId = (s) => s.replace(/[^A-Za-z0-9]/g, '_');
  function htmlDetalhe(ano, it) {
    const id = `${ano}|${it.k}`, l = it.linha;
    const vs = I.vincChave.get(id) || [];
    const exc = vs.map((x) => `${x.p},${it.k},excluir,,`).join('\n');
    return `<div class="detalhe-po"><div><div class="grafico"><canvas id="gpo-${cssId(id)}"></canvas></div></div>
      <dl><dt>Chave</dt><dd><code>${esc(it.k)}</code> (UO.Ação.PO)</dd>
      <dt>Programa PPA</dt><dd>${esc(l.pr)} – ${esc(l.prd)}</dd>
      <dt>PLOA</dt><dd>${rs(l.v[V.PLOA])}</dd><dt>Liquidado</dt><dd>${rs(it.v[V.LIQ])}</dd>
      <dt>Classificação</dt><dd>${vs.map((x) => `<span class="chip">${esc((I.prog.get(x.p) || {}).curto || x.p)}</span> <span class="cod">${esc(x.o || '')}</span>`).join('<br>')}</dd>
      <dt>Ajustar</dt><dd><button class="btn-mini" data-copiar="${esc(exc)}">Copiar linha de exclusão</button></dd></dl></div>`;
  }
  function graficoPO(id, ano, it) {
    const s = serieItem(ano, it);
    if (!s) { const c = $(id); if (c) c.parentElement.innerHTML = '<p class="vazio">Sem série mensal para este PO.</p>'; return; }
    const M = mesRef(ano);
    const ac = (arr) => acumular(arr).map((x, i) => (i < M ? x : null));
    desenhar(id, { type: 'bar', data: { labels: rotulosMeses(ano), datasets: [
      { type: 'line', label: 'Empenhado acumulado', data: ac(s.e), borderColor: COR.emp, backgroundColor: COR.emp, borderWidth: 2, pointRadius: 2, tension: .15, spanGaps: true, segment: tracejaLacuna },
      { type: 'line', label: 'Pago acumulado', data: ac(s.p), borderColor: COR.pago, backgroundColor: COR.pago, borderWidth: 2, pointRadius: 2, tension: .15, spanGaps: true, segment: tracejaLacuna },
      { label: 'Empenhado no mês', data: s.e, backgroundColor: 'rgba(19,81,180,.25)', borderRadius: 3, borderSkipped: 'start' },
    ] }, options: { interaction: { mode: 'index', intersect: false }, scales: { y: eixoValor, x: eixoCat }, plugins: { tooltip: dicaValor, legend: { position: 'bottom', labels: { font: { size: 11 } } } } } });
  }
  function renderPOs() {
    const itens = selecao(S.ano);
    $('pos-escopo').textContent = `· ${escopoTxt()}`;
    const t = soma(itens);
    $('pos-contagem').innerHTML = `<strong>${itens.length}</strong> plano(s) orçamentário(s) · dotação atual ${rs(t[V.ATUAL])} · empenhado ${rs(t[V.EMP])} (${pct(razao(t[V.EMP], t[V.ATUAL]))})`;
    tabelaPOs($('t-pos'), S.ano, itens, $('pos-paginas'));
  }
  function renderRevisar() {
    const repo = repoGitHub();
    $('revisar-editar').innerHTML = repo ? `<a class="btn-secundario" style="display:inline-flex;align-items:center;text-decoration:none" target="_blank" rel="noopener" href="https://github.com/${repo}/edit/main/config/vinculos_manuais.csv">Abrir vinculos_manuais.csv no GitHub</a>` : '';
    const todos = (D.candidatos || []).map((c) => ({ c, l: I.linha.get(`${c.ano}|${c.k}`) })).filter((x) => x.l && passaFiltro(x.l));
    const retiradas = todos.filter((x) => x.c.sug), outras = todos.filter((x) => !x.c.sug);
    if (!['retiradas', 'termos'].includes(S.revFiltro)) S.revFiltro = retiradas.length ? 'retiradas' : 'termos';
    $('rev-filtro').innerHTML = `<button class="${S.revFiltro === 'retiradas' ? 'btn-primario' : 'btn-secundario'}" data-rf="retiradas">Retiradas automaticamente (${retiradas.length})</button>
      <button class="${S.revFiltro === 'termos' ? 'btn-primario' : 'btn-secundario'}" data-rf="termos">Não classificadas com termos de tecnologia (${outras.length})</button>`;
    $('rev-filtro').querySelectorAll('[data-rf]').forEach((b) => b.addEventListener('click', () => { S.revFiltro = b.dataset.rf; renderRevisar(); }));
    $('rev-explica').innerHTML = S.revFiltro === 'retiradas'
      ? 'Planos orçamentários que entrariam num programa pela UO (por exemplo, o FNDCT), mas foram retirados automaticamente porque a ação e o PO não mencionam nenhum termo de transformação digital. Para trazer um de volta, clique em <strong>Copiar linha</strong> e cole em <code>config/vinculos_manuais.csv</code>.'
      : 'Planos orçamentários que não entraram em nenhum programa, mas mencionam termos de tecnologia ou dados. Escolha o programa, copie a linha e cole em <code>config/vinculos_manuais.csv</code>.';
    const lst = S.revFiltro === 'retiradas' ? retiradas : outras;
    const opts = (sug) => D.programas.map((p) => `<option value="${p.id}"${p.id === sug ? ' selected' : ''}>${esc(p.curto)}</option>`).join('');
    const totAtual = lst.reduce((s, x) => s + (x.l.v[V.ATUAL] || 0), 0);
    $('t-revisar').innerHTML = `<thead><tr><th>Ação › Plano orçamentário</th><th>UO</th><th class="n">Exercício</th><th class="n">Dotação atual</th><th class="n">Empenhado</th><th>${S.revFiltro === 'retiradas' ? 'Motivo' : 'Termo encontrado'}</th><th>Trazer para</th></tr></thead><tbody>${lst.map(({ c, l }) => `<tr>
      <td class="desc"><strong>${esc(l.acd)}</strong> › ${esc(l.pod)}<div class="cod">${esc(c.k)}</div></td><td>${esc(l.uod)}<div class="cod">${esc(l.ogd)}</div></td>
      <td class="n">${c.ano}</td><td class="n">${rs(l.v[V.ATUAL])}</td><td class="n">${rs(l.v[V.EMP])}</td>
      <td>${c.sug ? esc(c.motivo) : `<code>${esc(c.termo)}</code>`}</td>
      <td style="white-space:nowrap"><select aria-label="Programa">${opts(c.sug)}</select> <button class="btn-mini" data-inc="${esc(c.k)}">Copiar linha</button></td></tr>`).join('')}
      ${lst.length ? `<tr class="total"><td colspan="3">${lst.length} linha(s)</td><td class="n">${rs(totAtual)}</td><td colspan="3"></td></tr>` : `<tr><td colspan="7" class="vazio">Nenhuma linha.</td></tr>`}</tbody>`;
    $('t-revisar').querySelectorAll('[data-inc]').forEach((b) => b.addEventListener('click', () => {
      const p = b.parentElement.querySelector('select').value;
      copiar(`${p},${b.dataset.inc},incluir,1,trazida de volta pela aba de revisão`);
    }));
  }
  function repoGitHub() {
    const h = location.hostname;
    if (!h.endsWith('.github.io')) return null;
    const dono = h.split('.')[0], nome = location.pathname.split('/').filter(Boolean)[0] || `${dono}.github.io`;
    return `${dono}/${nome}`;
  }
  function copiar(txt) {
    (navigator.clipboard ? navigator.clipboard.writeText(txt) : Promise.reject()).then(
      () => toast('Linha copiada. Cole em config/vinculos_manuais.csv.'),
      () => { window.prompt('Copie a linha abaixo:', txt); });
  }

  function renderMetodo() {
    const m = D.meta;
    const conc = (m.conciliacao || []).map((c) => `<tr><td>${c.ano}</td><td class="n">${c.meses}</td><td class="n">${rs(c.empenhado_portal)}</td><td class="n">${rs(c.empenhado_siop)}</td><td class="n">${pct(c.parcela_sem_correspondencia)}</td></tr>`).join('');
    const av = (m.avisos || []).map((a) => `<li>${esc(a)}</li>`).join('');
    $('metodo').innerHTML = `<div class="metodo cartao">
      <h3>De onde vêm os dados</h3>
      <ul>
        <li><strong>Dotação e execução anual</strong> (PLOA, LOA, dotação atual, empenhado, liquidado e pago): SIOP, pelo endpoint SPARQL consultado com a função <code>despesaDetalhada()</code> do pacote R <code>orcamentoBR</code>${m.versao_orcamentoBR ? ` (versão ${esc(m.versao_orcamentoBR)})` : ''}, detalhado por órgão, UO, programa, ação e plano orçamentário. Orçamentos Fiscal e da Seguridade Social.</li>
        <li><strong>Série mês a mês</strong>: o SIOP/orcamentoBR não tem dimensão mensal. Para os meses fechados, o painel usa o arquivo mensal <em>Execução da despesa</em> do Portal da Transparência, que vem da mesma base (SIAFI) e traz o código do PO. O último ponto da série é sempre o SIOP do dia: o mês corrente (e os meses que ainda não têm arquivo do Portal) recebe a diferença entre o SIOP e o acumulado dos meses do Portal. Sem o arquivo do Portal, a série é montada a partir dos retratos do SIOP que o painel guarda a cada atualização.</li>
        <li><strong>Dotação ao longo do ano</strong>: retratos do SIOP guardados em <code>dados/historico/</code>; a série começa a partir da primeira coleta.</li>
        <li><strong>Atualização</strong>: dias úteis, pelo GitHub Actions. Última geração: ${esc(m.gerado_em)}.</li>
      </ul>
      <h3>Como as linhas entram em cada programa</h3>
      <ul>
        <li>FNDCT: entram só os POs da UO do fundo cuja ação ou PO menciona algum termo de transformação digital (coluna <code>exige_texto</code> de <code>config/regras.csv</code>). Os demais são retirados automaticamente e ficam listados em Linhas a revisar, de onde podem ser trazidos de volta.</li>
        <li>Regras em <code>config/regras.csv</code>: por UO (fundos como FNDCT e FUST), por código de ação ou PO, por expressão regular sobre a descrição da ação e do PO, ou por fonte de recursos (FISTEL).</li>
        <li>Ajustes manuais em <code>config/vinculos_manuais.csv</code>: incluir ou excluir um PO (chave <code>UO.Ação.PO</code>) de um programa, com fator opcional para atribuir só uma parcela.</li>
        <li>Um PO pode estar em mais de um programa (por exemplo, FNDCT e PBIA). Os totais de cada programa contam o PO inteiro; o total de “Todos os programas” conta cada PO uma vez só.</li>
        <li>“Outras ações de TD” só recebe linhas que não entraram em nenhum outro programa.</li>
      </ul>
      <h3>Comparações e projeção</h3>
      <ul>
        <li>LOA e dotação atual são comparadas com os valores anuais do exercício anterior. Empenhado, liquidado e pago do exercício corrente são comparados com o acumulado do ano anterior até a mesma data (proporcional ao dia no mês corrente).</li>
        <li>A projeção aplica ao exercício corrente o calendário de empenho do ano anterior: divide o empenhado até hoje pela fração que tinha sido empenhada, na mesma data, no ano anterior.</li>
        <li>Valores correntes (sem correção pela inflação). Restos a pagar não estão incluídos.</li>
      </ul>
      <h3>Limitações</h3>
      <ul>
        <li>Recursos fora do OGU (crédito do BNDES, Orçamento de Investimento das estatais como Serpro, Dataprev e Finep) não aparecem no SIOP/orcamentoBR.</li>
        <li>Programas sem nenhum plano orçamentário identificado não aparecem no painel. Para incluir um, cadastre-o em <code>config/programas.csv</code> e vincule os POs em <code>config/vinculos_manuais.csv</code>.</li>
        <li>O arquivo mensal do Portal da Transparência sai com alguns meses de atraso. Nos meses ainda sem arquivo, a execução aparece agregada no mês corrente, pela diferença entre o SIOP do dia e o último mês do Portal.</li>
        <li>Na regra por fonte (FISTEL), a série mensal é estimada proporcionalmente, porque o arquivo do Portal não traz a fonte.</li>
      </ul>
      <h3>Conciliação Portal × SIOP (empenhado de todo o orçamento)</h3>
      <div class="tabela-rolagem"><table class="tabela"><thead><tr><th>Exercício</th><th class="n">Meses no Portal</th><th class="n">Empenhado (Portal)</th><th class="n">Empenhado (SIOP)</th><th class="n">Sem correspondência de chave</th></tr></thead><tbody>${conc || '<tr><td colspan="5" class="vazio">Sem arquivos do Portal.</td></tr>'}</tbody></table></div>
      <p class="nota">No exercício corrente o Portal cobre só os meses fechados, por isso fica abaixo do SIOP. Modo de leitura do arquivo do Portal: ${esc(m.portal_modo)}.</p>
      ${av ? `<h3>Avisos da última coleta</h3><ul>${av}</ul>` : ''}
    </div>`;
  }

  // ------------------------------------------------------------ exportação
  function exportar(ano, itens, nome) {
    const n = (x) => (x == null ? '' : String(Math.round(x)));
    const cab = ['exercicio', 'chave', 'orgao', 'uo', 'uo_desc', 'programa_ppa', 'acao', 'acao_desc', 'po', 'po_desc', 'programas_td', 'ploa', 'loa', 'dotacao_atual', 'empenhado', 'liquidado', 'pago', 'dotacao_ano_anterior'];
    const q = (s) => `"${String(s ?? '').replace(/"/g, '""')}"`;
    const linhas = itens.map((it) => { const l = it.linha, v = it.v; return [ano, it.k, l.og, l.uo, l.uod, l.pr, l.ac, l.acd, l.po, l.pod, it.progs.join('|'), n(v[0]), n(v[1]), n(v[2]), n(v[3]), n(v[4]), n(v[5]), n(anteriorDe(ano, it))].map(q).join(';'); });
    const blob = new Blob(['﻿' + [cab.join(';'), ...linhas].join('\r\n')], { type: 'text/csv;charset=utf-8' });
    const a = document.createElement('a'); a.href = URL.createObjectURL(blob); a.download = nome; a.click();
    setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  }

  // -------------------------------------------------------------- controle
  function trocarAba(aba) {
    S.aba = aba;
    document.querySelectorAll('.abas button').forEach((b) => b.setAttribute('aria-selected', String(b.dataset.aba === aba)));
    document.querySelectorAll('section.aba').forEach((s) => { s.hidden = s.id !== `aba-${aba}`; });
    render();
    salvarHash();
  }
  function render() {
    ({ geral: renderGeral, programas: renderProgramas, pos: renderPOs, revisar: renderRevisar, metodo: renderMetodo }[S.aba] || renderGeral)();
  }
  function salvarHash() {
    const p = new URLSearchParams({ aba: S.aba, ano: S.ano, prog: S.prog });
    if (S.orgao) p.set('orgao', S.orgao); if (S.busca) p.set('q', S.busca); if (S.aba === 'programas' && S.progSel) p.set('sel', S.progSel);
    history.replaceState(null, '', `#${p}`);
  }
  function lerHash() {
    const p = new URLSearchParams(location.hash.slice(1));
    if (p.get('ano') && D.meta.anos.includes(+p.get('ano'))) S.ano = +p.get('ano');
    if (p.get('prog') && (p.get('prog') === 'TODOS' || I.prog.has(p.get('prog')))) S.prog = p.get('prog');
    if (p.get('orgao')) S.orgao = p.get('orgao');
    if (p.get('q')) S.busca = p.get('q');
    if (p.get('sel')) S.progSel = p.get('sel');
    if (p.get('aba') && ['geral', 'programas', 'pos', 'revisar', 'metodo'].includes(p.get('aba'))) S.aba = p.get('aba');
  }
  function montarFiltros() {
    const anos = [...D.meta.anos].sort((a, b) => b - a);
    $('f-ano').innerHTML = anos.map((a) => `<option value="${a}">${a}${a === D.meta.ano_corrente ? ' (em execução)' : ''}</option>`).join('');
    $('f-prog').innerHTML = '<option value="TODOS">Todos os programas (sem dupla contagem)</option>' + D.programas.map((p) => `<option value="${p.id}">${esc(p.nome)}</option>`).join('');
    const orgs = new Map(); D.vinculos.forEach((v) => { const l = I.linha.get(`${v.a}|${v.k}`); if (l) orgs.set(l.og, l.ogd); });
    $('f-orgao').innerHTML = '<option value="">Todos os órgãos</option>' + [...orgs].sort((a, b) => a[1].localeCompare(b[1], 'pt-BR')).map(([c, d]) => `<option value="${esc(c)}">${esc(c)} – ${esc(d)}</option>`).join('');
    $('f-ano').value = S.ano; $('f-prog').value = S.prog; $('f-orgao').value = S.orgao; $('f-busca').value = S.busca;
    $('f-ano').addEventListener('change', (e) => { S.ano = +e.target.value; S.pagPos = 0; render(); salvarHash(); });
    $('f-prog').addEventListener('change', (e) => { S.prog = e.target.value; S.pagPos = 0; if (S.prog !== 'TODOS') S.progSel = S.prog; render(); salvarHash(); });
    $('f-orgao').addEventListener('change', (e) => { S.orgao = e.target.value; S.pagPos = 0; render(); salvarHash(); });
    let t; $('f-busca').addEventListener('input', (e) => { clearTimeout(t); t = setTimeout(() => { S.busca = e.target.value.trim(); S.pagPos = 0; render(); salvarHash(); }, 250); });
    $('f-limpar').addEventListener('click', () => { S.prog = 'TODOS'; S.orgao = ''; S.busca = ''; $('f-prog').value = 'TODOS'; $('f-orgao').value = ''; $('f-busca').value = ''; render(); salvarHash(); });
    document.querySelectorAll('.abas button').forEach((b) => b.addEventListener('click', () => trocarAba(b.dataset.aba)));
    document.querySelectorAll('[data-ir]').forEach((b) => b.addEventListener('click', () => trocarAba(b.dataset.ir)));
    $('pos-csv').addEventListener('click', () => exportar(S.ano, selecao(S.ano), `planos_orcamentarios_${S.ano}.csv`));
  }

  async function iniciar() {
    try {
      const r = await fetch(`dados/painel.json?v=${Date.now()}`, { cache: 'no-store' });
      if (!r.ok) throw new Error(`HTTP ${r.status}`);
      D = await r.json();
    } catch (e) {
      $('erro').hidden = false;
      $('erro').innerHTML = '<strong>Os dados ainda não estão disponíveis.</strong> Se o painel acabou de ser publicado, rode o workflow “Atualizar painel” na aba Actions do repositório e aguarde alguns minutos.';
      $('atualizacao').textContent = '';
      document.querySelectorAll('section.aba').forEach((s) => { s.hidden = true; });
      return;
    }
    indexar();
    S.ano = D.meta.ano_corrente && D.meta.anos.includes(D.meta.ano_corrente) ? D.meta.ano_corrente : Math.max(...D.meta.anos);
    lerHash();
    if (D.meta.simulado) $('faixa-simulado').hidden = false;
    const g = new Date(D.meta.gerado_em.replace(/([+-]\d\d)(\d\d)$/, '$1:$2'));
    $('atualizacao').textContent = `Dados de ${dataBR(D.meta.data_referencia)} · atualizado ${isNaN(g) ? '' : g.toLocaleString('pt-BR', { dateStyle: 'short', timeStyle: 'short' })}`;
    if ((D.meta.avisos || []).length) {
      $('erro').hidden = false; $('erro').className = 'alerta alerta-aviso';
      $('erro').innerHTML = `<strong>A última coleta teve ${D.meta.avisos.length} aviso(s).</strong> Veja os detalhes na aba Metodologia.`;
    }
    montarFiltros();
    trocarAba(S.aba);
  }
  iniciar();
})();
