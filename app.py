# app.py
# ============================================
# Observatório do Trabalho do RN – Dashboard (Streamlit)
# Tema dark, gráficos Matplotlib/Seaborn com tipografia clara
# ============================================

import os
import re
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import streamlit as st

# -----------------------------
# 0) Configurações gerais
# -----------------------------

BASE_DIR = os.path.join(os.path.dirname(__file__), "data")
ARQ_TOTAL = os.path.join(BASE_DIR, "informalidade_total_BR_RN_2023_2025t1.csv")
ARQ_SEXO  = os.path.join(BASE_DIR, "informalidade_por_sexo_BR_RN_2023_2025t1.csv")
ARQ_FAIXA = os.path.join(BASE_DIR, "informalidade_por_faixa_BR_RN_2023_2025t1.csv")
ARQ_RACA  = os.path.join(BASE_DIR, "informalidade_por_raca_BR_RN_2023_2025t1.csv")

st.set_page_config(
    page_title="Observatório do Trabalho do RN",
    page_icon="📊",
    layout="wide",
)

# ---- CSS (dark) e melhorias de legibilidade -------------------------------
st.markdown("""
<style>
/* fundo escuro geral */
html, body, [data-testid="stAppViewContainer"] {
  background-color: #0e1117;
  color: #ffffff;
}

/* barra branca no topo com curso/aluno */
.banner-top {
  background: #ffffff;
  color: #111111;
  padding: 8px 14px 10px 14px;
  border-radius: 6px;
  margin-bottom: 12px;
}
.banner-top .curso { font-weight: 800; letter-spacing: .3px; }
.banner-top .aluno { font-size: .95rem; opacity:.85; }

/* títulos */
h1, h2, h3 { color: #ffffff; }

/* cards de insight */
.insight {
  background: #141923;
  border: 1px solid #223;
  border-radius: 10px;
  padding: 12px 14px;
  line-height: 1.55rem;
}

/* blocos de código escuros e legíveis */
.block-title {
  background: #0d1117;
  color: #d1e3ff;
  border: 1px solid #1f2a44;
  padding: 8px 12px;
  border-radius: 8px 8px 0 0;
  font-weight: 600;
}
.code-wrap {
  background: #0b0f16 !important;
  border: 1px solid #1f2a44;
  border-top: 0;
  border-radius: 0 0 8px 8px;
  padding: 8px 8px 0 8px;
}
/* força texto claro dentro de qualquer <pre>/<code> */
pre, code, .stCode pre, .stCode code {
  color: #e6edf3 !important;
  background: #0b0f16 !important;
}
section.main > div { padding-top: 0; }

/* melhora tabs */
[data-testid="stHorizontalBlock"] button {
  color: #dfe7f3 !important;
}
</style>
<div class="banner-top">
  <div class="curso">VISUALIZAÇÃO DE DADOS</div>
  <div class="aluno">Aluno: Renato Assis</div>
</div>
""", unsafe_allow_html=True)

st.title("Mercado de Trabalho - Rio Grande do Norte (Projeto Observatório do Trabalho do Rio Grande do Norte)")

# ---- Matplotlib/Seaborn (dark) ---------------------------------------------
sns.set_theme(style="ticks")
plt.rcParams.update({
    "font.family": "Arial",
    "figure.dpi": 110,
    "figure.facecolor": "#0e1117",
    "axes.facecolor": "#0e1117",
    "axes.edgecolor": "white",
    "axes.labelcolor": "white",
    "axes.grid": False,
    "xtick.color": "white",
    "ytick.color": "white",
    "text.color": "white",
    "legend.edgecolor": "white",
})

PAL_TOTAL = sns.color_palette("crest", 2)      # Brasil x RN
PAL_SEXO  = sns.color_palette("viridis", 2)    # Homem x Mulher
PAL_FAIXA = sns.color_palette("Blues", 9)
PAL_RACA  = sns.color_palette("Purples", 9)

# -----------------------------
# 1) Funções auxiliares
# -----------------------------
def ler_csv_pt(path):
    return pd.read_csv(path, sep=";", decimal=",")

def ordenar_trimestres(df, col="trimestre"):
    def parse(s):
        m = re.search(r"(\d+)[ºo]?\s*tri/?\s*(\d{4})", str(s))
        if not m: return (9999, 9)
        tri, ano = int(m.group(1)), int(m.group(2))
        return (ano, tri)
    ordem = sorted(df[col].dropna().unique(), key=parse)
    df[col] = pd.Categorical(df[col], categories=ordem, ordered=True)
    return df

def fmt_ptbr(v, casas=1):
    if pd.isna(v): return ""
    return f"{v:.{casas}f}".replace(".", ",")

def diff_pp(a, b):
    if np.isnan(a) or np.isnan(b): return np.nan
    return a - b

def ultimo_valor(df, filtro: dict, col_val="taxa_informalidade"):
    d = df.copy()
    for k, v in filtro.items():
        d = d[d[k] == v]
    d = d.sort_values("trimestre")
    if d.empty: return np.nan, None
    return float(d[col_val].iloc[-1]), d["trimestre"].iloc[-1]

def legend_center(fig, labels, handles=None, ncol=4):
    if handles is None:
        handles = [plt.Line2D([], [], color=PAL_TOTAL[0], lw=2.2),
                   plt.Line2D([], [], color=PAL_TOTAL[1], lw=2.2)]
    fig.legend(handles, labels, loc="lower center", ncol=ncol,
               frameon=False, bbox_to_anchor=(0.5, -0.02))

def polish_axes(ax, ylabel=""):
    ax.grid(False)
    ax.set_ylabel(ylabel)
    for sp in ("top", "right"):
        ax.spines[sp].set_visible(False)

def draw_line_sparse_markers(ax, x, y, label, color, lw=2.2, annotate=True, ls='-'):
    """
    Linha com marcadores apenas em: primeiro, pico (máx), piso (mín) e último.
    """
    vals = np.array(y, dtype=float)
    xs = np.arange(len(x))
    ax.plot(xs, vals, color=color, lw=lw, label=label, zorder=2, linestyle=ls)

    idx_first, idx_last = 0, len(vals) - 1
    idx_max = int(np.nanargmax(vals))
    idx_min = int(np.nanargmin(vals))
    idxs = list(dict.fromkeys([idx_first, idx_max, idx_min, idx_last]))

    ax.scatter(idxs, vals[idxs], color=color, s=55, zorder=3)
    if annotate:
        for i in idxs:
            ax.text(i, vals[i] + 0.18, fmt_ptbr(vals[i]),
                    ha="center", va="bottom", fontsize=11, color="white")

    ax.set_xticks(xs)
    ax.set_xticklabels(list(x), rotation=33, ha="right", color="white")

def format_series_top(s: pd.Series, top: int | None = None) -> str:
    """
    Converte uma Series em string 'rótulo: valor%'.
    Corrige o erro de fatiamento em iteradores (usa head(top).items()).
    """
    it = s.head(top).items() if top else s.items()
    return ", ".join([f"{idx}: {fmt_ptbr(val)}%" for idx, val in it])

def annotate_heatmap_peaks(ax, mat: pd.DataFrame,
                           mark_rows=True, mark_global=True,
                           color="white"):
    """
    Marca picos/vales no heatmap.
    - mark_rows=True: marca o máximo (▲) e o mínimo (▼) de CADA LINHA (faixa etária).
    - mark_global=True: marca o máximo global (★) e o mínimo global (✖) da matriz.
    """
    n_rows, n_cols = mat.shape
    z = 5  # zorder pra ficar acima das células

    # --- Por linha (faixa etária): ▲ máximo, ▼ mínimo
    if mark_rows:
        for i in range(n_rows):
            row = mat.iloc[i].values.astype(float)
            jmax = int(np.nanargmax(row))
            jmin = int(np.nanargmin(row))
            # pico (máximo) ▲
            ax.scatter(jmax + 0.5, i + 0.5, marker='^', s=180,
                       facecolors="none", edgecolors=color, linewidths=2.2,
                       zorder=z)
            # vale (mínimo) ▼
            ax.scatter(jmin + 0.5, i + 0.5, marker='v', s=160,
                       facecolors="none", edgecolors=color, linewidths=2.0,
                       zorder=z)

    # --- Extremos globais (★ máximo, ✖ mínimo)
    if mark_global:
        arr = mat.values.astype(float)
        i_max, j_max = np.unravel_index(np.nanargmax(arr), arr.shape)
        i_min, j_min = np.unravel_index(np.nanargmin(arr), arr.shape)

        # Máximo global ★ (um pouco maior)
        ax.scatter(j_max + 0.5, i_max + 0.5, marker='*', s=360,
                   facecolors="none", edgecolors=color, linewidths=2.8,
                   zorder=z+1)
        # Mínimo global ✖
        ax.scatter(j_min + 0.5, i_min + 0.5, marker='x', s=260,
                   color=color, linewidths=2.4, zorder=z+1)


# -----------------------------
# 2) Carregar dados
# -----------------------------
tb_total = ordenar_trimestres(ler_csv_pt(ARQ_TOTAL))
tb_sexo  = ordenar_trimestres(ler_csv_pt(ARQ_SEXO))
tb_faixa = ordenar_trimestres(ler_csv_pt(ARQ_FAIXA))
tb_raca  = ordenar_trimestres(ler_csv_pt(ARQ_RACA))

# -----------------------------
# 3) TABS
# -----------------------------
tabs = st.tabs(["Início", "Quadro geral", "Sexo", "Faixa etária", "Raça/cor", "Gap RN–Brasil", "Heatmap RN", "Séries por raça"])

# -----------------------------
#  Início
# -----------------------------
with tabs[0]:
    st.header("Objetivo da apresentação")

    st.markdown("""
- Mostrar a **evolução da informalidade** no RN em comparação ao Brasil, usando **Matplotlib + Seaborn** (tema dark, tipografia clara e boas práticas).
- Entregar **insights com números** e **código completo** de cada gráfico (Etapa 2 — bibliotecas).
- Organização em abas: quadro geral, recortes de **sexo**, **idade**, **raça/cor**, **gap RN–Brasil** e **heatmap** (leitura rápida).
""")

    st.markdown("""
### Contexto e narrativa (Etapa 1)

**Persona:** *Iris Maria de Oliveira* — gestora estadual na **SETHAS-RN**.  
**Missão:** reduzir a **informalidade** e a **vulnerabilidade ocupacional**, com foco em **jovens** e grupos com maior exposição (recortes de **sexo** e **raça/cor**).

**O que a Iris precisa**  
- **Comparar o RN ao Brasil** rapidamente (níveis e distância em p.p.).  
- **Identificar grupos críticos**: por **sexo**, **faixa etária** e **raça/cor**.  
- **Evidências simples** para priorizar **programas e recursos** (qualificação, formalização, compras públicas, crédito).

**Mapa de empatia — resumo**  
- **Dores:** informalidade entre jovens; desigualdades raciais; orçamento limitado; difícil priorização territorial.  
- **Ganhos desejados:** metas claras por público; monitoramento simples; comunicação convincente com parceiros.  
- **Vê/Ouve:** dados dispersos e relatórios extensos; pede resultados curtos e acionáveis.  
- **Faz/Sente:** precisa decidir “onde atacar primeiro”; pressiona por transparência e foco.

**Problema central**  
> A **informalidade no RN é elevada e heterogênea** entre grupos etários e raciais, ampliando vulnerabilidade e travando ganhos de renda.

**Como ler os gráficos**  
- Observe **níveis** (último trimestre), **tendências** (subidas/descidas), **diferenças** (p.p.) e **pontos extremos** (picos/vales).  
- Legendas ficam **centralizadas** para evitar conflito com as curvas; marcadores aparecem **apenas** nos pontos-chave.
""")

# -----------------------------
#  Quadro geral (G1) — uma figura com as duas linhas
# -----------------------------
with tabs[1]:
    st.header("Informalidade ao longo do tempo — RN em comparação ao Brasil (série trimestral)")

    base_br = tb_total.query("local == 'Brasil'").sort_values("trimestre")
    base_rn = tb_total.query("local == 'Rio Grande do Norte'").sort_values("trimestre")

    fig, ax = plt.subplots(figsize=(16, 7))
    draw_line_sparse_markers(ax, base_br["trimestre"], base_br["taxa_informalidade"],
                             label="Brasil", color=PAL_TOTAL[0], ls='-')
    draw_line_sparse_markers(ax, base_rn["trimestre"], base_rn["taxa_informalidade"],
                             label="Rio Grande do Norte", color=PAL_TOTAL[1], ls='-')
    polish_axes(ax, ylabel="Taxa (%)")
    legend_center(fig, ["Brasil", "Rio Grande do Norte"], ncol=2)
    st.pyplot(fig, use_container_width=True)

    # Insights mais analíticos
    br_ini = float(base_br["taxa_informalidade"].iloc[0]); br_fim = float(base_br["taxa_informalidade"].iloc[-1])
    rn_ini = float(base_rn["taxa_informalidade"].iloc[0]); rn_fim = float(base_rn["taxa_informalidade"].iloc[-1])
    tri_ult = base_br["trimestre"].iloc[-1]
    st.markdown(f"""
<div class="insight">
<b>Leitura:</b> do 1º tri/2023 ao <b>{tri_ult}</b>, o Brasil caiu de <b>{fmt_ptbr(br_ini)}%</b> para <b>{fmt_ptbr(br_fim)}%</b> 
(<b>{fmt_ptbr(br_fim - br_ini)} p.p.</b>), enquanto o RN foi de <b>{fmt_ptbr(rn_ini)}%</b> para <b>{fmt_ptbr(rn_fim)}%</b> 
(<b>{fmt_ptbr(rn_fim - rn_ini)} p.p.</b>). O gap atual RN–Brasil é de <b>{fmt_ptbr(rn_fim - br_fim)} p.p.</b>.<br>
<b>Ação:</b> manter políticas anticíclicas para consolidar a queda no RN; reforçar portas de entrada para formalização 
(emissão de CNPJ/MEI simplificada, compras públicas e crédito orientado).
</div>
""", unsafe_allow_html=True)

    # Código (rodapé)
    with st.expander("Ver CÓDIGO completo (pandas, matplotlib.pyplot, seaborn)", expanded=False):
        st.markdown('<div class="block-title">Bibliotecas usadas → pandas, matplotlib.pyplot, seaborn</div>', unsafe_allow_html=True)
        st.markdown('<div class="code-wrap">', unsafe_allow_html=True)
        st.code("""
base_br = tb_total.query("local == 'Brasil'").sort_values("trimestre")
base_rn = tb_total.query("local == 'Rio Grande do Norte'").sort_values("trimestre")

fig, ax = plt.subplots(figsize=(16, 7))
draw_line_sparse_markers(ax, base_br["trimestre"], base_br["taxa_informalidade"],
                         label="Brasil", color=PAL_TOTAL[0], ls='-')
draw_line_sparse_markers(ax, base_rn["trimestre"], base_rn["taxa_informalidade"],
                         label="Rio Grande do Norte", color=PAL_TOTAL[1], ls='-')
polish_axes(ax, ylabel="Taxa (%)")
legend_center(fig, ["Brasil", "Rio Grande do Norte"], ncol=2)
st.pyplot(fig, use_container_width=True)
        """, language="python")
        st.markdown('</div>', unsafe_allow_html=True)

# -----------------------------
#  Sexo (G2)
# -----------------------------
with tabs[2]:
    st.header("Quem está mais exposto? Informalidade por sexo (série trimestral)")

    fig, axs = plt.subplots(nrows=2, ncols=1, figsize=(16, 9), sharex=True)
    for i, loc in enumerate(["Brasil", "Rio Grande do Norte"]):
        base = tb_sexo.query("local == @loc").sort_values("trimestre")
        for j, sexo in enumerate(["Homem", "Mulher"]):
            df_ = base[base["sexo"] == sexo]
            draw_line_sparse_markers(axs[i], df_["trimestre"], df_["taxa_informalidade"],
                                     label=sexo, color=PAL_SEXO[j])
        polish_axes(axs[i], ylabel="Taxa (%)")
    legend_center(fig, ["Homem", "Mulher"], ncol=2)
    st.pyplot(fig, use_container_width=True)

    # Insights por local
    for loc in ["Brasil", "Rio Grande do Norte"]:
        vh, tri = ultimo_valor(tb_sexo, {"local": loc, "sexo": "Homem"})
        vm, _   = ultimo_valor(tb_sexo, {"local": loc, "sexo": "Mulher"})
        st.markdown(f"""
<div class="insight">
<b>{loc}</b> (<b>{tri}</b>): Homens = <b>{fmt_ptbr(vh)}%</b>, Mulheres = <b>{fmt_ptbr(vm)}%</b> 
→ diferença de <b>{fmt_ptbr(vh - vm)} p.p.</b>.<br>
<b>Ação:</b> para mulheres, reforçar formalização ligada a serviços pessoais, cuidado e comércio; 
para homens, qualificação vinculada à construção e logística com carteira assinada.
</div>
""", unsafe_allow_html=True)

    with st.expander("Ver CÓDIGO completo (pandas, matplotlib.pyplot, seaborn)", expanded=False):
        st.markdown('<div class="block-title">Bibliotecas usadas → pandas, matplotlib.pyplot, seaborn</div>', unsafe_allow_html=True)
        st.markdown('<div class="code-wrap">', unsafe_allow_html=True)
        st.code("""
fig, axs = plt.subplots(nrows=2, ncols=1, figsize=(16, 9), sharex=True)
for i, loc in enumerate(["Brasil", "Rio Grande do Norte"]):
    base = tb_sexo.query("local == @loc").sort_values("trimestre")
    for j, sexo in enumerate(["Homem", "Mulher"]):
        df_ = base[base["sexo"] == sexo]
        draw_line_sparse_markers(axs[i], df_["trimestre"], df_["taxa_informalidade"],
                                 label=sexo, color=PAL_SEXO[j])
    polish_axes(axs[i], ylabel="Taxa (%)")
legend_center(fig, ["Homem", "Mulher"], ncol=2)
st.pyplot(fig, use_container_width=True)
        """, language="python")
        st.markdown('</div>', unsafe_allow_html=True)

# -----------------------------
#  Faixa etária (G3)
# -----------------------------
with tabs[3]:
    st.header("Onde a informalidade se concentra? Comparação por faixa etária")

    ord_faixas = ["14 a 24 anos", "25 a 39 anos", "40 a 59 anos", "60 anos ou mais"]
    tb_f = tb_faixa.copy()
    tb_f["faixa_etaria"] = pd.Categorical(tb_f["faixa_etaria"], categories=ord_faixas, ordered=True)

    for loc in ["Brasil", "Rio Grande do Norte"]:
        st.subheader(loc)
        base = tb_f.query("local == @loc")
        trimestres = list(base["trimestre"].cat.categories)
        pal = sns.color_palette("Blues", len(trimestres))

        fig, ax = plt.subplots(figsize=(16, 6.2))
        sns.barplot(data=base, x="faixa_etaria", y="taxa_informalidade",
                    hue="trimestre", palette=pal, ax=ax, dodge=True)
        # labels brancos nas barras
        for container in ax.containers:
            ax.bar_label(container, fmt=lambda v: fmt_ptbr(v), padding=1.5, fontsize=9, color="white")
        ax.tick_params(axis="x", rotation=10, labelcolor="white")
        polish_axes(ax, ylabel="Taxa (%)")
        ax.legend(title=None, loc="upper center", bbox_to_anchor=(0.5, 1.18),
                  ncol=6, frameon=False)
        st.pyplot(fig, use_container_width=True)

    # Insight focado em jovens
    br_24, tri = ultimo_valor(tb_faixa, {"local":"Brasil", "faixa_etaria":"14 a 24 anos"})
    rn_24, _   = ultimo_valor(tb_faixa, {"local":"Rio Grande do Norte", "faixa_etaria":"14 a 24 anos"})
    st.markdown(f"""
<div class="insight">
<b>Ponto crítico:</b> jovens (14–24) lideram a informalidade. No <b>{tri}</b>, Brasil <b>{fmt_ptbr(br_24)}%</b> × RN <b>{fmt_ptbr(rn_24)}%</b>.  
<b>Ação:</b> trilhas de qualificação curta + intermediação ativa + MEI orientado; priorizar municípios com maior densidade de jovens.
</div>
""", unsafe_allow_html=True)

    with st.expander("Ver CÓDIGO completo (pandas, seaborn, matplotlib.pyplot)", expanded=False):
        st.markdown('<div class="block-title">Bibliotecas usadas → pandas, seaborn, matplotlib.pyplot</div>', unsafe_allow_html=True)
        st.markdown('<div class="code-wrap">', unsafe_allow_html=True)
        st.code("""
ord_faixas = ["14 a 24 anos", "25 a 39 anos", "40 a 59 anos", "60 anos ou mais"]
tb_f = tb_faixa.copy()
tb_f["faixa_etaria"] = pd.Categorical(tb_f["faixa_etaria"], categories=ord_faixas, ordered=True)

for loc in ["Brasil", "Rio Grande do Norte"]:
    base = tb_f.query("local == @loc")
    trimestres = list(base["trimestre"].cat.categories)
    pal = sns.color_palette("Blues", len(trimestres))

    fig, ax = plt.subplots(figsize=(16, 6.2))
    sns.barplot(data=base, x="faixa_etaria", y="taxa_informalidade",
                hue="trimestre", palette=pal, ax=ax, dodge=True)
    for container in ax.containers:
        ax.bar_label(container, fmt=lambda v: fmt_ptbr(v), padding=1.5, fontsize=9, color="white")
    ax.tick_params(axis="x", rotation=10, labelcolor="white")
    polish_axes(ax, ylabel="Taxa (%)")
    ax.legend(title=None, loc="upper center", bbox_to_anchor=(0.5, 1.18),
              ncol=6, frameon=False)
    st.pyplot(fig, use_container_width=True)
        """, language="python")
        st.markdown('</div>', unsafe_allow_html=True)

# -----------------------------
#  Raça/cor (G4) – apenas barras comparativas
# -----------------------------
with tabs[4]:
    st.header("Desigualdades na informalidade — recorte por raça/cor")

    ord_raca = ["Branco", "Preto", "Demais Raças"]
    tb_r = tb_raca.copy()
    tb_r["raca"] = pd.Categorical(tb_r["raca"], categories=ord_raca, ordered=True)

    for loc in ["Brasil", "Rio Grande do Norte"]:
        st.subheader(loc)
        base = tb_r.query("local == @loc")
        trimestres = list(base["trimestre"].cat.categories)
        pal = sns.color_palette("Purples", len(trimestres))

        fig, ax = plt.subplots(figsize=(16, 6.2))
        sns.barplot(data=base, x="raca", y="taxa_informalidade",
                    hue="trimestre", palette=pal, ax=ax, dodge=True)
        for container in ax.containers:
            ax.bar_label(container, fmt=lambda v: fmt_ptbr(v), padding=1.5, fontsize=9, color="white")
        polish_axes(ax, ylabel="Taxa (%)")
        ax.legend(title=None, loc="upper center", bbox_to_anchor=(0.5, 1.18),
                  ncol=6, frameon=False)
        st.pyplot(fig, use_container_width=True)

    st.markdown("""
<div class="insight">
<b>Leitura:</b> o recorte racial reforça desigualdades persistentes — níveis mais altos entre <b>Pretos</b> e <b>Demais Raças</b> do que entre <b>Brancos</b>.  
<b>Ação:</b> combinar qualificação com inclusão produtiva local (arranjos de serviços e comércio) e contratação pública com metas de diversidade.
</div>
""", unsafe_allow_html=True)

    with st.expander("Ver CÓDIGO completo (pandas, seaborn, matplotlib.pyplot)", expanded=False):
        st.markdown('<div class="block-title">Bibliotecas usadas → pandas, seaborn, matplotlib.pyplot</div>', unsafe_allow_html=True)
        st.markdown('<div class="code-wrap">', unsafe_allow_html=True)
        st.code("""
ord_raca = ["Branco", "Preto", "Demais Raças"]
tb_r = tb_raca.copy()
tb_r["raca"] = pd.Categorical(tb_r["raca"], categories=ord_raca, ordered=True)

for loc in ["Brasil", "Rio Grande do Norte"]:
    base = tb_r.query("local == @loc")
    trimestres = list(base["trimestre"].cat.categories)
    pal = sns.color_palette("Purples", len(trimestres))

    fig, ax = plt.subplots(figsize=(16, 6.2))
    sns.barplot(data=base, x="raca", y="taxa_informalidade",
                hue="trimestre", palette=pal, ax=ax, dodge=True)
    for container in ax.containers:
        ax.bar_label(container, fmt=lambda v: fmt_ptbr(v), padding=1.5, fontsize=9, color="white")
    polish_axes(ax, ylabel="Taxa (%)")
    ax.legend(title=None, loc="upper center", bbox_to_anchor=(0.5, 1.18),
              ncol=6, frameon=False)
    st.pyplot(fig, use_container_width=True)
        """, language="python")
        st.markdown('</div>', unsafe_allow_html=True)

# -----------------------------
#  Gap RN – Brasil (G5)
# -----------------------------
with tabs[5]:
    st.header("Quanto o RN se afasta do Brasil? Gap de informalidade (pontos percentuais)")

    br = tb_total[tb_total["local"]=="Brasil"][["trimestre","taxa_informalidade"]].rename(columns={"taxa_informalidade":"br"})
    rn = tb_total[tb_total["local"]=="Rio Grande do Norte"][["trimestre","taxa_informalidade"]].rename(columns={"taxa_informalidade":"rn"})
    base_gap = (br.merge(rn, on="trimestre", how="inner")
                  .sort_values("trimestre")
                  .assign(gap=lambda d: d["rn"] - d["br"]))

    fig, ax = plt.subplots(figsize=(16, 6.3))
    draw_line_sparse_markers(ax, base_gap["trimestre"], base_gap["gap"],
                             label="Gap RN → Brasil (p.p.)", color=PAL_TOTAL[1])
    ax.axhline(0, color="gray", ls="--", lw=1)
    polish_axes(ax, ylabel="p.p.")
    legend_center(fig, ["Gap RN → Brasil (p.p.)"], ncol=1)
    st.pyplot(fig, use_container_width=True)

    gv, tri = float(base_gap["gap"].iloc[-1]), base_gap["trimestre"].iloc[-1]
    st.markdown(f"""
<div class="insight">
<b>Leitura:</b> no <b>{tri}</b>, o gap RN–Brasil é de <b>{fmt_ptbr(gv)} p.p.</b>.  
<b>Ação:</b> perseguir redução do gap com políticas de formalização focalizadas (MEI, compras locais, fiscalização orientadora).
</div>
""", unsafe_allow_html=True)

    with st.expander("Ver CÓDIGO completo (pandas, matplotlib.pyplot, seaborn)", expanded=False):
        st.markdown('<div class="block-title">Bibliotecas usadas → pandas, matplotlib.pyplot, seaborn</div>', unsafe_allow_html=True)
        st.markdown('<div class="code-wrap">', unsafe_allow_html=True)
        st.code("""
br = tb_total[tb_total["local"]=="Brasil"][["trimestre","taxa_informalidade"]].rename(columns={"taxa_informalidade":"br"})
rn = tb_total[tb_total["local"]=="Rio Grande do Norte"][["trimestre","taxa_informalidade"]].rename(columns={"taxa_informalidade":"rn"})
base_gap = (br.merge(rn, on="trimestre", how="inner")
              .sort_values("trimestre")
              .assign(gap=lambda d: d["rn"] - d["br"]))

fig, ax = plt.subplots(figsize=(16, 6.3))
draw_line_sparse_markers(ax, base_gap["trimestre"], base_gap["gap"],
                         label="Gap RN → Brasil (p.p.)", color=PAL_TOTAL[1])
ax.axhline(0, color="gray", ls="--", lw=1)
polish_axes(ax, ylabel="p.p.")
legend_center(fig, ["Gap RN → Brasil (p.p.)"], ncol=1)
st.pyplot(fig, use_container_width=True)
        """, language="python")
        st.markdown('</div>', unsafe_allow_html=True)

# -----------------------------
#  Heatmap RN (G6) — com marcadores
# -----------------------------
with tabs[6]:
    st.header("RN: onde a informalidade é mais alta? (heatmap por faixa etária e trimestre)")

    rn = tb_faixa[tb_faixa["local"]=="Rio Grande do Norte"].copy()
    pv = rn.pivot(index="faixa_etaria", columns="trimestre", values="taxa_informalidade")

    fig, ax = plt.subplots(figsize=(18, 7.2))
    sns.heatmap(pv, cmap="Blues", annot=False, cbar_kws={"label":"Taxa (%)"}, ax=ax)
    annotate_heatmap_peaks(ax, pv, mark_rows=True, mark_global=True, color="white")  # <<<< AQUI
    ax.set_xlabel(""); ax.set_ylabel("")
    ax.tick_params(axis="x", rotation=20, colors="white")
    ax.tick_params(axis="y", colors="white")
    st.pyplot(fig, use_container_width=True)

    # Leitura analítica
    row_means = pv.mean(axis=1).sort_values(ascending=False)
    col_means = pv.mean(axis=0).sort_values(ascending=False)

    st.markdown(f"""
<div class="insight">
<b>Leitura:</b> triângulos <b>▲</b> marcam os <b>picos</b> (máximos) em cada faixa; <b>▼</b> marcam os <b>vales</b> (mínimos).
A <b>★</b> indica o <b>máximo global</b> do mapa e a <b>✖</b> o <b>mínimo global</b>.<br>
<b>Médias por faixa (RN):</b> {", ".join([f"{i}: {fmt_ptbr(v)}%" for i,v in row_means.items()])}.<br>
<b>Trimestres mais “carregados”</b> (média RN): {", ".join([f"{i}: {fmt_ptbr(v)}%" for i,v in col_means.head(3).items()])}.<br>
<b>Ação:</b> usar as faixas com <b>▲</b> persistentes para priorizar políticas (qualificação, MEI, compras públicas) e
monitorar os <b>▼</b> como referência de “piso” desejável.
</div>
""", unsafe_allow_html=True)


# -----------------------------
#  Séries por raça – 3 gráficos (G7)
# -----------------------------
with tabs[7]:
    st.header("Séries por raça — gráficos separados (Branco, Preto, Demais Raças)")
    st.caption("Leitura: séries separadas e na mesma escala facilitam a comparação visual consistente.")

    for raca in ["Branco", "Preto", "Demais Raças"]:
        st.subheader(raca)
        fig, ax = plt.subplots(figsize=(16, 6))
        for j, loc in enumerate(["Brasil", "Rio Grande do Norte"]):
            df_ = tb_raca[(tb_raca["raca"]==raca) & (tb_raca["local"]==loc)].sort_values("trimestre")
            draw_line_sparse_markers(ax, df_["trimestre"], df_["taxa_informalidade"],
                                     label=loc, color=PAL_TOTAL[j])
        polish_axes(ax, ylabel="Taxa (%)")
        legend_center(fig, ["Brasil", "Rio Grande do Norte"], ncol=2)
        st.pyplot(fig, use_container_width=True)

        br_val, tri = ultimo_valor(tb_raca, {"raca":raca, "local":"Brasil"})
        rn_val, _   = ultimo_valor(tb_raca, {"raca":raca, "local":"Rio Grande do Norte"})
        st.markdown(f"""
<div class="insight">
<b>{raca}</b> – <b>{tri}</b>: Brasil <b>{fmt_ptbr(br_val)}%</b> × RN <b>{fmt_ptbr(rn_val)}%</b> 
→ gap de <b>{fmt_ptbr(rn_val - br_val)} p.p.</b>.  
<b>Ação:</b> integrar qualificação, crédito e compras públicas com metas de diversidade para acelerar a convergência.
</div>
""", unsafe_allow_html=True)

    with st.expander("Ver CÓDIGO completo (pandas, matplotlib.pyplot, seaborn)", expanded=False):
        st.markdown('<div class="block-title">Bibliotecas usadas → pandas, matplotlib.pyplot, seaborn</div>', unsafe_allow_html=True)
        st.markdown('<div class="code-wrap">', unsafe_allow_html=True)
        st.code("""
for raca in ["Branco", "Preto", "Demais Raças"]:
    fig, ax = plt.subplots(figsize=(16, 6))
    for j, loc in enumerate(["Brasil", "Rio Grande do Norte"]):
        df_ = tb_raca[(tb_raca["raca"]==raca) & (tb_raca["local"]==loc)].sort_values("trimestre")
        draw_line_sparse_markers(ax, df_["trimestre"], df_["taxa_informalidade"],
                                 label=loc, color=PAL_TOTAL[j])
    polish_axes(ax, ylabel="Taxa (%)")
    legend_center(fig, ["Brasil", "Rio Grande do Norte"], ncol=2)
    st.pyplot(fig, use_container_width=True)
        """, language="python")
        st.markdown('</div>', unsafe_allow_html=True)
