# ==============================================================================
# PATCH DAS FIGURAS DE REDE (Seções 16 e 17 do script)
# O que muda: (1) sem título dentro da imagem (a legenda do LaTeX já descreve);
# (2) cores fixas por tipo de controle, iguais nos dois painéis e nas duas figuras;
# (3) no painel N = 7, o BTG passa a "Privado nacional" (não há Pan para justificar
#     uma categoria própria); (4) legenda de cores única no painel de subperíodos;
# (5) nomes de arquivo iguais aos do LaTeX.
# Rode o script duas vezes (PAINEL <- "N7_principal" e "N8_extensao") e copie para
# a pasta figuras/ os 4 arquivos gerados em outputs/<painel>/graficos/.
# ==============================================================================

# ---- (A) Colocar logo antes da Seção 16 (depois de definir tipo_controle) ----
SUF <- if (length(NOMES) == 7) "n7" else "n8"

tipo_plot <- tipo_controle
if (!"BPAN4" %in% NOMES) tipo_plot["BPAC11"] <- "Privado nacional"

# Mesmas cores da paleta Set2, mas atribuídas por nome (não mudam entre painéis)
CORES_CONTROLE <- c(
  "Privado nacional"       = "#66C2A5",
  "Privado nacional (BTG)" = "#FC8D62",
  "Privado estrangeiro"    = "#8DA0CB",
  "Público federal"        = "#E78AC3",
  "Público estadual"       = "#A6D854"
)

# ---- (B) Seção 16: trocar a linha do V(g_net)$Tipo ----
# ANTES: V(g_net)$Tipo <- tipo_controle[V(g_net)$name]
V(g_net)$Tipo <- tipo_plot[V(g_net)$name]

# ---- (C) Seção 16: substituir o bloco "p_rede <- ggraph(layout_fr) + ... ggsave(...)" ----
p_rede <- ggraph(layout_fr) +
  geom_edge_link(
    aes(width = weight, alpha = weight),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap = circle(6, "mm"), color = "grey40"
  ) +
  scale_edge_width(range = c(0.3, 2.5), name = "|NET| par a par (p.p.)") +
  scale_edge_alpha(range = c(0.4, 0.9), guide = "none") +
  geom_node_point(aes(size = sqrt(TO), fill = Tipo), shape = 21, color = "black", stroke = 0.6) +
  scale_size_continuous(range = c(6, 16), name = "TO (raiz quadrada, p.p.)") +
  scale_fill_manual(values = CORES_CONTROLE, name = "Tipo de controle", drop = TRUE) +
  geom_node_text(aes(label = name), vjust = -1.6, size = 4, fontface = "bold") +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave(outfile(sprintf("graficos/rede_%s_amostra_completa.png", SUF)), p_rede,
       width = 11, height = 8, dpi = 300, bg = "white")

# ---- (D) Seção 17, dentro de construir_grafo_subperiodo() ----
# ANTES: V(g_sub)$Tipo <- tipo_controle[V(g_sub)$name]
V(g_sub)$Tipo <- tipo_plot[V(g_sub)$name]
# ANTES: scale_fill_brewer(palette = "Set2", guide = "none")
# DEPOIS (a legenda de cores é coletada uma única vez, abaixo):
#   scale_fill_manual(values = CORES_CONTROLE, name = "Tipo de controle", drop = TRUE)


p_subperiodos <- wrap_plots(paineis_sub, ncol = 2, guides = "collect") &
  guides(fill = guide_legend(nrow = 2, title.position = "top", override.aes = list(size = 5))) &
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.title = element_text(size = 9, face = "bold"),
        legend.text  = element_text(size = 8.5),
        legend.key.size = unit(4.5, "mm"),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(outfile(sprintf("graficos/rede_%s_subperiodos.png", SUF)), p_subperiodos,
       width = 12, height = 10.5, dpi = 300, bg = "white")