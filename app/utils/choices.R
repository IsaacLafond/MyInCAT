groupby_choices <- list(
  "None" = NULL,
  "Experiment" = "experiment",
  "Sample" = "orig.ident",
  "Cluster" = "seurat_clusters",
  "Subcluster" = "subcluster"
)

subset_choices <- c(
  # experiment
  "Agca_snLLC" = c(
    # samples
    "Agca_snLLC_CTL" = c(),
    "Agca_snLLC_LLC" = c()
  ),
  "Brown_scLLC" = c(
    "Brown_scLLC_CTL" = c(),
    "Brown_scLLC_2w" = c(),
    "Brown_scLLC_2.5w" = c(),
    "Brown_scLLC_3.5w" = c()
  ),
  "Kim_scB16F10" = c(
    "Kim_scB16F10_CTL" = c(),
    "Kim_scB16F10_B16F10" = c()
  ),
  "Pryce_scC26" = c(
    "Pryce_scC26_CTL" = c(),
    "Pryce_scC26_C26" = c()
  ),
  "Pryce_scKPP" = c(
    "Pryce_scKPP_CTL" = c(),
    "Pryce_scKPP_KPP" = c()
  ),
  "Zhang_snKIC" = c(
    "Zhang_snKIC_CTL" = c(),
    "Zhang_snKIC_KIC" = c()
  )
)

# ===========================
# Cell types and subclusters
# ===========================
# sat_cells <- "Satellite cells" = c(
#   "Quiescent SCs",
#   "Activated SCs",
#   "Cachectic SCs",
#   "Pro-angiogenic SCs"
# )
# myoblasts <- "Myoblasts" = c(
#   "Committed myoblasts",
#   "Differentiating myoblasts (2b)",
#   "Differentiating myoblasts (2b/x)",
#   "Differentiating myoblasts (2x)"
# )
# myonuclei <- "Myonuclei" = c(
#   "Type 1 myonuclei",
#   "Type 2a/x myonuclei",
#   "Type 2b myonuclei",
#   "Type 2b/x myonuclei",
#   "Type 2x myonuclei"
# )
# endothelial_cells <- "Endothelial cells" = c(
#   "Capillary ECs",
#   "Arterial ECs",
#   "Venous ECs"
# )
# vsmcs <- "vSMCs" = c(
#   "vSMCs"
# )
# pericytes <- "Pericytes" = c(
#   "Pericytes"
# )
# faps <- "FAPs" = c(
#   "Stem FAPs",
#   "Activated FAPs",
#   "Pro-remodeling FAPs",
#   "Pro-angiogenic FAPs",
#   "Pro-adipogenic FAPs",
#   "Immuno FAPs",
#   "Tenm2+ FAPs"
# )
# tenocytes <- "Tenocytes" = c(
#   "Tenocytes"
# )
# adipocytes <- "Adipocytes" = c(
#   "Adipocytes"
# )
# platelets <- "Platelets" = c(
#   "Platelets"
# )
# mast_cells <- "Mast cells" = c(
#   "Mast cells"
# )
# dendritic_cells <- "Dendritic cells" = c(
#   "Dendritic cells"
# )
# monocytes <- "Monocytes" = c(
#   "Patrolling monocytes",
#   "Inflammatory monocytes"
# )
# macrophages <- "Macrophages" = c(
#   "M1 macrophages",
#   "M1/M2 macrophages",
#   "M2 macrophages"
# )
# neutrophils <- "Neutrophils" = c(
#   "Promyelocytes",
#   "Myelocytes",
#   "Mature neutrophils"
# )
# b_cells <- "B cells" = c(
#   "Pro-B cells",
#   "Naive B cells",
#   "Immature B cells",
#   "Regulatory B cells",
#   "Plasma cells"
# )
# t_cells <- "T cells" = c(
#   "Naive T cells",
#   "Cytotoxic T cells"
# )
# nk_cells <- "NK cells" = c(
#   "NK cells"
# )
# neural_glial_cells <- "Neural/glial cells" = c(
#   "Neural/glial cells"
# )
# schwann_cells <- "Schwann cells" = c(
#   "Schwann cells"
# )



# ===========================
# Experiments
# ===========================
  # Agca_snLLC
  # Brown_scLLC
  # Kim_scB16F10
  # Pryce_scC26
  # Pryce_scKPP
  # Zhang_snKIC
# ===========================
# Samples
# ===========================
  # Agca_snLLC_CTL
  # Agca_snLLC_LLC
  # Brown_scLLC_CTL
  # Brown_scLLC_2w
  # Brown_scLLC_2.5w
  # Brown_scLLC_3.5w
  # Kim_scB16F10_CTL
  # Kim_scB16F10_B16F10
  # Pryce_scC26_CTL
  # Pryce_scC26_C26
  # Pryce_scKPP_CTL
  # Pryce_scKPP_KPP
  # Zhang_snKIC_CTL
  # Zhang_snKIC_KIC
# ===========================
# Clusters
# ===========================
  # Satellite cells
  # Myoblasts	(only in Agca_snLLC_CTL, Agca_snLLC_LLC, Brown_scLLC_3.5w, Zhang_snKIC_KIC)
  # Myonuclei
  # Endothelial cells
  # vSMCs
  # Pericytes
  # FAPs
  # Tenocytes
  # Adipocytes (nowhere?)
  # Platelets
  # Mast cells (not in Zhang_snKIC_KIC, Zhang_snKIC_CTL, Agca_snLLC_LLC, Agca_snLLC_CTL)
  # Dendritic cells	(not in Zhang_snKIC_KIC, Agca_snLLC_LLC)
  # Monocytes
  # Macrophages
  # Neutrophils (not in Zhang_snKIC_CTL)
  # B cells
  # T cells	0 (not in Zhang_snKIC_KIC, Agca_snLLC_CTL)
  # NK cells (not in Agca_snLLC_CTL)
  # Neural/glial cells
  # Schwann cells
# ===========================
# Sub clusters
# ===========================
# Satellite cells
  # Quiescent SCs
  # Activated SCs
  # Cachectic SCs
  # Pro-angiogenic SCs
# Myoblasts
  # Committed myoblasts
	# Differentiating myoblasts (2b)
	# Differentiating myoblasts (2b/x)
	# Differentiating myoblasts (2x)
# Myonuclei
	# Type 1 myonuclei
	# Type 2a/x myonuclei
	# Type 2b myonuclei
	# Type 2b/x myonuclei
	# Type 2x myonuclei
# Endothelial cells
  # Capillary ECs
	# Arterial ECs
	# Venous ECs
# vSMCs	vSMCs
# Pericytes	Pericytes
# FAPs
	# Stem FAPs
	# Activated FAPs
	# Pro-remodeling FAPs
	# Pro-angiogenic FAPs
	# Pro-adipogenic FAPs
	# Immuno FAPs
	# Tenm2+ FAPs
# Tenocytes	Tenocytes
# Platelets	Platelets
# Mast cells	Mast cells
# Dendritic cells	Dendritic cells
# Monocytes
	# Patrolling monocytes
	# Inflammatory monocytes
# Macrophages
	# M1 macrophages
	# M1/M2 macrophages
	# M2 macrophages
# Neutrophils
	# Promyelocytes
	# Myelocytes
	# Mature neutrophils
# B cells
	# Pro-B cells
	# Naive B cells
	# Immature B cells
	# Regulatory B cells
	# Plasma cells
# T cells
	# Naive T cells
	# Cytotoxic T cells
# NK cells	NK cells
# Neural/glial cells	Neural/glial cells
# Schwann cells	Schwann cells