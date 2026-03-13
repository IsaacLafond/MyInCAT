groupby_choices <- c(
  # "None" = NULL,
  "Experiment" = "experiment",
  "Sample" = "orig.ident",
  "Cluster" = "seurat_clusters",
  "Subcluster" = "subcluster"
)

# ===========================
# Cell types and subclusters
# ===========================
sat_cells <- c(
  "Quiescent SCs",
  "Activated SCs",
  "Cachectic SCs",
  "Pro-angiogenic SCs"
)
myoblasts <- c(
  "Committed myoblasts",
  "Differentiating myoblasts (2b)",
  "Differentiating myoblasts (2b/x)",
  "Differentiating myoblasts (2x)"
)
myonuclei <- c(
  "Type 1 myonuclei",
  "Type 2a/x myonuclei",
  "Type 2b myonuclei",
  "Type 2b/x myonuclei",
  "Type 2x myonuclei"
)
endothelial_cells <- c(
  "Capillary ECs",
  "Arterial ECs",
  "Venous ECs"
)
vsmcs <- c(
  "vSMCs"
)
pericytes <- c(
  "Pericytes"
)
faps <- c(
  "Stem FAPs",
  "Activated FAPs",
  "Pro-remodeling FAPs",
  "Pro-angiogenic FAPs",
  "Pro-adipogenic FAPs",
  "Immuno FAPs",
  "Tenm2+ FAPs"
)
tenocytes <- c(
  "Tenocytes"
)
platelets <- c(
  "Platelets"
)
mast_cells <- c(
  "Mast cells"
)
dendritic_cells <- c(
  "Dendritic cells"
)
monocytes <- c(
  "Patrolling monocytes",
  "Inflammatory monocytes"
)
macrophages <- c(
  "M1 macrophages",
  "M1/M2 macrophages",
  "M2 macrophages"
)
neutrophils <- c(
  "Promyelocytes",
  "Myelocytes",
  "Mature neutrophils"
)
b_cells <- c(
  "Pro-B cells",
  "Naive B cells",
  "Immature B cells",
  "Regulatory B cells",
  "Plasma cells"
)
t_cells <- c(
  "Naive T cells",
  "Cytotoxic T cells"
)
nk_cells <- c(
  "NK cells"
)
neural_glial_cells <- c(
  "Neural/glial cells"
)
schwann_cells <- c(
  "Schwann cells"
)

# ===========================
# Subset options tree
# ===========================
subset_choices <- c(
  # experiment
  "Agca_snLLC" = c(
    # samples
    "Agca_snLLC_CTL" = c(
      "Satellite cells" = sat_cells,
      "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      # "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      # "T cells" = t_cells,
      # "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Agca_snLLC_LLC" = c(
      "Satellite cells" = sat_cells,
      "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      # "Mast cells" = mast_cells,
      # "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    )
  ),
  # experiment
  "Brown_scLLC" = c(
    "Brown_scLLC_CTL" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Brown_scLLC_2w" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Brown_scLLC_2.5w" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Brown_scLLC_3.5w" = c(
      "Satellite cells" = sat_cells,
      "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    )
  ),
  # experiment
  "Kim_scB16F10" = c(
    "Kim_scB16F10_CTL" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Kim_scB16F10_B16F10" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    )
  ),
  # experiment
  "Pryce_scC26" = c(
    "Pryce_scC26_CTL" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Pryce_scC26_C26" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    )
  ),
  # experiment
  "Pryce_scKPP" = c(
    "Pryce_scKPP_CTL" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Pryce_scKPP_KPP" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    )
  ),
  # experiment
  "Zhang_snKIC" = c(
    "Zhang_snKIC_CTL" = c(
      "Satellite cells" = sat_cells,
      # "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      # "Mast cells" = mast_cells,
      "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      # "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    ),
    "Zhang_snKIC_KIC" = c(
      "Satellite cells" = sat_cells,
      "Myoblasts" = myoblasts,
      "Myonuclei" = myonuclei,
      "Endothelial cells" = endothelial_cells,
      "vSMCs" = vsmcs,
      "Pericytes" = pericytes,
      "FAPs" = faps,
      "Tenocytes" = tenocytes,
      "Platelets" = platelets,
      # "Mast cells" = mast_cells,
      # "Dendritic cells" = dendritic_cells,
      "Monocytes" = monocytes,
      "Macrophages" = macrophages,
      "Neutrophils" = neutrophils,
      "B cells" = b_cells,
      # "T cells" = t_cells,
      "NK cells" = nk_cells,
      "Neural/glial cells" = neural_glial_cells,
      "Schwann cells" = schwann_cells
    )
  )
)



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