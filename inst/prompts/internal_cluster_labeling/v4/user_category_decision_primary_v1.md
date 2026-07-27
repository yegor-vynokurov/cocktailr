Task mode: `category_decision_primary_v1`

Choose the broad ecological category for this cluster.

Return only the category name. Do not explain. Do not add any prefix.

Good answers look like:

dry grassland
woodland
transitional meadow

Bad answers:

CATEGORY: dry grassland
CATEGORY_LABEL: dry grassland
The category is dry grassland because species prefer open dry sites.
"dry grassland"

If no broad category is safe, return only:

ABSTAIN

{{LABEL_MODE_GUIDANCE_TEXT}}

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Draft reasoning:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
