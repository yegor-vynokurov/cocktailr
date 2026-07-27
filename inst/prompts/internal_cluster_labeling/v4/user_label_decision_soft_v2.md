Task mode: `label_decision_soft_v2`

Return one broad useful short combined label or `ABSTAIN`.

This compatibility prompt is not used by the decomposed v4 flow, but remains
available for older callers that request the legacy label-decision stage.

Return plain text only. Do not use JSON or code fences.

{{LABEL_MODE_GUIDANCE_TEXT}}

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Draft reasoning:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
