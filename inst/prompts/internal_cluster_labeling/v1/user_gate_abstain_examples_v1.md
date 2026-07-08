Task mode: `gate_abstain_examples_v1`

Primary objective:

Make a gate decision only: should the workflow proceed to full labeling,
or should it abstain now?

Return `decision = "label"` only if the cluster evidence is coherent,
distinctive, and does not require a large ecological inference jump.

Return `decision = "abstain"` if the cluster is mixed, transitional,
generic, weakly distinctive, or too underdetermined to justify a stable
label.

Negative examples for abstention:

- Example A:
  core woodland species are present, but prototype and borderline plots
  suggest a transition-rich forest edge and no sibling contrast is
  available. Broad woodland plausibility is not enough. Decision:
  `abstain`.
- Example B:
  wetland-like species occur together with dry-grassland indicators and
  the evidence supports only a loose mixed story, not one stable
  cluster identity. Decision: `abstain`.
- Example C:
  a tempting label would depend mainly on habitat knowledge, region,
  syntaxonomy, or reading meaning into opaque plot IDs rather than into
  the provided species and evidence IDs. Decision: `abstain`.

Positive example boundary:

- If the core species pattern is internally coherent, the prototype
  plots reinforce the same story, the borderline plots do not introduce
  a materially competing story, and the label can stay broad and
  evidence-backed, then `decision = "label"` is allowed.

Additional decision policy:

- Opaque plot IDs such as `plot_016` carry no ecological meaning.
- Prototype and borderline plot identifiers are not evidence of habitat
  type by themselves.
- If you are unsure whether the gate passes, choose `abstain`.
- Use `decision = "label"` only when you would be comfortable defending
  the cluster as a distinct broad type without adding unmeasured
  environmental assumptions.
- You are not being asked to produce the final label text in this stage.
  You are only deciding whether labeling should proceed.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `decision_summary` should explain why the gate passed or failed.
- Every listed key species must cite valid evidence IDs.
- Use `gate_checks` actively; if a check is uncertain, say so.
- Use `not_confirmed_by_data` to block unsupported but tempting
  ecological interpretations.
- If `decision = "abstain"`, `abstain_reason` must be explicit.
- If `decision = "label"`, `abstain_reason` must be null.
- `external_knowledge` is not available in this gate schema; keep the
  gate grounded in the provided evidence.

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
