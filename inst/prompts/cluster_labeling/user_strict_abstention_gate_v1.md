Task mode: `strict_abstention_gate_v1`

Primary objective:

Reduce false-positive ecological labels. Start from a default
presumption of abstention and switch to `status = "labeled"` only if the
evidence clearly supports one broad, defensible label.

Mandatory decision gate:

Use `status = "labeled"` only if all of the following are true:

1. Core coherence:
   topological species and phi-ranked species point to the same broad
   compositional or structural story.
2. Distinctiveness:
   prototype plots look internally consistent and borderline plots do
   not suggest a materially competing story.
3. Low inference jump:
   the label can be stated without assuming unmeasured soil chemistry,
   region, syntaxonomy, or hidden environmental context.
4. Limitation tolerance:
   the listed limitations do not materially weaken the label.

If any gate fails, or if you are uncertain whether it passes, use
`status = "abstain"`.

Additional decision policy:

- Transitional or mixed clusters should usually abstain, even when a
  loose family guess seems possible.
- A broad family guess such as `grassland`, `wetland`, or `woodland` is
  not enough by itself; the cluster should look distinct, not merely
  vaguely compatible.
- Opaque plot IDs such as `plot_016` carry no ecological meaning and
  must not influence interpretation.
- Prototype and borderline plot names are identifiers only. Use their
  scores and their relation to the core species pattern, not the text of
  the identifier itself.
- If you label instead of abstaining, confidence should usually stay at
  or below `0.60` unless the evidence is unusually strong.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `canonical_label` must be lowercase snake_case when present.
- `display_label` should be short and plain when present.
- `interpretation_summary` must explain why the cluster passed or failed
  the abstention gate.
- Every data-backed claim must cite valid evidence IDs.
- Every listed key species must cite valid evidence IDs.
- `external_knowledge` should usually stay empty in this mode.
- Use `not_confirmed_by_data` actively to block tempting but unsupported
  ecological interpretations.
- If `status = "abstain"`, keep `canonical_label` and `display_label`
  null.
- If `status = "abstain"`, still fill `ontology_slots` for broad
  descriptors only when they are directly supported by the provided
  evidence; otherwise leave the slot null.
- Use `checks_to_run` to say what extra contrast or ecological context
  would most help resolve the abstention.

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
