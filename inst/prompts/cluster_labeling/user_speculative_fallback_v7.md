PHI4-ORIENTED LABEL-REQUIRED RESCUE PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

This pass must return the least specific useful label rather than abstaining.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Decision rule:

1. Pick the strongest broad direction from the species evidence.
2. If one family is visible, use it.
3. If the cluster is mixed, use a mixed or transition label.
4. If the signal is weak, still return the safest broad label.

Preferred canonical labels in this pass:

- `woodland_like_assemblage`
- `wet_meadow_like_assemblage`
- `dry_meadow_like_assemblage`
- `transition_edge_assemblage`
- `mixed_generalist_assemblage`
- `mixed_herbaceous_assemblage`
- `ruderal_like_assemblage`

Helpful phi4-oriented species examples:

- `Fagus`, `Quercus`, `Mercurialis`, `Dryopteris`, `Galium odoratum`,
  `Anemone`, `Hedera`
  -> favor `woodland_like_assemblage`
- `Carex acuta`, `Iris pseudacorus`, `Filipendula ulmaria`,
  `Caltha palustris`, `Juncus effusus`, `Mentha aquatica`,
  `Deschampsia cespitosa`, `Lychnis flos-cuculi`
  -> favor `wet_meadow_like_assemblage`
- `Festuca ovina`, `Thymus serpyllum`, `Galium verum`,
  `Lotus corniculatus`, `Plantago lanceolata`, `Salvia pratensis`,
  `Bromus erectus`, `Achillea millefolium`
  -> favor `dry_meadow_like_assemblage`
- if two families overlap and neither is dominant
  -> favor `transition_edge_assemblage` or `mixed_generalist_assemblage`

Rules:

1. Prefer `status = "labeled"`.
2. Only use `status = "abstain"` if the JSON contract would otherwise be
   broken; in ordinary mixed cases, still label.
3. Keep `basis_in_data` factual and short: species presence, phi ranking,
   prototype consistency, cover pattern.
4. Keep habitat or moisture interpretation out of `basis_in_data`.
5. Keep `external_knowledge` empty.
6. Keep `key_species` short and concrete: usually 2 to 4 items.
7. Keep `interpretation_summary` to 1 or 2 short sentences and explicitly
   say that this is a broad best-effort orientation label when evidence is
   mixed.
8. If your chosen label contains `woodland`, `meadow`, `wet`, `dry`,
   `ruderal`, or `edge`, add one matching statement in
   `not_confirmed_by_data` saying that this is only a coarse orientation from
   species composition and is not a confirmed habitat assignment.
9. If nothing clearly fits, prefer `mixed_generalist_assemblage` over
   abstention.
10. Do not add markdown, commentary, or code fences.

Schema-critical rules:

- if `status = "labeled"`, `canonical_label` must be non-null lowercase
  snake_case
- if `status = "labeled"`, `display_label` must be non-null short plain
  English, not snake_case
- if `status = "labeled"`, `abstain_reason` must be `null`
- if `status = "abstain"`, `canonical_label` and `display_label` must be
  `null`, and `abstain_reason` must be non-empty

Return JSON only.
