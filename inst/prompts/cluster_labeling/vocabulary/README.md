# Coarse Label Vocabulary

This folder stores editable coarse label vocabularies for constrained
cluster-labeling prompts.

The current small-model constrained prompts use:

- `coarse_label_vocabulary_core_v1.json`

## Why this exists

For some local models, free-form label generation collapses into either:

- repeated abstention
- overly generic labels
- unstable label wording across similar clusters

A constrained vocabulary reduces the decision space. The model still has
to read the evidence, but it chooses from a short, explicit label list
instead of inventing a new label every time.

## File format

The JSON file contains a top-level `labels` list. Each label should
define:

- `canonical_label`
- `display_label`
- `short_description`
- `use_when`

The runtime turns this JSON into prompt text and injects it into the
selected prompt variant.

## How to customize safely

Recommended workflow:

1. Copy `coarse_label_vocabulary_core_v1.json`
2. Rename it clearly, for example:
   - `coarse_label_vocabulary_boreal_v1.json`
   - `coarse_label_vocabulary_central_asia_v1.json`
   - `coarse_label_vocabulary_project_x_v1.json`
3. Edit the copied file instead of mutating the old one in place
4. Point the runtime to the copied file with:

```r
options(
  cocktailr.cluster_label_vocabulary_path =
    "path/to/your/coarse_label_vocabulary_project_x_v1.json"
)
```

## What to keep small

For small local models, fewer labels usually work better.

Recommended first target:

- about 6 to 12 labels

If you add too many overlapping labels, the constrained prompt becomes
less constrained in practice.

## Good editing rules

- Prefer broad ecological directions over fine syntaxonomic names
- Keep `canonical_label` stable and lowercase snake_case
- Keep `display_label` short and human-readable
- Keep `short_description` compact and concrete
- Use `use_when` to explain the signal, not to write an essay

## Suggested expansion directions

If future datasets move beyond the current synthetic meadow/woodland
space, consider adding broad families such as:

- boreal conifer assemblage
- heath or dwarf-shrub assemblage
- alpine grass-forb assemblage
- steppe-like dry herb assemblage
- halophytic or saline assemblage
- shrubland or scrub assemblage
- riparian tall-herb assemblage

Prefer adding such families in a copied new vocabulary file rather than
silently changing the old one.
