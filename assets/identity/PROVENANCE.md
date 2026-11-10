# Past the Grid identity provenance

Coastal adopted this local identity layer from the canonical `pastthegrid-site`
repository at commit:

```text
9b0c33ef6036f2e86cf3d8dcfde73141256120fb
```

Adopted on 10 November 2026 for Coastal identity integration Slice 1.

## Adopted assets

- `symbol-dark.svg`: local copy of the geometry and attributes in
  `pastthegrid-site/assets/symbol-dark.svg`, with no visual modification.
- `favicon.svg`: local copy of the geometry and attributes in
  `pastthegrid-site/assets/favicon.svg`, with no visual modification.
- `tokens.css`: a deliberately scoped extraction of the colour, typography and
  spacing values in `pastthegrid-site/assets/identity.css`. Global homepage
  element and component rules are intentionally excluded.

The canonical specification remains
`pastthegrid-site/docs/visual-identity.md`. Coastal owns its application-specific
use of these assets in `assets/coastal.css` and remains independently buildable.
