# Publishing

The public dashboard is published from root `index.html`.

## Published Artifacts

`index.Rmd` renders a non-self-contained dashboard to:

```text
index.html
index_files/
assets/
```

`index.html` contains the dashboard content and references JavaScript, CSS, and other generated dependencies under `index_files/`. Dashboard photos remain external resources rather than being downloaded and embedded by Pandoc. This keeps `index.html` below GitHub's per-file size limit and removes the render-time dependency on downloading every photo.

`assets/` contains the locally versioned Past the Grid identity tokens, SVGs
and Coastal shell styling. These are source assets rather than generated render
dependencies. The publish step verifies the required identity files and stages
them explicitly, so local preview and GitHub Pages use the same relative paths
without fetching identity assets from the public homepage.

The renderer builds both artifacts in a clean staging directory and replaces the existing pair only after a successful render. This prevents stale or duplicate dependencies accumulating in `index_files/`. The publish step also refuses dependency files with conflict-style names such as `leaflet 2.js`.

Pushing the dashboard artifacts and local identity assets publishes the updated dashboard through GitHub Pages. Rendering and publishing are separate operations.

## Before Publishing

Run:

```r
Rscript scripts/check_inputs.R
```

Publish only if the input check passes.

## Suggested Publishing Flow

1. Update metadata or source code.
2. Run `scripts/render_and_publish.sh`.

The shell entry point validates inputs, renders the dashboard, and publishes it sequentially. It stops without publishing if validation or rendering fails.

To inspect the generated output before publishing, continue to run the three underlying scripts separately.

## Generated Output

Generated output is intentionally committed by the explicit publish entry point:

- `index.html`
- `index_files/`
- `assets/`

Avoid publishing generated churn from failed or partial renders. Rendering never stages, commits, or pushes files.
