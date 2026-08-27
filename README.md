# QGIS SVG cleaner

Shinylive app that rewrites SVG symbols so their fill, stroke, and stroke-width can be edited from QGIS's symbol properties, without breaking the file for every other renderer. For each uploaded SVG it:

- writes `fill`, `stroke`, and `stroke-width` into each shape as ordinary presentation attributes holding literal values
- adds a QGIS placeholder (`param(fill)`, `param(outline)`, `param(outline-width)`) alongside, inside `style=""`, for each box that is checked
- optionally copies every *other* property in a `<style>` rule — `stroke-linecap`, `stroke-linejoin`, `stroke-miterlimit`, `display`, `isolation` — onto the elements that rule matches
- comments out every `<style>` block, keeping it in the file for reference

It returns a single edited `.svg`, or a ZIP (`svg-params.zip`) for a batch. Runs entirely in the browser via WebAssembly — uploads never leave the client.

## Output format

```xml
<line fill="none" stroke="#000" stroke-width="2" class="st0"
      stroke-linecap="round" stroke-linejoin="round"
      style="stroke:param(outline) #000;stroke-width:param(outline-width)" .../>
```

Each value is declared twice on purpose. QGIS reads the `param()` form out of `style=""`; every spec-compliant renderer (e.g., InkScape, Illustrator) falls back to the presentation attribute.

## Develop

```r
shiny::runApp("app")
```

Requires `shiny` and `zip`.

## Deploy

Push to `main` → `.github/workflows/deploy.yml` runs `shinylive::export("app", "site")` and publishes to GitHub Pages. Enable Pages for the repo (Settings → Pages → Source: GitHub Actions) before the first push.

## Known limitations

- Only simple CSS selectors can be promoted: an optional type (or `*`) plus any number of `.class` / `#id` tokens. A selector using a combinator, attribute test, or pseudo-class is reported in the UI and left behind rather than matched incorrectly, and a stylesheet containing an at-rule (`@media`, `@font-face`) is skipped wholesale.
- On a shape, the sidebar is authoritative for `fill` / `stroke` / `stroke-width`: values the file already declares for those three are replaced, not preserved. Everything else the file declares is kept.
- A `<style>` block that itself contains `--` cannot be wrapped in an XML comment; those blocks are left in place and the user is warned. Properties are still promoted out of them, but the live stylesheet will outrank the presentation attributes.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for the full text.

## Support

Developed and maintained by the [Open Source Program Office](https://opensource.syracuse.edu/) at Syracuse University. Reach out for feedback and suggested improvements:

- [GitHub Issues](../../issues)
- [Email](mailto:ospo@syr.edu)

## Acknowledgments

This project was supported as part of grants (#[G2023-20946](https://sloan.org/grant-detail/g-2023-20946), #[G-2025-79206](https://sloan.org/grant-detail/g-2025-79206)) from the Alfred P. Sloan Foundation.
