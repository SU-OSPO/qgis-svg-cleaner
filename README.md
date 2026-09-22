# QGIS SVG cleaner

Shinylive app that rewrites SVG symbols so their fill, stroke, and stroke-width can be edited from QGIS's symbol properties, without breaking the file for every other renderer. For each uploaded SVG it:

- writes `fill`, `stroke`, and `stroke-width` into each shape as presentation attributes, holding a QGIS placeholder (`param(fill)`, `param(outline)`, `param(outline-width)`) for each box that is checked and a literal otherwise
- repeats the literal values on the root `<svg>`, so shapes have something real to inherit outside QGIS
- optionally copies every *other* property in a `<style>` rule — `stroke-linecap`, `stroke-linejoin`, `stroke-miterlimit`, `display`, `isolation` — onto the elements that rule matches
- comments out every `<style>` block, keeping it in the file for reference

It returns a single edited `.svg`, or a ZIP (`svg-params.zip`) for a batch. Runs entirely in the browser via WebAssembly — uploads never leave the client.

## Output format

```xml
<svg viewBox="0 0 48 48" fill="none" stroke="#000" stroke-width="2">
  <line fill="none" stroke="param(outline) #000" stroke-width="param(outline-width)"
        class="st0" stroke-linecap="round" stroke-linejoin="round" .../>
</svg>
```

The placeholders go in the presentation attributes, and the literal fallbacks go on the root.

- **The placeholder must be in the attribute, not in `style=""`.** Qt5 (the renderer QGIS draws with) applies a presentation attribute *on top of* `style=""`, the reverse of the CSS cascade.
- **The root needs the literals.** A `param()` is not a valid paint value, so every spec-compliant renderer ignores the attribute. Inheriting from the root applies it throughout the image.
- **`param(outline-width)` carries a default, in millimetres.** QGIS reads this in millimetres (`0.2` placeholder).
- **Properties are promoted out of the stylesheet before it is commented out.** Qt ignores `<style>` blocks entirely. Promoting these properties ensures they aren't silently discarded.

`param(fill)` stays off by default. QGIS turns a param default into a `QColor`, and `QColor("none")` is invalid and reports itself as `#000000`, so pairing `param(fill)` with a "no fill" default floods line art solid black. The app warns if you combine the two.

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
- A `<style>` block that itself contains `--` cannot be wrapped in an XML comment; those blocks are left in place and the user is warned. Properties are still promoted out of them, but in a renderer that honours stylesheets the live rules will outrank the promoted presentation attributes.
- QGIS stores stroke width in millimetres and does not scale it with the marker size, so no single default is right everywhere.
- The literal fallbacks on the root `<svg>` are inherited by *every* descendant, not just the shapes the app rewrites.
- Opened directly in a Qt-based viewer (rather than through QGIS) the file draws nothing, because Qt resolves the invalid `stroke-width` to zero instead of inheriting.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for the full text.

## Support

Developed and maintained by the [Open Source Program Office](https://opensource.syracuse.edu/) at Syracuse University. Reach out for feedback and suggested improvements:

- [GitHub Issues](../../issues)
- [Email](mailto:ospo@syr.edu)

## Acknowledgments

This project was supported as part of grants (#[G2023-20946](https://sloan.org/grant-detail/g-2023-20946), #[G-2025-79206](https://sloan.org/grant-detail/g-2025-79206)) from the Alfred P. Sloan Foundation.
