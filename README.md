# QGIS SVG cleaner

Shinylive app that rewrites SVG symbols so their fill, stroke, and stroke-width can be edited from QGIS's symbol properties. For each uploaded SVG it:

- comments out every `<style>` block (required — CSS rules outrank presentation attributes, so inline parameters would otherwise never take effect)
- inserts `fill`, `stroke`, and `stroke-width` into each shape, either as a QGIS placeholder (`param(fill)`, `param(outline)`, `param(outline-width)`) or as a fixed literal, depending on which boxes are checked

It returns a single edited `.svg`, or a ZIP (`svg-params.zip`) for a batch. Runs entirely in the browser via WebAssembly — uploads never leave the client.

## Develop

```r
shiny::runApp("app")
```

Requires `shiny` and `zip`.

## Deploy

Push to `main` → `.github/workflows/deploy.yml` runs `shinylive::export("app", "site")` and publishes to GitHub Pages. Enable Pages for the repo (Settings → Pages → Source: GitHub Actions) before the first push.

## Known limitations

- An SVG that already has inline `fill` / `stroke` / `stroke-width` attributes would end up with duplicates.
- A `<style>` block that itself contains `--` cannot be wrapped in an XML comment; those blocks are left in place and the user is warned.

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for the full text.

## Support

Developed and maintained by the [Open Source Program Office](https://opensource.syracuse.edu/) at Syracuse University. Reach out for feedback and suggested improvements:

- [GitHub Issues](../../issues)
- [Email](mailto:ospo@syr.edu)

## Acknowledgments

This project was supported as part of grants (#[G2023-20946](https://sloan.org/grant-detail/g-2023-20946), #[G-2025-79206](https://sloan.org/grant-detail/g-2025-79206)) from the Alfred P. Sloan Foundation.
