# snoyman.com contributor instructions

These instructions apply to the entire repository.

## General agent collaboration

For general agent-collaboration and engineering guidance, see [P2P Agents](https://github.com/snoyberg/p2p-agents/blob/master/AGENTS.md). Treat that repository as general reference material; this file is authoritative for snoyman.com-specific instructions and takes precedence where it intentionally differs.

Do not mechanically recurse between guidance repositories. For visual work, the local canonical brand guide below is authoritative; the P2P Agents repository points back here intentionally for the shared Snoyman / Velox Warp visual system.

## Family brand system

`docs/brand-guidelines.md` is the canonical source of truth for the shared visual language used across snoyman.com, Velox Warp, TryCrypto, and related sites.

Before substantial UI, styling, branding, or visual-content work, read the current canonical guide. Re-check it before finalizing a substantial UI PR if the work may have introduced a new recurring pattern.

Stable family defaults include:

- light, calm surfaces with dark navy/ink text and restrained teal accents;
- Inter/system sans for ordinary UI, with deliberate product-specific display typography permitted;
- generous whitespace, readable measures, subtle borders, modest radii, and low-opacity shadows;
- clear keyboard focus, accessible contrast, responsive layouts, and reduced-motion support;
- product identity through a small number of deliberate deviations rather than unrelated palettes or component styles.

snoyman.com is the human/personal center of the family. Preserve its readable long-form blog treatment, personal photography, and less-corporate character.

If work in this repository reveals an improvement that should apply across the family, update `docs/brand-guidelines.md` in the same PR. If a change is intentionally specific to snoyman.com, document the exception locally rather than weakening the shared rule.
