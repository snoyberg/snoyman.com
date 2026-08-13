# Snoyman web family brand guidelines

These guidelines define the shared visual language for Michael Snoyman's sites and related projects. They are intentionally a design system **without** a shared component library: sites should feel related without becoming clones or acquiring runtime/build-time coupling to another repository.

Current core sites:

- `snoyman.com`
- `veloxwarp.com`
- `TryCrypto`

Future sites should start here unless they have a deliberate reason to diverge.

## Brand goal

A visitor moving between these sites should feel that they came from the same workshop: technically serious, calm, precise, approachable, and built by people rather than a marketing machine.

Family resemblance should come from repeated design decisions, not from putting the same logo on every product.

## Principles

### 1. Technical, not "techy"

Prefer clarity and confidence over visual spectacle. Avoid generic technology and startup visual clichés: neon gradients, glowing interfaces, glassmorphism everywhere, gratuitous motion, fake terminal decoration, dense dashboard chrome, and decorative network diagrams that communicate nothing.

### 2. Content first

Typography, hierarchy, spacing, and readable measure do most of the work. Decoration should support content rather than compete with it.

### 3. Calm surfaces, strong ink, restrained accents

The family baseline is light-mode-first with off-white or white surfaces, dark blue/ink text, teal as the primary accent, subtle borders, and modest shadows.

Dark sections are welcome when they have a job to do—code/output areas, workbenches, footers, or strong contrast moments—but an entire product does not need to become dark mode to feel technical.

### 4. Related, not identical

Each product may have one or two distinctive traits, such as a purposeful display typeface, a warmer surface palette, a secondary accent, or a stronger geometric treatment. Those traits can make a product memorable while the underlying family remains recognizable.

A product-specific trait should feel intentional. Randomly choosing a different blue, radius scale, font stack, or shadow treatment is not product identity.

### 5. Accessible by default

Aim for WCAG AA contrast for ordinary text and controls. Do not rely on color alone for state. Preserve visible keyboard focus. Respect `prefers-reduced-motion`. Interactive targets should generally be at least 44px in one dimension.

The lighter teals already used in some sites are good decorative colors but are too low-contrast for ordinary text on white. Use the darker family teal for text and controls.

## Core color roles

These are semantic roles, not a command that every site use every literal value everywhere.

| Role | Value | Use |
| --- | --- | --- |
| Navy | `#102A43` | headings, strong UI, dark brand fields |
| Ink | `#1F2933` | primary body text |
| Muted | `#627D98` | secondary copy and metadata |
| Teal | `#087F8C` | accessible links, controls, primary accent |
| Teal bright | `#2F9E9E` | decorative lines, large marks, soft backgrounds; avoid for small text on white |
| Rule | `#D9E2EC` | borders and separators |
| Mist | `#F7FAFC` | cool page background |
| White | `#FFFFFF` | primary surface |
| Warm paper | `#F3EFE5` | optional product-specific surface |
| Coral | `#D96C2F` | optional secondary/status accent; use sparingly |

Suggested CSS starting point:

```css
:root {
  --family-navy: #102a43;
  --family-ink: #1f2933;
  --family-muted: #627d98;
  --family-teal: #087f8c;
  --family-teal-bright: #2f9e9e;
  --family-rule: #d9e2ec;
  --family-mist: #f7fafc;
  --family-white: #ffffff;
  --family-warm-paper: #f3efe5;
  --family-coral: #d96c2f;
}
```

Projects may alias these into their own semantic tokens (`--brand`, `--surface`, `--evidence`, etc.). Prefer semantic aliases over scattering literals throughout stylesheets.

## Typography

### Shared default

Use a clean sans-serif UI/body stack. Inter is the preferred family face when available, with system fallbacks:

```css
font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
```

Do not require a webfont download merely to make a page usable. The system fallback should look intentional.

### Product display type

A product may add **one purposeful display family** when it materially contributes to its character. Keep navigation, forms, and ordinary UI in the shared sans-serif unless there is a strong reason otherwise.

### Monospace

Use monospace for actual machine-oriented material: hashes, keys, signatures, code, protocol identifiers, structured output, or compact technical metadata. Do not use mono broadly as a shortcut for "developer aesthetic."

### Hierarchy

- Headlines: bold or semibold, tight tracking, compact line height.
- Body: comfortable line height, usually `1.55`–`1.75`.
- Long prose: target roughly `48rem`–`54rem` measure.
- Eyebrows/metadata: small, semibold/bold, optionally uppercase with increased tracking; use sparingly.
- Giant display headings are fine on product landing pages, but the rest of the page should remain quiet enough to support them.

## Layout and spacing

Use generous whitespace and a small number of obvious content widths rather than filling every viewport pixel.

Recommended conventions:

- General content shell: approximately `920px`–`1080px` maximum width.
- Long prose: approximately `48rem`–`54rem`.
- Page gutters: at least `24px` on small screens and around `40px` on larger screens when practical.
- Major section spacing: roughly `64px`–`112px` on desktop, reduced responsively.
- Related-item spacing should be visibly smaller than section spacing.

Do not make every section a card. Empty space and rules are valid grouping mechanisms.

## Shape, borders, and shadow

The family should feel crisp but not severe.

- Use subtle 1px borders in `--family-rule` or an equivalent translucent ink.
- Typical control/card radii should be modest: about `6px`–`14px`.
- Large hero or isolated presentation cards may go to about `20px`–`28px`.
- Avoid mixing many radius scales on one page.
- Shadows should be soft and low-opacity, principally for separation. Avoid heavy elevation stacks.
- A product may intentionally choose a sharper geometry. If it does, apply that choice consistently.

## Links and controls

- Links should remain recognizably links. Underlines are encouraged in prose.
- Primary buttons should use navy or the accessible dark teal with high-contrast text.
- Secondary buttons should generally use an outline/quiet treatment rather than another saturated color.
- Hover is enhancement, never the only sign that something is interactive.
- Keyboard focus must remain clearly visible.
- Avoid pill-shaped buttons as a universal default. Reserve pills for tags, statuses, filters, or compact metadata where the shape conveys meaning.

## Imagery and marks

Different products should keep their own marks. Do not force the Velox Warp logo onto unrelated products such as TryCrypto.

Shared visual cues can appear in:

- navy + teal color relationships;
- line-based geometric marks;
- restrained diagrams;
- crisp technical illustrations;
- consistent OG-image typography and spacing.

Prefer SVG for logos and simple marks when practical. Provide appropriate raster fallbacks where platforms require them.

## Motion

Motion should explain state or reinforce spatial relationships. Keep transitions short and subtle, usually around `120ms`–`220ms`.

Avoid ambient motion, parallax, automatic carousels, and animation added solely to make a static page feel "alive." Always honor `prefers-reduced-motion`.

## Voice reflected in UI

Visual design and copy should reinforce one another:

- precise rather than grandiose;
- direct rather than salesy;
- confident without pretending certainty;
- willing to expose technical detail when it matters;
- human and occasionally playful, but never gimmicky.

Product copy may of course have its own voice. TryCrypto can sound more instructional; snoyman.com can be more personal.

## Product profiles

### snoyman.com

Role: personal site, blog, professional landing page.

Keep:

- clean sans-serif UI;
- highly readable long-form serif body for blog articles;
- teal + ink palette;
- personal photography;
- content density appropriate for a long-lived personal archive.

Avoid turning it into a corporate product page. It should feel like the human center of the family.

### Velox Warp

Role: company / consulting identity.

This is the cleanest expression of the base family system:

- navy, teal, white/mist;
- Inter/system sans;
- strong typographic hierarchy;
- sparse, professional composition;
- existing line-based Velox Warp mark.

Use this site as the baseline when a new project has no stronger product-specific direction.

### TryCrypto

Role: approachable interactive cryptography education.

Stay close to the base system, with slightly friendlier educational surfaces:

- off-white background;
- white lesson surfaces;
- dark output/code fields;
- generous spacing;
- minimal visual noise.

As the site grows, adopt the family navy/teal semantic tokens rather than inventing a new palette.

## Starting a new site

For a new project:

1. Start from the core color roles and Inter/system sans stack.
2. Use the spacing, border, and accessibility conventions above.
3. Choose at most one strong product-specific visual idea initially: e.g. a display typeface, warm vs cool surface, distinctive diagram style, or secondary accent.
4. Document that deviation in the project's `AGENTS.md` so future work reinforces rather than erodes it.
5. Before adding a new color, radius family, typography system, or recurring decoration, ask whether an existing family choice already solves the problem.

## Agent / implementation policy

Repositories with user-facing web UI should contain an `AGENTS.md` instruction that:

- identifies this document as the canonical family design reference;
- records any deliberate product-specific deviations;
- requires new UI work to preserve accessibility and responsive behavior;
- tells the agent to check the canonical document at the start of substantive UI/branding work and again before finalizing a substantial UI PR when GitHub access is available;
- tells the agent to surface a conflict rather than silently overriding a product requirement or deliberate sub-brand decision;
- requires proposed **family-wide** design-system changes to be made in (or accompanied by) a PR against `snoyberg/snoyman.com`, rather than silently creating a local fork of the policy.

Canonical source:

`https://github.com/snoyberg/snoyman.com/blob/master/docs/brand-guidelines.md`

## Change policy

Treat these guidelines as defaults, not immutable law. When a site genuinely benefits from a different choice, make the deviation explicit and intentional.

A family-wide guideline change should answer two questions:

1. Is this an improvement to the shared design language rather than a local preference?
2. Which existing sites should be migrated, and is that migration worth the churn?

When a project discovers such an improvement, update the canonical guide first (or in a companion PR) and then apply it locally. Do not let a product repo become the accidental source of truth for a family-wide rule.

Do not mechanically restyle stable pages merely because a token changed. Consistency is a means to better products, not an end in itself.
