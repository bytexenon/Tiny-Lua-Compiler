# Contributing to Tiny Lua Compiler (TLC)

Thanks for your interest in contributing.

TLC is an educational Lua 5.1 compiler and VM focused on clarity, correctness, and approachability. Contributions are very welcome - from bug fixes and tests to docs and performance improvements.

## TL;DR

1. Open an issue (or pick an existing one).
2. Keep changes focused and small.
3. Add/update tests.
4. Keep Lua 5.1 compatibility and cross-version runtime compatibility.
5. **Never run an auto-formatter on `tlc.lua`.**
6. Submit a PR with a clear explanation.

---

## Project Goals

Please keep these in mind when proposing changes:

- **Educational first**: readable code and comments are a feature.
- **Lua 5.1 semantics**: behavior should match Lua 5.1 unless explicitly documented otherwise.
- **Single-file core**: the compiler/VM lives in `tlc.lua` by design.
- **Low dependency footprint**: do not use external files or libraries in the core file.

If a change improves performance but hurts clarity, explain the tradeoff in the PR.

---

## Ways to Contribute

- Fix bugs (parser/codegen/vm behavior mismatches).
- Add regression tests.
- Improve comments and docs.
- Improve error messages.
- Optimize hot paths without changing semantics.
- Add benchmarks or benchmark coverage.

---

## Development Setup

### Requirements

- Lua **5.1** (recommended for development/validation)

### Run tests

```bash
lua5.1 tests/test.lua
```

### Run benchmarks

```bash
lua5.1 benchmarks/benchmark.lua
```

---

## Contribution Workflow

1. **Discuss first** for non-trivial changes
   - Open an issue describing problem, approach, and tradeoffs.
2. **Create a branch**
   - Example: `fix/repeat-until-close`, `docs/parser-comments`
3. **Implement in small commits**
   - Keep each commit logical and reviewable.
4. **Add/adjust tests**
   - Every bug fix should include a regression test when possible.
5. **Open a PR**
   - Include context, what changed, why, and test evidence.

---

## Coding Guidelines

### Compatibility and semantics

- Preserve Lua 5.1 behavior unless intentionally changing a documented non-goal.
- Avoid "clever" rewrites that make behavior harder to reason about.
- TLC must run on all Lua versions (5.1 through 5.5+), while targeting Lua 5.1 bytecode semantics.
- Do not use functions or syntax that breaks on any supported Lua version.
- If using functions moved or removed in newer versions, create a compatibility alias at the top-level constants section:
  ```lua
  local unpack = (unpack or table.unpack) -- Lua 5.1/5.2+ compatibility.
  ```
  Common functions requiring aliases or custom implementation: `unpack`/`table.unpack`, `math.frexp` (completely removed in Lua 5.2+), `string.pack`/`string.unpack` (Lua 5.3+)
- Do not use `goto` - it was introduced in Lua 5.2 and is not available in Lua 5.1.

### Style

**Do not run any automatic formatter (`stylua`, `lua-fmt`, etc.) on `tlc.lua`.**
The file is formatted by hand with intentional alignment and spacing. An
auto-formatted diff will be rejected even if the logic is correct.

- Match existing style in `tlc.lua` exactly - indentation, spacing, alignment.
- Prefer explicit, descriptive names over short or cryptic ones.
  - Good: `conditionRegister`, `loopStartPC`
  - Bad: `r`, `pc`, `tmp`
- Keep code lines under 80 characters.
  - If a line must exceed 80 chars (rare), wrap readably - don't compact to fit.
- Align related assignments vertically when the surrounding code already does so:
  ```lua
  local foo    = ...
  local barBaz = ...
  ```
- Declare variables as `local` unless there is a specific reason not to.
- Avoid deeply nested ternary-style expressions; break them into named locals.
- Keep hot-path code changes accompanied by a short rationale comment.
- Avoid unrelated refactors, renames, or whitespace-only changes in bug-fix PRs.

### Comments

TLC comments are part of the product - they are what makes the codebase educational.

- Keep comments accurate, concrete, and adjacent to the code they explain.
- Explain _why_, not just _what_, especially for non-obvious decisions.
- Do not leave commented-out code in PRs.
- Update all affected comments in the same PR as the behavior change.
- Do not paraphrase existing comments unnecessarily - only update if content is wrong or stale.
- Add opcode comments when emitting new instructions.

---

## Testing Expectations

For behavior changes, include at least one of:

- A new case in `tests/test.lua`.
- A focused test file if needed.
- A reduced reproducer in the PR description.

Suggested regression areas:

- Multi-return semantics
- Upvalue capture and closing
- Loop control flow (`break`, `repeat-until`)
- Method calls / implicit `self`
- Numeric and generic `for`

---

## Performance Changes

Performance work is welcome, especially in tokenizer and VM hot paths.

If your PR is performance-focused:

- Include before/after benchmark numbers.
- Mention environment (Lua version, OS, CPU).
- Confirm no semantic changes.

---

## Pull Request Checklist

Before opening a PR, verify:

- [ ] Change is scoped and explained.
- [ ] Lua 5.1 bytecode semantics preserved.
- [ ] Cross-version Lua runtime compatibility preserved (5.1-5.5+).
- [ ] No Lua 5.2+ syntax or functions used without a compatibility alias.
- [ ] `tlc.lua` was **not** run through an auto-formatter.
- [ ] Tests pass locally (`lua5.1 tests/test.lua`).
- [ ] New or changed behavior is covered by a test.
- [ ] Affected comments and docs updated in the same PR.
- [ ] No unrelated refactors, renames, or whitespace changes.
- [ ] New/edited code follows the 80-column line limit.
- [ ] Commit messages are clear and scoped.

---

## Reporting Bugs

Please include:

1. **Input code** (minimal repro).
2. **Expected behavior**.
3. **Actual behavior** (error/output).
4. **Lua version + OS**.
5. If possible, whether standard Lua 5.1 differs.

Minimal repros help a lot.

---

## Scope Notes (Important)

Some requests may be intentionally out of scope for TLC (e.g., heavy optimization pipeline, major architecture split), unless they strongly support educational goals.

If proposing a larger feature, please open an issue first so we can align on direction.

---

## Community

Be kind, constructive, and specific in reviews and discussions.

Thanks again for contributing ❤️
