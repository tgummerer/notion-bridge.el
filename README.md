# notion-bridge

> **WARNING: This package was vibe coded with Claude. I tested it very lightly, but don't come complain if it eats some of your notion pages**

> **WARNING: I'm not going to care about backwards compatibility here. Things can and will change as I see fit**

## What it does

notion-bridge is a bidirectional sync bridge between [Notion](https://notion.so)
and Emacs org-mode.  You can pull Notion pages into org buffers, edit them
locally with all the power of org-mode, and push changes back.  It also
supports pulling your entire Notion workspace into a directory of interlinked
org files.

## Installation

### use-package with load-path

```elisp
(use-package notion-bridge
  :load-path "~/path/to/notion-bridge"
  :after org)
```

### straight.el

```elisp
(straight-use-package
 '(notion-bridge :type git :host github :repo "user/notion-bridge"))
```

### Dependencies

- Emacs 28.1+
- `transient` (ships with recent Emacs; otherwise available from MELPA)
- `curl` on your `$PATH`

## Setup

### 1. Create a Notion integration

1. Go to <https://www.notion.so/my-integrations>
2. Click **New integration**, give it a name, and select the workspace.
3. Copy the **Internal Integration Secret** (starts with `ntn_` or `secret_`).
4. In each Notion page you want to access, click **...** > **Add connections**
   and select your integration.

### 2. Configure the token in Emacs

**Option A -- interactive:**

```
M-x notion-bridge-set-token
```

This saves the token to your `auth-source` file (e.g. `~/.authinfo.gpg`) so
it persists across sessions.

**Option B -- auth-source manually:**

Add a line to `~/.authinfo` or `~/.authinfo.gpg`:

```
machine api.notion.com password ntn_YOUR_TOKEN_HERE
```

**Option C -- set directly:**

```elisp
(setq notion-bridge-api-token "ntn_YOUR_TOKEN_HERE")
```

## Commands

All commands are available through the transient menu bound to `C-c n` in
notion-bridge-mode buffers.

| Key     | Command                        | Description                                    |
|---------|--------------------------------|------------------------------------------------|
| `C-c n p` | `notion-bridge-pull`         | Pull a Notion page by ID into an org buffer    |
| `C-c n a` | `notion-bridge-pull-all`     | Pull all accessible pages into a directory     |
| `C-c n P` | `notion-bridge-push`         | Push the current org buffer back to Notion     |
| `C-c n r` | `notion-bridge-refresh`      | Re-pull the current page in-place              |
| `C-c n s` | `notion-bridge-search`       | Search Notion and pull a selected result       |
| `C-c n o` | `notion-bridge-open-page`    | Open a previously-pulled page from the index   |
| `C-c n b` | `notion-bridge-browse`       | Open the current page in your browser          |
| `C-c n i` | `notion-bridge-status`       | Show sync status in the minibuffer             |
| `C-c n c` | `notion-bridge-comment`      | Post a comment (or reply to a thread at point) |
| `C-c n t` | `notion-bridge-set-token`    | Set and persist the Notion API token           |
| `C-c n l` | `notion-bridge-link-buffer`  | Link the current buffer to a Notion page ID    |

## Features

- **Bidirectional sync** -- pull pages from Notion to org, push edits back.
- **Inter-page links** -- Notion page mentions and URLs become org file links
  when using `pull-all`; they resolve back on push.
- **Nested lists** -- arbitrarily nested bullet, numbered, and checkbox lists
  round-trip correctly, including children blocks.
- **Toggle blocks** -- rendered as `#+begin_details` / `#+end_details` special
  blocks with the toggle title as a parameter.
- **Code blocks** -- language-tagged `#+begin_src` / `#+end_src` blocks.
- **Comments** -- page-level and block-level Notion comments are displayed as
  inline overlays with a wavy underline indicator.  Hover (or use eldoc) to
  read the comment text.  A full comments section is appended at the bottom of
  the buffer.  You can post new comments or reply to threads with
  `notion-bridge-comment`.
- **Parallel API calls** -- block-level comment fetching and bulk block
  deletion use parallel curl processes batched in groups of 3 to stay within
  Notion's rate limits.
- **Conflict detection** -- push checks whether the remote page changed since
  your last pull and warns before overwriting.
- **Sidecar cache** -- block IDs, hashes, and comment data are persisted in
  `~/.emacs.d/.cache/notion-bridge/` so that the mode can restore state when
  reopening files.

## File format

Pulled org files contain standard org-mode content plus two special keywords:

```org
#+TITLE: My Page
#+NOTION_PAGE_ID: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
#+NOTION_LAST_SYNCED: 2026-03-26T12:00:00+0000
```

- `NOTION_PAGE_ID` links the buffer to a Notion page.
- `NOTION_LAST_SYNCED` records when the page was last pulled for conflict
  detection.

`notion-bridge-mode` activates automatically on any org buffer that contains a
`NOTION_PAGE_ID` keyword.

## Architecture

| File                     | Responsibility                                            |
|--------------------------|-----------------------------------------------------------|
| `notion-bridge-api.el`   | curl-based synchronous and parallel HTTP client for the Notion API.  Handles auth, rate-limit retries, pagination. |
| `notion-bridge-org.el`   | Bidirectional conversion between Notion block JSON (plists) and org-mode text.  Handles rich text annotations, inline markup parsing, and all supported block types. |
| `notion-bridge-cache.el` | Sidecar cache using readable elisp sexps.  Stores block ID mappings, content hashes, and serialized comments keyed by md5 of the org file path. |
| `notion-bridge-sync.el`  | Orchestration layer: pull, push, pull-all, conflict detection, comment fetching/rendering, block-tracked insertion for overlay placement. |
| `notion-bridge.el`       | User-facing commands, minor mode, transient menu, eldoc integration. |

## Testing

Run all tests:

```sh
make test
```

Or directly:

```sh
emacs -Q -batch -L . -l ert \
  -l test/notion-bridge-org-test.el \
  -l test/notion-bridge-api-test.el \
  -l test/notion-bridge-cache-test.el \
  -l test/notion-bridge-sync-test.el \
  -f ert-run-tests-batch-and-exit
```

Individual test suites:

```sh
make test-org
make test-api
make test-cache
make test-sync
```

## Known limitations

- **Rate limits** -- Notion enforces 3 requests/second.  The parallel request
  helper batches in groups of 3 with a short delay between batches, but
  heavy use (large workspaces with many pages) can still hit 429 errors.
- **Unsupported block types** -- some Notion block types (e.g. tables,
  columns, synced blocks, databases inline) are not fully supported.  Unknown
  types fall back to rendering their rich text content, if any.
- **Comment positions** -- Notion comments are associated with blocks, not
  character ranges.  The overlay placement maps comments to the line where
  the block appears, but cannot highlight the specific text range the
  commenter selected in Notion.
- **Large pages** -- pages with hundreds of blocks require many sequential API
  calls for recursive block fetching, which can be slow.
- **Title property** -- the push command updates the page title from
  `#+TITLE`, but only for the standard "title" property type.
