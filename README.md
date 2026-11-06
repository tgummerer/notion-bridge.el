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
- **Incremental push** -- push diffs the buffer against what you pulled and
  only edits, inserts, or deletes the blocks you changed.  Everything else in
  the page, including mentions, colors and comments on untouched blocks, is
  left exactly as it is.
- **Conflict detection** -- before writing, push re-reads the page and checks
  every block it is about to touch.  Blocks changed in Notion since your pull
  are listed and only overwritten if you confirm; changes elsewhere on the
  page are preserved.
- **Sidecar cache** -- the pulled block tree and comment data are persisted in
  `~/.emacs.d/.cache/notion-bridge/` so that conflict detection and comment
  overlays work after reopening files.

## Safety

- **Pages and databases are never deleted.**  DELETE requests for pages are
  refused outright, and so is deleting any block that is or contains a
  subpage, an inline database, or a synced block (deleting those in Notion
  trashes the page or breaks every copy).
- **Writes are confined to the open page.**  Every write is checked against
  the page you are pushing and the blocks fetched from it; anything else is
  refused before a request is made.  Nothing is written outside a push or a
  comment on the open page.
- **Nothing is written until the plan is checked.**  Push computes all edits,
  validates them against Notion, shows a summary (listing every deletion) and
  asks before writing.  If a write fails midway, the record of what succeeded
  is kept, so pushing again continues without duplicating content.
- **Local edits are not discarded silently.**  Pull, refresh and pull-all
  ask (or skip the file) when a buffer has unsaved or unpushed changes.
- **The token stays off the command line**: curl reads it from a private
  temporary config file, and request bodies are sent on stdin.

## File format

Pulled org files contain standard org-mode content plus these keywords:

```org
#+TITLE: My Page
#+NOTION_PAGE_ID: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
#+NOTION_LAST_SYNCED: 2026-03-26T12:00:00+0000
```

- `NOTION_PAGE_ID` links the buffer to a Notion page.
- `NOTION_LAST_SYNCED` records when the page was last synced (informational;
  conflict detection uses the cached block tree).

Blocks that can't be edited as org (subpages, databases, images, callouts,
embeds, columns, toggle headings, ...) appear as read-only placeholders:

```org
#+NOTION_BLOCK: 1a2b3c... child_page Meeting notes
# [[notion:1a2b3c...][open subpage]]
```

Leave the line where it is to keep the block; remove it to delete the block
(refused for subpages, databases and synced blocks).  The `#` lines are a
preview and are not pushed.  Text that org would misread (a line starting
with `* ` or `- `, stray `*`, `/` or `_`) is escaped with invisible
zero-width spaces, which are stripped again on push.

`notion-bridge-mode` activates automatically on any org buffer that contains a
`NOTION_PAGE_ID` keyword.

## Architecture

| File                     | Responsibility                                            |
|--------------------------|-----------------------------------------------------------|
| `notion-bridge-api.el`   | curl-based HTTP client for the Notion API.  Handles auth, retries (honouring Retry-After), pagination, and the write scope that confines every write to the open page. |
| `notion-bridge-org.el`   | Bidirectional conversion between Notion block JSON (plists) and org-mode text.  Handles rich text annotations, inline markup parsing, and all supported block types. |
| `notion-bridge-cache.el` | Sidecar cache using readable elisp sexps.  Stores block ID mappings, content hashes, and serialized comments keyed by md5 of the org file path. |
| `notion-bridge-sync.el`  | Orchestration layer: pull, diff-based push with validation and conflict detection, pull-all, comments. |
| `notion-bridge.el`       | User-facing commands, minor mode, transient menu, eldoc integration. |

## Testing

Run all tests:

```sh
make test
```

Or directly:

```sh
emacs -Q -batch -L . -L test -l ert \
  -l test/notion-bridge-org-test.el \
  -l test/notion-bridge-roundtrip-test.el \
  -l test/notion-bridge-api-test.el \
  -l test/notion-bridge-cache-test.el \
  -l test/notion-bridge-sync-test.el \
  -l test/notion-bridge-push-test.el \
  -f ert-run-tests-batch-and-exit
```

`test/notion-bridge-push-test.el` runs pull/edit/push scenarios end to end
against an in-memory Notion (`test/notion-bridge-fake.el`); no test touches
the network.

Individual test suites:

```sh
make test-org
make test-api
make test-cache
make test-sync
```

## Known limitations

- **Rate limits** -- Notion enforces about 3 requests/second.  Comment
  fetching is paced to match, and rate-limited requests are retried, but very
  large workspaces make `pull-all` slow.
- **Read-only blocks** -- blocks without an org equivalent are placeholders:
  you can keep or remove them, not edit them.  Tables are editable, but an
  edited table is replaced as a whole.
- **Lossy edits** -- when you edit a block, formatting org can't express
  (text colors, user and date mentions, inline equations) becomes plain text
  in that block.  Untouched blocks keep everything.  Headings deeper than
  level 3 become level 3 (Notion's maximum); TODO keywords, tags and org
  comments stay local and are never sent.
- **Inserting at the very top** -- Notion can only insert after an existing
  block, so a new first block is added by recreating the old first block
  after it; this fails (with a message) if that block is read-only.
- **Comment positions** -- Notion comments are associated with blocks, not
  character ranges.  The overlay placement maps comments to the line where
  the block appears, but cannot highlight the specific text range the
  commenter selected in Notion.
- **Large pages** -- pages with hundreds of blocks require many sequential API
  calls for recursive block fetching, which can be slow.
