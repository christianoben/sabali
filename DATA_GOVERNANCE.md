# Sabali data-governance sign-off

Status: **review required before production**

This document is the release record for the Sabali data boundary. It must be
reviewed by the data owner, security/privacy owner, and a statistician before
real patient or otherwise sensitive data is enabled.

## Data boundary

Raw uploaded rows are processed in the Shiny session's memory. They are not
sent to an AI endpoint. The AI planner may receive only aggregate metadata:
column names, confirmed measurement types, factor levels, missing-value
counts, numeric ranges/means/standard deviations, selected roles, and aggregate
dataset structure. The interpreter may receive only displayed aggregate result
tables and analysis context.

The app registers Shiny upload paths and removes the explicitly registered
paths at session end. Word-report plot images are temporary and are deleted
after being embedded in the document. No raw data, API keys, or report files
are written by the application to a project-owned persistent directory.

## AI endpoint policy

- AI is off by default when no endpoint is configured.
- Only an endpoint approved for the data classification may be configured.
- The free OpenCode Zen “Big Pickle” route is non-zero-retention during its
  free period and is permitted only for synthetic or de-identified data.
- Production secrets must be injected through the deployment secret store or
  process environment. `.Renviron` is local-only and must never be committed.
- The request boundary must be re-reviewed if a planner/interpreter payload is
  expanded beyond the aggregate fields described above.

## Retention and access

Session data is isolated in Shiny's per-session reactive state. The deployment
must use HTTPS, authenticated access, least-privilege service credentials,
restricted outbound network access, and server-level temporary-directory
controls appropriate to the data classification. Operators must not enable
debug request/response logging for sensitive sessions.

The in-memory audit record records event, UTC timestamp, field names, and table
dimensions only. It intentionally excludes values, row identifiers, prompts,
tokens, and endpoint responses. It is suitable for operational evidence, not
for reconstructing patient activity.

## Sign-off

| Role | Name | Organisation | Decision | Date | Signature / ticket |
|---|---|---|---|---|---|
| Data owner |  |  | Pending |  |  |
| Security / privacy reviewer |  |  | Pending |  |  |
| Statistician |  |  | Pending |  |  |
| Application owner |  |  | Pending |  |  |

No production release is approved until all four rows are completed and the
deployment configuration has been checked against `REVIEW_CHECKLIST.md`.
