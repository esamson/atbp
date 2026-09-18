# Intent: md2c generated-page footer

`md2c` adds one generated footer to every Confluence page that it publishes.
The footer tells readers that Markdown sources generate the page.

## Footer

The footer is an ADF expand item.
Its title is `Don't edit here!`.
Its body is `This page was generated from markdown sources.`.

## Constraints

- Add the footer after Markdown, diagram, and extension transformations.
- Keep source Markdown and the generated child-page extension before the footer.
- Include the footer in the published-content version so that old pages update once.
- Keep tests offline and use the existing fake Confluence client.

## Out of scope

- Change the footer title, body, placement, or ADF item type.
- Change user-authored Markdown.
- Change the Confluence client API or add a dependency.
- Send live requests during unit tests.
