This is some **bold text**, __other bold text__ and some _italicized text_.
But make sure that if spaces_are_not_surrounding the delimiters, then
the text__is__plain for underscores - but not*for*asterisks. But backticks
will `always produce` code ``ya know``? And if you need two backticks,
you can ```do `` within three backticks```.

    parseMarkdown :: Parser Markdown
    parseMarkdown = Markdown <$> parseSegments <* eof

And know that <kbd>z</kbd><kbd>z</kbd> centers your cursor location in normal mode.
And make sure that &quot;quoted strings&quot; get their damn html entities decoded.
