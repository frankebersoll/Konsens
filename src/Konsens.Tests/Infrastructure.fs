namespace Konsens.Tests.Infrastructure

module XPath =

    let tag tag = sprintf "//%s" tag
    let withClass c path = sprintf "%s[contains(concat(' ',normalize-space(@class),' '),' %s ')]" path c
    let withText text path = sprintf "%s[contains(text(),'%s')]" path text
    let withPlaceholder text path = sprintf "%s[@placeholder='%s']" path text
    let parent path = path + "/.."
    let inner tag path = sprintf "%s//%s" path tag
    let index i path = sprintf "(%s)[%i]" path i

    let inputWithContext context text =
        tag context
        |> withText text
        |> parent
        |> inner "input"

    let inputWithLabel = inputWithContext "label"
    let inputWithP = inputWithContext "p"
