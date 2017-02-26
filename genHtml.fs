module FSRemarks.GenHtml
open System

let createHeader(title: string) =
    let str = new System.Text.StringBuilder()
    [
    str.AppendLine("<head>")
    str.AppendLine(String.Format("<title>{0}</title>", title))
    str.AppendLine("")
    str.AppendLine("<style>")
    str.AppendLine("</style>")
    str.AppendLine("</head>")
    ] |> ignore
    str.ToString()

printfn "%s" <| createHeader("example.html")

let createExpandScript() =
    let str = new System.Text.StringBuilder()
    [
    str.AppendLine("<script>")
    str.AppendLine("function expandSection(str){")
    str.AppendLine("\tvar s = document.getElementById(str);")
    str.AppendLine("\tif (s.style.display === 'block') {")
    str.AppendLine("\t\ts.style.display = 'none';")
    str.AppendLine("\t} else {")
    str.AppendLine("\t\ts.style.display = 'block';")
    str.AppendLine("\t}")
    str.AppendLine("}")
    str.AppendLine("</script>")
    ] |> ignore
    str.ToString()

printfn "%s" <| createExpandScript()


