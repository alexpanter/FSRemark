module FSRemarks.GenHtml
open System



let createStyle (file: IO.StreamWriter) (parser: Types.ParsedSection list) =

let createHeader(title: string) =
    ()

printfn "%s" <| createHeader("example.html")

let createExpandScript(file: IO.StreamWriter) =
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


