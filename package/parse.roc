app "parse"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        parser: "../dependencies/roc-parser/package/main.roc",
    }
    imports [pf.Stdout, pf.Stderr, pf.Task.{ Task }, pf.Arg, pf.File, pf.Path.{ Path }, Xml]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    when args is
        [_process, path] ->
            parsedResult <- parseFile (Path.fromStr path) |> Task.attempt
            when parsedResult is
                Ok parsed ->
                    Stdout.line (parsed |> Inspect.toStr)

                Err (ParsingFailure error) -> Stderr.line "Parsing error: \(error)"
                Err (ParsingIncomplete rest) -> Stderr.line "Parsing incomplete: \(rest)"
                Err error -> Stderr.line "Error parsing file: \(Inspect.toStr error)"

        _ -> Stdout.line "Please call me with just the URL to an XML file."

parseFile : Path -> Task Xml.Xml _
parseFile = \path ->
    result <- File.readUtf8 (path) |> Task.await

    Xml.parseString result
    |> Task.fromResult
