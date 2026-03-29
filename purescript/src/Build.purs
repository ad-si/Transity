-- | Build scripts for the project
module Build
  ( main
  ) where

import Prelude

import Data.Array (filter, foldMap, sort)
import Data.Options ((:=))
import Data.String (Pattern(..), Replacement(..), replace, split, toLower)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import MarkdownIt (Preset(..), html, newMarkdownIt, render)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (copyFile, mkdir', readTextFile, readdir, writeTextFile)
import Node.FS.Extra (copy, remove)
import Node.FS.Perms (permsReadWrite)
import Node.Path (basename, basenameWithoutExt, concat, extname)

type FileEntry =
  { name :: String
  , content :: String
  }

commonStyles :: String
commonStyles =
  """
  <style>
    body {
      font-family: Arial, sans-serif;
      line-height: 1.3;
      margin: 0;
      padding: 0;
    }

    #wrapper {
      max-width: 60rem;
      margin: 0 auto;
      display: flex;
    }

    aside { padding: 1rem; }
    aside ul {
      list-style-type: none;
      padding: 0;
    }
    aside a, aside a:visited {
      text-decoration: none;
      color: hsl(0, 0%, 40%);
    }
    aside a:hover {
      text-decoration: underline;
    }

    main { padding: 1rem; }

    h1 { font-size: 2rem; }

    h2 { font-size: 1.5rem; }

    h3 { font-size: 1.25rem; }

    h4 { font-size: 1.1rem; }

    h5 { font-size: 1.0rem; }

    h6 { font-size: 0.9rem; }

    p { margin: 1rem 0; }

    code {
      background-color: hsl(0, 0%, 95%);
      border-radius: 0.25rem;
      padding: 0.1rem 0.2rem;
    }

    a:link { color: rgb(100, 140, 200); }
    a:visited { color: rgb(160, 0, 180); }

    img { max-width: 100%; }

    table {
      width: 100%;
      border-collapse: collapse;
    }

    th, td {
      border: 1px solid #ddd;
      padding: 0.5rem;
    }

    th { background-color: #f4f4f4; }

    blockquote {
      background-color: #f4f4f4;
      padding: 0.5rem;
    }

    hr {
      border: 0;
      border-top: 1px solid #ddd;
    }
  </style>
  """

wrapWithTemplate :: String -> String -> String
wrapWithTemplate toc htmlContent =
  """
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Transity Documentation</title>
    <link rel="stylesheet" href="/docs/stackoverflow-light.min.css">
    <script src="/docs/highlight.min.js"></script>
    <script>
      hljs.registerLanguage("ledger", () => ({
        case_insensitive: false,
        contains: [
          {
            scope: "symbol",
            begin: /[0-9]{4}\-[01][0-9]-[0-3][0-9]/, end: /\s/,
          },
          {
            scope: "number",
            begin: /\$[0-9\.]+/, end: /\s/,
          },
        ],
      }))
      hljs.highlightAll()
    </script>
    """ <> commonStyles
    <>
      """
  </head>
  <body>
    <div id="wrapper">
    """
    <> toc
    <>
      """
      <main>
      """
    <> htmlContent
    <>
      """
      </main>
    </div>
  </body>
  </html>
  """

cleanUpName :: String -> String
cleanUpName str =
  case
    str
      # replace (Pattern ".md") (Replacement "")
      # split (Pattern "_")
    of
    [ _, rest ] -> rest
    _ -> str

getToc :: Array String -> String
getToc headings =
  """
  <aside>
    <ul>
      <li
    """
    <> foldMap
      ( \heading -> do
          let
            href =
              if heading == "index.md" --
              then ""
              else heading # toLower # setExt ".html"

          "<li><a href=\"/docs/" <> href <> "\">"
            <>
              ( heading
                  # cleanUpName
                  # replace (Pattern "index.md") (Replacement "Home")
              )
            <>
              "</a></li>"
      )
      headings
    <>
      """
    </ul>
  </aside>
  """

-- | Set the extension of a file path
-- | ```purs
-- | setExt ".html" "index.md" == "index.html"
-- | ```
setExt :: String -> String -> String
setExt ext path = do
  let
    extension = extname path
    basename = basenameWithoutExt path extension
  basename <> ext

buildDocs :: Aff Unit
buildDocs = do
  let
    srcDir = "docs_src"
    targetDir = "docs/docs"

  files <- srcDir # readdir <#> sort

  liftEffect $ log "Log the files:"

  fileEntries <- for files \fileName -> do
    mdContent <- readTextFile UTF8 $ concat [ srcDir, fileName ]
    markdownIt <- liftEffect $ newMarkdownIt Default $ html := true
    htmlContent <- liftEffect $ render markdownIt mdContent
    pure $ { name: fileName, content: htmlContent }

  mkdir' targetDir { mode: permsReadWrite, recursive: true }

  -- Write each file's HTML content to a file
  for_ fileEntries \fileEntry -> do
    let
      targetFile = fileEntry.name # toLower # setExt ".html"
      toc = getToc
        ( [ "index.md" ] <> -- Must be first in the list
            ( files
                # filter (\name -> name /= "index.md")
            )
        )

    liftEffect $ log $ "Writing file: " <> targetFile
    writeTextFile UTF8
      (concat [ targetDir, targetFile ])
      (wrapWithTemplate toc fileEntry.content)

  copy "images" $ concat [ targetDir, "images" ]

  let hightlightJsDir = "node_modules/@highlightjs/cdn-assets"
  copyFileTo targetDir $ concat [ hightlightJsDir, "highlight.min.js" ]
  copyFileTo targetDir $ concat
    [ hightlightJsDir, "styles/stackoverflow-light.min.css" ]

copyFileTo :: String -> String -> Aff Unit
copyFileTo targetDir filePath = do
  let targetPath = concat [ targetDir, basename filePath ]
  remove targetPath
  copyFile filePath targetPath

main :: Effect Unit
main = launchAff_ do
  buildDocs
