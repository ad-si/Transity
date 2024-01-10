module Transity.Xlsx where

import Prelude ((#), (<#>), (<>), ($), (==), bind, pure, show, Unit)

import Control.Alt ((<|>))
import Data.Array (sortBy, concat, take, catMaybes)
import Data.Foldable (fold, intercalate)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord (compare)
import Data.Rational (toNumber) as Rational
import Data.String (joinWith, Pattern(..), Replacement(..), replaceAll)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

import Transity.Data.Amount as Amount
import Transity.Data.Amount (Amount(..))
import Transity.Data.Transfer (Transfer(..))
import Transity.Data.Transaction (Transaction(..))
import Transity.Data.Ledger (Ledger(..), entitiesToInitialTransfers)
import Transity.Utils (utcToIsoString)


newtype FileEntry = FileEntry
  { path :: String
  , content :: String
  }


foreign import writeToZipImpl
  :: forall a. Fn3 (Maybe a) (Maybe String) (Array FileEntry) (EffectFnAff Unit)

writeToZip :: Maybe String -> Array FileEntry -> Aff Unit
writeToZip outPath files = fromEffectFnAff $
  runFn3 writeToZipImpl Nothing outPath files


newtype SheetRow = SheetRow
  { utc :: String
  , account :: String
  , amount :: String
  , commodity :: String
  , note :: String
  , files :: Array String
  }


getSheetRows :: Ledger -> Maybe (Array SheetRow)
getSheetRows (Ledger {transactions, entities}) = do
  let
    getQunty (Amount quantity _ ) = show $ Rational.toNumber quantity
    getCmdty (Amount _ commodity ) = unwrap commodity

    splitTransfer
      :: { note :: Maybe String, files :: Array String, transfer :: Transfer }
      -> Maybe (Array SheetRow)
    splitTransfer { note: note , files: files, transfer: (Transfer tfer) } =
      let
        fromAmnt = Amount.negate tfer.amount

        getFromAndTo :: String -> Array SheetRow
        getFromAndTo date =
          [ SheetRow
              { utc: date
              , account: tfer.from
              , amount: getQunty fromAmnt
              , commodity: getCmdty fromAmnt
              , note: [note, tfer.note]
                  # catMaybes
                  # intercalate ", "
              , files: files
              }
          , SheetRow
              { utc: date
              , account: tfer.to
              , amount: getQunty tfer.amount
              , commodity: getCmdty tfer.amount
              , note: [note, tfer.note]
                  # catMaybes
                  # intercalate ", "
              , files: files
              }
          ]
      in
        (tfer.utc <#> utcToIsoString) <#> getFromAndTo

  splitted <- do
    transactions
    <#> (\(Transaction tact) -> tact.transfers
          <#> (\(Transfer tfer) ->
                { note: tact.note
                , files: tact.files
                , transfer: Transfer (tfer { utc = tfer.utc <|> tact.utc })
                }
              )
        )
    # concat
    <#> splitTransfer
    # sequence

  let
    initialEntries = entitiesToInitialTransfers entities <#>
      \(Transfer t) ->
          let isoString = fromMaybe "INVALID DATE" $ t.utc <#> utcToIsoString
          in
            [ SheetRow
              { utc: isoString
              , account: replaceAll
                  (Pattern ":_default_")
                  (Replacement "")
                  t.from
              , amount: getQunty t.amount
              , commodity: getCmdty t.amount
              , note: fromMaybe "" t.note
              , files: []
              }
            ]

  pure $ (splitted <> initialEntries) # concat


escapeHtml :: String -> String
escapeHtml unsafeStr =
  unsafeStr
   # replaceAll (Pattern "&") (Replacement "&amp;")
   # replaceAll (Pattern "<") (Replacement "&lt;")
   # replaceAll (Pattern ">") (Replacement "&gt;")
   # replaceAll (Pattern "\"") (Replacement "&quot;")
   # replaceAll (Pattern "'") (Replacement "&#039;")


entriesAsXml :: Ledger -> Maybe String
entriesAsXml ledger = do
  sheetRows <- getSheetRows ledger

  let
    -- Replace all \ with / for calculations to make it work cross platform
    -- Remove "'file://" for LibreOffice
    -- Should therefore work on macOS and Windows with Excel and LibreOffice
    -- (Apple's Numbers does not support local file links at all)
    hyperlinkFormula = """
      =HYPERLINK(
        SUBSTITUTE(
          LEFT(
            SUBSTITUTE(CELL("filename"), "\", "/"),
            FIND(
              "?",
              SUBSTITUTE(
                SUBSTITUTE(CELL("filename"), "\", "/"),
                "/",
                "?",
                LEN(SUBSTITUTE(CELL("filename"), "\", "/"))
                  - LEN(
                      SUBSTITUTE(
                        SUBSTITUTE(CELL("filename"), "\", "/"),
                        "/",
                        ""
                      )
                    )
              )
            )
          ) & "{{ filename }}",
          "'file://",
          ""
        ),
        "{{ filename }}"
      )
      """

    wrapValue dataType val =
      case dataType of
        "inlineStr" ->
          "<c t=\"inlineStr\"><is><t>"
          <> escapeHtml val
          <> "</t></is></c>"

        "formula" ->
            "<c t=\"str\">"
            <> (if val == ""
                then ""
                else
                  ("<f>"
                    <> (escapeHtml $ replaceAll
                        (Pattern "{{ filename }}")
                        (Replacement val )
                        hyperlinkFormula)
                    <> "</f>"
                  )
                )
            <> "</c>"

        _ ->
          "<c t=\"" <> dataType <> "\"><v>"
          <> escapeHtml val
          <> "</v></c>"

    -- This workaround is necessary
    -- since there must only be one HYPERLINK per cell
    -- TODO: Make it work for an unlimited number of files
    limitTo4Files files =
      take 4 (files <> ["", "", "", ""])


    wrapStr = wrapValue "inlineStr"

    headerRow :: String
    headerRow =
      "<row>\n"
        <> (wrapStr "Timestamp (UTC)")
        <> (wrapStr "Account")
        <> (wrapStr "Amount")
        <> (wrapStr "Commodity")
        <> (wrapStr "Note")
        <> (wrapStr "File 1")
        <> (wrapStr "File 2")
        <> (wrapStr "File 3")
        <> (wrapStr "File 4")
        <> "\n"
        <> "</row>"

    dataRows :: String
    dataRows = sheetRows
      # sortBy (\(SheetRow rowRecA) (SheetRow rowRecB) ->
          compare rowRecA.utc rowRecB.utc)
      <#> (\(SheetRow rowRec) ->
        "<row>\n"
        <> (wrapStr (rowRec.utc <> "Z"))
        <> (wrapStr rowRec.account)
        <> (wrapValue "n" rowRec.amount)
        <> (wrapStr rowRec.commodity)
        <> (wrapStr rowRec.note)
        <> (limitTo4Files rowRec.files
              <#> wrapValue "formula"
              # fold
            )
        <> "\n"
        <> "</row>")
      # joinWith "\n"

  pure $ headerRow <> dataRows


contentTypesContent :: String
contentTypesContent = """<?xml version="1.0" encoding="UTF-8"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default
    ContentType="application/xml"
    Extension="xml"
  />
  <Default
    ContentType="application/vnd.openxmlformats-package.relationships+xml"
    Extension="rels"
  />
  <Override
    ContentType="application/vnd.openxmlformats-package.relationships+xml"
    PartName="/_rels/.rels"
  />
  <Override
    ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"
    PartName="/xl/workbook.xml"
  />
  <Override
    ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"
    PartName="/xl/styles.xml"
  />
  <Override
    ContentType="application/vnd.openxmlformats-package.relationships+xml"
    PartName="/xl/_rels/workbook.xml.rels"
  />
  <Override
    ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"
    PartName="/xl/worksheets/sheet1.xml"
  />
  <Override
    ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"
    PartName="/xl/sharedStrings.xml"
  />
  <Override
    ContentType="application/vnd.openxmlformats-package.core-properties+xml"
    PartName="/docProps/core.xml"
  />
  <Override
    ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"
    PartName="/docProps/app.xml"
  />
</Types>
"""


relsContent :: String
relsContent = """<?xml version="1.0" encoding="UTF-8"?>
<Relationships
  xmlns="http://schemas.openxmlformats.org/package/2006/relationships"
>
  <Relationship
    Id="rId1"
    Target="xl/workbook.xml"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument"
  />
  <Relationship
    Id="rId2"
    Target="docProps/core.xml"
    Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties"
  />
  <Relationship
    Id="rId3"
    Target="docProps/app.xml"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties"
  />
</Relationships>
"""


appContent :: String
appContent = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Properties
  xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties"
>
  <Application>Transity</Application>
  <AppVersion>0.0</AppVersion>
</Properties>
"""


-- TODO: Implement correct timestamp
now :: String
now = "2021-01-01T00:00:00Z"


coreContent :: String
coreContent = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<cp:coreProperties
  xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
  xmlns:dc="http://purl.org/dc/elements/1.1/"
  xmlns:dcterms="http://purl.org/dc/terms/"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
>
  <dc:creator>Transity</dc:creator>
  <cp:lastModifiedBy>Transity</cp:lastModifiedBy>
  <dc:description>
    All transfers of journal created with Transity
  </dc:description>
  <dcterms:created xsi:type="dcterms:W3CDTF">"""
    <> now <>
  """</dcterms:created>
  <dcterms:modified xsi:type="dcterms:W3CDTF">"""
    <> now <>
  """</dcterms:modified>
</cp:coreProperties>
"""


xlRelsContent :: String
xlRelsContent = """<?xml version="1.0" encoding="UTF-8"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship
    Id="rId1"
    Target="worksheets/sheet1.xml"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"
  />
  <Relationship
    Id="rId2"
    Target="styles.xml"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles"
  />
  <Relationship
    Id="rId3"
    Target="sharedStrings.xml"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings"
  />
</Relationships>
"""


sharedStrContent :: String
sharedStrContent = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<sst
  count="0"
  uniqueCount="0"
  xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"
>
</sst>
"""


-- Mandatory (otherwise neither Excel nor Apple Numbers can open it)
-- More information: https://stackoverflow.com/a/26062365/1850340
stylesContent :: String
stylesContent = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <fonts count="1">
    <font>
      <sz val="10"/>
      <name val="Arial"/>
      <family val="2"/>
    </font>
  </fonts>
  <fills count="2">
    <fill>
      <patternFill patternType="none"/>
    </fill>
    <fill>
      <patternFill patternType="gray125"/>
    </fill>
  </fills>
  <borders count="1">
    <border>
      <left/>
      <right/>
      <top/>
      <bottom/>
      <diagonal/>
    </border>
  </borders>
  <cellStyleXfs count="2">
    <xf
      borderId="0"
      fillId="0"
      fontId="0"
      numFmtId="0"
    />
    <xf
      borderId="0"
      fillId="0"
      fontId="0"
      numFmtId="0"
    />
  </cellStyleXfs>
</styleSheet>
"""


workbookContent :: String
workbookContent = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook
  xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
>
  <sheets>
    <sheet
      name="Sheet1"
      r:id="rId1"
      sheetId="1"
    />
  </sheets>
</workbook>
"""


rowsToSheet:: String -> String
rowsToSheet rows = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet
  xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"
  xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
  xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  xmlns:x14="http://schemas.microsoft.com/office/spreadsheetml/2009/9/main"
  xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing"
>
  <sheetPr filterMode="false">
    <pageSetUpPr fitToPage="false"/>
  </sheetPr>
  <dimension ref="A1:C2"/>
  <sheetViews>
    <sheetView
      colorId="64"
      defaultGridColor="true"
      rightToLeft="false"
      showFormulas="false"
      showGridLines="true"
      showOutlineSymbols="true"
      showRowColHeaders="true"
      showZeros="true"
      tabSelected="true"
      topLeftCell="A1"
      view="normal"
      workbookViewId="0"
      zoomScale="100"
      zoomScaleNormal="100"
      zoomScalePageLayoutView="100"
    >
      <selection
        activeCell="C4"
        activeCellId="0"
        pane="topLeft"
        sqref="C4"
      />
    </sheetView>
  </sheetViews>
  <sheetFormatPr
    defaultColWidth="11.53515625"
    defaultRowHeight="12.8"
    outlineLevelCol="0"
    outlineLevelRow="0"
    zeroHeight="false"
  />
  <sheetData>
""" <> rows <> """
  </sheetData>
  <printOptions
    gridLines="false"
    gridLinesSet="true"
    headings="false"
    horizontalCentered="false"
    verticalCentered="false"
  />
  <pageMargins
    bottom="1.05277777777778"
    footer="0.7875"
    header="0.7875"
    left="0.7875"
    right="0.7875"
    top="1.05277777777778"
  />
  <pageSetup
    blackAndWhite="false"
    cellComments="none"
    copies="1"
    draft="false"
    firstPageNumber="1"
    fitToHeight="1"
    fitToWidth="1"
    horizontalDpi="300"
    orientation="portrait"
    pageOrder="downThenOver"
    paperSize="9"
    scale="100"
    useFirstPageNumber="true"
    verticalDpi="300"
  />
  <headerFooter differentFirst="false" differentOddEven="false">
    <oddHeader>
      &amp;C&amp;&quot;Times New Roman,Regular&quot;&amp;12&amp;A
    </oddHeader>
    <oddFooter>
      &amp;C&amp;&quot;Times New Roman,Regular&quot;&amp;12Page &amp;P
    </oddFooter>
  </headerFooter>
</worksheet>
"""


entriesAsXlsx :: Ledger -> Array FileEntry
entriesAsXlsx ledger = do
  case entriesAsXml ledger of
    Nothing -> []
    Just rowsString ->
      [ FileEntry { path: "[Content_Types].xml", content: contentTypesContent }
      , FileEntry { path: "_rels/.rels", content: relsContent }
      , FileEntry { path: "docProps/app.xml", content: appContent }
      , FileEntry { path: "docProps/core.xml", content: coreContent }
      , FileEntry { path: "xl/_rels/workbook.xml.rels", content: xlRelsContent }
      , FileEntry { path: "xl/sharedStrings.xml", content: sharedStrContent }
      , FileEntry { path: "xl/styles.xml", content: stylesContent }
      , FileEntry { path: "xl/workbook.xml", content: workbookContent }
      , FileEntry
          { path: "xl/worksheets/sheet1.xml"
          , content: rowsToSheet rowsString
          }
      ]


