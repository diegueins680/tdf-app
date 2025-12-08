{-# LANGUAGE OverloadedStrings #-}
module TDF.Contracts.Server where

import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Aeson               as A
import           Data.Aeson               ((.:), (.:?), (.!=))
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Servant

import           TDF.Contracts.API
import qualified TDF.Handlers.InputList   as InputList

server :: Server ContractsAPI
server = createH :<|> pdfH :<|> sendH
  where
    createH :: A.Value -> Handler A.Value
    createH v = pure (A.object ["status" A..= ("created" :: T.Text), "id" A..= ("uuid-demo" :: T.Text), "payload" A..= v])

    pdfH :: T.Text -> Handler BL.ByteString
    pdfH contractId = do
      let latex = renderContractLatex contractId
      result <- liftIO (InputList.generateInputListPdf latex)
      case result of
        Right bytes -> pure bytes
        Left errMsg -> throwError err500 { errBody = BL.fromStrict (encodeUtf8 errMsg) }

    sendH :: T.Text -> A.Value -> Handler A.Value
    sendH _ _ = pure (A.object ["status" A..= ("sent" :: T.Text)])


renderContractLatex :: T.Text -> T.Text
renderContractLatex contractId =
  T.unlines
    [ "\\documentclass{article}"
    , "\\usepackage[margin=2.5cm]{geometry}"
    , "\\usepackage{array,booktabs,xcolor}"
    , "\\definecolor{accent}{RGB}{67,97,238}"
    , "\\begin{document}"
    , "\\begin{center}"
    , "\\Huge\\bfseries The Dream Factory"
    , "\\vspace{0.3cm}\\\\textcolor{accent}{Contract}"
    , "\\end{center}"
    , "\\vspace{0.6cm}"
    , "\\noindent Contract ID: \\textbf{" <> latexEscape contractId <> "}\\\\par"
    , "\\vspace{0.3cm}"
    , "\\noindent This PDF is a placeholder generated from the API. Replace this with the actual LaTeX template when available."
    , "\\end{document}"
    ]

latexEscape :: T.Text -> T.Text
latexEscape = T.concatMap escapeChar
  where
    escapeChar c = case c of
      '&'  -> "\\&"
      '%'  -> "\\%"
      '$'  -> "\\$"
      '#'  -> "\\#"
      '_'  -> "\\_"
      '{'  -> "\\{"
      '}'  -> "\\}"
      '~'  -> "\\textasciitilde{}"
      '^'  -> "\\textasciicircum{}"
      '\\' -> "\\textbackslash{}"
