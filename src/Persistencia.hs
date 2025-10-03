{-Este módulo implementa a camada de persistência do projeto Aeroporto.
A ideia é que todas as informações (passageiros, companhias, voos e reservas)
sejam gravadas em arquivos de texto simples para que permaneçam disponíveis
mesmo após o programa ser encerrado.-}

module Persistencia
  ( carregarSistema
  , salvarSistema
  ) where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import Text.Read (readMaybe)
import Tipos

-- Funções utilitárias genéricas (agora tolerantes a arquivos vazios/invalidos)
carregarLista :: Read a => FilePath -> IO [a]
carregarLista arq = do
  existe <- doesFileExist arq
  if not existe
     then return []
     else do
       conteudo <- readFile arq
       case readMaybe conteudo of
         Just xs -> return xs
         Nothing -> do
           putStrLn $ "[Aviso] Conteudo invalido em " ++ arq ++ " (usando lista vazia)."
           return []

salvarLista :: Show a => FilePath -> [a] -> IO ()
salvarLista arq xs = writeFile arq (show xs)

-- Caminhos
arquivoPassageiros, arquivoCompanhias, arquivoVoos, arquivoReservas :: FilePath
arquivoPassageiros = "dados/passageiros.db"
arquivoCompanhias  = "dados/companhias.db"
arquivoVoos        = "dados/voos.db"
arquivoReservas    = "dados/reservas.db"

carregarSistema :: IO Sistema
carregarSistema = do
  createDirectoryIfMissing True "dados"
  ps <- carregarLista arquivoPassageiros
  cs <- carregarLista arquivoCompanhias
  vs <- carregarLista arquivoVoos
  rs <- carregarLista arquivoReservas
  return (Sistema ps cs vs rs)

salvarSistema :: Sistema -> IO ()
salvarSistema sys = do
  createDirectoryIfMissing True "dados"
  salvarLista arquivoPassageiros (passageiros sys)
  salvarLista arquivoCompanhias  (companhias  sys)
  salvarLista arquivoVoos        (voos        sys)
  salvarLista arquivoReservas    (reservas    sys)

