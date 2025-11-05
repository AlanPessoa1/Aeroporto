module Persistencia
  ( carregarSistema
  , salvarSistema
  ) where

import System.Directory (doesFileExist, createDirectoryIfMissing)
import Text.Read (readMaybe)
import Tipos
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

-- Leitura segura no Windows (usa ByteString)
carregarLista :: Read a => FilePath -> IO [a]
carregarLista arq = do
  existe <- doesFileExist arq
  if not existe
     then return []
     else do
       conteudo <- BS.readFile arq   -- <- NÃO fica com lock
       let texto = C.unpack conteudo
       case readMaybe texto of
         Just xs -> return xs
         Nothing -> do
           putStrLn $ "[Aviso] Conteudo invalido em " ++ arq ++ " (usando lista vazia)."
           return []

-- Escrita segura no Windows (flush e fecha imediatamente)
salvarLista :: Show a => FilePath -> [a] -> IO ()
salvarLista arq xs =
  BS.writeFile arq (C.pack (show xs))  -- <- trava 0 segundos


-- Arquivos
arquivoUsuarios, arquivoPassageiros, arquivoCompanhias, arquivoVoos, arquivoReservas :: FilePath
arquivoUsuarios    = "dados/usuarios.db"
arquivoPassageiros = "dados/passageiros.db"
arquivoCompanhias  = "dados/companhias.db"
arquivoVoos        = "dados/voos.db"
arquivoReservas    = "dados/reservas.db"

carregarSistema :: IO Sistema
carregarSistema = do
  createDirectoryIfMissing True "dados"

  us <- carregarLista arquivoUsuarios
  ps <- carregarLista arquivoPassageiros
  cs <- carregarLista arquivoCompanhias
  vs <- carregarLista arquivoVoos
  rs <- carregarLista arquivoReservas

  return (Sistema us ps cs vs rs)

salvarSistema :: Sistema -> IO ()
salvarSistema sys = do
  createDirectoryIfMissing True "dados"
  salvarLista arquivoUsuarios    (usuarios    sys)
  salvarLista arquivoPassageiros (passageiros sys)
  salvarLista arquivoCompanhias  (companhias  sys)
  salvarLista arquivoVoos        (voos        sys)
  salvarLista arquivoReservas    (reservas    sys)

  


