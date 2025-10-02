module Persistencia
  ( carregarSistema
  , salvarSistema
  ) where

import Tipos

-- | No futuro: ler de arquivos. Por enquanto, retorna o sistema vazio.
carregarSistema :: IO Sistema
carregarSistema = do
  putStrLn "[Persistencia] Carregando dados (stub)..."
  pure sistemaVazio

-- | No futuro: salvar em arquivos. Por enquanto, só imprime.
salvarSistema :: Sistema -> IO ()
salvarSistema _ = do
  putStrLn "[Persistencia] Salvando dados (stub)... OK"
