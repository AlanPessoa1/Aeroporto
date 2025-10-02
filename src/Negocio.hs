module Negocio
  ( inserirPassageiro
  , inserirPassageiroDados
  , listarPassageiros
  , proximoId
  ) where

import Tipos
import Data.Char (isSpace)

-- | Remove espaços do início e fim
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Gera próximo ID de uma lista (maior + 1, começa em 1)
proximoId :: [a] -> (a -> Int) -> Int
proximoId xs f =
  if null xs then 1 else maximum (map f xs) + 1

-- | Lista todos os passageiros (útil para o menu)
listarPassageiros :: Sistema -> [Passageiro]
listarPassageiros = passageiros

-- | Inserção com struct pronto (ignora o id passado e gera um novo)
inserirPassageiro :: Passageiro -> Sistema -> Either String Sistema
inserirPassageiro p sys =
  inserirPassageiroDados (nome p) (documento p) sys

-- | Inserção a partir de nome/documento (validações simples)
inserirPassageiroDados :: String -> String -> Sistema -> Either String Sistema
inserirPassageiroDados nomeRaw docRaw sys =
  let nome' = trim nomeRaw
      doc'  = trim docRaw
  in
    if null nome'
       then Left "Nome não pode ser vazio."
    else if null doc'
       then Left "Documento não pode ser vazio."
    else if any (\x -> documento x == doc') (passageiros sys)
       then Left "Já existe um passageiro com esse documento."
    else
      let novoId = proximoId (passageiros sys) idPassageiro
          novoP  = Passageiro novoId nome' doc'
      in Right sys { passageiros = passageiros sys ++ [novoP] }
