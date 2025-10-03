module Negocio
  ( inserirPassageiro
  , inserirPassageiroDados
  , listarPassageiros
  , proximoId
  , inserirCompanhia
  , inserirCompanhiaNome
  , listarCompanhias
  ) where

import Tipos
import Data.Char (isSpace)

-- | Remove espaços do início e fim
-- | Remove espaços em branco no início e no fim de uma string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Gera próximo ID de uma lista (maior + 1, começa em 1)
--   dado um campo que fornece o ID atual.
-- Se a lista estiver vazia, retorna 1.
-- Caso contrário, retorna (máximo dos IDs + 1).
proximoId :: [a] -> (a -> Int) -> Int
proximoId xs f =
  if null xs then 1 else maximum (map f xs) + 1

-- ====== PASSAGEIROS ======

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


-- ====== COMPANHIAS ======

-- | Lista todas as companhias (para o menu)
listarCompanhias :: Sistema -> [Companhia]
listarCompanhias = companhias

-- | Inserção com struct (ignora id passado e gera um novo)
inserirCompanhia :: Companhia -> Sistema -> Either String Sistema
inserirCompanhia c sys = inserirCompanhiaNome (nomeCompanhia c) sys

-- | Inserção a partir do nome (validações simples)
inserirCompanhiaNome :: String -> Sistema -> Either String Sistema
inserirCompanhiaNome nomeRaw sys =
  let nome' = trim nomeRaw
  in if null nome'
        then Left "Nome da companhia não pode ser vazio."
     else if any (\x -> nomeCompanhia x == nome') (companhias sys)
        then Left "Já existe uma companhia com esse nome."
     else
       let novoId = proximoId (companhias sys) idCompanhia
           novaC  = Companhia novoId nome'
       in Right sys { companhias = companhias sys ++ [novaC] }

