# Sistema de Gerenciamento de Aeroporto

Sistema completo de gerenciamento de aeroporto desenvolvido em Haskell, que permite administrar passageiros, companhias aéreas, voos e reservas através de uma interface de linha de comando.

## Funcionalidades

### Módulos Implementados

- **Gestão de Passageiros**

  - Cadastro de novos passageiros
  - Listagem de todos os passageiros
  - Validação de documentos únicos

- **Gestão de Companhias Aéreas**

  - Cadastro de companhias aéreas
  - Listagem de todas as companhias
  - Validação de nomes únicos

- **Gestão de Voos**

  - Cadastro de voos
  - Listagem de todos os voos
  - Validação de origem, destino e horário
  - Vinculação com companhias aéreas

- **Sistema de Reservas** (lógica implementada)
  - Criação de reservas
  - Confirmação de reservas
  - Cancelamento de reservas
  - Controle de status (Pendente, Confirmada, Cancelada)
  - Prevenção de reservas duplicadas

## Tecnologias Utilizadas

- **Linguagem**: Haskell
- **Build Tool**: Stack
- **Persistência**: Arquivos de texto (formato Show/Read do Haskell)
- **Encoding**: UTF-8 para suporte a caracteres portugueses

## Estrutura do Projeto

O projeto segue uma arquitetura em camadas:

```
src/
├── Main.hs           # Ponto de entrada da aplicação
├── Tipos.hs          # Definição de tipos de dados
├── Negocio.hs        # Lógica de negócios pura
├── Persistencia.hs   # Camada de persistência
└── Menus.hs          # Interface de usuário (menus)

dados/                # Diretório de persistência
├── passageiros.db
├── companhias.db
├── voos.db
└── reservas.db
```

### Camadas da Arquitetura

1. **Tipos.hs** - Define todas as entidades do domínio

   - `Passageiro`: id, nome, documento
   - `Companhia`: id, nomeCompanhia
   - `Voo`: id, origem, destino, horario, companhiaId
   - `Reserva`: id, passageiroId, vooId, status
   - `Sistema`: estado global da aplicação

2. **Negocio.hs** - Funções puras de lógica de negócios

   - Validações de dados
   - Operações CRUD
   - Regras de negócio
   - Retorna `Either String Sistema` para tratamento de erros

3. **Persistencia.hs** - Gerenciamento de dados persistentes

   - Leitura/escrita em arquivos texto
   - Serialização usando Show/Read
   - Tratamento de arquivos ausentes ou inválidos

4. **Menus.hs** - Interface de usuário textual

   - Sistema de menus interativos
   - Exibição de dados formatados
   - Captura de entrada do usuário

5. **Main.hs** - Inicialização e execução
   - Carregamento do estado inicial
   - Loop principal de menus
   - Salvamento ao sair

## Pré-requisitos

- [Stack](https://docs.haskellstack.org/en/stable/README/) instalado
- Windows, Linux ou macOS

## Instalação e Execução

### 1. Clone o repositório

```bash
git clone <url-do-repositorio>
cd Aeroporto
```

### 2. Build do projeto

```bash
stack build
```

### 3. Executar a aplicação

#### No Windows (PowerShell)

Para garantir a correta exibição de caracteres portugueses:

```powershell
chcp 65001
$OutputEncoding = [Console]::OutputEncoding = [Text.UTF8Encoding]::UTF8
stack run
```

#### No Linux/macOS

```bash
stack run
```

## Como Usar

### Menu Principal

Ao executar a aplicação, você verá o menu principal com as seguintes opções:

```
=== Sistema de Gerenciamento de Aeroporto ===
1. Gerenciar Passageiros
2. Gerenciar Companhias
3. Gerenciar Voos
4. Gerenciar Reservas
5. Relatorios
0. Sair
```

### Exemplos de Uso

#### Cadastrar um Passageiro

1. Selecione opção `1` (Gerenciar Passageiros)
2. Selecione opção `1` (Cadastrar Passageiro)
3. Digite o nome do passageiro
4. Digite o documento do passageiro

#### Cadastrar um Voo

1. Selecione opção `3` (Gerenciar Voos)
2. Selecione opção `1` (Cadastrar Voo)
3. Liste as companhias para escolher uma ID
4. Digite origem, destino, horário e ID da companhia

## Estrutura de Dados

### Sistema de IDs

- Todos os registros possuem IDs auto-incrementais
- IDs começam em 1
- Novo ID = máximo dos IDs existentes + 1

### Persistência de Dados

- Dados são salvos automaticamente ao sair da aplicação
- Arquivos são criados no diretório `dados/`
- Formato: texto plano usando serialização Haskell
- Carga automática na inicialização

## Regras de Negócio

### Validações

- **Passageiros**: Nome e documento não podem ser vazios; documento deve ser único
- **Companhias**: Nome não pode ser vazio; deve ser único
- **Voos**: Origem, destino e horário não podem ser vazios; companhia deve existir
- **Reservas**: Passageiro e voo devem existir; não permite reservas duplicadas ativas

### Status de Reservas

- **Pendente**: Reserva criada mas não confirmada
- **Confirmada**: Reserva confirmada pelo sistema
- **Cancelada**: Reserva cancelada

### Transições de Status

```
Pendente → Confirmada (confirmarReserva)
Pendente → Cancelada (cancelarReserva)
Confirmada → Cancelada (cancelarReserva)
```

## Comandos Úteis

```bash
# Build do projeto
stack build

# Executar aplicação
stack run

# Abrir REPL interativo
stack ghci

# Limpar artefatos de build
stack clean
```

## Tratamento de Erros

- Validações retornam mensagens descritivas de erro
- Erros são exibidos com prefixo `[ERRO]`
- Sistema continua executando após erros de validação
- Dados inválidos não são persistidos

## Status de Implementação

### Funcionalidades Completas

- Gestão de Passageiros
- Gestão de Companhias
- Gestão de Voos
- Lógica de Reservas
