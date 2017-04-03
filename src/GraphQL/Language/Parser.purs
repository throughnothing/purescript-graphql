{-
Parser based off the GraphQL Spec:
https://facebook.github.io/graphql/#EnumValue
-}
module GraphQL.Language.Parser
  ( operationType
  , document
  , argument
  , arguments
  , name
  , tok
  , opt
  , parens
  , braces
  , brackets
  , quotes
  , between
  , whiteSpace
  )where

import GraphQL.Language.AST as GA
import GraphQL.Types as GT
import Text.Parsing.StringParser.Combinators as SC
import Text.Parsing.StringParser.String as S
import Control.Alternative (class Alternative, (<|>))
import Control.Applicative ((*>))
import Control.Apply ((<$), (<$>), (<*>))
import Control.Bind ((<*))
import Control.Lazy (defer)
import Control.Monad (pure, void)
import Data.BooleanAlgebra ((||))
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Int (floor)
import Data.List.Lazy (foldMap)
import Data.List.Types (List)
import Data.Monoid (class Monoid, append, mempty)
import Data.NonEmpty ((:|), NonEmpty)
import Data.String (singleton)
import Data.Traversable (foldl, foldr, fold)
import Data.Unit (Unit)
import Global (readFloat, readInt)
import Prelude ((<>), map, (<<<), ($), bind)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.Combinators ((<?>))

-- | Parse a GraphQL `name`, which is `/[_A-Za-z][_0-9A-Za-z]*/`
name :: Parser GA.Name
name = S.regex "[_A-Za-z][_0-9A-Za-z]*"

-- | Document Parser

document :: Parser GA.Document
document = S.whiteSpace *> manyNE definition

operationType :: Parser GA.OperationType
operationType = GA.Query    <$ tok "query"
            <|> GA.Mutation <$ tok "mutation"
            <?> "operationType error"

definition :: Parser GA.Definition
definition = GA.DefinitionOperation <$> operationDefinition
         <|> GA.DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser GA.OperationDefinition
operationDefinition = GA.OperationSelectionSet <$> selectionSet
                  <|> GA.OperationDefinition   <$> operationType
                                               <*> SC.optionMaybe name
                                               <*> opt variableDefinitions
                                               <*> opt directives
                                               <*> selectionSet
                  <?> "operationDefinition error"

-- * SelectionSet

selectionSet :: Parser GA.SelectionSet
selectionSet = braces $ manyNE (defer \_ -> selection)

selectionSetOpt :: Parser GA.SelectionSetOpt
selectionSetOpt = braces $ SC.many1 (defer \_ -> selection)

selection :: Parser GA.Selection
selection = GA.SelectionField          <$> (defer \_ -> field)
        <|> GA.SelectionFragmentSpread <$> fragmentSpread
        <|> GA.SelectionInlineFragment <$> (defer \_ -> inlineFragment)
        <?> "selection error!"

-- * Field

field :: Parser GA.Field
field = GA.Field <$> SC.optionMaybe alias
              <*> name
              <*> opt arguments
              <*> opt directives
              <*> (defer \_ -> opt selectionSetOpt)

alias :: Parser GA.Alias
alias = name <* tok ":"


-- * Variables

variableDefinitions :: Parser GA.VariableDefinitions
variableDefinitions = parens $ SC.many1 variableDefinition

variableDefinition :: Parser GA.VariableDefinition
variableDefinition = GA.VariableDefinition <$> variable
                                           <*  tok ":"
                                           <*> type_
                                           <*> SC.optionMaybe defaultValue

variable :: Parser GA.Variable
variable = tok "$" *> name

defaultValue :: Parser GA.DefaultValue
defaultValue = tok "=" *> value

-- * Input Types

type_ :: Parser GA.InputType
type_ = GA.TypeNamed   <$> name -- TODO <* but "!"
    <|> GA.TypeList    <$> brackets (defer \_ -> type_)
    <|> GA.TypeNonNull <$> (defer \_ -> nonNullType)
    <?> "type_ error!"

nonNullType :: Parser GA.NonNullType
nonNullType = GA.NonNullTypeNamed <$> name <* tok "!"
          <|> GA.NonNullTypeList  <$> brackets (defer \_ -> type_)  <* tok "!"
          <?> "nonNullType error!"

-- * Directives

directives :: Parser GA.Directives
directives = SC.many directive

directive :: Parser GA.Directive
directive = GA.Directive
        <$  tok "@"
        <*> name
        <*> opt arguments

-- | Arguments

arguments :: Parser GA.Arguments
arguments = parens $ whiteSpace *> (SC.many1 argument) <* whiteSpace

argument :: Parser GA.Argument
argument = GA.Argument <$> name <* tok ":" <*> value <* whiteSpace

-- * Fragments

fragmentSpread :: Parser GA.FragmentSpread
fragmentSpread = GA.FragmentSpread <$  tok "..."
                                <*> fragmentName
                                <*> opt directives

inlineFragment :: Parser GA.InlineFragment
inlineFragment = GA.InlineFragment <$  tok "..."
                                <*> SC.optionMaybe typeCondition
                                <*> opt directives
                                <*> (defer \x -> selectionSet)

fragmentDefinition :: Parser GA.FragmentDefinition
fragmentDefinition = GA.FragmentDefinition
                 <$  tok "fragment"
                 <*> name
                 <*> typeCondition
                 <*> opt directives
                 <*> selectionSet

-- | TODO: Implente `but tok "on"` to exclude those
fragmentName :: Parser GA.FragmentName
fragmentName = name

typeCondition :: Parser GA.TypeCondition
typeCondition = tok "on" *> name

-- | Values

value :: Parser GA.Value
value = GA.ValueVariable <$> variable
    <|> GA.ValueFloat    <$> floatValue
    <|> GA.ValueInt      <$> intValue
    <|> GA.ValueBoolean  <$> booleanValue
    <|> GA.ValueNull     <$  tok "null"
    <|> GA.ValueString   <$> stringValue
    <|> GA.ValueEnum     <$> enumValue
    <|> GA.ValueList     <$> listValue
    <|> GA.ValueObject   <$> objectValue
    <?> "value error!"
  where
    floatValue :: Parser GA.FloatValue
    floatValue = map (readFloat) $ S.regex "\\d+\\.\\d+"

    intValue :: Parser GA.IntValue
    intValue = mapInt $ S.regex "\\d+"
      where
        mapInt :: Parser String -> Parser Int
        mapInt ps = map (\s -> floor (readInt 10 s)) ps

    booleanValue :: Parser GA.BooleanValue
    booleanValue = true  <$ tok "true"
               <|> false <$ tok "false"

    stringValue :: Parser GA.StringValue
    stringValue = quotes $ charToStr (SC.many (S.noneOf ['"']))

    -- | TODO: ensure not `true`, `false` or `null`
    -- https://facebook.github.io/graphql/#EnumValue
    enumValue :: Parser GA.Name
    enumValue = name

    listValue :: Parser (List GA.Value)
    listValue = brackets $ SC.many1 (defer \_ -> value)

    objectValue :: Parser (List GA.ObjectField)
    objectValue = braces $ SC.many1 (defer \_ -> objectField)

objectField :: Parser GA.ObjectField
objectField = GA.ObjectField <$> name <* tok ":" <*> (defer \_ -> value)


-- | Internal

parserCharToStr :: Parser (List Char) -> Parser String
parserCharToStr c = map (fold <<< map singleton) c

tok :: String -> Parser String
tok s = whiteSpace *> (S.string s) <* whiteSpace

opt :: ∀ a. Monoid a => Parser a -> Parser a
opt = SC.option mempty


-- | Between Helpers

parens :: ∀ a. Parser a -> Parser a
parens = between "(" ")"

braces :: ∀ a. Parser a -> Parser a
braces = between "{" "}"

quotes :: ∀ a. Parser a -> Parser a
quotes = between "\"" "\""

brackets :: ∀ a. Parser a -> Parser a
brackets = between "[" "]"

between :: ∀ a. String -> String -> Parser a -> Parser a
between open close = SC.between (S.string open) (S.string close)


-- | Other Helpers

manyNE :: ∀ a. Parser a -> Parser (NonEmpty List a)
manyNE p = (:|) <$> p <*> (SC.many p)



-- | TODO: Copied whiteSpace function, and added ',` case...prob a better way
-- | TODO: Handle Comments
whiteSpace :: Parser String
whiteSpace = do
  cs <- SC.many (S.satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t' || c == ',')
  pure (foldMap singleton cs)

charToStr :: Parser (List Char) -> Parser String
charToStr p = map (foldMap singleton) p

