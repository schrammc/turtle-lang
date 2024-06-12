{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Language.Turtle.Frontend.ParsedASTSpec (spec) where

import Test.Hspec
import Language.Turtle.Frontend.ParsedAST
import Data.Functor.Identity
import Data.Typeable
import Data.Data

spec :: Spec
spec = describe "ParsedAST" $ 
  describe "Generic mapping / descent" $ do 
    let expr = EList 
            [ Annotated () $ EList [ Annotated () $ ELiteral $ NumLit 1]
            , Annotated () $ EList [ Annotated () $ ELiteral $ NumLit 2]
            ]
    it "identity" $ 
      runIdentity (astMapM (const $ pure ()) (const $ pure ()) pure expr) `shouldBe` expr

    it "add one" $ 
      let f :: forall a m . (Data a, Applicative m) => a -> m a
          f x = 
            case eqT @a @Literal  of
                Nothing -> pure x
                Just Refl -> pure $ case x of 
                    NumLit n -> NumLit (n+1)
                    _ -> x
          expr' = EList 
            [ Annotated () $ EList [ Annotated () $ ELiteral $ NumLit 2]
            , Annotated () $ EList [ Annotated () $ ELiteral $ NumLit 3]
            ]
      in runIdentity (astMapM (const $ pure ()) (const $ pure ()) f expr) `shouldBe` expr'