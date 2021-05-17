module TokenSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TokenService

spec :: Spec
spec = do
    describe "Gerador de Caracteres" $ do
        context "Quando solicitado uma lista de tuplas a partir de uma seed" $ do
            it "Comprimento da lista retornada deve ser 52 caracteres mais o comprimento da semente" $ do
                let strValue = "str"
                let result = shTokenChars strValue
                52 + length strValue `shouldBe` length result

    describe "Comprimento do Token" $ do
        context "Quando solicitado um token" $ do
            it "Comprimento do token deve ser de 5 dígitos" $ do
                token <- shRandomToken "AkmTq" 5
                length token `shouldBe` 5
    
    describe "Aleatoriedade do Token" $ do
        context "Quando solicitados 3 tokens" $ do
            it "O valor dos tokens deve ser diferente, aleatório." $ do
                token1 <- shRandomToken "AkmTq" 5
                token2 <- shRandomToken "AkmTq" 5
                token3 <- shRandomToken "AkmTq" 5
                (token1 /= token2) `shouldBe` True
                (token1 /= token3) `shouldBe` True
                (token2 /= token3) `shouldBe` True
