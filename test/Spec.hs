import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Control.Monad         (unless)
import           Data.Either

import           Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties"
    [ QC.testProperty "((\\v . v) i)_n = i" $
        \i ->
            let myid = F (Abs "x" x)
                fi = F $ AppN myid $ A $ I i
                IntVal result = runMValue $ interp fi
            in  i == result
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testGroup "Program 1" $
        [ testCase "Call by name is 2" $ intResult (program1 Name) @?= 2
        , testCase "Call by value is 1" $ intResult (program1 Value) @?= 1
        , testCase "Lazy call is 1" $ intResult (program1 Lazy) @?= 1
        ]
    , testGroup "Program 2" $
        [ testCase "Call by name is 0" $ intResult (program2 Name) @?= 0
        , testCase "Call by value is 1" $ intResult (program2 Value) @?= 1
        , testCase "Lazy call is 0" $ intResult (program2 Lazy) @?= 0
        ]
    , testGroup "Callcc" $
        [ testCase "Call current continuation" $
            let
                program =
                    F (AppV
                        (C Callcc)
                        (F (Abs "k"
                            (A (Plus
                                (F (AppV
                                    (F (Var "k"))
                                    (A (I 100))
                                ))
                                one
                            ))
                        ))
                    )

            in  intResult program @?= 100
        , testCase "Ignore current continuation" $
            let
                IntVal val = runMValue $ interp $
                    F (AppV
                        (C Callcc)
                        (F (Abs "k"
                            (A (I 200))
                        ))
                    )

            in  val @?= 200
        ]
    , testCase "Nondeterminism" $
        let p = A (Plus (N (Amb [zero, two, x])) (N (Amb [zero, one])))
            results = runM $ interp p
            errors = lefts results
            numbers = (\((IntVal y,_),_) -> y) <$> rights results
            errorsok = errors == ["Can't find x"]
            numbersok = length numbers == 4
                && 0 `elem` numbers
                && 1 `elem` numbers
                && 2 `elem` numbers
                && 3 `elem` numbers
        in  if not errorsok
                then assertFailure "Errors not ok"
                else unless numbersok $ assertFailure "Numbers not ok"
    ]

intResult :: Term -> Integer
intResult program = val
    where
        IntVal val = runMValue $ interp program

data Style = Name | Value | Lazy

-- ((\r. ((\x. x + x ; deref r) (r := deref r + 1 ; 1))_style (ref 0))_V
program1 :: Style -> Term
program1 style =
    F (AppV
        (F (Abs "r"
            (constructor style
                (F (Abs "x"
                    (R (Seq
                        (A (Plus x x))
                        (R (Deref r))
                    ))
                ))
                (R (Seq
                    (R (Assign
                        r
                        (A (Plus
                            (R (Deref r))
                            one
                        ))
                    ))
                    one
                ))
            )
        ))
        (R (Ref zero))
    )

-- ((\r. ((\x. deref r) (r := deref r + 1 ; 1))_style (ref 0))_V
program2 :: Style -> Term
program2 style =
    F (AppV
        (F (Abs "r"
            (constructor style
                (F (Abs "x"
                    (R (Deref r))
                ))
                (R (Seq
                    (R (Assign
                        r
                        (A (Plus
                            (R (Deref r))
                            one
                        ))
                    ))
                    one
                ))
            )
        ))
        (R (Ref zero))
    )

constructor :: Style -> Term -> Term -> Term
constructor style e1 e2 = case style of
    Name  -> F $ AppN e1 e2
    Value -> F $ AppV e1 e2
    Lazy  -> L $ AppL e1 e2

x, r, zero, one, two :: Term
x = F (Var "x")
r = F (Var "r")
zero = A (I 0)
one = A (I 1)
two = A (I 2)
