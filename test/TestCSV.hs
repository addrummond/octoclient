import CSV (parse)

import Test.HUnit

import qualified Data.ByteString.Char8 as C8

p = parse . C8.pack

csvTests =
  TestList [
    TestLabel "empty"
      (TestCase (assertEqual "for empty" [ ] (p "")))
  , TestLabel "oneField"
      (TestCase (assertEqual "for one field" [[ C8.pack "foo" ]] (p "foo")))
  , TestLabel "simpleSingleLine"
      (TestCase ((assertEqual "for multiple fields" [[ C8.pack "foo", C8.pack "bar", C8.pack "amp" ]] (p "foo,bar,amp"))))
  , TestLabel "multiLine"
      (TestCase ((assertEqual "for multiple lines" [[ C8.pack "foo", C8.pack "bar", C8.pack "amp" ], [ C8.pack "10", C8.pack "20", C8.pack "30" ]] (p "foo,bar,amp\n10,20,30"))))
  , TestLabel "simpleEscapedQuotes"
      (TestCase ((assertEqual "for escaped quotes" [[ C8.pack "\"\"\"" ]] (p "\"\"\"\"\"\""))))
  , TestLabel "simpleEscapedQuotesInQuotes"
      (TestCase ((assertEqual "for escaped quotes in quotes" [[ C8.pack "foo\"\"\"bar" ]] (p "\"foo\"\"\"\"\"\"\"bar"))))
  , TestLabel "commaInQuotes"
      (TestCase ((assertEqual "for comma in quotes" [[ C8.pack "foo,bar,amp", C8.pack "foobar" ], [ C8.pack "goo" ]] (p "foo\",bar,\"amp,foobar\ngoo"))))
  , TestLabel "newlineInQuotes"
      (TestCase (assertEqual "for newline in quotes" [ [ C8.pack "foo\nbar", C8.pack "amp" ] ] (p "foo\"\nbar\",amp")))
  , TestLabel "complicatedCase"
      (TestCase (assertEqual "for a complicated case"
        [ [ C8.pack "foo", C8.pack "bar", C8.pack "amp" ],
          [ C8.pack "a,b,c", C8.pack "d", C8.pack "e\nfg", C8.pack "foo", C8.pack "", C8.pack "", C8.pack "" ],
          [ C8.pack "", C8.pack "\"bar\"", C8.pack "" ] ]
        (p "foo,bar,amp\r\n\"a,b,c\",d,e\"\nfg\",foo,,,\n,\"\"bar\"\",")))
  ]

main = runTestTT csvTests
