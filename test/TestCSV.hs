import CSV (parse)

import Test.HUnit

import qualified Data.ByteString.Char8 as C8

p = parse . C8.pack
csv = map (map C8.pack)

csvTests =
  TestList [
    TestLabel "empty"
      (TestCase (assertEqual "for empty" [ ] (p "")))
  , TestLabel "oneField"
      (TestCase (assertEqual "for one field" (csv [[ "foo" ]]) (p "foo")))
  , TestLabel "simpleSingleLine"
      (TestCase ((assertEqual "for multiple fields" (csv [[ "foo", "bar", "amp" ]]) (p "foo,bar,amp"))))
  , TestLabel "simpleSingleLineWithWhitespaceAfterComma"
      (TestCase ((assertEqual "for multiple fields with whitespace after comma" (csv [[ "foo", "bar", "amp" ]]) (p "foo, bar,\t amp"))))
  , TestLabel "multiLine"
      (TestCase ((assertEqual "for multiple lines" (csv [[ "foo", "bar", "amp" ], [ "10", "20", "30" ]]) (p "foo,bar,amp\n10,20,30"))))
  , TestLabel "simpleEscapedQuotes"
      (TestCase ((assertEqual "for escaped quotes" (csv [[ "\"\"\"" ]]) (p "\"\"\"\"\"\""))))
  , TestLabel "simpleEscapedQuotesInQuotes"
      (TestCase ((assertEqual "for escaped quotes in quotes" (csv [[ "foo\"\"\"bar" ]]) (p "\"foo\"\"\"\"\"\"\"bar"))))
  , TestLabel "commaInQuotes"
      (TestCase ((assertEqual "for comma in quotes" (csv [[ "foo,bar,amp", "foobar" ], [ "goo" ]]) (p "foo\",bar,\"amp,foobar\ngoo"))))
  , TestLabel "newlineInQuotes"
      (TestCase (assertEqual "for newline in quotes" (csv [[ "foo\nbar", "amp" ]]) (p "foo\"\nbar\",amp")))
  , TestLabel "complicatedCase"
      (TestCase (assertEqual "for a complicated case"
        (csv [ [ "foo", "bar", "amp" ],
               [ "a,b,c", "d", "e\nfg", "foo", "", "", "" ],
               [ "", "\"bar\"", "" ] ])
        (p "foo,bar,amp\r\n\"a,b,c\",d,e\"\nfg\",foo,,,\n,\"\"bar\"\",")))
  ]

main = runTestTT csvTests
