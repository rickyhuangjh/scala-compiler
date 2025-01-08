/*
   Copyright 2024 Ondrej Lhotak. All rights reserved.

   Permission is granted for private study use by students registered in
   CS 241E in the Fall 2024 term.

   The contents of this file may not be published, in whole or in part,
   in print or electronic form.

   The contents of this file may be included in work submitted for CS
   241E assignments in Fall 2024. The contents of this file may not be
   submitted, in whole or in part, for credit in any other course.
*/
package cs241e.assignments

import cs241e.scanparse.DFAs.*

object A7 {
    /** A sample DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes. */
    val binaryNumbers = DFA(
        alphabet = "01".toSet,
        states = Set("start", "0", "not0"),
        start = "start",
        accepting = Set("0", "not0"),
        transition = {
            case ("start", '0') => "0"
            case ("start", '1') => "not0"
            case ("not0", _) => "not0"
        })

    /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
     * and are not divisible by 3.
     */
    lazy val notDiv3 = DFA(
        alphabet = "01".toSet,
        states = Set("start", "0m3", "1m3", "2m3"),
        start = "start",
        accepting = Set("1m3", "2m3"),
        transition = {
            case ("start", '1') => "1m3"
            case ("0m3", '0') => "0m3"
            case ("0m3", '1') => "1m3"
            case ("1m3", '0') => "2m3"
            case ("1m3", '1') => "0m3"
            case ("2m3", '0') => "1m3"
            case ("2m3", '1') => "2m3"
        })

    /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
     * and are not divisible by 2 or by 3.
     */
    lazy val notDiv23 = DFA(
        alphabet = "01".toSet,
        states = Set("start", "0m6", "1m6", "2m6", "3m6", "4m6", "5m6"),
        start = "start",
        accepting = Set("1m6", "5m6"),
        transition = {
            case ("start", '1') => "1m6"
            case ("0m6", '0') => "0m6"
            case ("0m6", '1') => "1m6"
            case ("1m6", '0') => "2m6"
            case ("1m6", '1') => "3m6"
            case ("2m6", '0') => "4m6"
            case ("2m6", '1') => "5m6"
            case ("3m6", '0') => "0m6"
            case ("3m6", '1') => "1m6"
            case ("4m6", '0') => "2m6"
            case ("4m6", '1') => "3m6"
            case ("5m6", '0') => "4m6"
            case ("5m6", '1') => "5m6"
        })

    /** A DFA that recognizes a decimal number between -128 and 127 inclusive, with no useless zeroes.
     * (Zeroes are required and only permitted if removing them changes the meaning of the number.)
     * The alphabet symbols are {0,1,2,3,4,5,6,7,8,9,-}.
     */
    lazy val decimalNumber = DFA(
        alphabet = "-0123456789".toSet,
        states = Set("start", "-", "1", "12", "-1", "-12", "any", "final"),
        start = "start",
        accepting = Set("1", "12", "-1", "-12", "any", "final"),
        transition = {
            // start
            case ("start", '-') => "-"
            case ("start", '0') => "final"
            case ("start", '1') => "1"
            case ("start", '2') => "any"
            case ("start", '3') => "any"
            case ("start", '4') => "any"
            case ("start", '5') => "any"
            case ("start", '6') => "any"
            case ("start", '7') => "any"
            case ("start", '8') => "any"
            case ("start", '9') => "any"
            
            // 1
            case ("1", '0') => "any"
            case ("1", '1') => "any"
            case ("1", '2') => "12"
            case ("1", '3') => "final"
            case ("1", '4') => "final"
            case ("1", '5') => "final"
            case ("1", '6') => "final"
            case ("1", '7') => "final"
            case ("1", '8') => "final"
            case ("1", '9') => "final"
            
            // 12
            case ("12", '0') => "final"
            case ("12", '1') => "final"
            case ("12", '2') => "final"
            case ("12", '3') => "final"
            case ("12", '4') => "final"
            case ("12", '5') => "final"
            case ("12", '6') => "final"
            case ("12", '7') => "final"
            
            // -
            case ("-", '1') => "-1"
            case ("-", '2') => "any"
            case ("-", '3') => "any"
            case ("-", '4') => "any"
            case ("-", '5') => "any"
            case ("-", '6') => "any"
            case ("-", '7') => "any"
            case ("-", '8') => "any"
            case ("-", '9') => "any"

            // -1
            case ("-1", '0') => "any"
            case ("-1", '1') => "any"
            case ("-1", '2') => "-12"
            case ("-1", '3') => "final"
            case ("-1", '4') => "final"
            case ("-1", '5') => "final"
            case ("-1", '6') => "final"
            case ("-1", '7') => "final"
            case ("-1", '8') => "final"
            case ("-1", '9') => "final"
            
            // -12
            case ("-12", '0') => "final"
            case ("-12", '1') => "final"
            case ("-12", '2') => "final"
            case ("-12", '3') => "final"
            case ("-12", '4') => "final"
            case ("-12", '5') => "final"
            case ("-12", '6') => "final"
            case ("-12", '7') => "final"
            case ("-12", '8') => "final"
            
            // any 
            case ("any", '0') => "final"
            case ("any", '1') => "final"
            case ("any", '2') => "final"
            case ("any", '3') => "final"
            case ("any", '4') => "final"
            case ("any", '5') => "final"
            case ("any", '6') => "final"
            case ("any", '7') => "final"
            case ("any", '8') => "final"
            case ("any", '9') => "final"
        }
    )

    /** A DFA with alphabet {a, b, c} that recognizes any string that contains all three letters in
     * alphabetical order (i.e. "abc"), possibly interspersed with more letters. For example, "acbac"
     * and "cbacbacba" are in the language, but not "acba".
     */
    lazy val abc = DFA(
        alphabet = "abc".toSet,
        states = Set("wantA", "wantB", "wantC", "ok"),
        start = "wantA",
        accepting = Set("ok"),
        transition = {
            case ("wantA", 'a') => "wantB"
            case ("wantA", 'b') => "wantA"
            case ("wantA", 'c') => "wantA"
            case ("wantB", 'a') => "wantB"
            case ("wantB", 'b') => "wantC"
            case ("wantB", 'c') => "wantB"
            case ("wantC", 'a') => "wantC"
            case ("wantC", 'b') => "wantC"
            case ("wantC", 'c') => "ok"
            case ("ok", 'a') => "ok"
            case ("ok", 'b') => "ok"
            case ("ok", 'c') => "ok"
        }
    )

    /** A DFA that recognizes any string from the alphabet {a,b,c} containing abc as a substring. */
    lazy val abcSubstring = DFA(
        alphabet = "abc".toSet,
        states = Set("wantA", "wantB", "wantC", "ok"),
        start = "wantA",
        accepting = Set("ok"),
        transition = {
            case ("wantA", 'a') => "wantB"
            case ("wantA", 'b') => "wantA"
            case ("wantA", 'c') => "wantA"
            case ("wantB", 'a') => "wantB"
            case ("wantB", 'b') => "wantC"
            case ("wantB", 'c') => "wantA"
            case ("wantC", 'a') => "wantB"
            case ("wantC", 'b') => "wantA"
            case ("wantC", 'c') => "ok"
            case ("ok", 'a') => "ok"
            case ("ok", 'b') => "ok"
            case ("ok", 'c') => "ok"
        }
    )
}
