# This file is a part of Julia. License is MIT: https://julialang.org/license

# some test strings
astr = "Hello, world.\n"
u8str = "∀ ε > 0, ∃ δ > 0: |x-y| < δ ⇒ |f(x)-f(y)| < ε"

# I think these should give error on 4 also, and "" is not treated
# consistently with SubString("",1,1), nor with Char[]
for ind in (0, 5)
    @test_throws BoundsError findnext(SubString("",1,1), "foo", ind)
    @test_throws BoundsError rsearch("foo", SubString("",1,1), ind)
    @test_throws BoundsError searchindex("foo", SubString("",1,1), ind)
    @test_throws BoundsError rsearchindex("foo", SubString("",1,1), ind)
end

# Note: the commented out tests will be enabled after fixes to make
# sure that search/rsearch/searchindex/rsearchindex are consistent
# no matter what type of AbstractString the second argument is
@test_throws BoundsError findnext(equalto('a'), "foo", 0)
@test_throws BoundsError findnext(x -> x in Char[], "foo", 5)
# @test_throws BoundsError rsearch("foo", Char[], 0)
@test_throws BoundsError rsearch("foo", Char[], 5)

# @test_throws BoundsError searchindex("foo", Char[], 0)
# @test_throws BoundsError searchindex("foo", Char[], 5)
# @test_throws BoundsError rsearchindex("foo", Char[], 0)
# @test_throws BoundsError rsearchindex("foo", Char[], 5)

# @test_throws ErrorException in("foobar","bar")
@test_throws BoundsError findnext(equalto(0x1),b"\x1\x2",0)
@test rsearchindex(b"foo",b"o",0) == 0
@test rsearchindex(SubString("",1,0),SubString("",1,0)) == 1

# ascii search
for str in [astr, GenericString(astr)]
    @test_throws BoundsError findnext(equalto('z'), str, 0)
    @test_throws BoundsError findnext(equalto('∀'), str, 0)
    @test findfirst(equalto('x'), str) == 0
    @test findfirst(equalto('\0'), str) == 0
    @test findfirst(equalto('\u80'), str) == 0
    @test findfirst(equalto('∀'), str) == 0
    @test findfirst(equalto('H'), str) == 1
    @test findfirst(equalto('l'), str) == 3
    @test findnext(equalto('l'), str, 4) == 4
    @test findnext(equalto('l'), str, 5) == 11
    @test findnext(equalto('l'), str, 12) == 0
    @test findfirst(equalto(','), str) == 6
    @test findnext(equalto(','), str, 7) == 0
    @test findfirst(equalto('\n'), str) == 14
    @test findnext(equalto('\n'), str, 15) == 0
    @test_throws BoundsError findnext(equalto('ε'), str, nextind(str,endof(str))+1)
    @test_throws BoundsError findnext(equalto('a'), str, nextind(str,endof(str))+1)
end

# ascii rsearch
for str in [astr]
    @test rsearch(str, 'x') == 0
    @test rsearch(str, '\0') == 0
    @test rsearch(str, '\u80') == 0
    @test rsearch(str, '∀') == 0
    @test rsearch(str, 'H') == 1
    @test rsearch(str, 'H', 0) == 0
    @test rsearch(str, 'l') == 11
    @test rsearch(str, 'l', 5) == 4
    @test rsearch(str, 'l', 4) == 4
    @test rsearch(str, 'l', 3) == 3
    @test rsearch(str, 'l', 2) == 0
    @test rsearch(str, ',') == 6
    @test rsearch(str, ',', 5) == 0
    @test rsearch(str, '\n') == 14
end

# utf-8 search
for str in (u8str, GenericString(u8str))
    @test_throws BoundsError findnext(equalto('z'), str, 0)
    @test_throws BoundsError findnext(equalto('∀'), str, 0)
    @test findfirst(equalto('z'), str) == 0
    @test findfirst(equalto('\0'), str) == 0
    @test findfirst(equalto('\u80'), str) == 0
    @test findfirst(equalto('∄'), str) == 0
    @test findfirst(equalto('∀'), str) == 1
    @test_throws UnicodeError findnext(equalto('∀'), str, 2)
    @test findnext(equalto('∀'), str, 4) == 0
    @test findfirst(equalto('∃'), str) == 13
    @test_throws UnicodeError findnext(equalto('∃'), str, 15)
    @test findnext(equalto('∃'), str, 16) == 0
    @test findfirst(equalto('x'), str) == 26
    @test findnext(equalto('x'), str, 27) == 43
    @test findnext(equalto('x'), str, 44) == 0
    @test findfirst(equalto('δ'), str) == 17
    @test_throws UnicodeError findnext(equalto('δ'), str, 18)
    @test findnext(equalto('δ'), str, nextind(str,17)) == 33
    @test findnext(equalto('δ'), str, nextind(str,33)) == 0
    @test findfirst(equalto('ε'), str) == 5
    @test findnext(equalto('ε'), str, nextind(str,5)) == 54
    @test findnext(equalto('ε'), str, nextind(str,54)) == 0
    @test findnext(equalto('ε'), str, nextind(str,endof(str))) == 0
    @test findnext(equalto('a'), str, nextind(str,endof(str))) == 0
    @test_throws BoundsError findnext(equalto('ε'), str, nextind(str,endof(str))+1)
    @test_throws BoundsError findnext(equalto('a'), str, nextind(str,endof(str))+1)
end

# utf-8 rsearch
for str in [u8str]
    @test rsearch(str, 'z') == 0
    @test rsearch(str, '\0') == 0
    @test rsearch(str, '\u80') == 0
    @test rsearch(str, '∄') == 0
    @test rsearch(str, '∀') == 1
    @test rsearch(str, '∀', 0) == 0
    @test rsearch(str, '∃') == 13
    @test rsearch(str, '∃', 14) == 13
    @test rsearch(str, '∃', 13) == 13
    @test rsearch(str, '∃', 12) == 0
    @test rsearch(str, 'x') == 43
    @test rsearch(str, 'x', 42) == 26
    @test rsearch(str, 'x', 25) == 0
    @test rsearch(str, 'δ') == 33
    @test rsearch(str, 'δ', 32) == 17
    @test rsearch(str, 'δ', 16) == 0
    @test rsearch(str, 'ε') == 54
    @test rsearch(str, 'ε', 53) == 5
    @test rsearch(str, 'ε', 4) == 0
end

# string search with a single-char string
@test findfirst("x", astr) == 0:-1
@test findfirst("H", astr) == 1:1
@test findnext("H", astr, 2) == 0:-1
@test findfirst("l", astr) == 3:3
@test findnext("l", astr, 4) == 4:4
@test findnext("l", astr, 5) == 11:11
@test findnext("l", astr, 12) == 0:-1
@test findfirst("\n", astr) == 14:14
@test findnext("\n", astr, 15) == 0:-1

@test findfirst("z", u8str) == 0:-1
@test findfirst("∄", u8str) == 0:-1
@test findfirst("∀", u8str) == 1:1
@test findnext("∀", u8str, 4) == 0:-1
@test findfirst("∃", u8str) == 13:13
@test findnext("∃", u8str, 16) == 0:-1
@test findfirst("x", u8str) == 26:26
@test findnext("x", u8str, 27) == 43:43
@test findnext("x", u8str, 44) == 0:-1
@test findfirst("ε", u8str) == 5:5
@test findnext("ε", u8str, 7) == 54:54
@test findnext("ε", u8str, 56) == 0:-1

# string rsearch with a single-char string
@test rsearch(astr, "x") == 0:-1
@test rsearch(astr, "H") == 1:1
@test rsearch(astr, "H", 2) == 1:1
@test rsearch(astr, "H", 0) == 0:-1
@test rsearch(astr, "l") == 11:11
@test rsearch(astr, "l", 10) == 4:4
@test rsearch(astr, "l", 4) == 4:4
@test rsearch(astr, "l", 3) == 3:3
@test rsearch(astr, "l", 2) == 0:-1
@test rsearch(astr, "\n") == 14:14
@test rsearch(astr, "\n", 13) == 0:-1

@test rsearch(u8str, "z") == 0:-1
@test rsearch(u8str, "∄") == 0:-1
@test rsearch(u8str, "∀") == 1:1
@test rsearch(u8str, "∀", 0) == 0:-1
#TODO: setting the limit in the middle of a wide char
#      makes search fail but rsearch succeed.
#      Should rsearch fail as well?
#@test rsearch(u8str, "∀", 2) == 0:-1 # gives 1:3
@test rsearch(u8str, "∃") == 13:13
@test rsearch(u8str, "∃", 12) == 0:-1
@test rsearch(u8str, "x") == 43:43
@test rsearch(u8str, "x", 42) == 26:26
@test rsearch(u8str, "x", 25) == 0:-1
@test rsearch(u8str, "ε") == 54:54
@test rsearch(u8str, "ε", 53) == 5:5
@test rsearch(u8str, "ε", 4) == 0:-1

# string search with a single-char regex
@test findfirst(r"x", astr) == 0:-1
@test findfirst(r"H", astr) == 1:1
@test findnext(r"H", astr, 2) == 0:-1
@test findfirst(r"l", astr) == 3:3
@test findnext(r"l", astr, 4) == 4:4
@test findnext(r"l", astr, 5) == 11:11
@test findnext(r"l", astr, 12) == 0:-1
@test findfirst(r"\n", astr) == 14:14
@test findnext(r"\n", astr, 15) == 0:-1
@test findfirst(r"z", u8str) == 0:-1
@test findfirst(r"∄", u8str) == 0:-1
@test findfirst(r"∀", u8str) == 1:1
@test findnext(r"∀", u8str, 4) == 0:-1
@test findfirst(r"∀", u8str) == findfirst(r"\u2200", u8str)
@test findnext(r"∀", u8str, 4) == findnext(r"\u2200", u8str, 4)
@test findfirst(r"∃", u8str) == 13:13
@test findnext(r"∃", u8str, 16) == 0:-1
@test findfirst(r"x", u8str) == 26:26
@test findnext(r"x", u8str, 27) == 43:43
@test findnext(r"x", u8str, 44) == 0:-1
@test findfirst(r"ε", u8str) == 5:5
@test findnext(r"ε", u8str, 7) == 54:54
@test findnext(r"ε", u8str, 56) == 0:-1
for i = 1:endof(astr)
    @test findnext(r"."s, astr, i) == i:i
end
for i = 1:endof(u8str)
    if isvalid(u8str,i)
        @test findnext(r"."s, u8str, i) == i:i
    end
end

# string search with a zero-char string
for i = 1:endof(astr)
    @test findnext("", astr, i) == i:i-1
end
for i = 1:endof(u8str)
    @test findnext("", u8str, i) == i:i-1
end
@test findfirst("", "") == 1:0

# string rsearch with a zero-char string
for i = 1:endof(astr)
    @test rsearch(astr, "", i) == i:i-1
end
for i = 1:endof(u8str)
    @test rsearch(u8str, "", i) == i:i-1
end
@test rsearch("", "") == 1:0

# string search with a zero-char regex
for i = 1:endof(astr)
    @test findnext(r"", astr, i) == i:i-1
end
for i = 1:endof(u8str)
    # TODO: should regex search fast-forward invalid indices?
    if isvalid(u8str,i)
        @test findnext(r"", u8str, i) == i:i-1
    end
end

# string search with a two-char string literal
@test findfirst("xx", "foo,bar,baz") == 0:-1
@test findfirst("fo", "foo,bar,baz") == 1:2
@test findnext("fo", "foo,bar,baz", 3) == 0:-1
@test findfirst("oo", "foo,bar,baz") == 2:3
@test findnext("oo", "foo,bar,baz", 4) == 0:-1
@test findfirst("o,", "foo,bar,baz") == 3:4
@test findnext("o,", "foo,bar,baz", 5) == 0:-1
@test findfirst(",b", "foo,bar,baz") == 4:5
@test findnext(",b", "foo,bar,baz", 6) == 8:9
@test findnext(",b", "foo,bar,baz", 10) == 0:-1
@test findfirst("az", "foo,bar,baz") == 10:11
@test findnext("az", "foo,bar,baz", 12) == 0:-1

# issue #9365
# string search with a two-char UTF-8 (2 byte) string literal
@test findfirst("éé", "ééé") == 1:3
@test findnext("éé", "ééé", 1) == 1:3
# string search with a two-char UTF-8 (3 byte) string literal
@test findfirst("€€", "€€€") == 1:4
@test findnext("€€", "€€€", 1) == 1:4
# string search with a two-char UTF-8 (4 byte) string literal
@test findfirst("\U1f596\U1f596", "\U1f596\U1f596\U1f596") == 1:5
@test findnext("\U1f596\U1f596", "\U1f596\U1f596\U1f596", 1) == 1:5

# string search with a two-char UTF-8 (2 byte) string literal
@test findfirst("éé", "éé") == 1:3
@test findnext("éé", "éé", 1) == 1:3
# string search with a two-char UTF-8 (3 byte) string literal
@test findfirst("€€", "€€") == 1:4
@test findnext("€€", "€€", 1) == 1:4
# string search with a two-char UTF-8 (4 byte) string literal
@test findfirst("\U1f596\U1f596", "\U1f596\U1f596") == 1:5
@test findnext("\U1f596\U1f596", "\U1f596\U1f596", 1) == 1:5

# string rsearch with a two-char UTF-8 (2 byte) string literal
@test rsearch("ééé", "éé") == 3:5
@test rsearch("ééé", "éé", endof("ééé")) == 3:5
# string rsearch with a two-char UTF-8 (3 byte) string literal
@test rsearch("€€€", "€€") == 4:7
@test rsearch("€€€", "€€", endof("€€€")) == 4:7
# string rsearch with a two-char UTF-8 (4 byte) string literal
@test rsearch("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 5:9
@test rsearch("\U1f596\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 5:9

# string rsearch with a two-char UTF-8 (2 byte) string literal
@test rsearch("éé", "éé") == 1:3        # should really be 1:4!
@test rsearch("éé", "éé", endof("ééé")) == 1:3
# string search with a two-char UTF-8 (3 byte) string literal
@test rsearch("€€", "€€") == 1:4        # should really be 1:6!
@test rsearch("€€", "€€", endof("€€€")) == 1:4
# string search with a two-char UTF-8 (4 byte) string literal
@test rsearch("\U1f596\U1f596", "\U1f596\U1f596") == 1:5        # should really be 1:8!
@test rsearch("\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 1:5

# string rsearch with a two-char string literal
@test rsearch("foo,bar,baz", "xx") == 0:-1
@test rsearch("foo,bar,baz", "fo") == 1:2
@test rsearch("foo,bar,baz", "fo", 1) == 0:-1
@test rsearch("foo,bar,baz", "oo") == 2:3
@test rsearch("foo,bar,baz", "oo", 2) == 0:-1
@test rsearch("foo,bar,baz", "o,") == 3:4
@test rsearch("foo,bar,baz", "o,", 1) == 0:-1
@test rsearch("foo,bar,baz", ",b") == 8:9
@test rsearch("foo,bar,baz", ",b", 6) == 4:5
@test rsearch("foo,bar,baz", ",b", 3) == 0:-1
@test rsearch("foo,bar,baz", "az") == 10:11
@test rsearch("foo,bar,baz", "az", 10) == 0:-1

# array rsearch
@test rsearch(UInt8[1,2,3],UInt8[2,3],3) == 2:3
@test rsearch(UInt8[1,2,3],UInt8[2,3],1) == 0:-1

# string search with a two-char regex
@test findfirst(r"xx", "foo,bar,baz") == 0:-1
@test findfirst(r"fo", "foo,bar,baz") == 1:2
@test findnext(r"fo", "foo,bar,baz", 3) == 0:-1
@test findfirst(r"oo", "foo,bar,baz") == 2:3
@test findnext(r"oo", "foo,bar,baz", 4) == 0:-1
@test findfirst(r"o,", "foo,bar,baz") == 3:4
@test findnext(r"o,", "foo,bar,baz", 5) == 0:-1
@test findfirst(r",b", "foo,bar,baz") == 4:5
@test findnext(r",b", "foo,bar,baz", 6) == 8:9
@test findnext(r",b", "foo,bar,baz", 10) == 0:-1
@test findfirst(r"az", "foo,bar,baz") == 10:11
@test findnext(r"az", "foo,bar,baz", 12) == 0:-1

@test searchindex("foo", 'o') == 2
@test searchindex("foo", 'o', 3) == 3

# string searchindex with a two-char UTF-8 (2 byte) string literal
@test searchindex("ééé", "éé") == 1
@test searchindex("ééé", "éé", 1) == 1
# string searchindex with a two-char UTF-8 (3 byte) string literal
@test searchindex("€€€", "€€") == 1
@test searchindex("€€€", "€€", 1) == 1
# string searchindex with a two-char UTF-8 (4 byte) string literal
@test searchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 1
@test searchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596", 1) == 1

# string searchindex with a two-char UTF-8 (2 byte) string literal
@test searchindex("éé", "éé") == 1
@test searchindex("éé", "éé", 1) == 1
# string searchindex with a two-char UTF-8 (3 byte) string literal
@test searchindex("€€", "€€") == 1
@test searchindex("€€", "€€", 1) == 1
# string searchindex with a two-char UTF-8 (4 byte) string literal
@test searchindex("\U1f596\U1f596", "\U1f596\U1f596") == 1
@test searchindex("\U1f596\U1f596", "\U1f596\U1f596", 1) == 1

# contains with a String and Char needle
@test contains("foo", "o")
@test contains("foo", 'o')

# string rsearchindex with a two-char UTF-8 (2 byte) string literal
@test rsearchindex("ééé", "éé") == 3
@test rsearchindex("ééé", "éé", endof("ééé")) == 3
# string rsearchindex with a two-char UTF-8 (3 byte) string literal
@test rsearchindex("€€€", "€€") == 4
@test rsearchindex("€€€", "€€", endof("€€€")) == 4
# string rsearchindex with a two-char UTF-8 (4 byte) string literal
@test rsearchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596") == 5
@test rsearchindex("\U1f596\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 5

# string rsearchindex with a two-char UTF-8 (2 byte) string literal
@test rsearchindex("éé", "éé") == 1
@test rsearchindex("éé", "éé", endof("ééé")) == 1
# string searchindex with a two-char UTF-8 (3 byte) string literal
@test rsearchindex("€€", "€€") == 1
@test rsearchindex("€€", "€€", endof("€€€")) == 1
# string searchindex with a two-char UTF-8 (4 byte) string literal
@test rsearchindex("\U1f596\U1f596", "\U1f596\U1f596") == 1
@test rsearchindex("\U1f596\U1f596", "\U1f596\U1f596", endof("\U1f596\U1f596\U1f596")) == 1

@test_throws ErrorException "ab" ∈ "abc"

# issue #15723
@test findfirst(equalto('('), "⨳(") == 4
@test findnext(equalto('('), "(⨳(", 2) == 5
@test findlast(equalto('('), "(⨳(") == 5
@test findprev(equalto('('), "(⨳(", 2) == 1
