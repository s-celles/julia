# This file is a part of Julia. License is MIT: https://julialang.org/license

const Chars = Union{Char,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}

"""
    findfirst(pattern::AbstractString, string::AbstractString)
    findfirst(pattern::Regex, string::String)

Find the first occurrence of `pattern` in `string`. Equivalent to
[`findnext(pattern, string, start(s))`](@ref).

# Examples
```jldoctest
julia> findfirst("z", "Hello to the world")
0:-1

julia> findfirst("Julia", "JuliaLang")
1:5
```
"""
findfirst(pattern::AbstractString, string::AbstractString) =
    findnext(pattern, string, start(string))

# AbstractString implementation of the generic findnext interface
function findnext(testf::Function, s::AbstractString, i::Integer=start(s))
    @boundscheck (i < 1 || i > nextind(s,endof(s))) && throw(BoundsError(s, i))
    @inbounds while !done(s,i)
        d, j = next(s,i)
        if testf(d)
            return i
        end
        i = j
    end
    return 0
end

in(c::Char, s::AbstractString) = (findfirst(equalto(c),s)!=0)

function _searchindex(s::Union{AbstractString,ByteArray},
                      t::Union{AbstractString,Char,Int8,UInt8},
                      i::Integer)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t1, j2 = next(t,start(t))
    while true
        i = _searchindex(s,t1,i)
        if i == 0 return 0 end
        c, ii = next(s,i)
        j = j2; k = ii
        matched = true
        while !done(t,j)
            if done(s,k)
                matched = false
                break
            end
            c, k = next(s,k)
            d, j = next(t,j)
            if c != d
                matched = false
                break
            end
        end
        if matched
            return i
        end
        i = ii
    end
end

_searchindex(s::AbstractString, t::Char, i::Integer) = findnext(equalto(t), s, i)

function _search_bloom_mask(c)
    UInt64(1) << (c & 63)
end

_nthbyte(s::String, i) = codeunit(s, i)
_nthbyte(a::ByteArray, i) = a[i]

_searchindex(s::String, t::String, i::Integer) =
    _searchindex(Vector{UInt8}(s), Vector{UInt8}(t), i)

function _searchindex(s::ByteArray, t::ByteArray, i::Integer)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return findnext(equalto(_nthbyte(t,1)), s, i)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tlast = _nthbyte(t,n)
    for j in 1:n
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast && j < n
            skip = n - j - 1
        end
    end

    i -= 1
    while i <= w
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 0
            while j < n - 1
                if _nthbyte(s,i+j+1) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n - 1
                return i+1
            end

            # no match, try to rule out the next character
            if i < w && bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
            else
                i += skip
            end
        elseif i < w
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i+n+1)) == 0
                i += n
            end
        end
        i += 1
    end

    0
end

searchindex(s::ByteArray, t::ByteArray, i::Integer) = _searchindex(s,t,i)

"""
    searchindex(s::AbstractString, substring, [start::Integer])

Similar to `search`, but return only the start index at which
the substring is found, or `0` if it is not.

# Examples
```jldoctest
julia> searchindex("Hello to the world", "z")
0

julia> searchindex("JuliaLang","Julia")
1

julia> searchindex("JuliaLang","Lang")
6
```
"""
searchindex(s::AbstractString, t::AbstractString, i::Integer) = _searchindex(s,t,i)
searchindex(s::AbstractString, t::AbstractString) = searchindex(s,t,start(s))
searchindex(s::AbstractString, c::Char, i::Integer) = _searchindex(s,c,i)
searchindex(s::AbstractString, c::Char) = searchindex(s,c,start(s))

function searchindex(s::String, t::String, i::Integer=1)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use searchindex on byte arrays instead)
    if endof(t) == 1
        findnext(equalto(t[1]), s, i)
    else
        _searchindex(s, t, i)
    end
end

function _search(s, t, i::Integer)
    idx = searchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

"""
    findnext(pattern::AbstractString, string::AbstractString, [start::Integer])
    findnext(pattern::Regex, string::String, [start::Integer])

Find the first occurrence of `pattern` in `string`. `pattern` can be either a
string, or a regular expression, in which case `string` must be of type `String`.
`start` optionally specifies a starting index.

The return value is a range of indexes where the matching sequence is found, such that
`s[findnext(x, s, i)] == x`:

`findnext("substring", string, i)` = `start:end` such that
`string[start:end] == "substring"`, or `0:-1` if unmatched.

# Examples
```jldoctest
julia> findnext("z", "Hello to the world", 1)
0:-1

julia> findnext("o", "Hello to the world", 6)
8:8

julia> findnext("Julia", "JuliaLang", 2)
1:5
```
"""
findnext(t::AbstractString, s::AbstractString, i::Integer=start(s)) = _search(s, t, i)
findnext(t::ByteArray, s::ByteArray, i::Integer=start(s)) = _search(s, t, i)

function rsearch(s::AbstractString, c::Chars)
    f = c isa Char ? f = equalto(c) : x -> x in c
    j = findfirst(f, RevString(s))
    j == 0 && return 0
    endof(s)-j+1
end

"""
    rsearch(s::AbstractString, chars::Chars, [start::Integer])

Similar to `search` but returning the last occurrence of the given characters within the
given string, searching in reverse from `start`.

# Examples
```jldoctest
julia> rsearch("aaabbb","b")
6:6
```
"""
function rsearch(s::AbstractString, c::Chars, i::Integer)
    f = c isa Char ? f = equalto(c) : x -> x in c
    e = endof(s)
    j = findnext(f, RevString(s), e-i+1)
    j == 0 && return 0
    e-j+1
end

function _rsearchindex(s, t, i)
    if isempty(t)
        return 1 <= i <= nextind(s,endof(s)) ? i :
               throw(BoundsError(s, i))
    end
    t = RevString(t)
    rs = RevString(s)
    l = endof(s)
    t1, j2 = next(t,start(t))
    while true
        i = rsearch(s,t1,i)
        if i == 0 return 0 end
        c, ii = next(rs,l-i+1)
        j = j2; k = ii
        matched = true
        while !done(t,j)
            if done(rs,k)
                matched = false
                break
            end
            c, k = next(rs,k)
            d, j = next(t,j)
            if c != d
                matched = false
                break
            end
        end
        if matched
            return nextind(s,l-k+1)
        end
        i = l-ii+1
    end
end

function _rsearchindex(s::Union{String,ByteArray}, t::Union{String,ByteArray}, k)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 0 <= k <= m ? max(k, 1) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return rsearch(s, _nthbyte(t,1), k)
    end

    w = m - n
    if w < 0 || k <= 0
        return 0
    end

    bloom_mask = UInt64(0)
    skip = n - 1
    tfirst = _nthbyte(t,1)
    for j in n:-1:1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tfirst && j > 1
            skip = j - 2
        end
    end

    i = min(k - n + 1, w + 1)
    while i > 0
        if _nthbyte(s,i) == tfirst
            # check candidate
            j = 1
            while j < n
                if _nthbyte(s,i+j) != _nthbyte(t,j+1)
                    break
                end
                j += 1
            end

            # match found
            if j == n
                return i
            end

            # no match, try to rule out the next character
            if i > 1 && bloom_mask & _search_bloom_mask(_nthbyte(s,i-1)) == 0
                i -= n
            else
                i -= skip
            end
        elseif i > 1
            if bloom_mask & _search_bloom_mask(_nthbyte(s,i-1)) == 0
                i -= n
            end
        end
        i -= 1
    end

    0
end

rsearchindex(s::ByteArray, t::ByteArray, i::Integer) = _rsearchindex(s,t,i)

"""
    rsearchindex(s::AbstractString, substring, [start::Integer])

Similar to [`rsearch`](@ref), but return only the start index at which the substring is found, or `0` if it is not.

# Examples
```jldoctest
julia> rsearchindex("aaabbb","b")
6

julia> rsearchindex("aaabbb","a")
3
```
"""
rsearchindex(s::AbstractString, t::AbstractString, i::Integer) = _rsearchindex(s,t,i)
rsearchindex(s::AbstractString, t::AbstractString) = (isempty(s) && isempty(t)) ? 1 : rsearchindex(s,t,endof(s))

function rsearchindex(s::String, t::String)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use rsearchindex instead)
    if endof(t) == 1
        rsearch(s, t[1])
    else
        _rsearchindex(s, t, sizeof(s))
    end
end

function rsearchindex(s::String, t::String, i::Integer)
    # Check for fast case of a single byte
    # (for multi-byte UTF-8 sequences, use rsearchindex instead)
    if endof(t) == 1
        rsearch(s, t[1], i)
    elseif endof(t) != 0
        _rsearchindex(s, t, nextind(s, i)-1)
    elseif i > sizeof(s)
        return 0
    elseif i == 0
        return 1
    else
        return i
    end
end

function _rsearch(s, t, i::Integer)
    idx = rsearchindex(s,t,i)
    if isempty(t)
        idx:idx-1
    else
        idx:(idx > 0 ? idx + endof(t) - 1 : -1)
    end
end

rsearch(s::AbstractString, t::AbstractString, i::Integer=endof(s)) = _rsearch(s, t, i)
rsearch(s::ByteArray, t::ByteArray, i::Integer=endof(s)) = _rsearch(s, t, i)

"""
    contains(haystack::AbstractString, needle::Union{AbstractString,Char})

Determine whether the second argument is a substring of the first.

# Examples
```jldoctest
julia> contains("JuliaLang is pretty cool!", "Julia")
true
```
"""
contains(haystack::AbstractString, needle::Union{AbstractString,Char}) = searchindex(haystack,needle)!=0

in(::AbstractString, ::AbstractString) = error("use contains(x,y) for string containment")
