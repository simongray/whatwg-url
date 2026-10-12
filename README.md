whatwg-url
==========

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/whatwg-url.svg)](https://clojars.org/dk.simongray/whatwg-url)

This is a Clojure and ClojureScript implementation of the
[WHATWG URL Standard](https://url.spec.whatwg.org/), which says how
browsers parse, resolve, write and compare URLs. URLs are plain maps.

Clojure's other URL libraries and the JDK's follow the URI RFCs or no
spec at all, so they read some URLs differently from a browser. That
matters for a URL that someone else wrote, e.g. a link whose host you
check before you fetch it, as the [comparison](#other-url-libraries)
shows.

- **The browser's parser.** A URL reads the same as in a browser, so a
  security check sees what the browser sees, e.g. the host of
  `http://0x7f.1/` is 127.0.0.1.
- **The whole standard.** Hosts with IPv4 numbers in any base, IPv6 and
  IDNA, the percent-encode sets, origins, the setters of the URL class,
  the query strings of forms, and public suffixes. A query is encoded in
  UTF-8 only, not in the legacy encodings that HTML gives some documents.
- **Validation errors as data.** A URL that parses can still be invalid,
  and you can ask why.

The same code runs on the JVM, in Node and in the browser. The platform's
own code does the work where every platform gives the same result, e.g.
NFC, and the library fills in the rest, so the results are the same on
each. There are no dependencies but Clojure, and it passes the URL tests
of [web-platform-tests](https://github.com/web-platform-tests/wpt/tree/master/url).

> This library brings together the URL code of
> [wary-fetch](https://github.com/simongray/wary-fetch),
> [html-pieces](https://github.com/simongray/html-pieces) and
> [microformats-clj](https://github.com/simongray/microformats-clj), which
> had each implemented a part of the URL Standard. Like them, it was
> developed with assistance from frontier LLMs.

Getting started
---------------

The library requires Clojure 1.11+ and Java 11+. For the latest release,
add it from [Clojars](https://clojars.org/dk.simongray/whatwg-url) to the
`:deps` in your `deps.edn`:

```clojure
dk.simongray/whatwg-url {:mvn/version "0.1.0"}
```

For changes that aren't released yet, use the SHA of the latest commit on
`master` instead:

```clojure
dk.simongray/whatwg-url
{:git/url "https://github.com/simongray/whatwg-url"
 :git/sha "…"}
```

For ClojureScript, shadow-cljs only reads Git dependencies from
`deps.edn`, so also set `:deps true` in your `shadow-cljs.edn`.

Parsing URLs
------------

Parse a URL with `url/parse`, optionally against a base:

```clojure
(require '[dk.simongray.whatwg-url :as url])

(url/parse "HTTPS://Example.COM:443/blog/../about?q=1#team")
;; => {:scheme "https" :host "example.com" :path ["about"]
;;     :query "q=1" :fragment "team"}

(url/parse "../about" "https://example.com/blog/post")
;; => {:scheme "https" :host "example.com" :path ["about"]}
```

A URL that doesn't parse gives nil. The map has only the parts the URL
has, and the path is a vector of segments, or a string for an opaque
path, e.g. that of `mailto:me@example.com`. Write a map as text with
`url/href`:

```clojure
(url/href {:scheme "https" :host "example.com" :path ["feed.xml"] :query "page=2"})
;; => "https://example.com/feed.xml?page=2"
```

Resolving links
---------------

To resolve a link against the URL of its page, as a browser does, use
`url/resolve`:

```clojure
(url/resolve "https://example.com/blog/post" "../about")
;; => "https://example.com/about"
```

To resolve many links against the same page, make a resolver, which
parses the page's URL only once:

```clojure
(map (url/resolver "https://example.com/blog/post") ["a" "/b" "https://x.example"])
;; => ("https://example.com/blog/a" "https://example.com/b" "https://x.example/")
```

The URL class
-------------

The functions named after the attributes of the standard's URL class give
their values, as `new URL(…)` does in JavaScript. They take a map, or
the text of a URL, e.g. a string or a `java.net.URI`:

```clojure
(url/hostname "https://Bücher.example/")
;; => "xn--bcher-kva.example"
```

They are `href`, `origin`, `protocol`, `username`, `password`, `host`,
`hostname`, `port`, `pathname`, `search`, `hash` and `search-params`,
and `url/scheme` gives the scheme without the colon of `protocol`. Note
that `host` is the host and port, as in the URL class, while the `:host`
of the map is the host alone.

To change a URL, give `url/with` the attributes to set, in order. Each is
set as the URL class's setter sets it, percent-encoding what needs it, so
it's also the way to build a URL from parts:

```clojure
(url/href (url/with "https://api.example.com"
                    :pathname "/v1/search"
                    :search-params {:q "clojure podcasts" :page 2}))
;; => "https://api.example.com/v1/search?q=clojure+podcasts&page=2"
```

A value that the setter refuses, e.g. a hostname with a port, leaves the
URL as it is. To change an attribute by a function of its value, as
`clojure.core/update` does, use `url/update`:

```clojure
(require '[dk.simongray.whatwg-url.params :as params])

(url/href (url/update "https://example.com/?q=a&page=1" :search-params params/set "page" 2))
;; => "https://example.com/?q=a&page=2"
```

To compare URLs, use `url/same-origin?`, `url/equivalent?`, and
`url/path-starts-with?` for a path that holds another, as cookies compare
paths.

Query strings and forms
-----------------------

The `dk.simongray.whatwg-url.params` namespace reads and writes
`application/x-www-form-urlencoded`, the format of the query of a URL and
of the body of a form. The pairs are a vector of name-value vectors, and
a map works too:

```clojure
(params/parse "q=clojure+podcasts&page=2")
;; => [["q" "clojure podcasts"] ["page" "2"]]

(params/serialize {:q "clojure podcasts" :page 2})
;; => "q=clojure+podcasts&page=2"
```

It also has the methods of the standard's URLSearchParams class, e.g.
`params/get` and `params/set`. For the pairs of a URL's query, use
`url/search-params`, and to set them, `(url/with u :search-params pairs)`.

Hosts
-----

The `dk.simongray.whatwg-url.hosts` namespace parses hosts as the URL
parser does, and has each of its steps as a function:

```clojure
(require '[dk.simongray.whatwg-url.hosts :as hosts])

(hosts/parse "0x7f.1")             ;; => "127.0.0.1"
(hosts/ipv4 "0x7f.1")              ;; => 2130706433
(hosts/ipv6 "::1")                 ;; => [0 0 0 0 0 0 0 1]
(hosts/domain->unicode "xn--bcher-kva.example")
;; => "bücher.example"
```

For a public suffix or a registrable domain, read the
[Public Suffix List](https://publicsuffix.org/list/) with
`hosts/suffix-rules` and give the rules to `hosts/public-suffix` or
`hosts/registrable-domain`. The library doesn't bundle the list, since
the list changes often.

Validation errors
-----------------

The parser is lenient, as browsers are. To check a URL that someone
wrote, ask for its validation errors, which are the error types of the
standard as keywords:

```clojure
(url/validation-errors "https://user@example.org/a b")
;; => [:invalid-credentials :invalid-url-unit]

(url/valid? "https://example.org/a%20b")
;; => true
```

IDNA and Unicode
----------------

A domain outside ASCII is converted by
[UTS #46](https://www.unicode.org/reports/tr46/) of Unicode 18.0.0, whose
tables the library carries. The `dk.simongray.whatwg-url.idna` namespace
has `to-ascii` and `to-unicode` with each flag of UTS #46, and
`dk.simongray.whatwg-url.punycode` has Punycode.

The library carries only the tables that the platforms lack or have from
an older version of Unicode, e.g. neither has UTS #46. The normalization
to NFC is the platform's, i.e. that of the JDK or of the JavaScript
engine, which may have an older version of Unicode too. NFC never
changes for the characters that a version has, and it leaves most new
characters alone, e.g. a new emoji, so the result is that of Unicode
18.0.0. The exception is a new character that NFC doesn't leave alone,
e.g. a combining mark of a new script, on a platform that doesn't know
it. Such a domain is refused rather than converted wrongly.

Other URL libraries
-------------------

This is the host that each library reads from some URLs, with
lambdaisland/uri 1.19.155 and cemerick/url 0.1.1. What differs from a
browser is in bold:

| URL | whatwg-url and browsers | lambdaisland/uri | cemerick/url | java.net.URI |
|---|---|---|---|---|
| `http://0x7f.1/` | 127.0.0.1 | **0x7f.1** | **0x7f.1** | **nil** |
| `http://2130706433/` | 127.0.0.1 | **2130706433** | **2130706433** | **2130706433** |
| `https:\\evil.example\path` | evil.example | **nil** | **empty** | **throws** |
| `https://faß.example/` | xn--fa-hia.example | **faß**.example | **faß**.example | **nil** |
| `http://[::ffff:127.0.0.1]/` | [::ffff:7f00:1] | **nil** | [::ffff:**127.0.0.1**] | [::ffff:**127.0.0.1**] |

The paths differ too:

| URL | whatwg-url and browsers | lambdaisland/uri | cemerick/url | java.net.URI |
|---|---|---|---|---|
| `http://example.com/public/../admin` | /admin | **/public/../admin** | /admin | **/public/../admin** |
| `http://example.com/public/%2e%2e/admin` | /admin | **/public/%2e%2e/admin** | **/public/%2e%2e/admin** | **/public/%2e%2e/admin** |

When normalized, java.net.URI gives /admin for the first.

**Resolving links differs as well.** On a page at
`https://good.example/`, a browser follows the link `/\evil.example` to evil.example, while
lambdaisland/uri and cemerick/url give a path on good.example and
java.net.URI throws. A check with either library that a redirect stays
on your own site would let this link through.

For URLs that you write yourself, e.g. the requests of an API client,
any of them will do. In ClojureScript, `js/URL` is the platform's own
implementation of the standard, but it gives neither validation errors
nor the URL as data, and its results vary with the engine and its
version. On the JVM, [galimatias](https://github.com/smola/galimatias)
implements an early version of the standard and was last released in
2015.

Principles
----------

- **Standards first.** The code follows the URL Standard of 7 October
  2026 and cites its sections. The tests run the URL tests of
  web-platform-tests and the conformance tests of UTS #46 and the Public
  Suffix List, and try random URLs against what the standard's goals
  promise, e.g. that parsing what the serializer wrote gives the same URL.
- **Plain data.** A URL is a map, a host is its text, and the pairs of a
  query are vectors, so you can build and take them apart with Clojure.
- **Convention over configuration.** The defaults are those of the
  standard, e.g. the flags of UTS #46 that it uses for a URL, and the
  options of each function change them.
- **One codebase.** The library is written in `.cljc` and uses the
  platform's own code only where every platform gives the same result,
  so it behaves the same everywhere.

Development
-----------

```bash
dev/fetch.sh                  # once, for the test data and Unicode files
clojure -X:test               # the tests on the JVM
npm install                   # once, for the Node tests
clojure -M:cljs compile test  # the tests in Node
clojure -X:tables             # the Unicode tables in resources
clojure -M:bench -m dk.simongray.whatwg-url.bench          # benchmarks on the JVM
clojure -M:cljs release bench && node target/bench.js      # benchmarks in Node
```

The script downloads the test data of web-platform-tests, the Unicode
Character Database and the Public Suffix List into `dev-resources/`,
which Git ignores, at the versions that the library follows. Without
them, the tests of their data check nothing.

The public namespaces are the root, `params`, `hosts`, `percent`, `idna`
and `punycode`. The others are internal: `parser` is the state machine of
the parser, `unicode` reads the tables, and `chars` has the code points
and UTF-8.

License
-------

The whatwg-url project is licensed under the [MIT licence](LICENSE). The
tables in `resources/` are made from the Unicode Character Database,
under the [Unicode License v3](https://www.unicode.org/license.txt).
