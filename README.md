whatwg-url
==========

[![Clojars Project](https://img.shields.io/clojars/v/dk.simongray/whatwg-url.svg)](https://clojars.org/dk.simongray/whatwg-url)

This library implements the
[WHATWG URL Standard](https://url.spec.whatwg.org/) in Clojure and
ClojureScript. It's the spec that browsers follow when they parse, resolve
and compare URLs, so you get the URL a browser would see, in a plain map.

- **The browser's parser.** The host of `http://0x7f.1/` is 127.0.0.1 here
  too, so a security check sees what the browser will fetch. Clojure's
  other URL libraries [read some URLs differently](#other-url-libraries).
- **The whole standard.** IPv4 in any base, IPv6, international domain
  names, origins, the setters of JavaScript's `URL` class, form-encoded
  query strings and public suffixes.
- **Validation errors as data.** A URL can parse and still be invalid. You
  can ask why.
- **The same results everywhere.** One codebase runs on the JVM, in Node and
  in browsers. It leans on the platform where all platforms agree and fills
  in the rest itself. It needs nothing but Clojure and passes the URL tests
  of [web-platform-tests](https://github.com/web-platform-tests/wpt/tree/master/url).

> This library brings together the URL code of
> [wary-fetch](https://github.com/simongray/wary-fetch),
> [html-pieces](https://github.com/simongray/html-pieces) and
> [microformats-clj](https://github.com/simongray/microformats-clj), which
> each implemented a part of the URL Standard. Like them, it was developed
> with assistance from frontier LLMs.

Getting started
---------------

The library needs Clojure 1.11+ and Java 11+. Add the latest release from
[Clojars](https://clojars.org/dk.simongray/whatwg-url) to the `:deps` of
your `deps.edn`:

```clojure
dk.simongray/whatwg-url {:mvn/version "0.1.0"}
```

For changes that aren't released yet, use the SHA of the latest commit on
`master` instead. In shadow-cljs a Git dependency only works through
`deps.edn`, so set `:deps true` in your `shadow-cljs.edn` too.

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

For text that isn't a URL, `url/parse` returns nil. The map only has the
parts that the URL has. Its path is a vector of segments, or a string for
an opaque path like that of `mailto:me@example.com`. To turn a map back
into text, use `url/href`:

```clojure
(url/href {:scheme "https" :host "example.com" :path ["feed.xml"] :query "page=2"})
;; => "https://example.com/feed.xml?page=2"
```

Resolving links
---------------

Resolve a link against the URL of its page with `url/resolve`, just as a
browser would:

```clojure
(url/resolve "https://example.com/blog/post" "../about")
;; => "https://example.com/about"
```

For many links on the same page, make a resolver, which parses the page's
URL only once:

```clojure
(map (url/resolver "https://example.com/blog/post") ["a" "/b" "https://x.example"])
;; => ("https://example.com/blog/a" "https://example.com/b" "https://x.example/")
```

The URL class
-------------

The getters and setters of JavaScript's `URL` class are here as functions.
Each one takes a map or the text of a URL (e.g. a string or a
`java.net.URI`):

```clojure
(url/hostname "https://Bücher.example/")
;; => "xn--bcher-kva.example"
```

The getters are `href`, `origin`, `protocol`, `username`, `password`,
`host`, `hostname`, `port`, `pathname`, `search`, `hash` and
`search-params`. There's also `url/scheme`, which is `protocol` without the
colon. As in JavaScript, `host` includes the port while the `:host` of a
map doesn't.

To change a URL, pass `url/with` the attributes to set. It sets them in
order, the way the setters of the `URL` class do (percent-encoding
included). That also makes it the way to build a URL from parts:

```clojure
(url/href (url/with "https://api.example.com"
                    :pathname "/v1/search"
                    :search-params {:q "clojure podcasts" :page 2}))
;; => "https://api.example.com/v1/search?q=clojure+podcasts&page=2"
```

A setter that refuses a value (e.g. a hostname with a port) leaves the URL
as it is. To update an attribute with a function, as with
`clojure.core/update`, use `url/update`:

```clojure
(require '[dk.simongray.whatwg-url.params :as params])

(url/href (url/update "https://example.com/?q=a&page=1" :search-params params/set "page" 2))
;; => "https://example.com/?q=a&page=2"
```

Compare URLs with `url/same-origin?`, `url/equivalent?` and
`url/path-starts-with?` (which compares paths the way cookies do).

Query strings and forms
-----------------------

The [`params`](src/dk/simongray/whatwg_url/params.cljc) namespace reads and
writes `application/x-www-form-urlencoded`, the format of query strings and
form bodies. It returns a vector of name-value pairs and also accepts a
map:

```clojure
(params/parse "q=clojure+podcasts&page=2")
;; => [["q" "clojure podcasts"] ["page" "2"]]

(params/serialize {:q "clojure podcasts" :page 2})
;; => "q=clojure+podcasts&page=2"
```

It also has the methods of JavaScript's `URLSearchParams` (e.g.
`params/get` and `params/set`). Use `url/search-params` to get the pairs of
a URL's query. To set them, use `(url/with u :search-params pairs)`.

The library always encodes a query in UTF-8. It doesn't support the legacy
encodings that HTML allows for some old documents.

Hosts
-----

The [`hosts`](src/dk/simongray/whatwg_url/hosts.cljc) namespace parses
hosts the way the URL parser does. Each step is a function of its own:

```clojure
(require '[dk.simongray.whatwg-url.hosts :as hosts])

(hosts/parse "0x7f.1")             ;; => "127.0.0.1"
(hosts/ipv4 "0x7f.1")              ;; => 2130706433
(hosts/ipv6 "::1")                 ;; => [0 0 0 0 0 0 0 1]
(hosts/domain->unicode "xn--bcher-kva.example")
;; => "bücher.example"
```

To find a public suffix or a registrable domain, load the
[Public Suffix List](https://publicsuffix.org/list/) with
`hosts/suffix-rules` and pass the rules to `hosts/public-suffix` or
`hosts/registrable-domain`. The list changes often, so you bring your own
copy.

Validation errors
-----------------

Like browsers, the parser is lenient. To check a URL that someone else
wrote, ask for its validation errors, which are the standard's error types
as keywords:

```clojure
(url/validation-errors "https://user@example.org/a b")
;; => [:invalid-credentials :invalid-url-unit]

(url/valid? "https://example.org/a%20b")
;; => true
```

International domain names
--------------------------

The library converts international domain names to ASCII with
[Unicode IDNA Compatibility Processing](https://www.unicode.org/reports/tr46/)
of Unicode 18.0.0. The [`idna`](src/dk/simongray/whatwg_url/idna.cljc)
namespace offers `to-ascii` and `to-unicode` with every option of that
spec. Punycode has a namespace of its own in
[`punycode`](src/dk/simongray/whatwg_url/punycode.cljc).

The library only carries the Unicode tables that the platforms lack or
have in an older version. For normalization (NFC) it uses the JDK or the
JavaScript engine, even when their Unicode is older. The result is still
that of Unicode 18.0.0, since NFC never changes for existing characters
and leaves most new ones (like a new emoji) alone. The rare exception is a
new character that NFC treats specially, such as a combining mark, on a
platform that doesn't know it yet. Rather than convert such a domain
wrongly, the parser refuses it.

Other URL libraries
-------------------

Here is the host that each library reads from a few URLs (with
lambdaisland/uri 1.19.155 and cemerick/url 0.1.1). Whatever differs from a
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

After `.normalize`, java.net.URI gets the first path right.

**Links resolve differently as well.** On a page at
`https://good.example/`, a browser follows the link `/\evil.example` to
evil.example. Both lambdaisland/uri and cemerick/url read it as a path on
good.example, while java.net.URI throws. A check that a redirect stays on
your own site would let this link through with either library.

For URLs that you write yourself, like the requests of an API client, any
of these libraries will do. ClojureScript also has `js/URL`, the platform's
own implementation of the standard. It doesn't return validation errors or
the URL as data, though. Its results also vary between engines and
versions. On the JVM, [galimatias](https://github.com/smola/galimatias)
implements an early version of the standard but hasn't had a release since
2015.

Principles
----------

- **Standards first.** The code follows the URL Standard of 7 October 2026
  and cites its sections. Besides the official test suites, generative
  tests check what the standard's goals promise.
- **Plain data.** URLs are maps, hosts are strings and query parameters are
  vectors, so the functions of Clojure work on all of them.
- **Convention over configuration.** By default each function does what
  the standard does for a URL. Its options change that.
- **One codebase.** The library is written in `.cljc`. It only relies on
  the platform where every platform gives the same answer, so it behaves
  the same everywhere.

Development
-----------

```bash
dev/fetch.sh                                         # once, for the test data and Unicode files
clojure -X:test                                      # the tests on the JVM
npm install                                          # once, for the Node tests
clojure -M:cljs compile test                         # the tests in Node
clojure -X:tables                                    # the Unicode tables in resources
clojure -M:bench -m dk.simongray.whatwg-url.bench    # benchmarks on the JVM
clojure -M:cljs release bench && node target/bench.js # benchmarks in Node
```

The fetch script downloads the test data of web-platform-tests, the
Unicode Character Database and the Public Suffix List into `dev-resources/`
(which Git ignores) at the versions that the library follows. Without
these files the conformance tests have nothing to check.

The public namespaces are the root, `params`, `hosts`, `percent`, `idna`
and `punycode`. The internal ones are under `impl`:

- `impl.parser`, the state machine of the parser
- `impl.serializer`, which writes a URL as text
- `impl.setters`, the setters of the URL class
- `impl.unicode`, which reads the tables
- `impl.chars`, the code points and UTF-8

License
-------

The whatwg-url project is licensed under the [MIT licence](LICENSE). The
tables in `resources/` are made from the Unicode Character Database,
under the [Unicode License v3](https://www.unicode.org/license.txt).
