# [probe][]

Concurrent Workers in Haskell

## CLI

```
$ stack exec probe -- -h

The URL Checker: Concurrent URL checker
probe
  -o FILE     --output=FILE          Output file
  -f FILE     --files=FILE           Output file
  -e          --errors               Print only error messages
  -x          --external             Print only externals links (naive)
  -v          --verbose              Enable verbose messages
  -n WORKERS  --concurrency=WORKERS  Number of concurrent connections (default 16)
  -V          --version              Print version
  -h          --help                 Show help
```

## Build, test and run all the things

``` sh
# Build the project.
stack build

# Build the executable
stack build probe:exe:probe

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock

# Run the Probe
stack exec probe -- -f ./sample/seeds.txt -n 2
```

### Sample

_(clipped for readability)_

```
$ stack build probe:exe:probe &&  stack exec probe -- -f ~/Downloads/simple -n 2

The URL Checker: Concurrent URL checker
[1]:[[https://github.com/][]]
[12]:[[https://help.github.com/terms][terms of service]]
...
[13]:[[https://help.github.com/privacy][privacy policy]]
[40]:[[https://atom.io][Atom]]
[41]:[[http://electron.atom.io/][Electron]]
[43]:[[https://developer.github.com][Developers]]
...
[55]:[[https://shop.github.com][Shop]]
[56]:[[https://github.com/contact][Contact GitHub]]
[57]:[[https://github.community][GitHub Community Forum]]
[58]:[[https://help.github.com][Help]]
[59]:[[https://status.github.com/][Status]]
[60]:[[https://github.com/site/terms][Terms]]
...
[63]:[[https://services.github.com/][Training]]
Found 0 broken links. Checked 1 links (1 unique) in 1 files.
bye
```

Errors only:

```
$ stack build probe:exe:probe &&  stack exec probe -- -f ~/Downloads/simple -n 2 -e
...
ERROR: cannot parse [TagOpen "a" [("href","\\\"/features#community-management\\\""),("class","\\\"muted-link"),("alt-text-small\\\"","")],TagText "Community"]
ERROR: cannot parse [TagOpen "a" [("href","\\\"/business\\\""),("class","\\\"js-selected-navigation-item"),("HeaderNavlink",""),("px-0",""),("py-3",""),("py-lg-2",""),("m-0\\\"",""),("data-ga-click","\\\"Header,"),("click,",""),("Nav",""),("menu",""),("-",""),("item:business\\\"",""),("data-selected-links","\\\"/business"),("business",""),("security",""),("business",""),("customers",""),("business\\\"","")],TagText "\\n                  Business\\n"]
ERROR: cannot parse [TagOpen "a" [("href","\\\"/explore\\\""),("class","\\\"js-selected-navigation-item"),("HeaderNavlink",""),("px-0",""),("py-3",""),("py-lg-2",""),("m-0\\\"",""),("data-ga-click","\\\"Header,"),("click,",""),("Nav",""),("menu",""),("-",""),("item:explore\\\"",""),("data-selected-links","\\\"/explore"),("trending",""),("trending",""),("developers",""),("integrations",""),("integrations",""),("feature",""),("code",""),("integrations",""),("feature",""),("collaborate",""),("integrations",""),("feature",""),("ship",""),("showcases",""),("showcases_search",""),("showcases_landing",""),("explore\\\"","")],TagText "\\n                  Explore\\n"]
ERROR: cannot parse [TagOpen "a" [("href","\\\"/marketplace\\\""),("class","\\\"js-selected-navigation-item"),("HeaderNavlink",""),("px-0",""),("py-3",""),("py-lg-2",""),("m-0\\\"",""),("data-ga-click","\\\"Header,"),("click,",""),("Nav",""),("menu",""),("-",""),("item:marketplace\\\"",""),("data-selected-links","\\\""),("marketplace\\\"","")],TagText "\\n                      Marketplace\\n"]
..
ERROR: cannot parse [TagOpen "a" [("href","\\\"/about/careers\\\""),("class","\\\"muted-link"),("alt-text-small\\\"","")],TagText "Careers"]
ERROR: cannot parse [TagOpen "a" [("href","\\\"/about/press\\\""),("class","\\\"muted-link"),("alt-text-small\\\"","")],TagText "Press"]
ERROR: cannot parse [TagOpen "a" [("href","\\\"\\\"")],TagText "Reload"]
```

```
$ stack build probe:exe:probe &&  stack exec probe -- -f ~/Downloads/simple -n 2 -x

The URL Checker: Concurrent URL checker

...
[12]:[[https://help.github.com/terms][terms of service]]
[40]:[[https://atom.io][Atom]]
[41]:[[http://electron.atom.io/][Electron]]
[42]:[[https://desktop.github.com/][GitHub Desktop]]
[43]:[[https://developer.github.com][Developers]]
[47]:[[https://education.github.com/][Education]]
[49]:[[https://partner.github.com/][Partners]]
[50]:[[https://community.github.com/][Sponsorships]]
[55]:[[https://shop.github.com][Shop]]
[58]:[[https://help.github.com][Help]]
[59]:[[https://status.github.com/][Status]]
[63]:[[https://services.github.com/][Training]]
Found 0 broken links. Checked 1 links (1 unique) in 1 files.
```

[probe]: https://github.com/zeroed/probe
