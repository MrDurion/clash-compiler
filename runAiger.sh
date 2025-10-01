CLASH_PPR_TYPES=False CLASH_PPR_QUALIFIERS=False CLASH_PPR_TICKS=False cabal run clash-ghc:clash -- --aiger ./examples/Wire.hs -fclash-clear -fclash-debug DebugFinal -fclash-spec-limit=64 
