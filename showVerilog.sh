cabal run clash -- examples/Wire.hs --verilog
yosys buildAIGER.ys
yosys -p "read_aiger circuit.aig; show"
