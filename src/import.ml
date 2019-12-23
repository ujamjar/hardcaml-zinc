include Base
include Hardcaml

let clock = Signal.input "clock" 1

let clear = Signal.input "clear" 1

let ( .:[] ) x hi lo = Signal.select x hi lo
